{-# LANGUAGE ScopedTypeVariables #-}

module Schema where

import XmlParser
import qualified Data.Maybe as M
import qualified RegEx as R

------------------------------------------------------------------------
-- Type declarations
------------------------------------------------------------------------

data Schema = Schema { targetNameSpace :: Name
                     , elements        :: [Element]
                     , simpleTypes     :: [SimpleType] 
                     , complexTypes    :: [ComplexType]
                     , groups          :: [Group]
                     , attributeGroups :: [AttributeGroup]
                     } deriving (Eq,Show)

type Namespace = Name
type Value = String
type NSPrefix = String
data QName = QName NSPrefix Name deriving (Eq)
type Type = QName

data Occurs = Occurs Int | Unbounded deriving (Eq,Show)
type MinOccurs = Occurs
type MaxOccurs = Occurs

data Element 
  = ElementRef                 Name MinOccurs MaxOccurs             (Maybe SubstitutionGroup)
  | ElementWithTypeRef         Name MinOccurs MaxOccurs Type        (Maybe SubstitutionGroup)
  | ElementWithSimpleTypeDecl  Name MinOccurs MaxOccurs SimpleType  (Maybe SubstitutionGroup)
  | ElementWithComplexTypeDecl Name MinOccurs MaxOccurs ComplexType (Maybe SubstitutionGroup)
  deriving (Eq,Show)

-- Ignoring this for now, don't think it affects XML generation
data SubstitutionGroup = SubstitutionGroup deriving (Eq,Show)

data SimpleType 
  = SimpleTypeRestriction (Maybe QName) BaseType Restriction 
  | SimpleTypeList        (Maybe QName) ItemType
  | SimpleTypeUnion       (Maybe QName) [SimpleType]
  deriving (Eq,Show)

data Restriction = Enumeration [Value]
                 | Pattern R.RegEx
                 | MaxExclusive Integer
                 deriving (Eq,Show)
type BaseType = QName
type ItemType = QName

data ComplexType
  = ComplexTypeAll            (Maybe QName) All            [Attribute]
  | ComplexTypeChoice         (Maybe QName) Choice         [Attribute]
  | ComplexTypeSequence       (Maybe QName) Sequence       [Attribute]
  | ComplexTypeSimpleContent  (Maybe QName) SimpleContent  [Attribute]
  | ComplexTypeComplexContent (Maybe QName) ComplexContent [Attribute]
  deriving (Eq,Show)

data All = All [SequenceItem] deriving (Eq,Show)
data Choice = Choice MinOccurs MaxOccurs [Element] deriving (Eq,Show)
data Sequence = Sequence [SequenceItem] deriving (Eq,Show)
data SequenceItem = SIElement Element
                  | SIChoice Choice
                  deriving (Eq,Show)
data SimpleContent = SimpleContent deriving (Eq,Show)
data ComplexContent = ComplexContent deriving (Eq,Show)

data Attribute = AttributeRef Ref
               | AttributeWithTypeRef Name Type Use 
               | AttributeWithTypeDecl Name SimpleType Use
               deriving (Eq,Show)

type Ref = QName

data Use = Required | Optional | Prohibited deriving (Eq,Show)

data Group = Group Name Sequence deriving (Eq,Show)

data AttributeGroup = AttributeGroup Name [Attribute] deriving (Eq,Show)

------------------------------------------------------------------------
-- Classes and instances
------------------------------------------------------------------------

class MaybeQNamed a where
  qname :: a -> Maybe QName

instance MaybeQNamed ComplexType where
  qname (ComplexTypeAll            mqn _ _) = mqn
  qname (ComplexTypeChoice         mqn _ _) = mqn
  qname (ComplexTypeSequence       mqn _ _) = mqn
  qname (ComplexTypeSimpleContent  mqn _ _) = mqn
  qname (ComplexTypeComplexContent mqn _ _) = mqn

instance MaybeQNamed SimpleType where
  qname (SimpleTypeRestriction mqn _ _) = mqn
  qname (SimpleTypeList        mqn _)   = mqn
  qname (SimpleTypeUnion       mqn _)   = mqn

instance Show QName where
  show (QName nsp n) | nsp == "" = n
                     | otherwise = nsp ++ ":" ++ n
                     
instance Named Element where
  name (ElementRef                 n _ _ _)   = n
  name (ElementWithTypeRef         n _ _ _ _) = n
  name (ElementWithSimpleTypeDecl  n _ _ _ _) = n
  name (ElementWithComplexTypeDecl n _ _ _ _) = n

instance Named QName where
  name (QName _ n) = n

instance Renamable Element where
  rename (ElementRef n mino maxo msg) nn = 
    ElementRef nn mino maxo msg
  rename (ElementWithTypeRef n mino maxo t msg) nn = 
    ElementWithTypeRef nn mino maxo t msg
  rename (ElementWithSimpleTypeDecl n t mino maxo msg) nn = 
    ElementWithSimpleTypeDecl nn t mino maxo msg
  rename (ElementWithComplexTypeDecl n mino maxo t msg) nn = 
    ElementWithComplexTypeDecl nn mino maxo t msg

------------------------------------------------------------------------
-- Transformations of XML data types into Schema data types
------------------------------------------------------------------------

xmlDocToSchema :: XmlDoc -> Schema
xmlDocToSchema 
  x@XmlDoc 
  { version  = v
  , encoding = e
  , root     = ElmNode n as (ElmList elms)
  } = 
  Schema 
  { targetNameSpace =  lookupE "targetNamespace" as
  , elements        = [ nodeToElement        t n | n <- elms, name n == "xsd:element" ]
  , simpleTypes     = [ nodeToSimpleType     t n | n <- elms, name n == "xsd:simpleType" ]
  , complexTypes    = [ nodeToComplexType    t n | n <- elms, name n == "xsd:complexType" ]
  , groups          = [ nodeToGroup          t n | n <- elms, name n == "xsd:group" ]
  , attributeGroups = [ nodeToAttributeGroup t n | n <- elms, name n == "xsd:attributeGroup" ]
  }
  where
    t = getTargetNamespacePrefix x

getTargetNamespacePrefix :: XmlDoc -> Name
getTargetNamespacePrefix x = 
  head [ name (stringToQName an) | (an,av) <- as, av == tns
                                 , prefix (stringToQName an) == "xmlns" ]
  where
    as = attrs $ root x
    tns = lookupE "targetNamespace" as
    prefix (QName p _) = p

-- Element
nodeToElement :: Namespace -> Node -> Element
nodeToElement tns (ElmNode n as (ElmList els)) =
  let elmName = lookupE "name" as in
  case name (head els) of
    "xsd:complexType" -> ElementWithComplexTypeDecl elmName
      (minOccurs as)
      (maxOccurs as)
      (nodeToComplexType tns (head els))
      Nothing
    "xsd:simpleType" -> ElementWithSimpleTypeDecl elmName
      (minOccurs as)
      (maxOccurs as)
      (nodeToSimpleType tns (head els))
      Nothing        
nodeToElement tns (TxtNode n as s) = undefined
nodeToElement tns node@(EmpNode n as) = 
  case getAttrValue "ref" node of
    ""  -> ElementWithTypeRef (lookupE "name" as) 
                              (minOccurs as)
                              (maxOccurs as)
                              (stringToQName $ lookupE "type" as)
                              Nothing
    ref -> ElementRef ref 
                      (minOccurs as)
                      (maxOccurs as)
                      Nothing

-- Attribute
nodeToAttribute :: Namespace -> Node -> Attribute
nodeToAttribute tns node@(ElmNode n as (ElmList els)) = 
  case name (head els) of
  "xsd:simpleType" -> AttributeWithTypeDecl n
    (nodeToSimpleType tns (head els))
    (getUse node)
  v -> error $ show $head els
nodeToAttribute tns (TxtNode n as s) = undefined
nodeToAttribute tns node@(EmpNode n as) = 
  case getAttrValue "ref" node of
    ""  -> AttributeWithTypeRef (lookupE "name" as) 
                                (stringToQName $ lookupE "type" as)
                                (getUse node) -- TODO Verify that this is indeed default 
    ref -> AttributeRef $ stringToQName ref

getUse :: Node -> Use
getUse node = case getAttrValue "use" node of
                "required" -> Required
                "optional" -> Optional
                "prohibited" -> Prohibited
                _ -> Optional

-- SimpleType
nodeToSimpleType :: Namespace -> Node -> SimpleType
nodeToSimpleType tns node@(ElmNode n as (ElmList els)) =
  let ElmNode tn tas (ElmList tels) = head els in
  case tn of
    "xsd:restriction" -> SimpleTypeRestriction
      (maybeStringToMaybeQName tns $ lookup "name" as)
      (stringToQName $ lookupE "base" ras)
      restriction
      where
        rnode@(ElmNode _ ras rels) = getChild "xsd:restriction" node
        restriction = case name (head $ elems rnode) of
          "xsd:pattern" -> 
             Pattern $ R.readRegEx $ getAttrValue "value" $ getChild "xsd:pattern" rnode
          "xsd:enumeration" -> 
             Enumeration $ map (getAttrValue "value") $ elems rnode
          "xsd:maxExclusive" -> 
             MaxExclusive $ read $ getAttrValue "value" $ head $ elems rnode
    e -> error $ "Failed to convert node  to SimpleType: " ++ e

-- | ComplexType
-- | The Name parameter is the target namespace of the schema, to which the type will be added
nodeToComplexType :: Namespace -> Node -> ComplexType
nodeToComplexType tns node@(ElmNode n as (ElmList els)) =
  let atts    = [ nodeToAttribute tns an | an <- elems node, name an == "xsd:attribute" ];
      elmName = maybeStringToMaybeQName tns $ lookup "name" as in
  case name (head els) of
  "xsd:all" -> ComplexTypeAll elmName 
    (All $ map (makeSequenceItem tns) $ elems $ getChild "xsd:all" node)
    atts
  "xsd:choice" -> ComplexTypeChoice elmName
    (nodeToChoice $ head els)
    atts
  "xsd:sequence" -> ComplexTypeSequence elmName
    (Sequence $ map (makeSequenceItem tns) $ elems $ getChild "xsd:sequence" node)
    atts
  "xsd:simpleContent" -> ComplexTypeSimpleContent elmName
    (SimpleContent)
    atts
  "xsd:complexContent" -> ComplexTypeComplexContent elmName
    (ComplexContent)
    atts
  v -> error $ show $ head els

-- The Name parameter is the target namespace prefix, used if not already present in s
-- Todo find out why "fmap stringToQName" doesn't work! 
maybeStringToMaybeQName :: Name -> Maybe String -> Maybe QName
maybeStringToMaybeQName tns (Just s) = 
  Just $ case stringToQName s of
           qn@(QName "" n) -> QName tns n
           qn              -> qn
maybeStringToMaybeQName _ Nothing  = Nothing

makeSequenceItem :: Namespace -> Node -> SequenceItem
makeSequenceItem tns node@(ElmNode _ _ (ElmList els)) = 
  case name node of
    "xsd:element" -> SIElement $ nodeToElement tns node
    "xsd:choice"  -> SIChoice  $ nodeToChoice node
    s -> error $ show s
makeSequenceItem tns node@(EmpNode _ _) = SIElement $ nodeToElement tns node

nodeToChoice :: Node -> Choice
nodeToChoice node@(ElmNode _ as (ElmList els)) = 
  Choice (minOccurs as)
         (maxOccurs as)
         [] -- TODO Finish this and add remaining cases

-- Group
nodeToGroup :: Namespace -> Node -> Group
nodeToGroup tns node@(ElmNode n as (ElmList els)) =
  let elmName = lookupE "name" $ as in
  case name (head els) of
    "xsd:sequence" -> Group elmName
      (Sequence $ map (makeSequenceItem tns) $ elems $ getChild "xsd:sequence" node)
    v -> error $ show $ head els

-- AttributeGroup
nodeToAttributeGroup :: Namespace -> Node -> AttributeGroup
nodeToAttributeGroup tns node@(ElmNode n as (ElmList els)) = --TODO Take into acount tns
  AttributeGroup (lookupE "name" $ as)
                 [ nodeToAttribute tns an | an <- elems node, name an == "xsd:attribute" ]

------------------------------------------------------------------------
-- Utility functions
------------------------------------------------------------------------

minOccurs :: [Attr] -> Occurs
minOccurs as = case lookup "minOccurs" as of
                 Just o  -> if o == "unbounded" then Unbounded else Occurs (read o) 
                 Nothing -> Occurs 1

maxOccurs :: [Attr] -> Occurs
maxOccurs as = case lookup "maxOccurs" as of
                 Just o  -> if o == "unbounded" then Unbounded else Occurs (read o) 
                 Nothing -> Occurs 1

elems :: Node -> [Node]
elems (ElmNode n as (ElmList els)) = els

getChild :: Name -> Node -> Node
getChild nodeName node = lookupE nodeName $ zip (map name els) els
  where
    els = elems node

findByName :: Named a => Name -> [a] -> Maybe a
findByName _ []                   = Nothing
findByName n (e:es) | name e == n = Just e
                    | otherwise   = findByName n es

findByMaybeQName :: MaybeQNamed a => QName -> [a] -> Maybe a
findByMaybeQName _ [] = Nothing
findByMaybeQName n (e:es) | M.fromJust (qname e) == n = Just e
                          | otherwise                 = findByMaybeQName n es

stringToQName :: String -> QName
stringToQName "" = QName ""     ""
stringToQName s = case length cs of
  1 -> QName "" (head cs)
  2 -> QName (head cs) (head $ tail cs)
  where
    cs = split s ':'                           

readSchema :: String -> Schema
readSchema = xmlDocToSchema . readTree

---- Generic utilities

lookupE :: Eq a => a -> [(a,b)] -> b
lookupE a l = M.fromJust $ lookup a l

lookupD :: Eq a => a -> [(a,b)] -> b -> b
lookupD a l d = case lookup a l of
  Just b  -> b
  Nothing -> d

split :: String -> Char -> [String]
split [] delim = [""]
split (c:cs) delim
   | c == delim = "" : rest
   | otherwise = (c : head rest) : tail rest
   where
       rest = split cs delim

------------

main2 = do s <- readFile "out.xsd"
           let g = readSchema s
           return g


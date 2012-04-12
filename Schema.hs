{-# LANGUAGE ScopedTypeVariables #-}

module Schema where

import XmlParser
import qualified Data.Maybe as M
import qualified RegEx as R

------------------------------------------------------------------------
-- Type declarations
------------------------------------------------------------------------

data Schema = Schema { targetNameSpace :: Name
                     , xsdTypePrefix   :: Name
                     , elements        :: [Element]
                     , attributes      :: [Attribute]
                     , simpleTypes     :: [SimpleType] 
                     , complexTypes    :: [ComplexType]
                     , groups          :: [Group]
                     , attributeGroups :: [AttributeGroup]
                     } deriving (Eq,Show)

type Namespace = Name
type Value = String
type NSPrefix = String
data QName = QName NSPrefix Name deriving (Eq)

type TypeName     = Name
type ElementName  = Name

type QTypeName    = QName
type QElementName = QName

data Occurs = Occurs Int | Unbounded deriving (Eq,Show)
type MinOccurs = Occurs
type MaxOccurs = Occurs

-- http://www.w3.org/TR/xmlschema-1/#declare-element
data Element 
  = ElementRef                 Ref   MinOccurs MaxOccurs             (Maybe SubstitutionGroup)
  | ElementWithTypeRef         QName MinOccurs MaxOccurs QTypeName   (Maybe SubstitutionGroup)
  | ElementWithSimpleTypeDecl  QName MinOccurs MaxOccurs SimpleType  (Maybe SubstitutionGroup)
  | ElementWithComplexTypeDecl QName MinOccurs MaxOccurs ComplexType (Maybe SubstitutionGroup)
  deriving (Eq,Show)

-- Ignoring this for now, don't think it affects XML generation
data SubstitutionGroup = SubstitutionGroup deriving (Eq,Show)

data SimpleType 
  = SimpleTypeRestriction (Maybe QTypeName) BaseType Restriction 
  | SimpleTypeList        (Maybe QTypeName) ItemType
  | SimpleTypeUnion       (Maybe QTypeName) [SimpleType]
  deriving (Eq,Show)

data Restriction = Enumeration [Value]
                 | Pattern R.RegEx
                 | MaxExclusive Integer
                 deriving (Eq,Show)
type BaseType = QName
type ItemType = QName

data ComplexType
  = ComplexTypeAll            (Maybe QTypeName) All            [Attribute]
  | ComplexTypeChoice         (Maybe QTypeName) Choice         [Attribute]
  | ComplexTypeSequence       (Maybe QTypeName) Sequence       [Attribute]
  | ComplexTypeSimpleContent  (Maybe QTypeName) SimpleContent  [Attribute]
  | ComplexTypeComplexContent (Maybe QTypeName) ComplexContent [Attribute]
  deriving (Eq,Show)

data Any      = Any      Namespace MinOccurs MaxOccurs deriving (Eq,Show)
data All      = All      [Element]                     deriving (Eq,Show)
data Choice   = Choice   [Item]                        deriving (Eq,Show)
data Sequence = Sequence [Item]                        deriving (Eq,Show)

-- TODO Add support for groups http://www.w3.org/TR/2004/REC-xmlschema-1-20041028/structures.html#element-all
data Item = IElement  Element
          | IGroup    Group
          | IChoice   Choice
          | ISequence Sequence
          | IAny      Any
          deriving (Eq,Show)

data SimpleContent = SimpleContent deriving (Eq,Show)

data ComplexContent = ComplexContent deriving (Eq,Show)
 
data Attribute = AttributeRef          Ref              Use
               | AttributeWithTypeRef  QName QTypeName  Use 
               | AttributeWithTypeDecl QName SimpleType Use
               deriving (Eq,Show)

type Ref = QName

data Use = Required | Optional | Prohibited deriving (Eq,Show)

data Group = Group Name Sequence deriving (Eq,Show)

data AttributeGroup = AttributeGroup Name [Attribute] deriving (Eq,Show)

------------------------------------------------------------------------
-- Classes and instances
------------------------------------------------------------------------

class QNamed a where
  qname :: a -> QName

class MaybeQNamed a where
  mqname :: a -> Maybe QName

instance QNamed Element where
  qname (ElementRef                 n _ _ _)   = n
  qname (ElementWithTypeRef         n _ _ _ _) = n
  qname (ElementWithSimpleTypeDecl  n _ _ _ _) = n
  qname (ElementWithComplexTypeDecl n _ _ _ _) = n

instance QNamed Attribute where
  qname (AttributeRef          qn _ )  = qn
  qname (AttributeWithTypeRef  qn _ _) = qn
  qname (AttributeWithTypeDecl qn _ _) = qn

instance MaybeQNamed ComplexType where
  mqname (ComplexTypeAll            mqn _ _) = mqn
  mqname (ComplexTypeChoice         mqn _ _) = mqn
  mqname (ComplexTypeSequence       mqn _ _) = mqn
  mqname (ComplexTypeSimpleContent  mqn _ _) = mqn
  mqname (ComplexTypeComplexContent mqn _ _) = mqn

instance MaybeQNamed SimpleType where
  mqname (SimpleTypeRestriction mqn _ _) = mqn
  mqname (SimpleTypeList        mqn _)   = mqn
  mqname (SimpleTypeUnion       mqn _)   = mqn

instance Show QName where
  show (QName nsp n) | nsp == "" = n
                     | otherwise = nsp ++ ":" ++ n
                     
instance Named Element where
  name (ElementRef                 (QName _ n) _ _ _)   = n
  name (ElementWithTypeRef         (QName _ n) _ _ _ _) = n
  name (ElementWithSimpleTypeDecl  (QName _ n) _ _ _ _) = n
  name (ElementWithComplexTypeDecl (QName _ n) _ _ _ _) = n

instance Named Attribute where
  name (AttributeRef          (QName _ n) _ )  = n
  name (AttributeWithTypeRef  (QName _ n) _ _) = n
  name (AttributeWithTypeDecl (QName _ n) _ _) = n

instance Named QName where
  name (QName _ n) = n

instance Renamable Element where
  rename (ElementRef (QName p n) mino maxo msg) nn = 
    ElementRef (QName p nn) mino maxo msg
  rename (ElementWithTypeRef (QName p n) mino maxo t msg) nn = 
    ElementWithTypeRef (QName p nn) mino maxo t msg
  rename (ElementWithSimpleTypeDecl (QName p n) t mino maxo msg) nn = 
    ElementWithSimpleTypeDecl (QName p nn) t mino maxo msg
  rename (ElementWithComplexTypeDecl (QName p n) mino maxo t msg) nn = 
    ElementWithComplexTypeDecl (QName p nn) mino maxo t msg

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
  { targetNameSpace = lookupE "targetNamespace"    as
  , xsdTypePrefix   = getXsdTypePrefix x
  , elements        = convert "xsd:element"        nodeToElement
  , attributes      = convert "xsd:attribute"      nodeToAttribute
  , simpleTypes     = convert "xsd:simpleType"     nodeToSimpleType
  , complexTypes    = convert "xsd:complexType"    nodeToComplexType
  , groups          = convert "xsd:group"          nodeToGroup
  , attributeGroups = convert "xsd:attributeGroup" nodeToAttributeGroup
  }
  where
    convert schemaComponentName converter = [ converter (getTargetNamespacePrefix x) n | n <- elms, name n == schemaComponentName ]

getTargetNamespacePrefix :: XmlDoc -> Name
getTargetNamespacePrefix x = getNamespacePrefix (lookupE "targetNamespace" $ attrs $ root x) x

getXsdTypePrefix :: XmlDoc -> Name
getXsdTypePrefix x = getNamespacePrefix "http://www.w3.org/2001/XMLSchema" x

-- | Returns the namespace prefix corresponding to an xmlns declaration
getNamespacePrefix :: Namespace -> XmlDoc -> Name
getNamespacePrefix ns x =
  head [ name (stringToQName an)
       | (an,av) <- as
       , av == ns
       , prefix (stringToQName an) == "xmlns"
       ]
  where
    as = attrs $ root x
    prefix (QName p _) = p

-- Element
nodeToElement :: Namespace -> Node -> Element
nodeToElement tns (ElmNode n as (ElmList els)) =
  case name (head els) of
    "xsd:complexType" -> ElementWithComplexTypeDecl
      (QName tns $ lookupE "name" as)
      (minOccurs as) (maxOccurs as)
      (nodeToComplexType tns (head els))
      Nothing
    "xsd:simpleType" -> ElementWithSimpleTypeDecl
      (QName tns $ lookupE "name" as)
      (minOccurs as) (maxOccurs as)
      (nodeToSimpleType tns (head els))
      Nothing        
nodeToElement tns (TxtNode n as s) = error "nodeToelElement is unimplemented for TxtNode"
nodeToElement tns node@(EmpNode n as) = 
  case getAttrValue "ref" node of
    ""  -> ElementWithTypeRef (QName tns $ lookupE "name" as)
                              (minOccurs as) (maxOccurs as)
                              (stringToQName $ lookupE "type" as)
                              Nothing
    ref -> ElementRef (stringToQName ref)
                      (minOccurs as) (maxOccurs as)
                      Nothing

-- Attribute
nodeToAttribute :: Namespace -> Node -> Attribute
nodeToAttribute tns node@(ElmNode n as (ElmList els)) = 
  case name (head els) of
    "xsd:simpleType" -> 
      AttributeWithTypeDecl
        (QName tns $ lookupE "name" as)
        (nodeToSimpleType tns (head els))
        (getUse node)
    v -> error $ show $ head els
nodeToAttribute tns (TxtNode n as s) = undefined
nodeToAttribute tns node@(EmpNode n as) = 
  case getAttrValue "ref" node of
    "" -> 
      AttributeWithTypeRef 
        (QName tns $ lookupE "name" as)
        (stringToQName $ lookupE "type" as)
        (getUse node)
    ref ->
      AttributeRef
        (stringToQName ref)
        (getUse node)

getUse :: Node -> Use
getUse node = 
  case getAttrValue "use" node of
    "required"   -> Required
    "optional"   -> Optional
    "prohibited" -> Prohibited
    _            -> Optional -- TODO Verify that this is indeed default 

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

-- TODO Rewrite to support other ns prefixes for the xsd namespace
-- | ComplexType
-- | The Name parameter is the target namespace of the schema, to which the type will be added
nodeToComplexType :: Namespace -> Node -> ComplexType
nodeToComplexType tns node@(ElmNode n as (ElmList children)) =
  if null children
  then error $ "Expected non-empty indicator node in complexType: " ++ n
  else let indicator = head [ e | e <- children, isIndicator e ]
           nodeAttributes = [ nodeToAttribute tns an | an <- children, name an == "xsd:attribute" ]
           elmName = maybeStringToMaybeQName tns $ lookup "name" as in
       case name indicator of
         "xsd:all" -> ComplexTypeAll elmName 
           (All $ map (nodeToElement tns) $ elems indicator)
            nodeAttributes
         "xsd:choice" -> ComplexTypeChoice elmName
           (nodeToChoice tns indicator)
           nodeAttributes
         "xsd:sequence" -> ComplexTypeSequence elmName
           (nodeToSequence tns indicator)
           nodeAttributes
         "xsd:simpleContent" -> ComplexTypeSimpleContent elmName
           (SimpleContent) -- TODO
           nodeAttributes
         "xsd:complexContent" -> ComplexTypeComplexContent elmName
           (ComplexContent) -- TODO
           nodeAttributes
         v -> error $ show $ indicator
  where
    isIndicator = 
       (`elem` ["xsd:all", "xsd:choice", "xsd:sequence", "xsd:simpleContent", "xsd:complexContent"]) . name

-- The Name parameter is the target namespace prefix, used if not already present in s
-- Todo find out why "fmap stringToQName" doesn't work! 
maybeStringToMaybeQName :: Name -> Maybe String -> Maybe QName
maybeStringToMaybeQName tns (Just s) = 
  Just $ case stringToQName s of
           qn@(QName "" n) -> QName tns n
           qn              -> qn
maybeStringToMaybeQName _ Nothing  = Nothing

makeItem :: Namespace -> Node -> Item
makeItem tns node@(ElmNode _ _ (ElmList els)) = 
  case name node of
    "xsd:element"  -> IElement  $ nodeToElement  tns node
    "xsd:group"    -> IGroup    $ nodeToGroup    tns node
    "xsd:choice"   -> IChoice   $ nodeToChoice   tns node
    "xsd:sequence" -> ISequence $ nodeToSequence tns node
    s -> error $ "Unsupported item: " ++ show s
makeItem tns node@(EmpNode _ _) = 
  case name node of
    "xsd:element"  -> IElement  $ nodeToElement  tns node
    "xsd:any"      -> IAny      $ nodeToAny          node

nodeToChoice :: Namespace -> Node -> Choice
nodeToChoice tns node@(ElmNode _ as (ElmList els)) = Choice $ map (makeItem tns) els 

nodeToGroup :: Namespace -> Node -> Group --TODO Review this
nodeToGroup tns node@(ElmNode n as (ElmList els)) =
  let elmName = lookupE "name" $ as in
  case name (head els) of
    "xsd:sequence" -> Group elmName
      (Sequence $ map (makeItem tns) $ elems $ getChild "xsd:sequence" node)
    v -> error $ show $ head els

nodeToSequence :: Namespace -> Node -> Sequence
nodeToSequence tns node = Sequence $ map (makeItem tns) $ elems node

nodeToAny :: Node -> Any
nodeToAny (EmpNode _ as) = Any namespace (minOccurs as) (maxOccurs as)
  where
    namespace = case lookup "namespace" as of
                  Just ns -> ns
                  Nothing -> "##any"


-- AttributeGroup
nodeToAttributeGroup :: Namespace -> Node -> AttributeGroup
nodeToAttributeGroup tns node@(ElmNode n as (ElmList els)) = --TODO Take into account tns
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

findByQName :: QNamed a => QName -> [a] -> Maybe a
findByQName _ [] = Nothing
findByQName n (e:es) | qname e == n = Just e
                     | otherwise    = findByQName n es

findByMaybeQName :: MaybeQNamed a => QName -> [a] -> Maybe a
findByMaybeQName _ [] = Nothing
findByMaybeQName n (e:es) | mqname e == Just n = Just e 
                          | otherwise          = findByMaybeQName n es

lookupAttribute :: Ref -> Schema -> Attribute
lookupAttribute ref typingContext =
  case findByQName ref (attributes typingContext) of
    Just a  -> a
    Nothing -> error $ "Attribute " ++ show ref ++ " not found in schema!"

lookupElement :: Ref -> Schema -> Element
lookupElement ref typingContext =
  case findByQName ref (elements typingContext) of
    Just a  -> a
    Nothing -> error $ "Element " ++ show ref ++ " not found in schema!"

lookupSimpleType :: QTypeName -> Schema -> SimpleType
lookupSimpleType t typingContext =
  case findByMaybeQName t (simpleTypes typingContext) of
    Just a  -> a
    Nothing -> error $ "SimpleType " ++ show t ++ " not found in schema!"

lookupComplexType :: QTypeName -> Schema -> ComplexType
lookupComplexType t typingContext =
  case findByMaybeQName t (complexTypes typingContext) of
    Just a  -> a
    Nothing -> error $ "ComplexType " ++ show t ++ " not found in schema!"

stringToQName :: String -> QName
stringToQName "" = QName "" ""
stringToQName s = case length cs of
  1 -> QName "" (head cs)
  2 -> QName (head cs) (head $ tail cs)
  where
    cs = split s ':'                           

autoRootElementName :: Schema -> Name -> Name
autoRootElementName s n =
  if null n
  then let se = elements s in
       if null se
       then error "Schema contains no elements!"
       else name $ head se
  else n 

readSchema :: String -> Schema
readSchema = xmlDocToSchema . readTree

---- Generic utilities

lookupE :: (Show a, Show b, Eq a) => a -> [(a,b)] -> b
lookupE a l = 
  case lookup a l of
    Just v -> v
    Nothing -> error $ "lookupE of " ++ show a ++ " in " ++ show l ++ " failed."

lookupD :: Eq a => a -> [(a,b)] -> b -> b
lookupD a l d = 
  case lookup a l of
    Just b  -> b
    Nothing -> d

lookupEQNamed :: Show a => QNamed a => QName -> [a] -> a
lookupEQNamed n es = lookupE n $ zip (map qname es) es

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


{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

------------------------------------------------------------------------
-- QuickCheck generators for the types defined in the Schema module
------------------------------------------------------------------------

------------------------------------------------------------------------
-- TODO
-- - Implement generation of remaining two-character escapes
-- - Handing of schema hierarchies
--   - Proper handling of namespaces
-- - Generators for CharClassEsc (CCEscCat/CCEscCompl) / CharProp / CharCategory / BlockName
------------------------------------------------------------------------

module Generator where

import Schema
import qualified RegEx as R
import qualified Data.Maybe as M
import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.Gen as G
import qualified Codec.Binary.Base64.String as B64
import Data.List ( intersperse, permutations )

------------------------------------------------------------------------
-- Constants
------------------------------------------------------------------------

stringCharacters   = ['\32'..'\127'] -- TODO Add handling of full Unicode
digitCharacters    = ['0'..'9']
nonDigitCharacters = [ c | c <- stringCharacters, not $ c `elem` digitCharacters ] 

------------------------------------------------------------------------
-- Type declarations
------------------------------------------------------------------------

data XmlDoc =
    XmlDoc { version  :: String
           , encoding :: String
           , root     :: Node
           , source   :: SchemaSource
           }

data Node = ElmNode Name [Attr] [Node] NodeSource
          | TxtNode Name [Attr] String NodeSource
          | EmpNode Name [Attr]        NodeSource

type Name = String

type Attr = (Name, String, AttrSource)

--Below, 'Nothing' is used when the definition did not come from the schema
type NodeSource   = Maybe Element
type AttrSource   = Maybe Attribute
type SchemaSource = Maybe Schema

------------------------------------------------------------------------
-- Classes and instances
------------------------------------------------------------------------

class Named a where
  name :: a -> Name

class HasAttrs a where
  attrs :: a -> [Attr]

instance Named Attr where
  name (n,s,_) = n

instance Named Node where
  name (ElmNode n _ _ _) = n
  name (TxtNode n _ _ _) = n
  name (EmpNode n _   _) = n

instance Named QName where
  name (QName p n) = n

instance HasAttrs Node where
  attrs (ElmNode _ as _ _) = as
  attrs (TxtNode _ as _ _) = as
  attrs (EmpNode _ as   _) = as

instance Show Node where
  show (ElmNode n as el _) = wrap n as $ concatMap show el
  show (TxtNode n as s  _) = wrap n as s
  show (EmpNode n as    _) = wrap n as ""

instance Show XmlDoc where
  show XmlDoc { version = v, encoding = e, root = r } =
    "<?xml version='" ++ v ++ "' encoding='" ++ e ++ "' ?>" ++ show r 

wrap n as c = (begTag $ n ++ showAttrList as) ++ c ++ (endTag n)
begTag n = "<"  ++ n ++ ">"
endTag n = "</" ++ n ++ ">"

showAttrList []         = ""
showAttrList ((n,s,_):as) = " " ++ n ++ "='" ++ s ++ "'" ++ showAttrList as


------------------------------------------------------------------------
-- Generators for XML schema types
------------------------------------------------------------------------

genSchema :: Schema -> ElementName -> Q.Gen XmlDoc
genSchema s rootElementName = 
  do r <- genElement rootElement s
     -- TODO Enforce assumption that maxOccurs of rootElement is 1
     let re = head r
         (QName prefix _) = stringToQName $ name re
         r' = addAttr re ("xmlns:" ++ prefix, targetNameSpace s, Nothing)
     return XmlDoc
            { version  = "1.0" -- TODO These things should be taken from the Schema
            , encoding = "ISO-8859-1"
            , root     = r'
            , source   = Just s
            }
  where
    rootElement = (case findByName rootElementName $ elements s of
                    Nothing -> error $ "Root element '" ++ rootElementName ++ "' not found in schema!"
                    Just e  -> e)

genElement :: Element -> Schema -> Q.Gen [Node]
genElement e s = case e of
  -- TODO Implement substitution groups
  ElementRef ref mino maxo msg ->
    genElement (lookupElement ref s) s
  ElementWithTypeRef n mino maxo t@(QName prefix name) msg ->
    sizedListOf mino maxo $ 
      if isXsdType t s
      then genTxtNode (reduceQName n s) [] (genBaseType name) nodeSource
      else lookupTypeGen t s n nodeSource
  ElementWithSimpleTypeDecl n mino maxo t ms -> 
    sizedListOf mino maxo $ genTxtNodeSimpleType t s (reduceQName n s) nodeSource
  ElementWithComplexTypeDecl n mino maxo t ms -> 
    sizedListOf mino maxo $ genComplexType t s n nodeSource
  where
    nodeSource = Just e

isXsdType :: QTypeName -> Schema -> Bool 
isXsdType t@(QName prefix name) s = prefix == xsdTypePrefix s 

-- | Reduces a QName to a Name.
-- | If the element is top-level (referenced or the root) it keeps the prefix, otherwise not.
reduceQName :: QName -> Schema -> Name
reduceQName n s =
  case findByQName n (elements s) of
    Nothing -> name n
    Just _  -> show n             

genBaseType :: TypeName -> Q.Gen String
genBaseType typeName = 
  case typeName of
    "string"       -> genString
    "decimal"      -> genDecimal
    "integer"      -> showGen genInt
    "base64Binary" -> genBase64String
    other          -> error $ "Unsupported schema base type: " ++ typeName

lookupSimpleTypeGen :: QTypeName -> Schema -> Q.Gen String
lookupSimpleTypeGen typeName typingContext =
   case findByMaybeQName typeName (simpleTypes typingContext) of
     Just simpleType -> genSimpleType simpleType typingContext
     Nothing -> error $ "SimpleType " ++ show typeName ++ " not found in schema!"

lookupTypeGen :: QTypeName -> Schema -> QElementName -> NodeSource -> Q.Gen Node
lookupTypeGen typeName typingContext elmName nodeSource =
  case findByMaybeQName typeName (simpleTypes typingContext) of
    Just simpleType -> 
      genTxtNode (reduceQName elmName typingContext) [] (genSimpleType simpleType typingContext) nodeSource
    Nothing ->
      case findByMaybeQName typeName (complexTypes typingContext) of
        Just complexType -> genComplexType complexType typingContext elmName nodeSource
        Nothing -> error $ "Type " ++ show typeName ++ " not found in schema!"

lookupAttributeGen :: Ref -> Schema -> Q.Gen (Maybe Attr)
lookupAttributeGen ref s = genAttribute (lookupAttribute ref s) s 

genAttribute :: Attribute -> Schema -> Q.Gen (Maybe Attr)
genAttribute a s = case a of
  AttributeRef ref use -> genAttributeAux (lookupAttribute ref s) $ Just use
  a                    -> genAttributeAux a                         Nothing
  where
    genAttributeAux aa useOvveride = case aa of
      AttributeWithTypeRef aQName aType use ->
        useToMaybe (lookupSimpleTypeGen aType s) 
                   (prefer useOvveride use)
                   (\v -> Just (name aQName, v, attrSource))
      AttributeWithTypeDecl aQName simpleType use ->
        useToMaybe (genSimpleType  simpleType s)
                   (prefer useOvveride use)
                   (\v -> Just (name aQName, v, attrSource))
      where
        attrSource = Just aa

    prefer (Just a) _ = a 
    prefer Nothing  b = b
    
    useToMaybe :: Q.Gen a -> Use -> (a -> Maybe b) -> Q.Gen (Maybe b)
    useToMaybe gen use wrapper =
      case use of
        Required -> 
          do v <- gen
             return $ wrapper v
        Optional -> 
          do b <- Q.arbitrary
             if b then do v <- gen
                          return $ wrapper v
                  else return Nothing
        Prohibited -> return Nothing

genComplexType :: ComplexType -> Schema -> QElementName -> NodeSource -> Q.Gen Node
genComplexType t typingContext elmName nodeSource = case t of
  ComplexTypeAll mn (All es) as -> 
    do i <- Q.elements [0..(length es - 1)]
       e <- genElmNode elmName (nodeGenPerms !! i) attrGens typingContext nodeSource
       return e
       where
         nodeGenPerms :: [[Q.Gen [Node]]]
         nodeGenPerms = permutations $ map (\e -> genElement e typingContext) es
         attrGens :: [Q.Gen (Maybe Attr)]
         attrGens = map (\a -> genAttribute a typingContext) as
  ComplexTypeSequence mn (Sequence sequenceItems) as -> 
    genElmNode elmName nodeGens attrGens typingContext nodeSource
    where
      nodeGens :: [Q.Gen [Node]]
      nodeGens = map (\i -> genItem i typingContext nodeSource) sequenceItems
      attrGens :: [Q.Gen (Maybe Attr)]
      attrGens = map (\a -> genAttribute a typingContext) as


genItem :: Item -> Schema -> NodeSource -> Q.Gen [Node]
genItem i s nodeSource =
  case i of
    IElement  x -> genElement  x s
    IGroup    x -> error "Unimplemented function: genGroup"
    IChoice   x -> genChoice   x s nodeSource
    ISequence x -> genSequence x s nodeSource
    IAny      x -> error "Unimplemented function: genAny"

genChoice :: Choice -> Schema -> NodeSource -> Q.Gen [Node]
genChoice (Choice items) s nodeSource =
  do c <- Q.oneof $ map (\i -> genItem i s nodeSource) items 
     return c

genSequence :: Sequence -> Schema -> NodeSource -> Q.Gen [Node]
genSequence (Sequence items) s nodeSource = concatGens $ map (\i -> genItem i s nodeSource) items

------------------------------------------------------------------------
-- Generators for XML document types
------------------------------------------------------------------------

genElmNode :: QElementName -> [Q.Gen [Node]] -> [Q.Gen (Maybe Attr)] -> Schema -> NodeSource -> Q.Gen Node
genElmNode name childNodeGens attributeGens s nodeSource = 
  do attrs <- foldGen attributeGens 
                      (\ma as -> case ma of
                                   Just a  -> as ++ [a]
                                   Nothing -> as)
     nodes <- concatGens childNodeGens
     return $ ElmNode (reduceQName name s) attrs nodes nodeSource

genTxtNode :: ElementName -> [Attr] -> Q.Gen String -> NodeSource -> Q.Gen Node
genTxtNode n as gen nodeSource = 
  do s <- gen
     return $ TxtNode n [] (xmlEncode s) nodeSource

-- TODO Implement this more efficiently!
genDecimal :: Q.Gen String
genDecimal = genMatch $ R.readRegEx "(\\+|-)?\\d+(\\.\\d+)?"

-- TODO Replace this wasteful impl by one that directly generates a valid Base64 string
genBase64String :: Q.Gen String
genBase64String = 
  do s <- genString
     return $ B64.encode s

genInt :: Q.Gen String
genInt = do (i::Int) <- Q.arbitrary
            return $ show i

genTxtNodeSimpleType :: SimpleType -> Schema -> ElementName -> NodeSource -> Q.Gen Node 
genTxtNodeSimpleType t typingContext n nodeSource =
  do s <- genSimpleType t typingContext
     return $ TxtNode n [] s nodeSource

genSimpleType :: SimpleType -> Schema -> G.Gen String
genSimpleType t typingContext   = 
  case t of
    -- TODO Check that this is really used
    (SimpleTypeRestriction (Just (QName prefix name)) baseType restriction) ->
      case restriction of
        Enumeration vs -> Q.oneof $ map return vs
        Pattern      r -> genMatch r
        MaxExclusive i -> error "MaxExclusive generation not implemented"
    (SimpleTypeRestriction Nothing baseType restriction) ->
      case restriction of
        Enumeration vs -> Q.oneof $ map return vs
        Pattern      r -> genMatch r
        MaxExclusive i -> error "MaxExclusive generation not implemented"
    -- (SimpleTypeList  (Maybe QName) ItemType) ->
    -- (SimpleTypeUnion       (Maybe QName) [SimpleType]) ->

-- TODO Add map of string chars to generate with as optional CLI parameter
genString :: Q.Gen String
genString = 
  do s <- Q.listOf $ Q.elements stringCharacters
     return $ xmlEncode s

xmlEncode :: String -> String
xmlEncode [] = []
xmlEncode (c:cs) = 
  case l of
    Nothing -> c :  xmlEncode cs
    Just e  -> e ++ xmlEncode cs
    where
      l = lookup c [ ('\"',"&quot;")
                   , ('\'',"&apos;")
                   , ('<' ,"&lt;")
                   , ('>' ,"&gt;")
                   , ('&' ,"&amp;")
                   ]

------------------------------------------------------------------------
-- Generator of strings matching a regular expression
------------------------------------------------------------------------

genMatch :: R.RegEx -> Q.Gen String
genMatch (R.Choice bs) = Q.oneof $ map genBranch bs

genBranch :: R.Branch -> Q.Gen String
genBranch (R.Branch ps) = concatGens $ map genPiece ps --TODO Also wrap with concatGen?

genPiece :: R.Piece -> Q.Gen String
genPiece (R.Piece a  Nothing) = genAtom a 
genPiece (R.Piece a (Just qi)) =
  case qi of
    R.QQuestionMark       -> concatGen $ sizedListOf (Occurs 0)   (Occurs 1)   $ genAtom a
    R.QStar               -> concatGen $ Q.listOf                              $ genAtom a
    R.QPlus               -> concatGen $ Q.listOf1                             $ genAtom a
    R.QQuantRange min max -> concatGen $ sizedListOf (Occurs min) (Occurs max) $ genAtom a
    R.QQuantMin   min     -> concatGen $ sizedListOf (Occurs min) Unbounded    $ genAtom a
    R.QQuantExact size    -> concatGen $ Q.vectorOf  size                      $ genAtom a

genAtom :: R.Atom -> Q.Gen String
genAtom a = 
  case a of
    R.AChar      c  -> return [c]
    R.ACharClass cc -> genCharClass cc
    R.ABrackets  re -> genMatch re

genCharClass :: R.CharClass -> Q.Gen String
genCharClass cc =
  case cc of
    R.CClassEsc  ccesc  -> genCharClassEsc  ccesc
    R.CClassExpr ccexpr -> genCharClassExpr ccexpr
    -- TODO Add support for the dot-all flas <<s>> (see Kay book Atoms section)
    R.CClassDot         -> Q.elements stringCharacters >>= \c -> return [c]
    -- TODO Add support for the multiline flag <<m>> (see Kay book Atoms section)
    R.CClassHat         -> error "TODO Add support for beginning-matching using ^"
    R.CClassDollar      -> error "TODO Add support for end-matching using $"

genCharClassEsc :: R.CharClassEsc -> Q.Gen String
genCharClassEsc ccesc =
  case ccesc of
    R.CCEscSingleChar sce -> genSingleCharEsc sce >>= \c -> return [c]
    R.CCEscMultiChar  c   -> 
      case c of
        's' -> singletonOf [' ','\t','\n','\r']
        --'S' -> -- TODO Implement using ^
        --'i' -> 
        --'I' -> -- TODO Implement using ^
        --'c' -> 
        --'C' -> -- TODO Implement using ^
        'd' -> singletonOf digitCharacters
        -- TODO Reimpl. using ^
        'D' -> Q.elements nonDigitCharacters >>= \c -> return $ xmlEncode [c] 
        --'w' -> 
        --'W' -> -- TODO Implement using ^
    R.CCEscCat        cp  -> error "Generator for character categories not implemented."
    R.CCEscCompl      cp  -> error "Generator for character category compl. not implemented."

genCharClassExpr :: R.CharClassExpr -> Q.Gen String
genCharClassExpr (R.CharClassExpr cg) = genCharGroup cg

genCharGroup :: R.CharGroup -> Q.Gen String
genCharGroup cg =
  case cg of
    R.CGroupPos          pcg -> genPosCharGroup pcg
    R.CGroupNeg          ncg -> genNegCharGroup ncg
    R.CGroupCharClassSub ccs -> genCharClassSub ccs

genPosCharGroup :: R.PosCharGroup -> Q.Gen String
genPosCharGroup (R.PosCharGroup pcgis) = Q.oneof $ map genPosCharGroupItem pcgis

genPosCharGroupItem :: R.PosCharGroupItem -> Q.Gen String
genPosCharGroupItem i =
  case i of
    R.PosCharGroupRange        r   -> genCharRange    r
    R.PosCharGroupCharClassEsc cce -> genCharClassEsc cce

genCharRange :: R.CharRange -> Q.Gen String
genCharRange cr =
  case cr of
    R.CodePointRange lo hi -> do l <- genCharOrEsc lo
                                 h <- genCharOrEsc hi
                                 c <- Q.elements [l..h] --TODO Test this, esp. on unicode
                                 return [c] 
    R.XmlCharIncDash c     -> return [c] 

genCharOrEsc :: R.CharOrEsc -> Q.Gen Char
genCharOrEsc coe =
  case coe of
    R.CharOrEscXmlChar       c   -> return c
    R.CharOrEscSingleCharEsc sce -> genSingleCharEsc sce

genNegCharGroup :: R.NegCharGroup -> Q.Gen String
genNegCharGroup (R.NegCharGroup pcg) = error "TODO Implement genNegCharGroup."

genCharClassSub :: R.CharClassSub -> Q.Gen String
genCharClassSub ccs = error "TODO Implement genCharClassSub."

genSingleCharEsc :: R.SingleCharEsc -> Q.Gen Char
genSingleCharEsc (R.SingleCharEsc c) = return c

------------------------------------------------------------------------
-- Generator combinators
------------------------------------------------------------------------

sizedListOf :: Occurs -> Occurs -> Q.Gen a -> Q.Gen [a]
sizedListOf min max g =
  let (init,mino) = case min of
                      Occurs mino -> (Q.vectorOf mino g, mino)
                      -- TODO Check if this is even a valid value here.
                      Unbounded   -> (Q.listOf g,        0)
      rest = case max of
               -- TODO Check max >= min during parsing 
               Occurs maxo -> Q.resize (maxo - mino) $ Q.listOf g
               Unbounded   -> Q.listOf g
  in do i <- init
        r <- rest
        return $ i ++ r

showGen :: (Show a) => Q.Gen a -> Q.Gen String
showGen gen = 
  do s <- gen
     return $ show s

concatGens :: [Q.Gen [a]] -> Q.Gen [a]
concatGens gs = foldGen gs (\v vs -> vs ++ v)

concatGen :: Q.Gen [[a]] -> Q.Gen [a]
concatGen gen = do vs <- gen
                   return $ concat vs

foldGen :: [Q.Gen b] -> (b -> [a] -> [a]) -> Q.Gen [a]
foldGen gs f = aux gs []
  where
    aux gens vals =
      if null gens
      then return vals 
      else do v <- head gens
              aux (tail gens) (f v vals)

singletonOf :: [a] -> Q.Gen [a]
singletonOf list = Q.elements list >>= \c -> return [c]

-- coinFlip :: Q.Gen a -> a -> Q.Gen a
-- coinFlip gen dflt = Q.oneof [True,False] >>= \c -> if c then return c else gen
     

------------------------------------------------------------------------
-- Utility generators
------------------------------------------------------------------------

-- Based on http://www.w3.org/TR/REC-xml-names/#NT-NCName, TODO Make this really compliant
genNCName :: G.Gen Name
genNCName = 
  do start <- Q.elements nameStartChars
     rest  <- Q.listOf $ Q.elements nameChars
     return $ start : rest
  where
    nameStartChars = 
      concat [ [':'], ['A'..'Z'], ['_'], ['a'..'z'] ]
             {- TODO Uncomment to make tests more exhaustive
             , ['\xC0'..'\xD6'],     ['\xD8'..'\xF6'],     ['\xF8'..'\x2FF']
             , ['\x370'..'\x37D'],   ['\x37F'..'\x1FFF'],  ['\x200C'..'\x200D']
             , ['\x2070'..'\x218F'], ['\x2C00'..'\x2FEF'], ['\x3001'..'\xD7FF']
             , ['\xF900'..'\xFDCF'], ['\xFDF0'..'\xFFFD'], ['\x10000'..'\xEFFFF']
             ]
             -}
    nameChars = nameStartChars ++ concat [ ['-'], ['.'], ['0'..'9'], ['\xB7'] ]
                                         {- TODO Uncomment to make tests more exhaustive
                                         , ['\x0300'..'\x036F'], ['\x203F'..'\x2040']]
                                         -}

------------------------------------------------------------------------
-- Utility functions
------------------------------------------------------------------------

addAttr :: Node -> Attr -> Node
addAttr (ElmNode n as el s) a = ElmNode n (a:as) el s
addAttr (TxtNode n as el s) a = TxtNode n (a:as) el s
addAttr (EmpNode n as    s) a = EmpNode n (a:as)    s

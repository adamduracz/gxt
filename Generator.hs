{-# LANGUAGE ScopedTypeVariables #-}

------------------------------------------------------------------------
-- QuickCheck generators for the types defined in the Schema module
------------------------------------------------------------------------

------------------------------------------------------------------------
-- TODO
-- - Implement generation of remaining two-character escapes
------------------------------------------------------------------------

module Generator where

import Schema
import XmlParser -- TODO Can I get Schema to re-export this instead?
import qualified RegEx as R
import qualified Data.Maybe as M
import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.Gen as G
import qualified Codec.Binary.Base64.String as B64
import Data.List (intersperse)

------------------------------------------------------------------------
-- Constants
------------------------------------------------------------------------

stringCharacters   = ['\32'..'\127'] -- TODO Add handling of full Unicode
digitCharacters    = ['0'..'9']
nonDigitCharacters = [ c | c <- stringCharacters, not $ c `elem` digitCharacters ] 

------------------------------------------------------------------------
-- Test functions
------------------------------------------------------------------------

main = do s <- readFile "out.xsd"
          let schema@Schema { targetNameSpace = name
                            , attributes      = as
                            , elements        = es
                            , simpleTypes     = sts
                            , complexTypes    = cts
                            , groups          = gs
                            , attributeGroups = ags
                            } = readSchema s
              gen = genSchema schema "priceList"
          Q.sample gen
          return ()

------------------------------------------------------------------------
-- Generators for XML schema types
------------------------------------------------------------------------

genSchema :: Schema -> Name -> Q.Gen XmlDoc
genSchema s rootElementName = 
  do r <- genElement rootElement s
     -- TODO Figure out how to do proper handling of namespaces
     -- TODO Enforce assumption that maxOccurs of rootElement is 1
     let r' = addAttribute (head r) ("xmlns:p", targetNameSpace s)
     return XmlDoc
            { version  = "1.0" -- TODO These things should be taken from the Schema
            , encoding = "ISO-8859-1"
            , root = r'
            }
  where
    rootElement =
      rename (case findByName rootElementName $ elements s of
                Nothing -> error $ "Root element '" ++ rootElementName ++ "' not found in schema!"
                Just e  -> e) 
             ("p:" ++ rootElementName) -- TODO Figure out how to do proper handling of namespaces

genElement :: Element -> Schema -> Q.Gen [Node]
genElement e s = case e of
  -- TODO Implement substitution groups
  ElementRef ref mino maxo msg ->
    genElement (lookupElement ref s) s 
  ElementWithTypeRef n mino maxo t@(QName prefix name) msg ->
    case prefix of
      -- Built in XML Schema base types
      "xsd" -> sizedListOf mino maxo $ genTxtNode n [] $ genBaseType name
      -- Types defined in the schema
      _     -> sizedListOf mino maxo $ lookupTypeGen t s n
  ElementWithSimpleTypeDecl n mino maxo t ms -> 
    sizedListOf mino maxo $ genTxtNodeSimpleType t s n
  ElementWithComplexTypeDecl n mino maxo t ms -> 
    sizedListOf mino maxo $ genComplexType t s n

genBaseType :: String -> Q.Gen String
genBaseType typeName = 
  case typeName of
    "string"       -> genString
    "decimal"      -> genDecimal
    "integer"      -> showGen genInt
    "base64Binary" -> genBase64String
    other          -> error $ "Unsupported schema base type: " ++ typeName

lookupSimpleTypeGen :: QName -> Schema -> Q.Gen String
lookupSimpleTypeGen typeName typingContext =
   case findByMaybeQName typeName (simpleTypes typingContext) of
     Just simpleType -> genSimpleType simpleType typingContext
     Nothing -> error $ "SimpleType " ++ show typeName ++ " not found in schema!"

lookupTypeGen :: QName -> Schema -> QName -> Q.Gen Node
lookupTypeGen typeName typingContext elmName =
  case findByMaybeQName typeName (simpleTypes typingContext) of
    Just simpleType -> genTxtNode elmName [] $ genSimpleType simpleType typingContext          
    Nothing ->
      case findByMaybeQName typeName (complexTypes typingContext) of
        Just complexType -> genComplexType complexType typingContext elmName
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
        useToMaybe (lookupSimpleTypeGen aType s) (prefer useOvveride use) (\v -> Just (name aQName, v))
      AttributeWithTypeDecl aQName simpleType use ->
        useToMaybe (genSimpleType  simpleType s) (prefer useOvveride use) (\v -> Just (name aQName, v))
    
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
             if b 
             then do v <- gen
                     return $ wrapper v
             else return Nothing
        Prohibited -> return Nothing

genComplexType :: ComplexType -> Schema -> QName -> Q.Gen Node
genComplexType t typingContext elmName = case t of
  ComplexTypeSequence mn (Sequence sequenceItems) as -> 
    genElmNode elmName sisNodeGens attrGens
    where
      sisNodeGens :: [Q.Gen [Node]]
      sisNodeGens = map (\si -> case si of
                                  SIElement e -> genElement e typingContext
                                  SIChoice  c -> genChoice  c typingContext)
                        sequenceItems
      attrGens    :: [Q.Gen (Maybe Attr)]
      attrGens    = map (\a -> genAttribute a typingContext) as

genChoice :: Choice -> Schema -> Q.Gen [Node]
genChoice = error "Choice is not yet supported!" -- TODO Implement Choice

------------------------------------------------------------------------
-- Generators for XML document types
------------------------------------------------------------------------

genElmNode :: QName -> [Q.Gen [Node]] -> [Q.Gen (Maybe Attr)] -> Q.Gen Node
genElmNode name childNodeGens attrGens = genElmNodeAux childNodeGens [] attrGens []
  where
    genElmNodeAux :: [Q.Gen [Node]]       -> [Node] 
                  -> [Q.Gen (Maybe Attr)] -> [Attr] 
                  ->  Q.Gen Node
    genElmNodeAux nGens nodes aGens attrs =
      if null aGens then
        if null nGens then
          return $ ElmNode (show name) attrs $ ElmList $ nodes 
        else
          do n <- head nGens
             genElmNodeAux (tail nGens) (nodes ++ n) aGens attrs
      else       
        do ma <- head aGens
           genElmNodeAux nGens nodes (tail aGens) (case ma of 
                                                     Just a  -> attrs ++ [a]
                                                     Nothing -> attrs)

genTxtNode :: QName -> [Attr] -> Q.Gen String -> Q.Gen Node
genTxtNode n as gen = 
  do s <- gen
     return $ TxtNode (show n) [] $ xmlEncode s

-- TODO Implement this more efficiently!
genDecimal :: Q.Gen String
genDecimal = genMatch $ R.readRegEx "(\\+|-)?\\d*(\\.)?\\d+"

-- TODO Replace this wasteful impl by one that directly generates a valid Base64 string
genBase64String :: Q.Gen String
genBase64String = 
  do s <- genString
     return $ B64.encode s

genInt :: Q.Gen String
genInt = do (i::Int) <- Q.arbitrary
            return $ show i

genTxtNodeSimpleType :: SimpleType -> Schema -> QName -> Q.Gen Node 
genTxtNodeSimpleType t typingContext n =
  do s <- genSimpleType t typingContext
     return $ TxtNode (show n) [] s

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

genMatch :: R.RegEx -> Q.Gen String
genMatch r = case r of
  R.Literal c -> case c of
    '.' -> Q.elements stringCharacters >>= \c -> return $ xmlEncode [c] 
    c   -> return [c]
  R.SCEscape c -> return [c]
  R.TCEscape c -> 
    case c of
      --'s'
      --'S'
      --'i'
      --'I'
      --'c'
      --'C'
      'd' -> Q.elements    digitCharacters >>= \c -> return [c]
      'D' -> Q.elements nonDigitCharacters >>= \c -> return $ xmlEncode [c]
      --'w'
      --'W'
  R.Sequence rs -> 
    do ms <- sequence $ map genMatch rs
       return $ concat ms
  R.Choice rs -> Q.oneof $ map genMatch rs 
  R.Repeat (min, Just max) rr -> 
    do ms <- sizedListOf (Occurs min) (Occurs max) $ genMatch rr
       return $ concat ms
  R.Repeat (min, Nothing) rr -> 
    do init <- Q.vectorOf min $ genMatch rr
       rest <- Q.listOf       $ genMatch rr
       return $ concat init ++ concat rest
  
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

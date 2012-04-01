{-# LANGUAGE ScopedTypeVariables #-}

------------------------------------------------------------------------
-- QuickCheck generators for the types defined in the Schema module
------------------------------------------------------------------------

------------------------------------------------------------------------
-- TODO
-- - Add generation of attributes
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

------------------------------------------------------------------------
-- Constants
------------------------------------------------------------------------

stringCharacters   = ['\32'..'\254']
digitCharacters    = ['0'..'9']
nonDigitCharacters = [ c | c <- stringCharacters, not $ c `elem` digitCharacters ] 

------------------------------------------------------------------------
-- Test functions
------------------------------------------------------------------------

elgen :: String -> Q.Gen [Node]
elgen s = let context@
                (Schema { targetNameSpace = name
                       , elements         = es
                       , simpleTypes      = sts
                       , complexTypes     = cts
                       , groups           = gs
                       , attributeGroups  = ags
                       }) = readSchema s
          in mkElementGen (M.fromJust $ findByName "priceList" es) context

main = do s <- readFile "out.xsd"
          let schema@
                Schema { targetNameSpace = name
                       , elements        = es
                       , simpleTypes     = sts
                       , complexTypes    = cts
                       , groups          = gs
                       , attributeGroups = ags
                       } = readSchema s
              gen = mkSchemaGen schema "priceList"
          Q.sample gen
          return ()

------------------------------------------------------------------------
-- Generators for XML schema types
------------------------------------------------------------------------

mkSchemaGen :: Schema -> Name -> Q.Gen XmlDoc
mkSchemaGen s rootElementName = 
  do r <- mkElementGen rootElement s
     -- TODO Figure out how to do proper handling of namespaces
     -- TODO Enforce assumption that maxOccurs of rootElement is 1
     let r' = addAttribute (head r) ("xmlns:p", targetNameSpace s)
     return XmlDoc
            { version  = "1.0" -- TODO These things should be taken from the Schema
            , encoding = "ISO-8859-1"
            , root = r'
            }
    where
    rootElement = -- TODO Figure out how to do proper handling of namespaces
      rename (M.fromJust $ findByName rootElementName $ elements s) ("p:" ++ rootElementName)

mkElementGen :: Element -> Schema -> Q.Gen [Node]
mkElementGen e typingContext = case e of
  -- TODO Implement substitution groups
  ElementRef n _ _ _ -> return [TxtNode n [] ""] -- TODO
  ElementWithTypeRef n mino maxo t@(QName prefix name) msg ->
    case prefix of
     -- Built in XML Schema base types
      "xsd" -> case name of
        "string"       -> sizedListOf mino maxo $ genTxtNodeString n
        "decimal"      -> sizedListOf mino maxo $ genTxtNodeDecimal n
        "integer"      -> sizedListOf mino maxo $ genTxtNodeInteger n
        "base64Binary" -> sizedListOf mino maxo $ genTxtNodeBase64String n
        other     -> error $ "Unsupported schema base type: " ++ (show t)
      -- Types defined in the schema
      custom -> sizedListOf mino maxo $ lookupTypeAndMkGen t typingContext n
  ElementWithSimpleTypeDecl n mino maxo t ms -> 
    sizedListOf mino maxo $ genTxtNodeSimpleType t typingContext n
  ElementWithComplexTypeDecl n mino maxo t ms -> 
    sizedListOf mino maxo $ mkComplexTypeGen t typingContext n

lookupTypeAndMkGen :: QName -> Schema -> Name -> Q.Gen Node
lookupTypeAndMkGen typeName typingContext elmName =
  case findByMaybeQName typeName (simpleTypes typingContext) of
    Just simpleType -> genTxtNodeSimpleType simpleType typingContext elmName          
    Nothing ->
      case findByMaybeQName typeName (complexTypes typingContext) of
        Just complexType -> mkComplexTypeGen complexType typingContext elmName
        Nothing -> error $ "Type " ++ show typeName ++ " not found in schema!"

mkComplexTypeGen :: ComplexType -> Schema -> Name -> Q.Gen Node
mkComplexTypeGen t typingContext elmName = case t of
  ComplexTypeSequence mn (Sequence sequenceItems) as -> 
    genElmNode elmName sisNodeGens
    where
      sisNodeGens :: [Q.Gen [Node]]
      sisNodeGens = map (\si -> case si of
                                  SIElement e -> mkElementGen e typingContext
                                  SIChoice  c -> mkChoiceGen  c typingContext)
                        sequenceItems

mkChoiceGen :: Choice -> Schema -> Q.Gen [Node]
mkChoiceGen = error "Choice is not yet supported!"

------------------------------------------------------------------------
-- Generators for XML document types
------------------------------------------------------------------------

genElmNode :: Name -> [Q.Gen [Node]] -> Q.Gen Node
genElmNode n sisNodeGens = genElmNodeAux sisNodeGens []
  where
    genElmNodeAux gens nodes =
      if null gens then
        -- TODO Replace [] with generated attributes
        return $ ElmNode n [] $ ElmList $ nodes 
      else       
        do n <- head gens
           genElmNodeAux (tail gens) (nodes ++ n)

-- TODO Implement this more efficiently!
genTxtNodeDecimal :: Name -> Q.Gen Node
genTxtNodeDecimal n = genTxtNode n [] $ genMatch $ R.readRegEx "(\\+|-)?\\d*(\\.)?\\d+"

genTxtNode :: (Show a) => Name -> [Attr] -> Q.Gen a -> Q.Gen Node
genTxtNode n as gen = 
  do s <- gen
     return $ TxtNode n [] $ show s

genTxtNodeString :: Name -> Q.Gen Node
genTxtNodeString n = 
  do s <- genString
     return $ TxtNode n [] s

-- TODO Replace this wasteful impl by one that directly generates a valid Base64 string
genTxtNodeBase64String :: Name -> Q.Gen Node
genTxtNodeBase64String n = 
  do s <- genString
     return $ TxtNode n [] $ B64.encode s

genTxtNodeInteger :: Name -> Q.Gen Node
genTxtNodeInteger n = genTxtNode n [] genInt

genInt :: Q.Gen Int
genInt =
  do (i::Int) <- Q.arbitrary
     return i 

genTxtNodeSimpleType :: SimpleType -> Schema -> Name -> Q.Gen Node 
genTxtNodeSimpleType t typingContext n =
  do s <- genSimpleType t typingContext
     return $ TxtNode n [] s

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
    -- (SimpleTypeList        (Maybe QName) ItemType) ->
    -- (SimpleTypeUnion       (Maybe QName) [SimpleType]) ->

genMatch :: R.RegEx -> Q.Gen String
genMatch r = case r of
  R.Literal c -> case c of
    '.' -> Q.elements stringCharacters >>= \c -> return [c] 
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
      'd' -> Q.elements digitCharacters >>= \c -> return [c]
      'D' -> Q.elements nonDigitCharacters >>= \c -> return [c]
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
genString = do s <- Q.listOf $ Q.elements stringCharacters
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

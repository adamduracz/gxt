{-# LANGUAGE ScopedTypeVariables #-}

------------------------------------------------------------------------
-- QuickCheck generators for the types defined in the Schema module
------------------------------------------------------------------------

module Generator where

import Schema
import XmlParser -- TODO Can I get Schema to re-export this instead?
import qualified Data.Maybe as M
import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.Gen as G

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
  ElementWithTypeRef n t@(QName prefix name) mino maxo msg ->
    case prefix of
     -- Built in XML Schema base types
      "xsd" -> case name of
        "string"  -> sizedListOf mino maxo $ genTxtNodeString n
        "integer" -> sizedListOf mino maxo $ genTxtNodeInteger n
        other     -> error $ "Unsupported schema base type: " ++ (show t)
      -- Types defined in the schema
      custom -> sizedListOf mino maxo $ lookupTypeAndMkGen t typingContext n
  ElementWithSimpleTypeDecl n _ _ -> 
    return [TxtNode n [] "IAmSimpleTyped"] -- TODO
  ElementWithComplexTypeDecl n mino maxo t ms -> 
    sizedListOf mino maxo $ mkComplexTypeGen t typingContext n

lookupTypeAndMkGen :: QName -> Schema -> Name -> Q.Gen Node
lookupTypeAndMkGen typeName typingContext elmName =
  -- First see if typeName refers to a SimpleType
  case findByMaybeQName typeName (simpleTypes typingContext) of
    Just simpleType -> case simpleType of 
      SimpleType maybeName restrictionBaseType restriction ->
        error $ "TODO: Add support for SimpleType generation"
    -- Then see if typeName refers to a ComplexType
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
      sisNodeGens = 
        map (\si -> 
              case si of
                SIElement e -> mkElementGen e typingContext
                {- TODO Add case for SIChoice-})
            sequenceItems

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

genTxtNodeString :: Name -> Q.Gen Node
genTxtNodeString n = do s <- genString
                        return $ TxtNode n [] s

genTxtNodeInteger :: Name -> Q.Gen Node
genTxtNodeInteger n = do (s::Int) <- Q.arbitrary
                         return $ TxtNode n [] $ show s

-- TODO Make this generate arbitart XML-escaped strings
genString :: Q.Gen String
genString = Q.listOf $ Q.elements $ ' ' : ['A'..'Z'] 

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

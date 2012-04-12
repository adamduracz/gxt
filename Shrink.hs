module Shrink where

import Generator
import Schema
import Data.List 
  ( permutations
  , tails 
  , isPrefixOf
  )

shrinkXmlDoc :: XmlDoc -> [XmlDoc]
shrinkXmlDoc
  ( XmlDoc { version  = v 
           , encoding = e
           , root     = node
           , source   = js@(Just s)
           } ) = 
  [ XmlDoc { version  = v 
           , encoding = e
           , root     = r
           , source   = js
           }
  | r <- shrinkNode node s
  ]

-- TODO Check if this needs to take a [Element] in case several
-- possible elements gave rise to the Node
shrinkNode :: Node -> Schema -> [Node]
shrinkNode node schema = case node of
  ElmNode n as els me ->
    case me of 
      Nothing -> [node] -- Can't shrink
      Just e  -> [ ElmNode n as' els' me
                 | as'  <- shrinkAttrs as schema
                 , els' <- map (\e -> shrinkNode e schema) els
                 ]
  TxtNode n as s me ->
    case me of 
      Nothing -> [node] -- Can't shrink
      Just e  -> [ TxtNode n as' s' me
                 | as' <- shrinkAttrs as schema
                 , s'  <- shrinkString s e
                 ]
  EmpNode n as me ->
    case me of 
      Nothing -> [node] -- Can't shrink
      Just e  -> [ EmpNode n as' me
                 | as' <- shrinkAttrs as schema
                 ]

shrinkString s e = [s] -- TODO Implement shrinkString

shrinkAttrs :: [Attr] -> Schema -> [[Attr]]
shrinkAttrs as schema = 
  [ [ a 
    | a <- as
    , not $ a `elem` s ]
  | s <- powerset $ shrinkableAttrs
  ]
  where
    shrinkableAttrs = [ a | a <- as, canShrink a schema ]
    canShrink :: Attr -> Schema -> Bool
    canShrink (name,_,source) schema =
      if isPrefixOf "xmlns" name
      then False
      else case source of
        Just (AttributeRef          _   use) -> use == Optional
        Just (AttributeWithTypeRef  _ _ use) -> use == Optional
        Just (AttributeWithTypeDecl _ _ use) -> use == Optional
        Nothing -> error $ "No attribute declaration matching '" ++ name ++ "' found in schema!"

-- Utilities

getElementAttributes :: Element -> Schema -> [Attribute]
getElementAttributes (ElementRef r _ _ _) s = 
  getElementAttributes (lookupEQNamed r $ elements s) s
getElementAttributes (ElementWithTypeRef _ _ _ t _) s = 
  getComplexTypeAttributes (lookupComplexType t s)
getElementAttributes (ElementWithSimpleTypeDecl _ t _ _ _) s = 
  []
getElementAttributes (ElementWithComplexTypeDecl _ _ _ t _) _ =
  getComplexTypeAttributes t

getComplexTypeAttributes :: ComplexType -> [Attribute]
getComplexTypeAttributes ct = case ct of 
  ComplexTypeAll            _ _ as -> as
  ComplexTypeChoice         _ _ as -> as
  ComplexTypeSequence       _ _ as -> as
  ComplexTypeSimpleContent  _ _ as -> as
  ComplexTypeComplexContent _ _ as -> as

-- Generic utility functions

powerset       :: [a] -> [[a]]
powerset []     = [[]]
powerset (x:xs) = xss /\/ map (x:) xss
                    where xss = powerset xs

(/\/)        :: [a] -> [a] -> [a]
[]     /\/ ys = ys
(x:xs) /\/ ys = x : (ys /\/ xs)
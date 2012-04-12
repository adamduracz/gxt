module Shrink where

import Generator
import Schema
import Data.List 
  ( permutations
  , tails 
  , isPrefixOf
  )
import Debug.Trace

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
  | r <- shrinkNode node
  ]

shrinkNode :: Node -> [Node]
shrinkNode node = case node of
  ElmNode n as els me ->
    case me of 
      Nothing -> [node] -- No schema information, can't shrink!
      Just e  -> [ ElmNode n as' els' me
                 | as'  <- shrinkAttrs as
                 , els' <- shrinkChildNodes els
                 ]
  TxtNode n as s me ->
    case me of 
      Nothing -> [node] -- No schema information, can't shrink!
      Just e  -> [ TxtNode n as' s' me
                 | as' <- shrinkAttrs as
                 , s'  <- shrinkString s e
                 ]
  EmpNode n as me ->
    case me of 
      Nothing -> [node] -- No schema information, can't shrink!
      Just e  -> [ EmpNode n as' me
                 | as' <- shrinkAttrs as
                 ]

shrinkString s e = [s] -- TODO Implement shrinkString (shrinking of simpleTypes)

trc a  = trace (show a) a
tr s a = trace (s ++ show a) a
tl l a = trace (show $ l a) a 

shrinkChildNodes :: [Node] -> [[Node]]
shrinkChildNodes ns =
  [ mandatoryChildNodes ++ son
  | on  <- powerset optionalChildNodes
  , son <- map shrinkNode on
  -- TODO Implement shrinking of mandatory nodes
  --, smn <- map shrinkNode mandatoryChildNodes
  ] 
  where
    mandatoryChildNodes  = filter (not.optional) ns
    optionalChildNodes   = filter      optional  ns
    optional :: Node -> Bool
    optional node = case node of
      ElmNode _ _ _ me -> isOpt me
      TxtNode _ _ _ me -> isOpt me
      EmpNode _ _   me -> isOpt me
    isOpt me = case me of
        Nothing -> False -- No schema information, can't shrink!
        Just e  -> Occurs 0 == (fst $ getBounds e)

shrinkAttrs :: [Attr] -> [[Attr]]
shrinkAttrs as = 
  [ mandatoryAttrs ++ oa | oa <- powerset optionalAttrs ]
  where
    mandatoryAttrs = filter (not.optional) as
    optionalAttrs  = filter      optional  as
    optional :: Attr -> Bool
    optional (name,_,source) =
      if isPrefixOf "xmlns" name
      then False
      else case source of
        Just (AttributeRef          _   use) -> use == Optional
        Just (AttributeWithTypeRef  _ _ use) -> use == Optional
        Just (AttributeWithTypeDecl _ _ use) -> use == Optional
        Nothing -> error $ "No attribute declaration matching '" ++ name ++ "' found in schema!"

-- Generic utility functions

powerset       :: [a] -> [[a]]
powerset []     = [[]]
powerset (x:xs) = xss /\/ map (x:) xss
                    where xss = powerset xs

(/\/)        :: [a] -> [a] -> [a]
[]     /\/ ys = ys
(x:xs) /\/ ys = x : (ys /\/ xs)
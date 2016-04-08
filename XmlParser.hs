{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module XmlParser where

import Text.ParserCombinators.Parsec
import qualified Data.Maybe as M

------------------------------------------------------------------------
-- XML Parser
------------------------------------------------------------------------

------------------------------------------------------------------------
-- TODO
-- - Implement decoding of XML-encoded characters, e.g. &amp; -> &
------------------------------------------------------------------------

------------------------------------------------------------------------
-- Type declarations
------------------------------------------------------------------------

data XmlDoc =
    XmlDoc { version :: String
           , encoding :: String
           , root :: Node
           } deriving (Eq)

data Node = ElmNode Name [Attr] ElmList
          | TxtNode Name [Attr] String
          | EmpNode Name [Attr]
          deriving (Eq)

type Name = String

type Attr = (Name, String)

data ElmList = ElmList [Node] deriving (Eq)

------------------------------------------------------------------------
-- Classes and instances
------------------------------------------------------------------------

class Named a where
  name :: a -> Name

class HasAttrs a where
  attrs :: a -> [Attr]

class Renamable a where
  rename :: a -> Name -> a

instance Named Attr where
  name (n,s) = n  

instance Named Node where
  name (ElmNode n _ _) = n
  name (TxtNode n _ _) = n
  name (EmpNode n _)   = n

instance HasAttrs Node where
  attrs (ElmNode _ as _) = as
  attrs (TxtNode _ as _) = as
  attrs (EmpNode _ as)   = as

instance Renamable Node where
  rename (ElmNode n as el) nn = ElmNode nn as el
  rename (TxtNode n as t)  nn = TxtNode nn as t
  rename (EmpNode n as)    nn = EmpNode nn as


-- instance Functor Node where
--  fmap f (ElmNode n as (ElmList els)) = (ElmNode n as (ElmList $ map f els))

-- Show Node

instance Show Node where
	show (ElmNode n as el) = wrap n as $ show el
	show (TxtNode n as s)  = wrap n as s
	show (EmpNode n as)    = wrap n as ""

instance Show ElmList where
	show (ElmList ns) = concatMap show ns
	
-- Show XmlDoc

instance Show XmlDoc where
	show XmlDoc { version = v, encoding = e, root = r } =
		"<?xml version='" ++ v ++ "' encoding='" ++ e ++ "' ?>" ++ show r 
	
wrap n as c = (begTag $ n ++ showAttrList as) ++ c ++ (endTag n)
begTag n = "<"  ++ n ++ ">"
endTag n = "</" ++ n ++ ">"

showAttrList []         = ""
showAttrList ((n,s):as) = " " ++ n ++ "='" ++ s ++ "'" ++ showAttrList as

--------------

xmlDoc :: Parser XmlDoc
xmlDoc = do
	spaces
	(v,e) <- xmlHeader
	spaces
	r <- node
	spaces
	eof
	return XmlDoc { version = v, encoding = e, root = r }

type Version = String
type Encoding = String

xmlHeader :: Parser (Version,Encoding)
xmlHeader = do
	string "<?xml"
	spaces
	as <- sepBy attr spaces
	spaces
	string "?>"
	return (lookupS "version" as, lookupS "encoding" as)

lookupS e l = case lookup e l of
	Nothing -> ""
	Just v  -> v

node :: Parser Node
node =   try elmNode 
	 <|> try txtNode 
	 <|> try empNode
	 <?> "valid element" 
	
elmNode :: Parser Node
elmNode = do
	(n,as) <- openTag
	spaces
	l <- endBy node spaces
	closeTag n
	return $ ElmNode n as $ ElmList l

txtNode :: Parser Node
txtNode = do
	(n,as) <- openTag
	t <- textContent
	closeTag n
	return $ TxtNode n as t

empNode :: Parser Node
empNode = do
	(n,as) <- tagInit
	string "/>"
	return $ EmpNode n as

openTag :: Parser (Name,[Attr])
openTag = do
	(n,as) <- tagInit
	char '>'
	return (n,as)

tagInit :: Parser (Name,[Attr])
tagInit = do
	char '<'
	spaces
	n <- nameString
	spaces
	as <- sepBy attr spaces
	spaces
	return (n,as)

closeTag :: String -> Parser String
closeTag n = string $ "</" ++ n ++ ">"

textContent :: Parser String
textContent = manyCept "<"

attr :: Parser Attr
attr = do
	n <- nameString
	spaces
	(char '=')
	spaces
	v <- attrValue
	spaces
	return (n,v)

attrValue :: Parser String
attrValue =   try (quotedValue '\'') 
		  <|> try (quotedValue '"')
		  <|> try unquotedValue
		  <?> "valid attribute value"

nameString :: Parser String
nameString = manyCept $ '/' : valueEnders

unquotedValue :: Parser String
unquotedValue = manyAnyTill $     lookAhead (char '/' >> spaces >> char '>') 
                              <|> lookAhead (oneOf " \t\n\r")

quotedValue :: Char -> Parser String
quotedValue q = char q >> manyAnyTill (char q)
	
valueEnders :: String
valueEnders = "=<>? \t\n\r"

-- Helper combinators
 
manyAnyTill = manyTill anyChar
	
manyCept = many . noneOf

---- Utilies

-- | Get an attribute's value based on its name, returns "" if no match
getAttrValue :: Name -> Node -> String
getAttrValue n (ElmNode _ as _) = lookupS n as
getAttrValue n (TxtNode _ as _) = lookupS n as
getAttrValue n (EmpNode _ as)   = lookupS n as

-- | Get an attribute's name based on its value
getAttrName :: String -> Node -> Maybe Name
getAttrName v (ElmNode _ attrs _) = look v attrs
  where
    look v []                      = Nothing
    look v ((n,v'):as) | v == v'   = Just n
                       | otherwise = look v as

addAttribute :: Node -> Attr -> Node
addAttribute (ElmNode n as el) a = ElmNode n (a:as) el
addAttribute (TxtNode n as el) a = TxtNode n (a:as) el
addAttribute (EmpNode n as)    a = EmpNode n (a:as)

------

readTree :: String -> XmlDoc
readTree input = case parse xmlDoc "xml" input of
	Left  err -> error $ show err
	Right val -> val

readXML inFile = do
	jas <- readFile inFile
	jasNode <- return $ readTree jas
	putStrLn $ show jasNode





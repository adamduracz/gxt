module RegEx where

import Text.ParserCombinators.Parsec

------------------------------------------------------------------------
-- Regular expression parser, based on 
-- http://books.google.se/books?id=Xw3_tzEJVEwC&lpg=PA450&ots=03NoIOjq9D&dq=negchargroup&pg=PA447#v=onepage&q&f=false
-- Notable departures from above spec are:
-- - Because XML schema regexes do not support lazy quantifiers, the 
--   Quantifier and Indicator constructs have been merged as QuantIndicator
-- - No backreferences because XML schema regexes do not support it
------------------------------------------------------------------------

------------------------------------------------------------------------
-- TODO
-- - Implement CharCategory based on table on page 924 of Kay book
-- - Subexpressions (see page 918 of Kay book)
------------------------------------------------------------------------


------------------------------------------------------------------------
-- Type declarations
------------------------------------------------------------------------

-- Top-level (Branches and Pieces)
data RegEx            = Choice [Branch]                   deriving (Eq,Show)
data Branch           = Branch [Piece]                    deriving (Eq,Show)
data Piece            = Piece Atom (Maybe QuantIndicator) deriving (Eq,Show) 

-- Quantifiers
data QuantIndicator   = QQuestionMark
                      | QStar
                      | QPlus
                      | QQuantRange Int Int
                      | QQuantMin   Int
                      | QQuantExact Int
                      deriving (Eq,Show)

-- Atoms
data Atom             = AChar      Char -- Any XML character except .\?*+|^${}()[]
                      | ACharClass CharClass 
                      | ABrackets  RegEx
                      deriving (Eq,Show)

data CharClass        = CClassEsc  CharClassEsc
                      | CClassExpr CharClassExpr
                      | CClassDot
                      | CClassHat
                      | CClassDollar
                      deriving (Eq,Show)

data CharClassExpr    = CharClassExpr CharGroup deriving (Eq,Show)
                    
data CharGroup        = CGroupPos          PosCharGroup
                      | CGroupNeg          NegCharGroup
                      | CGroupCharClassSub CharClassSub
                      deriving (Eq,Show)

data PosCharGroup     = PosCharGroup [PosCharGroupItem] deriving (Eq,Show)

data PosCharGroupItem = PosCharGroupRange        CharRange
                      | PosCharGroupCharClassEsc CharClassEsc
                      deriving (Eq,Show)

data NegCharGroup     = NegCharGroup PosCharGroup deriving (Eq,Show)

data CharClassSub     = CharClassSubPos PosCharGroup CharClassExpr
                      | CharClassSubNeg NegCharGroup CharClassExpr
                      deriving (Eq,Show)

-- Character Ranges
data CharRange        = CodePointRange CharOrEsc CharOrEsc
                      | XmlCharIncDash Char -- Any XML character except []\
                      deriving (Eq,Show)

data CharOrEsc        = CharOrEscXmlChar       Char -- Any XML character except []\-
                      | CharOrEscSingleCharEsc SingleCharEsc 
                      deriving (Eq,Show)

-- Character Class Escapes
data CharClassEsc     = CCEscSingleChar SingleCharEsc
                      | CCEscMultiChar  Char
                      | CCEscCat        CharProp
                      | CCEscCompl      CharProp -- Complement to CCEscCat
                      deriving (Eq,Show)

data SingleCharEsc    = SingleCharEsc Char deriving (Eq,Show)

data CharProp         = Category CharCategory
                      | IsBlock  BlockName
                      deriving (Eq,Show)

data CharCategory     = CharCategory -- TODO Implement this based on table on page 924 of Kay book
                      deriving (Eq,Show)

type BlockName        = String

------------------------------------------------------------------------
-- Constants
------------------------------------------------------------------------
                                  
singleCharacterEscapes = "nrt\\|.?*+(){}-[]^$" -- TODO Verify that it's really \\ and not \\\\
twoCharacterEscapes    = "sSiIcCdDwW"
digits                 = "0123456789"

------------------------------------------------------------------------
-- RegEx Parser
------------------------------------------------------------------------

-- Top-level (Branches and Pieces)
regEx :: Parser RegEx
regEx = sepBy1 branch (char '|') >>= \bs -> return $ Choice bs

branch :: Parser Branch
branch = many piece >>= \ps -> return $ Branch ps

piece :: Parser Piece
piece =   try (do a <- atom
                  q <- quantIndicator
                  return $ Piece a $ Just q)
      <|> try (do a <- atom
                  return $ Piece a   Nothing)

-- Quantifiers
quantIndicator =   try (char '?' >> (return  QQuestionMark))
               <|> try (char '*' >> (return  QStar))
               <|> try (char '+' >> (return  QPlus))
               <|> try (between (char '{') (char '}')
                                (do min <- quantExact
                                    char ','
                                    max <- quantExact
                                    return $ QQuantRange min max))
               <|> try (between (char '{') (char '}')
                                (do min <- quantExact
                                    char ','
                                    return $ QQuantMin min))
               <|>     (between (char '{') (char '}')
                                (do exact <- quantExact
                                    return $ QQuantExact exact))

quantExact :: Parser Int
quantExact = many1 (oneOf digits) >>= \q -> return $ read q

-- Atoms
atom :: Parser Atom
atom =   try (noneOf ".\\?*+|^${}()[]"            >>= \c  -> return $ AChar      c)
     <|> try (charClass                           >>= \cc -> return $ ACharClass cc)
     <|>     (between (char '(') (char ')') regEx >>= \r  -> return $ ABrackets  r)

charClass :: Parser CharClass
charClass =   try (charClassEsc  >>= \cce -> return $ CClassEsc  cce)
          <|> try (charClassExpr >>= \cce -> return $ CClassExpr cce)
          <|> try (char '.'      >>         (return   CClassDot))
          <|> try (char '^'      >>         (return   CClassHat))
          <|>     (char '$'      >>         (return   CClassDollar))

charClassExpr :: Parser CharClassExpr
charClassExpr = between (char '[') (char ']') charGroup >>= \cg -> return $ CharClassExpr cg

-- Character Groups
charGroup :: Parser CharGroup
charGroup =   try (negCharGroup >>= \g -> return $ CGroupNeg          g)
          <|> try (posCharGroup >>= \g -> return $ CGroupPos          g)
          <|>     (charClassSub >>= \g -> return $ CGroupCharClassSub g)

-- TODO Specifically test this contraption!! Might not to work...
posCharGroup :: Parser PosCharGroup
posCharGroup = 
  do gis <- many1 (try (charRange    >>= \cr -> return $ PosCharGroupRange        cr)
                   <|> (charClassEsc >>= \cr -> return $ PosCharGroupCharClassEsc cr)) 
     return $ PosCharGroup gis

negCharGroup :: Parser NegCharGroup 
negCharGroup = char '^' >> posCharGroup >>= \pcg -> return $ NegCharGroup pcg

charClassSub :: Parser CharClassSub
charClassSub = try (do pcg <- posCharGroup
                       char '-'
                       cce <- charClassExpr
                       return $ CharClassSubPos pcg cce)
               <|> (do ncg <- negCharGroup
                       char '-'
                       cce <- charClassExpr
                       return $ CharClassSubNeg ncg cce)

-- Character Ranges
charRange :: Parser CharRange
charRange = try (do min <- charOrEsc
                    char '-'
                    max <- charOrEsc
                    return $ CodePointRange min max)
            <|> (xmlCharIncDash >>= \c   -> return $ XmlCharIncDash c)

charOrEsc :: Parser CharOrEsc
charOrEsc = try (xmlChar       >>= \c   -> return $ CharOrEscXmlChar       c)
            <|> (singleCharEsc >>= \sce -> return $ CharOrEscSingleCharEsc sce)

xmlChar :: Parser Char
xmlChar = noneOf "[]\\-"

xmlCharIncDash :: Parser Char
xmlCharIncDash = noneOf "[]\\"

-- Character Class Escapes
charClassEsc :: Parser CharClassEsc
charClassEsc =   
      try (char '\\' >> oneOf twoCharacterEscapes >>= \mce -> return $ CCEscMultiChar  mce)
  <|> try (singleCharEsc                          >>= \c   -> return $ CCEscSingleChar c)
  <|> try (do string "\\p{"
              p <- charProp
              char '}'
              return $ CCEscCat p)
  <|> try (do string "\\P{"
              p <- charProp
              char '}'
              return $ CCEscCompl p)
  <|> (char '\\' >> noneOf "" >>= \c -> error $ "Unsupported single-character escape " ++ [c])

charProp :: Parser CharProp
charProp = try (charCategory             >>= \c  -> return $ Category c)
           <|> (string "Is" >> blockName >>= \bn -> return $ IsBlock bn)

blockName :: Parser BlockName
blockName = error $ "TODO Implement blockName parser based on Data.CharSet.Unicode.Block"

charCategory :: Parser CharCategory
charCategory = error $ "TODO Implement charCategory parser based on Data.CharSet.Unicode.Category"

singleCharEsc :: Parser SingleCharEsc
singleCharEsc = char '\\' >> oneOf singleCharacterEscapes >>= \e -> return $ SingleCharEsc e
------------------------------------------------------------------------
-- Utility functions
------------------------------------------------------------------------

readRegEx :: String -> RegEx
readRegEx input = case parse regEx "regEx" input of
	Right val -> val
	Left  err -> error $ show err

{-
literals :: RegEx -> [Char]
literals r = case r of
  Epsilon     -> []
  Literal c   -> [c]
  SCEscape c  -> [] -- TODO Add support for single character escape sequences
  TCEscape c  -> []
  Sequence rs -> concatMap literals rs
  Repeat _ r  -> literals r
  Choice rs   -> concatMap literals rs
-}

main1 = parseTest regEx "he(llo)*|wor+ld?"


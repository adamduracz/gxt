module RegEx where

import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.Gen as G
import Control.Applicative hiding ( (<|>), many )
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

import Data.Char ( digitToInt )

------------------------------------------------------------------------
-- Regular expression parser, based on 
-- http://books.google.se/books?id=Xw3_tzEJVEwC&lpg=PA450&ots=03NoIOjq9D&dq=negchargroup&pg=PA447#v=onepage&q&f=false
-- Notable departures from above spec are:
-- - Because XML schema regexes do not support lazy quantifiers, 
--   the Quantifier and Indicator constructs have been merged 
--   as QuantIndicator
-- - No backreferences because XML schema regexes do not support it
------------------------------------------------------------------------

------------------------------------------------------------------------
-- TODO
-- - Implement CharCategory based on table on page 924 of Kay book
-- - Clean up imports
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

data PosCharGroupItem = PosCharGroupRange        [CharRange]
                      | PosCharGroupCharClassEsc [CharClassEsc]
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
                                  
singleCharacterEscapes = "nrt.\\\\?*+|^${}()[]"
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
piece =   (try $ do a <- atom
                    q <- quantIndicator
                    return $ Piece a $ Just q)
      <|> (try $ do a <- atom
                    return $ Piece a   Nothing)

-- Quantifiers
quantIndicator =   try (char '?' >> (return  QQuestionMark))
               <|> try (char '*' >> (return  QStar))
               <|> try (char '+' >> (return  QPlus))
               <|> try (between (char '{') (char '}')
                                (do min <- oneOf digits
                                    char ','
                                    max <- oneOf digits
                                    return $ QQuantRange (digitToInt min) (digitToInt max)))
               <|> try (between (char '{') (char '}')
                                (do min <- oneOf digits
                                    char ','
                                    return $ QQuantMin $ digitToInt min))
               <|> try (between (char '{') (char '}')
                                (do exact <- oneOf digits
                                    return $ QQuantExact $ digitToInt exact))

-- Atoms
atom :: Parser Atom
atom =   try (noneOf ".\\?*+|^${}()[]"            >>= \c  -> return $ AChar      c)
     <|> try (charClass                           >>= \cc -> return $ ACharClass cc)
     <|> try (between (char '(') (char ')') regEx >>= \r  -> return $ ABrackets  r)

charClass =   try (charClassEsc  >>= \cce -> return $ CClassEsc  cce)
          <|> try (charClassExpr >>= \cce -> return $ CClassExpr cce)
          <|> try (char '.'      >>         (return   CClassDot))
          <|> try (char '^'      >>         (return   CClassHat))
          <|> try (char '$'      >>         (return   CClassDollar))

charClassExpr = between (char '[') (char ']') charGroup >>= \cg -> return $ CharClassExpr cg

-- Character Groups

charGroup =   try (posCharGroup >>= \g -> return $ CGroupPos          g)
          <|> try (negCharGroup >>= \g -> return $ CGroupNeg          g)
          <|> try (charClassSub >>= \g -> return $ CGroupCharClassSub g)

posCharGroup = undefined

negCharGroup = undefined

charClassSub = undefined

-- Character Class Escapes

charClassEsc = undefined


{-
  -- Parsers
  scesc         = try $ char '\\' >> oneOf singleCharacterEscapes >>= return
  tcesc         = try $ char '\\' >> oneOf twoCharacterEscapes >>= return
  lit           = try $ noneOf ".\\?*+|^${}()[]"
  charClassExpr = between (char '[') (char ']')
  charGroup     = try $ posCharGroup <|> negCharGroup <|> charClassSub
  posCharGroup  = between (char '(') (char ')')
  negCharGroup  = try $ (char '^') >> posCharGroup
  charClassSub  = try $ (posCharGroup <|> negCharGroup) >> (char '-') >> charClassExpr

  sequence a b = Sequence $ (seqTerms a) ++ (seqTerms b)
  choice a b   = Choice $ (choiceTerms a) ++ (choiceTerms b)

  seqTerms (Sequence ts) = ts
  seqTerms t = [t]

  choiceTerms (Choice ts) = ts
  choiceTerms t = [t]
-}

readRegEx :: String -> RegEx
readRegEx input = case parse regEx "regEx" input of
	Right val -> val
	Left  err -> error $ show err

------------------------------------------------------------------------
-- Utility functions
------------------------------------------------------------------------

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


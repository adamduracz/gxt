module Main where

import Generator hiding (main)
import Schema
import XmlParser
import qualified Data.Maybe as M
import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.Gen as G
import System (ExitCode, getArgs, system)
import System.Process (readProcessWithExitCode)
import System.Random
  ( Random
  , StdGen
  , randomR
  , split
  , newStdGen
  )

type ProcessInput  = (FilePath, [String], String)
type ProcessOutput = (ExitCode,  String , String)

-- TODO Extract these as command line parameters
inSchema     = "in.xsd"
outSchema    = "out.xsd"
numberOfRuns = 10

main = do args <- getArgs
          s <- readFile inSchema
          let schema = readSchema s
              gen = mkSchemaGen schema "priceList"
          randomXmlDocs <- generateTestData numberOfRuns gen
          transformedDocs <- transformXmls randomXmlDocs "transform.xsl"
          let docsToValidate = map (readTree . exitCode) transformedDocs
          -- mapM_ (putStrLn . show) docsToValidate
          valitationResults <- validateXmls docsToValidate outSchema
          mapM_ (putStrLn . show) valitationResults
          return ()
          where
            exitCode (_,e,_) = e

transformXmls :: [XmlDoc] -> String -> IO [ProcessOutput] 
transformXmls xmlDocs xsltPath = do
  resultTriples <- mapM readProcessWithExitCode' cmds
  return resultTriples
  where
    cmds = map (\xmlDoc -> transformCommand xmlDoc xsltPath) xmlDocs
    readProcessWithExitCode' (path,args,stdin) = readProcessWithExitCode path args stdin

-- | Produces a triple of input for the readProcessWithExitCode function
transformCommand :: XmlDoc -> String -> ProcessInput
transformCommand inputDoc xslPath =
  ( "xsltproc"    -- Path to the XSL transform command (xsltproc)  
  , [ xslPath     -- Path to the stylesheet
    , "-"         -- Makes xsltproc read XML from stdin
    ]
  , show inputDoc -- The XML string, will be passed to xsltproc through stdin
  )

validateXmls :: [XmlDoc] -> String -> IO [ProcessOutput]
validateXmls xmlDocs schemaPath = do
  resultTriples <- mapM readProcessWithExitCode' cmds
  return resultTriples
    where
      cmds = map (\xmlDoc -> validateXmlCommand xmlDoc schemaPath) xmlDocs
      readProcessWithExitCode' (path,args,stdin) = readProcessWithExitCode path args stdin

validateXmlCommand :: XmlDoc -> String -> ProcessInput
validateXmlCommand docToValidate schemaPath =
  ( "xmllint"          -- Path to the XML validator command (xmllint)  
  , [ "--noout"        -- Prevent xmllint from writing output to stdout
    , "--schema"       -- Indicate that the next argument is the schema path
    , schemaPath       -- Path to the schema
    , "-"              -- Makes xmllint read XML from stdin
    ]
  , show docToValidate -- The XML string, will be passed to xmllint through stdin
  )

generateTestData :: Int -> Q.Gen a -> IO [a]
generateTestData howMany g =
  do rnd0 <- newStdGen
     let m = G.unGen g
     let rnds rnd = rnd1 : rnds rnd2 where (rnd1,rnd2) = System.Random.split rnd
     return [(m r n) | (r,n) <- rnds rnd0 `zip` [0,2..(2 * howMany - 1)] ]


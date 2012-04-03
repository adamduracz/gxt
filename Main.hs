
------------------------------------------------------------------------
-- TODO
-- - Add option to write test cases which fail validation to disk
-- - Add an error count to the report, and perhaps a digest of all the errors?
------------------------------------------------------------------------

module Main where

import Generator hiding (main)
import Schema
import XmlParser
import qualified Data.Maybe as M
import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.Gen as G
import System
  ( ExitCode (ExitSuccess, ExitFailure)
  , getArgs
  , system
  )
import System.Random
  ( Random
  , StdGen
  , randomR
  , split
  , newStdGen
  )
import System.Process (readProcessWithExitCode)

type ProcessInput  = (FilePath, [String], String)
type ProcessOutput = (ExitCode,  String , String)

-- TODO Extract these as command line parameters
inSchemaPath  = "in.xsd"
outSchemaPath = "out.xsd"
xslPath       = "transform.xsl"    
numberOfRuns  = 10

mainG = do args <- getArgs
           s <- readFile inSchemaPath
           let schema = readSchema s
               gen = genSchema schema "priceList"
           putStrLn $ show schema
           randomXmlDocs <- generateTestData 1 gen
           mapM_ (putStrLn . show) randomXmlDocs
           transformedDocs <- transformXmls randomXmlDocs xslPath
           mapM_ (putStrLn . show) transformedDocs
           let docsToValidate = map (readTree . exitCode) transformedDocs
           -- mapM_ (putStrLn . show) docsToValidate
           return ()
           where
             exitCode (_,e,_) = e

main = do args <- getArgs
          s <- readFile inSchemaPath
          let schema = readSchema s
              gen = genSchema schema "priceList"
          randomXmlDocs <- generateTestData numberOfRuns gen
          --mapM_ (putStrLn . show) randomXmlDocs
          transformedDocs <- transformXmls randomXmlDocs xslPath
          --mapM_ (putStrLn . show) transformedDocs
          let docsToValidate = map (readTree . exitCode) transformedDocs
          --mapM_ (putStrLn . show) docsToValidate
          valitationResults <- validateXmls docsToValidate outSchemaPath
          --mapM_ (putStrLn . show) valitationResults
          putStrLn $ show $ makeReport valitationResults numberOfRuns
          return ()
          where
            exitCode (_,e,_) = e

makeReport :: [ProcessOutput] -> Int -> String
makeReport po numberOfRuns = case mergeValidationResults po of
  Nothing  -> "Styleheet passed " ++ show numberOfRuns ++ " runs!"
  (Just e) -> "Error found: " ++ e
  where
    mergeValidationResults :: [ProcessOutput] -> (Maybe String)
    mergeValidationResults pos = foldr mergeFunction Nothing pos
    mergeFunction :: ProcessOutput -> Maybe String -> Maybe String
    mergeFunction _                               p@(Just previousError) = p
    --TODO Make a better error report, also using syserr if it contains something interesting
    mergeFunction (ExitSuccess,   _,      _)      _ = Nothing
    mergeFunction (ExitFailure _, sysout, syserr) _ = Just $ sysout ++ syserr

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





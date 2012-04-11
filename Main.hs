
------------------------------------------------------------------------
-- TODO
-- - Make a better error report, also using syserr if it contains something interesting
-- - Add option to write test cases which fail validation to disk
-- - Add an error count to the report, and perhaps a digest of all the errors?
------------------------------------------------------------------------

module Main where

import Generator
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
inSchemaPathD    = "in.xsd"
outSchemaPathD   = "out.xsd"
xslPathD         = "transform.xsl"    
numberOfRunsD    = 10

parseArgs :: IO (String,String,String,Int,Name)
parseArgs = do args <- getArgs
               return $ case length args of
                          0 -> ( xslPathD
                               , inSchemaPathD
                               , outSchemaPathD
                               , numberOfRunsD
                               , "" -- Autodetect root element name
                               )
                          1 -> ( ""
                               , inSchemaPathD
                               , ""
                               , 1
                               , "" -- Autodetect root element name
                               )
                          3 -> ( args !! 0
                               , args !! 1
                               , args !! 2
                               , numberOfRunsD
                               , "" -- Autodetect root element name
                               )
                          5 -> ( args !! 0
                               , args !! 1
                               , args !! 2
                               , read $ args !! 3
                               , args !! 4
                               )
                          _ -> error "Use: faxt xslt in-xsd out-xsd [number-of-runs] [root-element-name]"

mainG = do (xslPath, inSchemaPath, outSchemaPath, numberOfRuns, rootElementName) <- parseArgs
           s <- readFile inSchemaPath
           let schema = readSchema s
               gen = genSchema schema $ autoRootElementName schema rootElementName
           --putStrLn $ show schema
           randomXmlDocs <- generateTestData 1 gen
           mapM_ (putStrLn . show) randomXmlDocs
           transformedDocs <- transformXmls randomXmlDocs xslPath
           --mapM_ (putStrLn . show) transformedDocs
           let docsToValidate = map (readTree . exitCode) transformedDocs
           --mapM_ (putStrLn . show) docsToValidate
           return ()
           where
             exitCode (_,e,_) = e

main = do (xslPath, inSchemaPath, outSchemaPath, numberOfRuns, rootElementName) <- parseArgs
          s <- readFile inSchemaPath
          let schema = readSchema s
              gen = genSchema schema $ autoRootElementName schema rootElementName
          randomXmlDocs <- generateTestData numberOfRuns gen
          --mapM_ (putStrLn . show) randomXmlDocs
          transformedDocs <- transformXmls randomXmlDocs xslPath
          --mapM_ (putStrLn . show) transformedDocs
          let docsToValidate = map (readTree . exitCode) transformedDocs
          --mapM_ (putStrLn . show) docsToValidate
          valitationResults <- validateXmls docsToValidate outSchemaPath
          --mapM_ (putStrLn . show) valitationResults
          putStrLn $ makeReport docsToValidate valitationResults numberOfRuns
          return ()
          where
            exitCode (_,e,_) = e

-- | Returns the first failing test case along with the corresponding validator output
firstFailure :: [ProcessOutput] -> Maybe (Int, String)
firstFailure pos = case failures of
  [] -> Nothing
  fs -> Just $ head fs
  where
    failures = [ (i,stderr) 
               | f@(i,(exitCode,_,stderr)) <- zip [1..] pos
               , exitCode /= ExitSuccess 
               ]

makeReport :: [XmlDoc] -> [ProcessOutput] -> Int -> String
makeReport docs pos numberOfRuns = case firstFailure pos of
  Nothing      -> "Styleheet passed " ++ show numberOfRuns ++ " runs!"
  Just (i,err) -> "Error found:\n" ++ err ++ "\nTest case:\n" ++ (show $ docs !! i)

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





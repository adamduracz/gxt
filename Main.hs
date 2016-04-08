{-# LANGUAGE DeriveDataTypeable #-}

------------------------------------------------------------------------
-- TODO
-- - Make a better error report, also using syserr if it contains something interesting
-- - Add option to write test cases which fail validation to disk
-- - Add an error count to the report, and perhaps a digest of all the errors?
-- - Implement shrink count
------------------------------------------------------------------------

module Main where

import qualified Generator as G
import Schema
import qualified XmlParser as X
import Shrink ( shrinkXmlDoc )

import qualified Data.Maybe as M
import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.Gen as Gen
import Test.QuickCheck.Random as QCRandom
import System.Exit
  ( ExitCode (ExitSuccess, ExitFailure)
  )
import System.Environment
  ( getArgs )
import System.Random
  ( Random
  , StdGen
  , randomR
  , split
  , newStdGen
  )
import System.Process ( readProcessWithExitCode )
import System.IO.Unsafe ( unsafePerformIO )
import System.Console.CmdArgs
import Debug.Trace

type ErrorString   = String
type ProcessInput  = (FilePath, [String], ErrorString)
type ProcessOutput = (ExitCode,  String , ErrorString)

-- CmdArgs configuration
data GXT = GXT
  { xslPath         :: String
  , inSchemaPath    :: String
  , outSchemaPath   :: String
  , numberOfRuns    :: Int   
  , rootElementName :: G.Name
  } deriving (Show, Data, Typeable)

gxtSummary = "Generator-Driven XSLT Tester.\n" ++ 
             "See https://github.com/adamduracz/gxt for more information."

main = do args <- cmdArgs $ 
            GXT { xslPath         = def &= argPos 0 &= typ "XSLT"
                , inSchemaPath    = def &= argPos 1 &= typ "IN-SCHEMA"
                , outSchemaPath   = def &= argPos 2 &= typ "OUT-SCHEMA"
                , numberOfRuns    = 10  &= typ "INT"
                                 &= help "Number of documents to generate"
                , rootElementName = def &= typ "NAME"
                                 &= help "Name of root element in generated XML"
                } &= summary gxtSummary
          s <- readFile $ inSchemaPath args
          let schema = readSchema s
              gen = G.genSchema schema $ autoRootElementName schema $ rootElementName args
          let randomXmlDocs = unsafeGenerateTestData (numberOfRuns args) gen
          let transformedDocs = unsafeTransformXmls randomXmlDocs $ xslPath args
          let docsToValidate = map (X.readTree . sysout) transformedDocs
          let valitationResults = unsafeValidateXmls docsToValidate $ outSchemaPath args
          putStrLn $ makeReport randomXmlDocs 
                                docsToValidate
                                valitationResults
                                (numberOfRuns args)
                                (xslPath args)
                                (outSchemaPath args)
          return ()
          where
            sysout (_,e,_) = e

unsafeTransformAndValidate :: G.XmlDoc -> FilePath -> FilePath -> Maybe (X.XmlDoc,ErrorString)
unsafeTransformAndValidate inDoc xsltPath outSchemaPath =
  fmap (\err -> (outDoc,err)) $ unsafeValidateXmlDoc outDoc outSchemaPath
  where
    outDoc                   = X.readTree sysout
    (exitCode,sysout,syserr) = unsafeTransformXmlDoc inDoc xsltPath

-- | Returns the first failing test case along with the corresponding validator output
firstFailure :: [ProcessOutput] -> Maybe (Int, ErrorString)
firstFailure pos = case failures of
  [] -> Nothing
  fs -> Just $ head fs
  where
    failures = [ (i,stderr) 
               | f@(i,(exitCode,_,stderr)) <- zip [0..] pos
               , exitCode /= ExitSuccess 
               ]

makeReport :: [G.XmlDoc]      -- Input docs
           -> [X.XmlDoc]      -- Output docs
           -> [ProcessOutput]
           -> Int
           -> FilePath
           -> FilePath
           -> String
makeReport inputDocs outputDocs pos numberOfRuns xsltPath outSchemaPath = 
  case firstFailure pos of
    Nothing      -> "Styleheet passed " ++ show numberOfRuns ++ " runs!"
    Just (i,err) -> "Error found:\n" ++ case shrink firstFailureInput xsltPath outSchemaPath of
      Nothing -> err
        ++ "\nTest case (unshrunk):\n" ++ show (inputDocs !! i)
        ++ "\n\nStylesheet output (unshrunk):\n" ++ show firstFailureOutput
      Just (shrunkInput,shrunkOutput,shrunkError) -> shrunkError 
        ++ "\nTest case (unshrunk):\n" ++ show (inputDocs !! i)
        ++ "\n\nStylesheet output (unshrunk):\n" ++ show firstFailureOutput
        ++ "\n\nTest case (shrunk):\n"  ++ show shrunkInput
        ++ "\n\nStylesheet output (shrunk):\n" ++ show shrunkOutput
      where
        firstFailureInput  = inputDocs  !! i
        firstFailureOutput = outputDocs !! i

trc a = trace (show a) a
tr s a = trace (s ++ show a) a
tl l a = trace (show $ l a) a 

shrink :: G.XmlDoc -> FilePath -> FilePath -> Maybe (G.XmlDoc,X.XmlDoc,ErrorString)
shrink x xsltPath outSchemaPath = case smallest of
  [] -> Nothing
  l  -> Just $ head l
  where
    smallest = [ d
               | (s,d) <- zip sizes failures
               , s == minSize
               ]
    minSize  = minimum sizes
    sizes    = map (\(inDoc,outDoc,err) -> length $ show inDoc) failures
    failures = [ ( inDoc
                 , fst $ M.fromJust f
                 , snd $ M.fromJust f
                 )
               | inDoc <- shrinkXmlDoc x
               , let f = unsafeTransformAndValidate inDoc xsltPath outSchemaPath
               , f /= Nothing
               ]

unsafeTransformXmls :: [G.XmlDoc] -> FilePath -> [ProcessOutput] 
unsafeTransformXmls xmlDocs xsltPath = unsafePerformIO (do
  resultTriples <- mapM readProcessWithExitCode' cmds
  return resultTriples)
  where
    cmds = map (\xmlDoc -> transformCommand xmlDoc xsltPath) xmlDocs
    readProcessWithExitCode' (path,args,stdin) = readProcessWithExitCode path args stdin

unsafeTransformXmlDoc :: G.XmlDoc -> FilePath -> ProcessOutput
unsafeTransformXmlDoc xmlDoc xsltPath = 
  unsafePerformIO $ readProcessWithExitCode' (transformCommand xmlDoc xsltPath) >>= return
  where
    readProcessWithExitCode' (path,args,stdin) = readProcessWithExitCode path args stdin

-- | Produces a triple of input for the readProcessWithExitCode function
transformCommand :: G.XmlDoc -> FilePath -> ProcessInput
transformCommand inputDoc xslPath =
  ( "xsltproc"    -- Path to the XSL transform command (xsltproc)  
  , [ xslPath     -- Path to the stylesheet
    , "-"         -- Makes xsltproc read XML from stdin
    ]
  , show inputDoc -- The XML string, will be passed to xsltproc through stdin
  )

unsafeValidateXmls :: [X.XmlDoc] -> FilePath -> [ProcessOutput]
unsafeValidateXmls xmlDocs schemaPath = unsafePerformIO (do
  resultTriples <- mapM readProcessWithExitCode' cmds
  return resultTriples)
    where
      cmds = map (\xmlDoc -> validateXmlCommand xmlDoc schemaPath) xmlDocs
      readProcessWithExitCode' (path,args,stdin) = readProcessWithExitCode path args stdin

unsafeValidateXmlDoc :: X.XmlDoc -> FilePath -> Maybe ErrorString
unsafeValidateXmlDoc xmlDocs schemaPath =
  case head $ unsafeValidateXmls [xmlDocs] schemaPath of
    (ExitSuccess,   _, _)   -> Nothing
    (ExitFailure _, _, err) -> Just err

validateXmlCommand :: X.XmlDoc -> FilePath -> ProcessInput
validateXmlCommand docToValidate schemaPath =
  ( "xmllint"          -- Path to the XML validator command (xmllint)  
  , [ "--noout"        -- Prevent xmllint from writing output to stdout
    , "--schema"       -- Indicate that the next argument is the schema path
    , schemaPath       -- Path to the schema
    , "-"              -- Makes xmllint read XML from stdin
    ]
  , show docToValidate -- The XML string, will be passed to xmllint through stdin
  )

unsafeGenerateTestData :: Int -> Q.Gen a -> [a]
unsafeGenerateTestData howMany g = unsafePerformIO $
  do rnd0 <- QCRandom.newTheGen
     let m = Gen.unGen g
     let rnds rnd = rnd1 : rnds rnd2 where (rnd1,rnd2) = System.Random.split rnd
     return [ (m (QCRandom.QCGen r) n) | (r,n) <- rnds rnd0 `zip` [0,2..(2 * howMany - 1)] ]





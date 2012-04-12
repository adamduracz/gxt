
------------------------------------------------------------------------
-- TODO
-- - Make a better error report, also using syserr if it contains something interesting
-- - Add option to write test cases which fail validation to disk
-- - Add an error count to the report, and perhaps a digest of all the errors?
------------------------------------------------------------------------

module Main where

import qualified Generator as G
import Schema
import qualified XmlParser as X
import Shrink ( shrinkXmlDoc )

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
import System.Process ( readProcessWithExitCode )
import System.IO.Unsafe ( unsafePerformIO )

type ErrorString   = String
type ProcessInput  = (FilePath, [String], ErrorString)
type ProcessOutput = (ExitCode,  String , ErrorString)

-- TODO Extract these as command line parameters
inSchemaPathD    = "in.xsd"
outSchemaPathD   = "out.xsd"
xslPathD         = "transform.xsl"    
numberOfRunsD    = 10

parseArgs :: IO (String,String,String,Int,G.Name)
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
               gen = G.genSchema schema $ autoRootElementName schema rootElementName
           --putStrLn $ show schema
           let randomXmlDocs = unsafeGenerateTestData 1 gen
           mapM_ (putStrLn . show) randomXmlDocs
           let transformedDocs = unsafeTransformXmls randomXmlDocs xslPath
           --mapM_ (putStrLn . show) transformedDocs
           let docsToValidate = map (X.readTree . exitCode) transformedDocs
           --mapM_ (putStrLn . show) docsToValidate
           return ()
           where
             exitCode (_,e,_) = e

main = do (xslPath, inSchemaPath, outSchemaPath, numberOfRuns, rootElementName) <- parseArgs
          s <- readFile inSchemaPath
          let schema = readSchema s
              gen = G.genSchema schema $ autoRootElementName schema rootElementName
          let randomXmlDocs = unsafeGenerateTestData numberOfRuns gen
          --mapM_ (putStrLn . show) randomXmlDocs
          let transformedDocs = unsafeTransformXmls randomXmlDocs xslPath
          --mapM_ (putStrLn . show) transformedDocs
          let docsToValidate = map (X.readTree . sysout) transformedDocs
          --mapM_ (putStrLn . show) docsToValidate
          let valitationResults = unsafeValidateXmls docsToValidate outSchemaPath
          --mapM_ (putStrLn . show) valitationResults
          putStrLn $ makeReport randomXmlDocs 
                                docsToValidate 
                                valitationResults
                                numberOfRuns 
                                xslPath
                                outSchemaPath
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
           -> [ProcessOutput] -- 
           -> Int             -- 
           -> FilePath        --  
           -> FilePath        -- 
           -> String          -- 
makeReport inputDocs outputDocs pos numberOfRuns xsltPath outSchemaPath = 
  case firstFailure pos of
    Nothing      -> "Styleheet passed " ++ show numberOfRuns ++ " runs!"
    Just (i,err) -> case shrink firstFailureInput xsltPath outSchemaPath of
      Nothing -> 
        "Error found:\n" ++ err ++ "\nTest case (unshrunk):\n" ++ show firstFailureOutput
      Just (shrunkInput,shrunkOutput,shrunkError) -> -- TODO Implement shrink count
        "Error found:\n" ++ shrunkError 
--                         ++ "\nTest case (unshrunk):\n"++ show firstFailureOutput
--                         ++ "\n\nStylesheet output (unshrunk):\n" ++ show (inputDocs !! i)
                         ++ "\nTest case (shrunk):\n"  ++ show shrunkInput
                         ++ "\n\nStylesheet output (shrunk):\n" ++ show shrunkOutput
      where
        firstFailureInput  = inputDocs  !! i
        firstFailureOutput = outputDocs !! i

shrink :: G.XmlDoc -> FilePath -> FilePath -> Maybe (G.XmlDoc,X.XmlDoc,ErrorString)
shrink x xsltPath outSchemaPath = case smallest of
  [] -> Nothing
  l  -> Just $ head l
  where
    smallest     = [ d
                   | (s,d) <- zip sizes failures
                   , s == minSize
                   ]
    minSize      = minimum sizes
    sizes        = map (\(inDoc,outDoc,err) -> length $ show inDoc) failures
    failures     = [ ( inDoc
                     , fst $ M.fromJust f
                     , snd $ M.fromJust f
                     )
                   | inDoc <- shrunk
                   , let f = unsafeTransformAndValidate inDoc xsltPath outSchemaPath
                   , f /= Nothing
                   ]
    shrunk       = shrinkXmlDoc x
    smallestSize = minimum sizes

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
  do rnd0 <- newStdGen
     let m = G.unGen g
     let rnds rnd = rnd1 : rnds rnd2 where (rnd1,rnd2) = System.Random.split rnd
     return [(m r n) | (r,n) <- rnds rnd0 `zip` [0,2..(2 * howMany - 1)] ]





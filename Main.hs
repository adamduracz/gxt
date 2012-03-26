module Main where

import Generator hiding (main)
import Schema
import XmlParser -- TODO Can I get Schema to re-export this instead?
import qualified Data.Maybe as M
import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.Gen as G
import System
import System.Random
  ( Random
  , StdGen
  , randomR
  , split
  , newStdGen
  )

main = do a <- getArgs
          s <- readFile "out.xsd"
          let schema@
                Schema { targetNameSpace = name
                       , elements        = es
                       , simpleTypes     = sts
                       , complexTypes    = cts
                       , groups          = gs
                       , attributeGroups = ags
                       } = readSchema s
              gen = mkSchemaGen schema "priceList"
          randomXmlDocs <- generateTestData 5 gen
          ecs <- validateXmls randomXmlDocs (head a)
          mapM_ (putStrLn . show) ecs
          return ()

validateXmls :: [XmlDoc] -> String -> IO [ExitCode]
validateXmls xmlDocs schemaPath = do
  let cmds = map (\xmlDoc -> validateXMLCommand xmlDoc schemaPath) xmlDocs
  ecs <- mapM system cmds 
  return ecs

-- | XMLDoc       - Document to be validated
-- | String       - Path to schema
-- | Maybe String - (Nothing) if document validates, (Maybe errorMessage) otherwise 
validateXMLCommand :: XmlDoc -> String -> String
validateXMLCommand docToValidate schemaPath =
  "echo \"" ++ show docToValidate ++  "\" | xmllint --noout --schema '" ++ schemaPath ++ "' -" 

generateTestData :: Int -> Q.Gen a -> IO [a]
generateTestData howMany g =
  do rnd0 <- newStdGen
     let m = G.unGen g
     let rnds rnd = rnd1 : rnds rnd2 where (rnd1,rnd2) = System.Random.split rnd
     return [(m r n) | (r,n) <- rnds rnd0 `zip` [0,2..howMany] ]


module Main where

import Test.HUnit
import System.FilePath
import System.IO (stdout)
import System.Exit (exitFailure, exitSuccess)

import LoadFileTests

myRunTestTT t = do (counts, 0) <- runTestText (putTextToHandle stdout True) t
                   return $ (errors counts) + (failures counts)

runAllTests = myRunTestTT $ test $ concat [ loadFileTests ]

main = do
     nerr <- runAllTests
     if nerr /= 0
        then exitFailure
        else exitSuccess


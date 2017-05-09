-----------------------------------------------------------------------------
-- |
-- Module      :   Language.HaLex.Examples.HelloWorld
-- Copyright   :  (c) Joãoo Saraiva 2017
-- License     :  LGPL
--
-- Maintainer  :  saraiva@di.uminho.pt
-- Stability   :  provisional
-- Portability :  portable
--
-- 
-- Hello World for the HaLeX Library 
--     - Defines a regular expression for the sentence "Hello World"
--     - Defines a NDFA for Hellow World
--     - Translates regular expression into ndfa and dfa 
--     - Saves the different automata in files
--
-----------------------------------------------------------------------------


module Language.HaLex.Examples.HelloWorld where

import Language.HaLex.RegExp
import Language.HaLex.Ndfa
import Language.HaLex.Dfa
import Language.HaLex.RegExp2Fa
import Language.HaLex.FaAsDiGraph
import Language.HaLex.FaOperations



helloWorldRE :: RegExp String
helloWorldRE = (Literal "Hello") `Then`
               (Literal " ") `Then`
               (Literal "World")

hwre2ndfa :: Ndfa Int String
hwre2ndfa = regExp2Ndfa helloWorldRE


helloWorld_ndfa :: Ndfa Int String
helloWorld_ndfa = Ndfa ["Hello"," ","World"]
                       [1,2,3,4,5]
                       [1]
                       [4]
                       d
  where d 1 (Just "Hello") = [2]
        d 2 (Just " ")     = [3]
        d 3 (Just "World") = [4]
        d _   _            = [5]

hw2dfa :: Dfa [Int] String
hw2dfa =  ndfa2dfa $ regExp2Ndfa helloWorldRE


hw_ndfa2file :: IO ()
hw_ndfa2file =  ndfa2graphviz2file helloWorld_ndfa "HW_ndfa"

hw_dfa2file :: IO ()
hw_dfa2file  =  dfa2graphviz2file hw2dfa "HW_dfa"


hw2dfa_beautified  :: Dfa Int String
hw2dfa_beautified =  beautifyDfa $ ndfa2dfa $ regExp2Ndfa helloWorldRE

main :: IO ()
main = do hw_ndfa2file
          hw_dfa2file
          dfa2graphviz2file hw2dfa_beautified "HW_dfa_beatified"
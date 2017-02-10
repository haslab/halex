{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.HaLex.Sentences
-- Copyright   :  (c) João Saraiva 2017
-- License     :  LGPL
--
-- Maintainer  :  jas@di.uminho.pt
-- Stability   :  provisional
-- Portability :  portable
--
-- Generation of sentences of the (regular) languages defined via
--   Regular Expressions and Finite Autumata
--
-----------------------------------------------------------------------------

module Language.HaLex.Test_HaLex_Quickcheck where

import Language.HaLex.Dfa
import Language.HaLex.Ndfa
import Language.HaLex.RegExp
import Language.HaLex.RegExp2Fa
import Language.HaLex.RegExpParser
import Language.HaLex.FaOperations
import Language.HaLex.Minimize
import Language.HaLex.Sentences
import Language.HaLex.FaClasses

import Test.QuickCheck
import Data.Char
import Control.Monad



instance Arbitrary (RegExp Char) where
  arbitrary = sized genRegExp


genRegExp :: Integral n => n -> Gen (RegExp Char)

genRegExp size
  | size>0 = frequency [(13, genLiteral)
                       ,(10, genThen)
                       ,(4 , genPlus)
		       ,(2 , genDigits) 
                       ,(1 , return Epsilon)
		       ]
  | otherwise = return Epsilon
  where
    genLiteral = do c <- elements "aeiouAEIOU-_+-*/\\"    -- arbitrary
                    return (Literal c)
    genDigits  = return digRegExp                            
    genThen    = do re1 <- genRegExp (size `div` 2)
                    re2 <- genRegExp (size `div` 2)
                    return (Then re1 re2)
    genPlus    = do re <- genRegExp (size `div` 2)
                    return (OneOrMore re)


-- digRegExp :: RegExp
digRegExp :: RegExp Char
digRegExp = foldr1 Or (map (\x -> Literal (intToDigit x)) [0..9])

digRegExp' = foldr (\l r -> Or (Literal (intToDigit l))
                               r) Empty [0..9]


genRegExp' size
  | size>0 = oneof [genThen , genPlus , genLiteral , return Epsilon ]
  | otherwise = return Epsilon
  where
    genLiteral = do c <-  elements "aeiouAEIOU-_+-*/\\"    -- arbitrary
                    return (Literal c)
    genThen    = do re1 <- genRegExp' (size `div` 2)
                    re2 <- genRegExp' (size `div` 2)
                    return (Then re1 re2)
    genPlus    = do re <- genRegExp' (size `div` 2)
                    return (OneOrMore re)

exRegExp = sample (arbitrary :: Gen (RegExp Char))


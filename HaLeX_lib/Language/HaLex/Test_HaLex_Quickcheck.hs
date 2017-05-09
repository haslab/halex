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



-- Sentences
--


-- genDfa :: Dfa 



genDfaSentences :: (Ord st, Ord sy) => Dfa st sy -> Gen [sy]
genDfaSentences dfa@(Dfa _ _ s z _) = genStrDfa tt syncStates s z
  where tt         = transitionTableDfa dfa
        syncStates = dfasyncstates dfa

genStrDfa :: Eq st
          => [(st, sy, st)]       -- ^ Transition Table
	  -> [st]                 -- ^ Sync States
	  -> st                   -- ^ starting state
	  -> [st]                 -- ^ final states
	  -> Gen [sy]             -- ^ generated sentence
genStrDfa tt syncSts st finals =
    do (_, c, dest) <- elements (filter (\(from,_,to) -> from == st
                                        && not (to `elem` syncSts)) tt)
       rst         <- genStrDfa tt syncSts dest finals
       if (dest `elem` finals) then return [c] else return (c : rst)


{-
genNdfa_Sentences :: Eq st => Ndfa st sy -> Gen [sy]
genNdfa_Sentences ndfa@(Ndfa v q s z d) = genStrNdfa tt (head s) z
  where tt = transitionTableNdfa ndfa 

-}

genNdfa_Sentences :: Eq st => Ndfa st sy -> Gen [sy]
genNdfa_Sentences ndfa@(Ndfa v q s z d) = 
  do let tt = transitionTableNdfa ndfa  
     st <- elements s
     genStrNdfa tt st z


genStrNdfa :: Eq st => [(st, Maybe sy, st)] -> st -> [st] -> (Gen [sy])
genStrNdfa tt st fin =
    do (_, c, nxt) <- elements (filter (\(stt, _, _) -> stt == st) tt)
       rst         <- genStrNdfa tt nxt fin
       if (nxt `elem` fin)
          then return (maybeToList c)
          else return ((maybeToList c) ++ rst)


maybeToList (Just x) = [x]
maybeToList Nothing  = []





{-

genWord :: [(Int,Maybe Char,Int)] -> [Int] -> Int -> Gen (String)
genWord states fs st =
  do (char, next) <- elements (getValidPaths states st)
     gen          <- genWord states fs next
     if (next ‘elem‘ fs)
        then return (maybeToList char)
        else return ((maybeToList char) ++ gen)


getValidPaths :: [(Int,Maybe Char,Int)] -> Int -> [(Maybe Char,Int)]
getValidPaths list s =
     map (\(x,y,z) -> (y,z)) (filter (\(st,_,_) -> st == s) list)



getLanguage :: Int -> IO ()
getLanguage n =
  do  cases <- generate (genRegExp n)
      let aut@(Ndfa voc _ st fin _ ) = (regExp2Ndfa cases)
      allStr <- generate (sequence [suchThat (frequency [(10, (genStr (transitionTableNdfa aut) (head st) fin)), (1, (shuffle voc))]) ((/=) "") | x <- [1..25]])
      do
          writeFile ("testFiles/RegExp" ++ show(n)) (((filter (/= '\'')).(filter (/= ' ')).show) cases)
          writeFile ("testFiles/test" ++ show(n)) (concat (map (++ "\n") allStr))



genRegExpWord :: Int -> Gen (RegExpWord)
genRegExpWord x = do regExp <- genRegExp x
                     let ndfa@(Ndfa _ _ _ fs _) = regExp2Ndfa regExp
                     word <- genWord (transitionTableNdfa ndfa) fs 1
                     return (Test (regExp,word))


-}


dfa = Dfa "abc" [1,2,3,4] 1 [4] d
  where d 1 'a'   = 2
        d 1 'b'   = 2
	d 1 'c'   = 2
	d 2  _    = 3
	d 3  'a'  = 2
	d 3  'b'  = 3
	d 3  'c'  = 4



dfa2 = Dfa "abc" [1,2,3,4] 1 [5] d
  where d 1 'a'   = 2
        d 1 'b'   = 2
	d 1 'c'   = 2
	d 2  _    = 3
	d 3  'a'  = 2
	d 3  'b'  = 5
	d 3  'c'  = 4
	d _   _   = 4

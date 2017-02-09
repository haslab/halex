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

module Language.HaLex.Test_HaLex where

import Language.HaLex.Dfa
import Language.HaLex.Ndfa
import Language.HaLex.RegExp
import Language.HaLex.RegExp2Fa
import Language.HaLex.RegExpParser
import Language.HaLex.FaOperations
import Language.HaLex.Minimize
import Language.HaLex.Sentences


import Data.Maybe
import Data.List

import Test.HUnit


re = fromJust $ parseRegExp "('+'|'-')?[0-9]*('.'?)[0-9]+"

re'  = fromJust $ parseRegExp "[a-z][a-z]*"
re'' = fromJust $ parseRegExp "a[^a]*a"


ndfa     = regExp2Ndfa re
dfa      = ndfa2dfa ndfa
dfa_int  = beautifyDfa dfa

dfa_min  = minimizeDfa dfa_int
dfa_min' = beautifyDfa dfa_min


test_acceptNdfa = TestList [ ndfaaccept ndfa "109"    ~?= True
                           , ndfaaccept ndfa "+13"    ~?= True
                           , ndfaaccept ndfa "-13.4"  ~?= True
                           , ndfaaccept ndfa "-.15"   ~?= True
                           , ndfaaccept ndfa "+0.123" ~?= True
			   , ndfaaccept ndfa "-.2.3"  ~?= False
                           , ndfaaccept ndfa ""       ~?= False
                           ]

test_acceptDfa  = TestList [ dfaaccept dfa "109"    ~?= True
                           , dfaaccept dfa "+13"    ~?= True
                           , dfaaccept dfa "-13.4"  ~?= True
                           , dfaaccept dfa "-.15"   ~?= True
                           , dfaaccept dfa "+0.123" ~?= True
			   , dfaaccept dfa "-.2.3"  ~?= False
                           , dfaaccept dfa ""       ~?= False
                           ]

test_acceptDfamin  = TestList [ dfaaccept dfa_min "109"    ~?= True
                              , dfaaccept dfa_min "+13"    ~?= True
                              , dfaaccept dfa_min "-13.4"  ~?= True
                              , dfaaccept dfa_min "-.15"   ~?= True
                              , dfaaccept dfa_min "+0.123" ~?= True
			      , dfaaccept dfa_min "-.2.3"  ~?= False
                              , dfaaccept dfa_min ""       ~?= False
                              ]


test_accept_gen_sentences =
    TestList [  and (map (dfaaccept dfa_min') sentences_re) ~?= True
             ,  and (map (matches'  re)       sentences_re) ~?= True
             ,  and (map (ndfaaccept ndfa)    sentences_re) ~?= True
             ]
       where sentences_re = sentencesRegExp re


dfaToHaskell = toHaskell dfa_int "Dfa_RE"


main = do runTestTT test_acceptNdfa
          runTestTT test_acceptDfa
          runTestTT test_acceptDfamin
          runTestTT test_accept_gen_sentences



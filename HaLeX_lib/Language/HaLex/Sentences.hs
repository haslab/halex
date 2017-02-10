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
-- Generation of sentences for (regular) languages defined via
--   Regular Expressions and Finite Autumata
--
-----------------------------------------------------------------------------

module Language.HaLex.Sentences ( sentencesRegExp
                                , sentencesNdfa
                                , sentencesDfa
                                , onePathDfa
                                ) where

import Language.HaLex.Dfa
import Language.HaLex.Ndfa
import Language.HaLex.RegExp
import Language.HaLex.RegExp2Fa
import Language.HaLex.RegExpParser
import Language.HaLex.FaOperations
import Language.HaLex.Minimize
import Data.Maybe
import Data.List

-----------------------------------------------------------------------------
-- * Generating Sentence


-- | Generates a set of sentences of the language defined by a given
--   Regular Expression

sentencesRegExp :: Ord sy => RegExp sy -> [[sy]]
sentencesRegExp =  sentencesDfa . regExp2Dfa

-- | Generates a set of sentences of the language defined by a given
--   NonDerterministic Finite Automaton

sentencesNdfa :: (Ord sy , Ord st) => Ndfa st sy -> [[sy]]
sentencesNdfa =  sentencesDfa . minimizeDfa . ndfa2dfa 


-- | Generates a set of sentences of the language defined by a given
--   Deterministic Finite Automaton.
-- 
--   It computes a set of paths starting from the start state and ending
--   in an accepting state, which include all transitions/edges of the
--   automaton.
--
--   This function does not computes the smallest set (of paths/sentebces),
--   as computed by the "Chinese Postman Problem"
--
--   Function written by MSc student José Nuno Macedo (72424)
--   in the context of the 2016/17 edition of the course 
--       "Analysis and Testing of Software", MIEI, Univ. Minho.
--

sentencesDfa ::  (Ord st, Eq sy, Ord sy) => Dfa st sy -> [[sy]]
sentencesDfa = nub . sentencesDfa'

-- | This auxiliar function uses the transition table computed from the
--   given automaton to generate a finite set of sentences the the
--   language.

sentencesDfa' ::  (Ord st, Ord sy) => Dfa st sy -> [[sy]]
sentencesDfa' d = sentences d tt tt
    where tt = transitionTableDfa d

-- | This function generates all paths (corresponding to valid sentences
--   of the language) that cover all transitions of the finite automaton. 
--   The transition table serves two purposes when calling this function:
--       - to know the transitions of the automaton
--       - to serve has the state recording all transitions not used (yet)
--         (in the begining this list should be the full transition
--          table of the dfa, and the function terminates when this
--          list is empty: no more tarnsitions need to be covered)

sentences ::  (Ord st, Ord sy)
          => Dfa st sy               -- ^ Automaton
          -> [(st, sy, st)]          -- ^ Dfa's Transition Table
          -> [(st, sy, st)]          -- ^ Table with transitions to be used
          -> [[sy]]                  -- ^ List of sentences

sentences     _               _ []      = []
sentences d@(Dfa _ _ s z _) tt mustUse = sys ++ rec_call
 where
       -- First, we compute all paths from the start state to each state
       -- in the set of final sates.  Each path produces the list of
       -- transitions that need to be used and the sentence formed by
       -- that path
       
       (newMustUses, sys) = unzip [onePath tt mustUse [] fs' s | fs' <- z ]

       -- The lists of transitions to be used (produced by each path)
       -- is merged into a sinle list of transitions still to be used

       newMustUse = foldr1 intersect newMustUses

       -- Recursive call with the new list os transitions still to be used.
       --   note that if the new transitions (to be used) are the same to the
       --   received ones, no progress was made. Thus no (non-finishing)
       --   recursive call is performed.

       rec_call = if   newMustUse == mustUse
                  then []
                  else (sentences d tt newMustUse)


-- | This function computes one path from a given start state to a given final
--   state. The function does not repeat transitions. This function
--   "walks backwards": it starts from the final state back to the start one.
--
--   It receives the Dfa's transition table (tt), the table with the
--   transitions that Can Be Used (cbu), the labels of the transitions used
--   thus far (sys), the final state (ft), the start state (st).
--   It returns a pair:
--        the transitions that were not used in this path 
--        the list of labels used in the path


onePath :: (Eq sy, Eq st)
        => [(st, sy, st)]           -- ^ Dfa's Transition Table
        -> [(st, sy, st)]           -- ^ Table with transitions to be used
        -> [sy]                     -- ^ list of labels (used so far) 
        -> st                       -- ^ final state
        -> st                       -- ^ start state
        -> ([(st, sy, st)] , [sy])  

onePath tt cbu sys ft st
     | ft == st   = (cbu, sys)
     | otherwise  = onePath tt (delete k cbu) (symbol:sys) before_f st
                -- at each recursive call it performs a backwards step
                -- the new final state is the origin of the chosen transition
                -- (where the previous final state (ft) is the destination).
                -- The transition used is deleted from can be used.
                -- The initial state (st) and trans. table (tt) do not change.
 where    
       -- computing the lists of transitions with the final state (ft)
       -- as destination: both for the trans. table and the can be used
       -- transitions
       
       priorityList =  filter (\(a,_,c) -> c == ft) cbu
       p2 = filter (\(a, _, c) -> a /= c && c == ft) tt

       -- selects the transition from the computed lsits, prefering
       -- the transitions coming from the can be used trable.

       k@(before_f, symbol, _) = head $ priorityList ++ p2


-- | This function computes one sentence of the language defined by
--   a deterministic fininte automaton

onePathDfa  ::  (Ord st, Ord sy) => Dfa st sy  -> [sy]
onePathDfa dfa@(Dfa v q s z d) = snd $ onePath ttdfa ttdfa [] (head z) s 
      where ttdfa = transitionTableDfa dfa

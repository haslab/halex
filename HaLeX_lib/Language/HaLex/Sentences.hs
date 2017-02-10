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

module Language.HaLex.Sentences where

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
--   This function does not computes the smallest set of paths, as
--   computed by the "Chinese Postman Problem"
--
--   Function written by MSc student José Nuno Macedo (72424)
--   in the context of the 2016/17 edition of the course 
--       "Analysis and Testing of Software", MIEI, Univ. Minho.
--

sentencesDfa ::  (Ord st, Eq sy, Ord sy) => Dfa st sy -> [[sy]]
sentencesDfa = nub . sentencesDfa'

-- | 
--    dado um automato finito, calcular caminhos de modo a cobrir todos os edges - produz resultados repetidos, devido ao resultado de transitionTableDfa

sentencesDfa' ::  (Ord st, Ord sy) => Dfa st sy -> [[sy]]
sentencesDfa' d = sentences d tt tt
 where tt = transitionTableDfa d

--  | 
-- 
-- 

sentences ::  (Ord st, Ord sy)
          => Dfa st sy               -- ^ Automaton
	  -> [(st, sy, st)]          -- ^ Dfa's Transition Table
	  -> [(st, sy, st)]          -- ^ Table with transitions to be used
	  -> [[sy]]                  -- ^ List of sentences

sentences     _               _ []      = []
sentences d@(Dfa _ _ s z _) tt mustUse = sys ++ rec_call
          --uma ida do inicio até cada um dos fins, mais chamada recursiva
 where (newMustUses, sys) = unzip [onePath tt mustUse [] fs' s | fs' <- z ]
                        -- calcular um caminho do inicio até cada um dos finais,
			-- tentando passar por o máximo de mustUse

       newMustUse = foldr1 intersect newMustUses
                        -- descobrir quantos edges faltam cobrir

       rec_call = if newMustUse == mustUse
                  then []
		  else (sentences d tt newMustUse)
                        -- cortar chamada recursiva se não se consegue
			-- progresso (impede ciclos infinitos que só
			-- adicionam lixo)



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


onePathDfa  ::  (Ord st, Ord sy) => Dfa st sy  -> [sy]
onePathDfa dfa@(Dfa v q s z d) = snd $ onePath ttdfa ttdfa [] (head z) s 
      where ttdfa = transitionTableDfa dfa

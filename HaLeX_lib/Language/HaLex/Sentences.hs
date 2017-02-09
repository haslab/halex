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

--check transition table of a string regExp
checkTable = transitionTableDfa . regExp2Dfa . fromJust . parseRegExp

--calculo das strings minimas necessarias para percorrer todos os edges do automato
chinesTest =  sentencesDfa' . regExp2Dfa . fromJust . parseRegExp

--  igual ao de cima mas sem repetidos
sentencesRegExp :: Ord sy => RegExp sy -> [[sy]]
sentencesRegExp =  sentencesDfa . regExp2Dfa


sentencesNdfa :: (Ord sy , Ord st) => Ndfa st sy -> [[sy]]
sentencesNdfa =  sentencesDfa . minimizeDfa . ndfa2dfa 


-- dado um automato finito, calcular caminhos de modo a cobrir todos os edges - com repetidos removidos
--
--  function written by MSc student: José Nuno Macedo, 72424
--

sentencesDfa ::  (Ord st, Eq sy, Ord sy) => Dfa st sy -> [[sy]]
sentencesDfa = nub . sentencesDfa'

--dado um automato finito, calcular caminhos de modo a cobrir todos os edges - produz resultados repetidos, devido ao resultado de transitionTableDfa

sentencesDfa' ::  (Ord st, Ord sy) => Dfa st sy -> [[sy]]
sentencesDfa' d = chines' d l l
 where l = transitionTableDfa d

--dado um automato finito, calcular caminhos de modo a cobrir todos os edges
-- @d autómato a considerar
-- @l transition table completa
-- @mustUse edges por usar
--output é lista de strings resultante

chines' ::  (Ord st, Ord sy) => Dfa st sy -> [(st, sy, st)] -> [(st, sy, st)] -> [[sy]]
chines' _ _ [] = []
chines' d@(Dfa _ _ is fs _) l mustUse = s ++ rec_call --uma ida do inicio até cada um dos fins, mais chamada recursiva
 where (newMustUses, s) = unzip [umCaminho l mustUse [] fs' is | fs' <- fs ] --calcular um caminho do inicio até cada um dos finais, tentando passar por o máximo de mustUse
       newMustUse = foldr1 intersect newMustUses -- descobrir quantos edges faltam cobrir
       rec_call = if newMustUse == mustUse then [] else (chines' d l newMustUse) --cortar chamada recursiva se não se consegue progresso (impede ciclos infinitos que só adicionam lixo)

-- :: transition table, lista dos a consumir, str até agora, simbolo final, simbolo inicial -> lista de strings


umCaminho :: (Eq sy, Eq st)
          => [(st, sy, st)] -> [(st, sy, st)] -> [sy] -> st -> st
          -> ([(st, sy, st)] , [sy])

umCaminho l mustUse str f i
 | f == i = (mustUse, str)
 |otherwise = umCaminho l (delete k mustUse) (symbol:str) before_f i --cada chamada recursiva dá um passo "atrás", voltando atrás um estado até chegar ao inicial. Remove-se edge usado da lista mustUse
 where k@(before_f, symbol, _) = head $ priorityList ++ p2 --tenta-se prioritariamente usar edges da lista mustUse
       priorityList =  filter (\(a,_,c) -> c==f) (mustUse)
       p2 = filter (\(a, _, c) -> a /= c && c == f) l



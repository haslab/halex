{-
 When you try to convert a Dfa or Ndfa into a
 GraphViz File or to GraphViz, there are some
 additional quotes we need to remove so this is 
 one way of dealing with such file. All the 
 double 'double quotes' have been removed.
 -}

import Data.List
import Data.Char

--To convert the file read into Tokens 
clex :: String -> [Token]
clex (c:cs)	| isDigit c = num_token : clex rest_num_token
		| isAlpha c = var_token : clex rest_var_token
		| otherwise = [c]:clex cs where
			var_token = c:takeWhile isIdChar cs
			num_token = c:takeWhile isDigit cs
			rest_num_token = dropWhile isDigit cs
			rest_var_token = dropWhile isIdChar cs
clex [] = []

isIdChar :: Char -> Bool
isIdChar c= isAlphaNum c || c=='_'

type Token = String

--Finally Checking for occurance of two double quotes together
convert :: [Token] -> [Token]
convert [] =[]
convert [x] = [x]
convert (x:y:xs) = if(x=="\"" && y=="\"") then x:(convert xs) 
			else x:(convert (y:xs))

--This string is generated to be written to file
final :: [Token]->String
final [] = []
final (x:xs) = x++(final xs)


--Open the File generate by ndfa2graphviz or dfa2graphviz
main = do
x<- readFile "file_with_extra_quotes.dot"
writeFile "correct_file.dot" (final (convert (clex x)))

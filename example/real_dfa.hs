import Dfa
dfa = Dfa v q s z delta
  where 
	 v = "+-.0123456789"
	 q = [1,2,3,4,5,6]
	 s = 1
	 z = [4,6]
	 -- delta :: st -> sy -> st 
	 delta 1 '+' = 2
	 delta 1 '-' = 2
	 delta 1 '.' = 3
	 delta 1 '0' = 4
	 delta 1 '1' = 4
	 delta 1 '2' = 4
	 delta 1 '3' = 4
	 delta 1 '4' = 4
	 delta 1 '5' = 4
	 delta 1 '6' = 4
	 delta 1 '7' = 4
	 delta 1 '8' = 4
	 delta 1 '9' = 4
	 delta 2 '+' = 5
	 delta 2 '-' = 5
	 delta 2 '.' = 3
	 delta 2 '0' = 4
	 delta 2 '1' = 4
	 delta 2 '2' = 4
	 delta 2 '3' = 4
	 delta 2 '4' = 4
	 delta 2 '5' = 4
	 delta 2 '6' = 4
	 delta 2 '7' = 4
	 delta 2 '8' = 4
	 delta 2 '9' = 4
	 delta 3 '+' = 5
	 delta 3 '-' = 5
	 delta 3 '.' = 5
	 delta 3 '0' = 6
	 delta 3 '1' = 6
	 delta 3 '2' = 6
	 delta 3 '3' = 6
	 delta 3 '4' = 6
	 delta 3 '5' = 6
	 delta 3 '6' = 6
	 delta 3 '7' = 6
	 delta 3 '8' = 6
	 delta 3 '9' = 6
	 delta 4 '+' = 5
	 delta 4 '-' = 5
	 delta 4 '.' = 3
	 delta 4 '0' = 4
	 delta 4 '1' = 4
	 delta 4 '2' = 4
	 delta 4 '3' = 4
	 delta 4 '4' = 4
	 delta 4 '5' = 4
	 delta 4 '6' = 4
	 delta 4 '7' = 4
	 delta 4 '8' = 4
	 delta 4 '9' = 4
	 delta 5 '+' = 5
	 delta 5 '-' = 5
	 delta 5 '.' = 5
	 delta 5 '0' = 5
	 delta 5 '1' = 5
	 delta 5 '2' = 5
	 delta 5 '3' = 5
	 delta 5 '4' = 5
	 delta 5 '5' = 5
	 delta 5 '6' = 5
	 delta 5 '7' = 5
	 delta 5 '8' = 5
	 delta 5 '9' = 5
	 delta 6 '+' = 5
	 delta 6 '-' = 5
	 delta 6 '.' = 5
	 delta 6 '0' = 6
	 delta 6 '1' = 6
	 delta 6 '2' = 6
	 delta 6 '3' = 6
	 delta 6 '4' = 6
	 delta 6 '5' = 6
	 delta 6 '6' = 6
	 delta 6 '7' = 6
	 delta 6 '8' = 6
	 delta 6 '9' = 6



C Avery Guetheing Cosc3410 Assignment 4
	
	module mysubs
	contains  		
		
C		Molput puts one molecule in X,Y. X,Y are randomized. If molecule already there, search again		
		SUBROUTINE MOLPUT(arraytest)
			CHARACTER(len=1) :: arraytest(20,20)
			INTEGER K,COUNT,COUNT2,L
8	      	X = RAND(0)
			Y = RAND(0)
			COUNT = 20*X+1.0
			COUNT2= 20*Y+1.0
C			PRINT *, COUNT,COUNT2
			IF (arraytest(COUNT,COUNT2) .EQ. 'A') GOTO 8
			arraytest(COUNT,COUNT2) = 'A'
			RETURN
		END
				  
C 		looks above the given location		
		logical function above(X,Y,arraytest) result(tf1)
		CHARACTER(len=1) :: arraytest(20,20)	  
		INTEGER X,Y
		if(X .EQ. 1) goto 101
C out of bounds location search
		if(arraytest(X-1,Y) .EQ. 'A') goto 100
		goto 101
100		 tf1 = .true.
			goto 102
101 	 tf1 = .false.
102		 return	
		 END function above
		 
	
C checks below position for given molecule	 
		logical function below(X,Y,arraytest) result(tf2)
		CHARACTER(len=1) :: arraytest(20,20)	  
		INTEGER X,Y
		if(X .EQ. 20) goto 105
C 		out of bounds location search
		if(arraytest(X+1,Y) .EQ. 'A') goto 103
		goto 105
103     tf2 = .true.
		goto 104
105		tf2 = .false.
104		return
		END function below
		
C		checks left position for given molecule
		logical function left(X,Y,arraytest) result(tf3)
		CHARACTER(len=1) :: arraytest(20,20)	  
		INTEGER X,Y
		IF(Y .EQ. 1) goto 107
C 		out of bounds location search
		if(arraytest(X,Y-1) .EQ. 'A') goto 106
		goto 107
106		tf3 = .true.
		goto 108
107		tf3 = .false.
108		RETURN
		END function left
	
C		checks right position for given molecule
		logical function right(X,Y,arraytest) result(tf4)
		CHARACTER(len=1) :: arraytest(20,20)	  
		INTEGER X,Y
		IF(Y .EQ. 20) goto 110
C 		out of bounds location search
		if(arraytest(X,Y+1) .EQ. 'A')goto 109
		goto 111
109			tf4 = .true.
			goto 111
110			tf4 = .false.
111		RETURN
		END function right
				  		  
				  
	END module mysubs
		 
		
		  

	program numerical
		  use mysubs
		  implicit none
C 		bunch of variable declarations		  
		  integer i,p,x,y,j,k,trial,sum,test,tests
		  logical a,b,c,d 
		  CHARACTER(len=1) :: Grid(20,20)
		  a = .true.
		  b = .true.
		  c = .true.
		  d = .true.
		  trial = 1;
C randomize X and Y		  
		X = RAND(TIME())
		Y = RAND(TIME())
		
C run trial five times		
		do 9 tests  = 1,5 
  		    Grid = '*'
			sum = 0;
    		  i = 0;
    		  j = 0;
    		  k = 0;
  		  	   test = 0;
C call molput 100 times
			do 2 test = 1,100
			  call MOLPUT(Grid)
2 			continue
C look around molecule
				do 15 j = 1,20
					do 15 k = 1, 20
						if(Grid(j,k) .ne. 'A') goto 15
						!print *, "x,y is equal to ", j, k
						a = above(j,k,Grid)
						!print *, "A is equal to ", a
						b = below(j,k,Grid)
						!print *, "B is equal to ", b
						c = left(j,k,Grid)
						!print *, "L is equal to ", c
						d = Right(j,k,Grid)
						!print *, "R is equal to ", d
						if (a .eqv. .true.) goto 15 
						if (b .eqv. .true.) goto 15 
						if (c .eqv. .true.) goto 15 
						if (d .eqv. .true.) goto 15 
						sum = sum + 1
15						continue
			print *, "Trial: ", trial
			print *, "Size of Array = 20 x 20"
			print *, "Isolated Molecules Total:",sum
C uncomment for printed grid
		 	 do 22 i = 1, ubound(Grid, 1)
			  	print *, Grid(i, :)
 22			continue
		trial = trial + 1;
9 			continue

	
	end program numerical
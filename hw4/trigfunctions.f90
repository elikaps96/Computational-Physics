!
!Purpose: The purpose of this program is to compute the below
!	  trgonometirc functions and output the results to a file.
!
!Author:Eli Kapetanopoulos
!
!v1.0 6/6/17
!
!Running Instruction: compile using code gfortran trigfunctions.f90
!		     and then run using ./a.out and see both .dat files
!		    in the directory. 
!======================================================================

        Program Trig
        
        Implicit None
	!Delceare vairbales
        Real :: fx, gx,pi,dx,x
        Integer :: None
	Parameter (pi=acos(-1.0))
	dx=((2*pi)/100)
	x=0

	!Set two output files
        Open (Unit =10, File = 'sin1.dat', Status = 'Unknown')
	Open (Unit =20, File = 'cos1.dat', Status = 'Unknown')
	
	!Functions 
	fx = sin(x)
	gx = cos(x)
        
        
        

        Do While(x <= 2*pi)

	fx = sin(x)
        gx = cos(x)        
	Write(10,*) x,fx
        Write(20,*) x,gx
        x=x+dx

        End Do

        Close (Unit = 20)
	Close (Unit = 10)

        Stop; End Program

!Purpose: the purpose of this program is to calculate
!	  the second deriviation for the given eqaution
!	  using two different methods and comparing the errors
!
!Author: Eli Kapetanopoulos
!V1.0 7/7/17
!
!Instructions: To compile this program using gfortran secondDeriv.f90
!              run the a.out executable
!
!============================
      Program secondDeriv
          Implicit None

          Real :: xo,dx,f,df,dFdx3,dFdx5,exact
          Integer :: i

          !xo is what we will evaluate the function at
          !dx is our stepsize
          !F is our function: F(x)=sin(x)

	  Open (Unit = 10, File = 'threePoint.dat', Status = 'Unknown')
          Open (Unit = 20, File = 'fivePoint.dat', Status = 'Unknown')



          dx = 1.0
          Do i = 1,5
          dx = dx/10.0
          xo = 26.0
          F = (xo*sin(xo))
	  exact = (2*cos(xo)-xo*(sin(xo)))


          !three point aprox
	  dFdx3 = ((F + dx) - (2*F)+(F - dx))/(dx**2)
	  Write(10,*) dx, abs(dFdx3 - exact)
	
	  !five point aprox
          dFdx5 = (-(F + 2*dx) +(16*(F + dx))- (30*F) + (16*(F - dx))&
          -(F - 2*dx)/12*(dx**2))   
          Write(20,*) dx, abs(dFdx5 - exact)

          
          End DO
  

	  close(10);close(20);
          Stop
          End Program

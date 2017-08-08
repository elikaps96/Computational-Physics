!Purpose: the purpose of this program is to use  Maxwell–Boltzmann
!	  calculation and trapezodil integration to plot temperature
!	  vs Velocity.
!
!Author: Eli Kapetanopoulos
!V1.0 7/10/17
!
!Instructions: To compile this program using gfortran maxwell.f90
!              run the a.out executable
!
!============================
      Program boltzman
          Implicit None

          Double Precision :: a, b, c, h_v, h_y, f, x, y, v_max, int
	      Double Precision :: Tc, T, m, beta,k,fvT,y_a, y_b 
          Integer :: i, n, j
          Real ::  sum, nint
          REAL, PARAMETER :: Pi = 3.1415927
          

	  Write(*,*) "Compute fraction of molecules whose speeds v < v_max:"
	  Write(*,*) "Enter temperature in Celsius: "
 	  Read(*,*) Tc
 	  Write(*,*) "Enter upper velocity limit in m/sec: "
  	  Read(*,*) v_max
	  Write(*,*) "Enter integration step size in m/sec (e.g., 1.): "
	  Read(*,*) h_v

	 !Convert from celsius to Kelvin
	 T = 273.15 + Tc
	 !Nitrogen, argon, oxygen
	 m = (28*(0.78)+32*(0.21)+40*(0.01))/1000
	 !K constant
	 k = 8.3145
	
         beta = sqrt((2*k*T)/m) 
	 a = 0.0; b = v_max;

	 !Algebra for lower limit
         y_a = a/beta 
	 !Algebra for upper limit
	 y_b = b/beta 
	 h_y = h_v/beta
	 !step-size
	 nint = (y_b-y_a)/h_y 
	 n = int(nint,4)
	
	!Output file used for plot
	Open (Unit = 10, File = 'maxwell5000.dat', Status = 'Unknown')


	! speeds from 0 to 5000 m/s
	 Do i = 0, 5000, 100 
		b = real(i,8);
		y_b = b/beta;
		h_y = (y_b-y_a)/real(n,8)
		sum = 0.0
		
		!Trapezodial rule
		Do j=1, n-1

            		y = y_a+h_y*real(j,8)
                        sum = sum + f(y)
		
        	End Do
		
        	fvT = ((f(y_a)+f(y_b)+(2.0)*(sum))/2.0)*h_y
        	fvT = ((4.0*pi)*fvT)/(sqrt(pi)**3)
		Write (10, *) b, fvT*100.0
    End Do

          close (10)
	  Stop; End Program
	
	  !F(x) function
      Double Precision  Function f(x)
            Implicit None
            Double Precision :: x 
            f = (x**2*exp(-x**2))
          return 
          End Function 

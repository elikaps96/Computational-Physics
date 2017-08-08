!Purpose: the purpose of this program is to use  
!
!Author: Eli Kapetanopoulos
!V1.0 7/20/17
!
!Instructions: To compile this program using gfortran sho.f90
!              run the a.out executable
!
!============================
      Program simpleharm
          Implicit None

	 !Declarations
      Double Precision :: xi,xf, y, et,w,vi,vf,vt,dxdt 
	  Double Precision :: ti, tf, tmax, k, m, dt
	  Integer :: i	
	
	  xi = 1.0; vi = 0.0
	  dt = 0.001; tmax = 20
	  k = 1.2 !Spring constant
	  m = 1.0 !kg
	  w = sqrt(k/m)
	  
	 !Open data files
	  Open(Unit = 10, File = 'exact.dat', Status = 'Unknown')
	  Open(Unit = 20, File = 'approximation.dat', Status = 'Unknown')
	  Open(Unit = 30, File = 'energy.dat', Status = 'Unknown')
	 
	 !exact solution
	 Do i = 1, 20000
	  
	  xf = xi*cos(w*tf)+(vi/w)*sin(w*tf)		
	  tf = i*dt
	  
	  write(10,*)tf,xf
	  
	 End Do
	 
	 ti = 0
	 !Approximation 
	 Do While (ti .le. tmax)
  	  tf = ti + dt

	  call euler_res(xi,vi,ti,dt,k,m,vf, xf)
	  write(20,*),tf,xf
	  
	  
	  !re-declare 
	  xi = xf
	  vi = vf
	  ti = tf
      et = (1.0/2.0)*m*(vi**2)+ (1.0/2.0)*k*(xi**2)
	
      write(30,*)tf,et  
	 End Do

	 close(10);close(20);close(30)
      
	 stop; End Program
        

	 Subroutine euler_res(xi,vi,ti,dt,k,m,vf, xf)
	  Double Precision, INTENT(in) :: xi,vi,ti, dt,k,m
	  Double Precision, INTENT(out) :: vf,xf
        vf = vi +dt*(-k/m)*xi  
        xf = xi + dt*vi

         End Subroutine euler_res


          
		

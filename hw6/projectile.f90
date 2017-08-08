!Purpose: The purpose of this program is to take
!         an angle and initial velocity of a launced
!         projectile from a user and calculate the 
!         final landing position.      
!
!Author:Eli Kapetanopoulos
!
!V1.0 6/27/17
!
!Instructions: To comeplie this program use gfortran
!          and the file name projectile.f90
!          Then run the a.out file.      
!================================================

      Program projectile
          !Variable decleration
          Implicit None
          
          Real :: Vfx, Vfy, Vo, Vox, Voy, ax, dt, time, theta, pi, g, t
          Real :: x, y, tmax, part2,height
          Real :: xi(150),yi(150)
          Integer :: i
          Parameter (pi =3.14159)
          parameter (g = 9.81)
          dt = .05
          t = 0

          !Read in Vo and theta from user
          Print*, 'Enter the angle theta: '
          Print*, ''
          Read*, theta

          Print*, 'Enter in the intial velocity: '
          Print*, ''
          Read*, Vo
          
          !Check user inputs are valid
          !Open coresponding output file 
          !and do calculations
          If (( theta == 15) .AND. (Vo == 50)) Then
            Open (Unit = 10, File = 'traji15.dat', Status = 'Unknown')


        Else If (( theta == 30) .AND. (Vo == 50)) Then
            Open (Unit = 10, File = 'traji30.dat', Status = 'Unknown')
           

        Else If (( theta == 45) .AND. (Vo == 25)) Then
            Open (Unit = 10, File = 'traji45.dat', Status = 'Unknown')
       


        ELse If (( theta == 60) .AND. (Vo == 25)) Then
            Open (Unit = 10, File = 'traji60.dat', Status = 'Unknown') 
         

        ELse
           Print*, 'invalid inputs, RE-RUN PROGRAM'
       End IF

       !Find Voy Vox
       theta = (theta * pi)/180
       
       Voy = (sin(theta)*Vo)
       Vox = (cos(theta)*Vo)

       !Print initial velocities
       print*,Voy,'Initial Y velocity'
       Print*,Vox, 'Initial X velocity'

       !Calculate max time based on height
       tmax = 2*((Voy/g))

       !Calculate Displacement
       x = Vox * tmax
       height  = Vo * sin(theta)*tmax + ((g*tmax**2)/2)       
        

       !Loop through projectile in air to build data files for graph
       Do i =0,150
    	
    	xi(i) = (Vox * t)
       ! yi(i) = ((Voy*t) +  ( (g * (1)))/2)
        yi(i) = (Voy*t) + (0.5*g*t*t)
    	Write(10,*),xi(i),yi(i)
    	t = t + dt
        if (t>=tmax)exit
        if (yi(i)<0)exit

      End Do
                                

       Close (10)
       Print*,'The final distance in the x direction is: ',x
       Print*,'The maximum height is: ',height
       Print*,'The max height is reached at t = ',tmax/2
       Print*, 'The total time in air is: ',tmax
          Stop; End Program


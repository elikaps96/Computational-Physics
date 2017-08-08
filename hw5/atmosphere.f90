!Purpose: The purpose of this program is to
!	  is to model the Temperature and
!	  pressure at the Troposphere, and lower
!	  and upper Stratopshere. 
!
!Author: Eli Kapetanopoulos
!
!V1.0 6/13/17
!
!Instructions: To comeplie this program use gfortran
!	       and the file name atmosphere.f90
!	       Then run the a.out file.
!==============================================

	Program atmosphere

	!Variable decleration
	Implicit None
	
	Real :: temp,pressure,density,height,i
	Integer :: dh
	Parameter ( dh=100 )
	i = 0

	!Open the three files that will be writen to in the loop
	Open (Unit = 10,File = "temp.dat",Status = "Unknown")
	Open (Unit = 20,File = "pressure.dat",Status = "Unknown")
	Open (Unit = 30,File = "density.dat",Status = "Unknown")

	!Loop from 0 to 50,000
	Do While (i <= 50000)
	
		!if i equals the altutie of the Troposphere write to files
		if (i <= 11000) Then
			temp = 15.04 - (0.00649)*i
			write (10,*)temp,i

			pressure = (101.29) * ((temp + 273.1) / (288.08))**(5.256)
			write (20,*)pressure,i

			density = ((pressure)/((0.2869)*(temp + 273.1)))
			write (30,*)density,i

		!If i equals the altitude of the  Stratosphere write to files 
		else if ( i >11000 .AND. i  <= 25000) Then
			temp = -56.46
                        write (10,*)temp,i

                        pressure = (22.65)*exp(1.73-(0.000157)*i)
                        write (20,*)pressure,i

                        density = ((pressure)/((0.2869)*(temp + 273.1)))
                        write (30,*)density,i

		!If i equals the altitude of the  Stratosphere write to files 
                else if ( i >25000 .AND. i  <= 50000) Then 
                        temp = -131.21+(0.00299)*i
                        write (10,*)temp,i

                        pressure = (2.488)*((temp+273.1)/(216.6))** (-11.388)
                        write (20,*)pressure,i

                        density = ((pressure)/((0.2869)*(temp + 273.1)))
                        write (30,*)density,i
		
		
		End if
	i = i + dh
	End Do
	!Close all three files
	Close (10);Close (20);Close (30);
	Stop; End Program atmosphere
	

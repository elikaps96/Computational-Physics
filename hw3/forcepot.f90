!====================================================================
!Purpose: The purpose of this program is to compute the gravitational 
!         Force between the Sun and Jupitor at the locations of the 
!         nine planets including Pluto. 
!
!
!
!Author: Eli Kapetanopoulos
!
!V1.0 6/6/2017
!====================================================================

	Program GravForce
	
	!Declare Variables
	Double Precision :: G, M1, M2, r, Fg, Pe
	Double Precision :: massSun, massJupiter
	Double Precision :: dMercury, dVenus, dEarth, dMars, dJupiter, dSaturn
	Double Precision :: dUranus, dNeptune, auToM  
	
	!Variable values
	G = 6.67E-1 !Grav constant N*m**2/kg**@
	massSun = 2.0E30 !kg
	MassJupiter = 1.898E27 !kg
	dMercury = 0.39 !A.U
	dVenus = 0.723 !A.U
	dEarth = 1 !A.U
	dMars = 1.524 !A.U
	dJupiter = 5.103 !A.U
	dSaturn = 9.539 !A.U
	dUranus = 19.18 !A.U
	dNeptune = 30.06 !A.U
	dPluto = 39.53 !A.U
	auToM = 1.496E11 !1 A.U to meeterr


	!Force of gravity equation with variables
	M1 = massSun 
	M2 = massJupiter
	Fg = G*((M1*M2)/(r**2))

	!Potential energy equation with variables
	Pe = -G*((massSun)/(r))

	!output
	
	!Mercury
	r = dMercury*auToM
	Fg= G*((M1*M2)/(r*r))
	Pe = -G*((massSun)/(r))
	Print*,"============================================"
	Print*,"Mercury"
	Print*,"Distance to sun: ",dMercury,"(A.U)"
	Print*,"Force of gravity: ",Fg
	Print*,"Potential energy: ",Pe
	
	!Venus
        r = dVenus*auToM
        Fg= G*((M1*M2)/(r*r))
	Pe = -G*((massSun)/(r))
        Print*,"============================================"
        Print*,"Venus"
        Print*,"Distance to sun: ",dVenus,"(A.U)"
        Print*,"Force of gravity: ",Fg
	Print*,"Potential energy: ",Pe

	!Earth
        r = dEarth*auToM
        Fg= G*((M1*M2)/(r*r))
	Pe = -G*((massSun)/(r))
        Print*,"============================================"
        Print*,"Earth"
        Print*,"Distance to sun: ",dEarth,"(A.U)"
        Print*,"Force of gravity: ",Fg
	Print*,"Potential energy: ",Pe

	!Mars
        r = dMars*auToM
        Fg= G*((M1*M2)/(r*r))
	Pe = -G*((massSun)/(r))
        Print*,"============================================"
        Print*,"Mars"
        Print*,"Distance to sun: ",dMars,"(A.U)"
        Print*,"Force of gravity: ",Fg
	Print*,"Potential energy: ",Pe

	!Jupiter
        r = dJupiter*auToM
        Fg= G*((M1*M2)/(r*r))
	Pe = -G*((massSun)/(r))
        Print*,"============================================"
        Print*,"Jupiter"
        Print*,"Distance to sun: ",dJupiter,"(A.U)"
        Print*,"Force of gravity: ",Fg
	Print*,"Potential energy: ",Pe

	!Saturn
        r = dSaturn*auToM
        Fg= G*((M1*M2)/(r*r))
	Pe = -G*((massSun)/(r))
        Print*,"============================================"
        Print*,"Saturn"
        Print*,"Distance to sun: ",dSaturn,"(A.U)"
        Print*,"Force of gravity: ",Fg
	Print*,"Potential energy: ",Pe

	!Uranus
        r = dUranus*auToM
        Fg= G*((M1*M2)/(r*r))
	Pe = -G*((massSun)/(r))
        Print*,"============================================"
        Print*,"Uranus"
        Print*,"Distance to sun: ",dUranus,"(A.U)"
        Print*,"Force of gravity: ",Fg
	Print*,"Potential energy: ",Pe

	!Neptune
        r = dNeptune*auToM
        Fg= G*((M1*M2)/(r*r))
	Pe = -G*((massSun)/(r))
        Print*,"============================================"
        Print*,"Neptune"
        Print*,"Distance to sun: ",dNeptune,"(A.U)"
        Print*,"Force of gravity: ",Fg
	Print*,"Potential energy: ",Pe

	!Pluto
        r = dPluto*auToM
        Fg= G*((M1*M2)/(r*r))
	Pe = -G*((massSun)/(r))
        Print*,"============================================"
        Print*,"Pluto"
        Print*,"Distance to sun: ",dPluto,"(A.U)"
        Print*,"Force of gravity: ",Fg
	Print*,"Potential energy: ",Pe

	Stop; End Program Gravforce

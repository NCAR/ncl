	PROGRAM CCPSPV

C
C Define error file, Fortran unit number, and workstation type,
C and workstation ID.
C
        PARAMETER (IERRF=6, LUNIT=2, IWTYPE=SED_WSTYPE, IWKID=1)

        PARAMETER (K=40,N=40,LRWK=1000,LIWK=1000)
	REAL Z(K,N), RWRK(LRWK)
	INTEGER M, IWRK(LIWK)

	CALL GETDAT (Z, K, M, N) 

C Mark values near the center of the array with a value of -1.0
	DO 5, I=2*K/5,3*K/5
	   DO 15, J=2*N/7,3*N/7
	      Z(I,J)=-1.0
 15	   CONTINUE
  5	CONTINUE

C Open GKS
        CALL GOPKS (IERRF, ISZDM)
        CALL GOPWK (IWKID, LUNIT, IWTYPE)
        CALL GACWK (IWKID)

C Tell Conpack that data points with a value of -1.0 shouldn't be drawn
	CALL CPSETR('SPV - SPECIAL VALUE',-1.0)

C Force Conpack to outline the special value region
	CALL CPSETI('PAI - PARAMETER ARRAY INDEX',-2)
	CALL CPSETI('CLU - CONTOUR LEVEL USE FLAGS',1)
C Initialize Conpack
	CALL CPRECT(Z,K,M,N,RWRK,LRWK,IWRK,LIWK)
C Draw perimeter
	CALL CPBACK(Z, RWRK, IWRK)
C Draw Contours
	CALL CPCLDR(Z,RWRK,IWRK)

C Close frame and close GKS
	CALL FRAME
        CALL GDAWK (IWKID)
        CALL GCLWK (IWKID)
        CALL GCLKS

	STOP
	END

	SUBROUTINE GETDAT (Z, K, M, N)
	INTEGER I,J,K,M,N
	REAL Z(K,N)

	M=K
	DO 10, I=1,M
	  DO 20, J=1,N
	    Z(I,J)= 10.E-5*(-16.*REAL(I**2*J) +
     +		    34*REAL(I*J**2) - REAL(6*I) + 93.)
  20	  CONTINUE
  10	CONTINUE

	RETURN
	END

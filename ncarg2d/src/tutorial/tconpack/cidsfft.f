	PROGRAM CIDSFT

        PARAMETER (NRAN=30,LRWK=3500,LIWK=4000)
        PARAMETER (MREG=50,NREG=50)
	REAL XRAN(NRAN), YRAN(NRAN), ZRAN(NRAN)
	REAL XREG(MREG), YREG(NREG), ZREG(MREG,NREG), RWRK(LRWK)
	INTEGER IWRK(LIWK)

	DATA XRAN /12., 60., 14., 33.,  8., 12., 43., 57., 22., 15.,
     1		   19., 12., 64., 19., 15., 55., 31., 32., 33., 29.,
     2		   18.,  1., 18., 42., 56.,  9.,  6., 12., 44., 19./
	DATA YRAN / 1.,  2.,  3., 53.,  7., 11., 13., 17., 19., 49.,
     1		    1., 31., 37.,  5.,  7., 47., 61., 17.,  5., 23.,
     2		   29.,  3.,  5., 41., 43.,  9., 13., 59.,  1., 67./
	DATA ZRAN /1.0, 1.5, 1.7, 1.4, 1.9, 1.0, 1.5, 1.2, 1.8, 1.4,
     1		   1.8, 1.7, 1.9, 1.5, 1.2, 1.1, 1.3, 1.7, 1.2, 1.6,
     2		   1.9, 1.0, 1.6, 1.3, 1.4, 1.8, 1.7, 1.5, 1.1, 1.0/


C Open GKS
	CALL OPNGKS
C
C  Find the min and max data values.
C
      XMIN = 0.0
      XMAX = 65.0
      YMIN =  0.0
      YMAX = 68.0
C
C Choose the X and Y coordinates for interpolation points on the 
C regular grid.
C
      XREG(1)=XMIN
      DO 101 I=2,MREG
        XREG(I)=XMIN + (XMAX - XMIN)* REAL(I-1)/(MREG-1)
  101 CONTINUE
C
      YREG(1)=YMIN
      DO 102 I=2,NREG
        YREG(I)=YMIN + (YMAX - YMIN)* REAL(I-1)/(NREG-1)
  102 CONTINUE

C Interpolate data onto a regular grid
	CALL IDSFFT (1,NRAN,XRAN,YRAN,ZRAN,
     +		MREG,NREG,MREG,XREG,YREG,ZREG,IWRK,RWRK)
C Initialize Conpack
	CALL CPRECT(ZREG, MREG, MREG, NREG, RWRK, LRWK, IWRK, LIWK)
C Draw Perimeter
	CALL CPBACK(ZREG, RWRK, IWRK)
C Draw Contours
	CALL CPCLDR(ZREG,RWRK,IWRK)

C Mark data points
	CALL MARK (XRAN,YRAN,NRAN)

C Close frame and close GKS
	CALL FRAME
	CALL CLSGKS

	STOP
	END

	SUBROUTINE MARK (XRAN, YRAN, NRAN)

	REAL XRAN(NRAN), YRAN(NRAN)
	INTEGER I

	CALL GETSET(DUM1,DUM2,DUM3,DUM4,XMIN,XMAX,YMIN,YMAX,IDUM5)
	CALL GSMKSC(.5)

        DO 11 I=1,NRAN
            CALL POINTS (XRAN(I), YRAN(I), 1, -4, 0)
  11    CONTINUE

	RETURN
	END

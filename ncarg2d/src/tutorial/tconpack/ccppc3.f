	PROGRAM CCPPC3

        PARAMETER (LRWK=3500,LIWK=4000,LMAP=75000)
        PARAMETER (MREG=50,NREG=50)
	REAL X(MREG),Y(NREG),ZREG(MREG,NREG), RWRK(LRWK)
	INTEGER IWRK(LIWK), MAP(LMAP)

	EXTERNAL CPDRPL

	CALL GETDAT (X, Y, ZREG, MREG, NREG, RWRK, IWRK, LRWK, LIWK)
C Open GKS
	CALL OPNGKS
C Initialize Areas
	CALL ARINAM(MAP,LMAP)
C Choose which labelling scheme will be used.
	CALL CPSETI('LLP - LINE LABEL POSTIONING FLAG',3)
C Label contours only when they are reasonably straight
	CALL CPSETR('PC3 - PENALTY SCHEME CONSTANT 3',10.0)
	CALL CPSETR('PW3 - PENALTY SCHEME WEIGHT 3',2.0)
C Initialize Conpack
	CALL CPRECT(ZREG, MREG, MREG, NREG, RWRK, LRWK, IWRK, LIWK)
C Force Conpack to chose contour levels
	CALL CPPKCL(ZREG, RWRK, IWRK)
C Modify Conpack chosen parameters
	CALL CPGETI('NCL - NUMBER OF CONTOURS',NCONS)
	DO 12, I=1,NCONS
	   CALL CPSETI('PAI - PARAMETER ARRAY INDEX',I)
C Force every line to be labeled.
	   CALL CPSETI('CLU - CONTOUR LEVEL USE	FLAG',3)
 12	CONTINUE
C Draw Perimeter
	CALL CPBACK(ZREG, RWRK, IWRK)
C Add contours to area map
	CALL CPCLAM(ZREG,RWRK,IWRK,MAP)
C Add labels to area map
	CALL CPLBAM(ZREG,RWRK,IWRK,MAP)
C Draw Contours
	CALL CPCLDM(ZREG,RWRK,IWRK,MAP,CPDRPL)
C Draw Labels
	CALL CPLBDR(ZREG,RWRK,IWRK)

C Close frame and close GKS
	CALL FRAME
	CALL CLSGKS

	STOP
	END

	SUBROUTINE GETDAT (X,Y,Z,M,N,RWRK,IWRK,LRWK,LIWK)

        PARAMETER (NRAN=30)

	REAL XRAN(NRAN), YRAN(NRAN), ZRAN(NRAN)
	REAL X(M), Y(N), Z(M,N), RWRK(LRWK)
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


C
C  Set the min and max data values.
C
      XMIN = 0.0
      XMAX = 65.0
      YMIN =  0.0
      YMAX = 68.0
C
C Choose the X and Y coordinates for interpolation points on the 
C regular grid.
C
      DO 101 I=1,M
        X(I)=XMIN + (XMAX - XMIN)* REAL(I-1)/M
  101 CONTINUE
C
      DO 102 I=1,N
        Y(I)=YMIN + (YMAX - YMIN)* REAL(I-1)/N
  102 CONTINUE

C Interpolate data onto a regular grid
	CALL IDSFFT (1,NRAN,XRAN,YRAN,ZRAN,M,N,M,X,Y,Z,IWRK,RWRK)

	RETURN
	END

	PROGRAM CCPRC

        PARAMETER (LRWK=3500,LIWK=4000,LMAP=75000)
        PARAMETER (M=50,N=50)
	REAL X(M),Y(N),Z(M,N), RWRK(LRWK)
	INTEGER IWRK(LIWK), MAP(LMAP)

	EXTERNAL CPDRPL

	CALL GETDAT (X, Y, Z, M, N, RWRK, IWRK, LRWK, LIWK)
C Open GKS
	CALL OPNGKS
C Initialize Areas
	CALL ARINAM(MAP,LMAP)
C Initialize Conpack
	CALL CPRECT(Z, M, M, N, RWRK, LRWK, IWRK, LIWK)
C Choose which labelling scheme will be used.
	CALL CPSETI('LLP - LINE LABEL POSITIONING FLAG',2)
	CALL CPSETR('RC1 - REGULAR SCHEME CONSTANT 1',.1)
C Force Conpack to chose contour levels
	CALL CPPKCL(Z, RWRK, IWRK)
C Modify Conpack chosen parameters
	CALL CPGETI('NCL - NUMBER OF CONTOUR LEVELS',NCONS)
	DO 12, I=1,NCONS
	   CALL CPSETI('PAI - PARAMETER ARRAY INDEX',I)
C Force every line to be labeled.
	   CALL CPSETI('CLU - CONTOUR LEVEL USE FLAG',3)
 12	CONTINUE
C Draw Perimeter
	CALL CPBACK(Z, RWRK, IWRK)
C Add contours and labels to area map
	CALL CPCLAM(Z, RWRK, IWRK, MAP)
	CALL CPLBAM(Z, RWRK, IWRK, MAP)
C Draw Contours
	CALL CPCLDM(Z, RWRK, IWRK, MAP, CPDRPL)
	CALL CPLBDR(Z,RWRK,IWRK)

C Close frame and close GKS
	CALL FRAME
	CALL CLSGKS

	WRITE (6,*) 'AREA MAP SIZE =',MAP(1) - MAP(6) + MAP(5)
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


      PROGRAM CCPPC1
C
C Define the error file, the Fortran unit number, the workstation type,
C and the workstation ID to be used in calls to GKS routines.
C
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)   ! NCGM
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=8,  IWKID=1)   ! X Windows
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=11, IWKID=1)   ! PDF
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=20, IWKID=1)   ! PostScript
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)

      PARAMETER (LRWK=3500,LIWK=4000,LMAP=75000)
      PARAMETER (MREG=50,NREG=50)
      REAL X(MREG),Y(NREG),ZREG(MREG,NREG), RWRK(LRWK)
      INTEGER IWRK(LIWK), MAP(LMAP)

      EXTERNAL CPDRPL

      CALL GETDAT (X, Y, ZREG, MREG, NREG, RWRK, IWRK, LRWK, LIWK)
C
C Open GKS
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Initialize Areas
C
      CALL ARINAM(MAP,LMAP)
C
C Choose which labelling scheme will be used.
C
      CALL CPSETI('LLP - LINE LABEL POSITIONING FLAG',3)
C
C Set the gradient parameter to label steep slopes
C
      CALL CPSETR('PC1 - PENALTY SCHEME CONSTANT 1',3.0)
C
C Initialize Conpack
C
      CALL CPRECT(ZREG, MREG, MREG, NREG, RWRK, LRWK, IWRK, LIWK)
C
C Force Conpack to chose contour levels
C
      CALL CPPKCL(ZREG, RWRK, IWRK)
C
C Modify Conpack chosen parameters
C
      CALL CPGETI('NCL - NUMBER OF CONTOUR LEVELS',NCONS)
      DO 12, I=1,NCONS
         CALL CPSETI('PAI - PARAMETER ARRAY INDEX',I)
C
C Force every line to be labeled.
C
         CALL CPSETI('CLU - CONTOUR LEVEL USE FLAG',3)
 12   CONTINUE
C
C Add contours to area map
C
      CALL CPCLAM(ZREG, RWRK, IWRK, MAP)
C
C Add labels to area map
C
      CALL CPLBAM(ZREG, RWRK, IWRK, MAP)
C
C Draw Perimeter
C
      CALL CPBACK(ZREG, RWRK, IWRK)
C
C Draw Labels
C
      CALL CPLBDR(ZREG,RWRK,IWRK)
C
C Draw Contours
C
      CALL CPCLDM(ZREG,RWRK,IWRK,MAP,CPDRPL)
C
C Close frame and close GKS
C
      CALL FRAME
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS

      STOP
      END

      SUBROUTINE GETDAT (X,Y,Z,M,N,RWRK,IWRK,LRWK,LIWK)

      PARAMETER (NRAN=30)

      REAL XRAN(NRAN), YRAN(NRAN), ZRAN(NRAN)
      REAL X(M), Y(N), Z(M,N), RWRK(LRWK)
      REAL XDELTA(50)
      INTEGER IWRK(LIWK)

      DATA XRAN /12., 60., 14., 33.,  8., 12., 43., 57., 22., 15.,
     +         19., 12., 64., 19., 15., 55., 31., 32., 33., 29.,
     +         18.,  1., 18., 42., 56.,  9.,  6., 12., 44., 19./
      DATA YRAN / 1.,  2.,  3., 53.,  7., 11., 13., 17., 19., 49.,
     +          1., 31., 37.,  5.,  7., 47., 61., 17.,  5., 23.,
     +         29.,  3.,  5., 41., 43.,  9., 13., 59.,  1., 67./
      DATA ZRAN /1.0, 1.5, 1.7, 1.4, 1.9, 1.0, 1.5, 1.2, 1.8, 1.4,
     +         1.8, 1.7, 1.9, 1.5, 1.2, 1.1, 1.3, 1.7, 1.2, 1.6,
     +         1.9, 1.0, 1.6, 1.3, 1.4, 1.8, 1.7, 1.5, 1.1, 1.0/
      DATA XDELTA/.00,.02,.04,.06,.08,.10,.12,.14,.16,.18,.20,
     +            .22,.24,.26,.28,.30,.32,.34,.36,.38,.40,.42,
     +            .44,.46,.48,.50,.52,.54,.56,.58,.60,.62,.64,
     +            .66,.68,.70,.72,.74,.76,.78,.80,.82,.84,.86,
     +            .88,.90,.92,.94,.96,.98/
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
         X(I)=XMIN + (XMAX - XMIN)*XDELTA(I)
 101  CONTINUE
C
      DO 102 I=1,N
         Y(I)=YMIN + (YMAX - YMIN)*XDELTA(I)
 102  CONTINUE
C
C Interpolate data onto a regular grid
C
      CALL IDSFFT (1,NRAN,XRAN,YRAN,ZRAN,M,N,M,X,Y,Z,IWRK,RWRK)
      
      RETURN
      END

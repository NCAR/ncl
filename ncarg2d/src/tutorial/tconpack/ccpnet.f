      PROGRAM CCPNET
C 
C Define error file, Fortran unit number, and workstation type,
C and workstation ID.
C 
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1, IWKID=1)
      
      PARAMETER (NRAN=30,LRWK=3500,LIWK=4000)
      PARAMETER (MREG=50,NREG=50)
      REAL XRAN(NRAN), YRAN(NRAN), ZRAN(NRAN)
      REAL XREG(MREG), YREG(NREG), ZREG(MREG,NREG), RWRK(LRWK)
      INTEGER IWRK(LIWK)
      
      DATA XRAN /12., 60., 14., 33.,  8., 12., 43., 57., 22., 15.,
     +     19., 12., 64., 19., 15., 55., 31., 32., 33., 29.,
     +     18.,  1., 18., 42., 56.,  9.,  6., 12., 44., 19./
      DATA YRAN / 1.,  2.,  3., 53.,  7., 11., 13., 17., 19., 49.,
     +     1., 31., 37.,  5.,  7., 47., 61., 17.,  5., 23.,
     +     29.,  3.,  5., 41., 43.,  9., 13., 59.,  1., 67./
      DATA ZRAN /1.0, 1.5, 1.7, 1.4, 1.9, 1.0, 1.5, 1.2, 1.8, 1.4,
     +     1.8, 1.7, 1.9, 1.5, 1.2, 1.1, 1.3, 1.7, 1.2, 1.6,
     +     1.9, 1.0, 1.6, 1.3, 1.4, 1.8, 1.7, 1.5, 1.1, 1.0/
C 
C Find the min and max data values.
C 
      XMIN = 0.0
      XMAX = 65.0
      YMIN =  0.0
      YMAX = 68.0
C 
C Choose the X and Y coordinates for interpolation points on the 
C regular grid.
C 
      DO 101 I=1,MREG
         XREG(I)=XMIN + (XMAX - XMIN)* REAL(I-1)/MREG
 101  CONTINUE
C 
      DO 102 I=1,NREG
         YREG(I)=YMIN + (YMAX - YMIN)* REAL(I-1)/NREG
 102  CONTINUE
      
      DO 103 I=1,NRAN
         ZRAN(I)=10.**(-6.)*ZRAN(I)
 103  CONTINUE
C
C Interpolate data onto a regular grid
C
      CALL IDSFFT (1,NRAN,XRAN,YRAN,ZRAN,
     +     MREG,NREG,MREG,XREG,YREG,ZREG,IWRK,RWRK)
C
C Open GKS and turn off clipping
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
      CALL GSCLIP(0)
C
C Set up exponent flags to get reasonable labels
C
      CALL CPSETI('NEU - NUMERIC EXPONENT USE FLAG',0)
C
C Initialize Conpack
C
      CALL CPRECT(ZREG, MREG, MREG, NREG, RWRK, LRWK, IWRK, LIWK)
C
C Draw Perimeter
C
      CALL CPBACK(ZREG, RWRK, IWRK)
C
C Draw Contours
C
      CALL CPCLDR(ZREG,RWRK,IWRK)
C
C Draw Labels
C
      CALL CPLBDR(ZREG,RWRK,IWRK)
C
C Close frame and close GKS
C
      CALL FRAME
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
      
      STOP
      END


      PROGRAM CIDSFFT
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

      PARAMETER (NRAN=30,LRWK=3500,LIWK=4000)
      PARAMETER (MREG=50,NREG=50)
      REAL XRAN(NRAN), YRAN(NRAN), ZRAN(NRAN)
      REAL XREG(MREG), YREG(NREG), ZREG(MREG,NREG), RWRK(LRWK)
      REAL XDELTA(MREG)
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
      DATA XDELTA/.00,.02,.04,.06,.08,.10,.12,.14,.16,.18,.20,
     +            .22,.24,.26,.28,.30,.32,.34,.36,.38,.40,.42,
     +            .44,.46,.48,.50,.52,.54,.56,.58,.60,.62,.64,
     +            .66,.68,.70,.72,.74,.76,.78,.80,.82,.84,.86,
     +            .88,.90,.92,.94,.96,.98/
C
C Open GKS
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
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
         XREG(I)=XMIN + (XMAX - XMIN)*XDELTA(I)
 101  CONTINUE
C
      YREG(1)=YMIN
      DO 102 I=2,NREG
         YREG(I)=YMIN + (YMAX - YMIN)*XDELTA(I)
 102  CONTINUE
C
C Interpolate data onto a regular grid
C
      CALL IDSFFT (1,NRAN,XRAN,YRAN,ZRAN,
     +    MREG,NREG,MREG,XREG,YREG,ZREG,IWRK,RWRK)
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
C Mark data points
C
      CALL MARK (XRAN,YRAN,NRAN)
C
C Close frame and close GKS
C
      CALL FRAME
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS

      STOP
      END

      SUBROUTINE MARK (XRAN, YRAN, NRAN)

      REAL XRAN(NRAN), YRAN(NRAN)
      INTEGER I

      CALL GETSET(DUM1,DUM2,DUM3,DUM4,XMIN,XMAX,YMIN,YMAX,IDUM5)
      CALL GSMKSC(.5)

      DO 11 I=1,NRAN
         CALL POINTS (XRAN(I), YRAN(I), 1, -4, 0)
 11   CONTINUE

      RETURN
      END

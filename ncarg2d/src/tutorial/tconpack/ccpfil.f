      PROGRAM CCPFIL

      PARAMETER (MREG=50,NREG=50)
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

      REAL XREG(MREG),YREG(NREG),ZREG(MREG,NREG)
      
      EXTERNAL COLOR
C      
C Get data array
C
      CALL GETDAT(XREG,YREG,ZREG,MREG,NREG)
C 
C Open GKS, open and activate a workstation.
C 
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C      
C Call Conpack color fill routine
C      
      CALL TCCPFIL(ZREG,MREG,NREG,-15,COLOR,IWKID)
C      
C Close frame
C      
      CALL FRAME
C 
C Deactivate and close workstation, close GKS.
C 
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
      
      STOP
      END
      
      SUBROUTINE TCCPFIL(ZREG,MREG,NREG,NCL,COLOR,IWKID)
      
      PARAMETER (LRWK=5000,LIWK=5000,LMAP=50000,NWRK=5000,NOGRPS=5)
      REAL ZREG(MREG,NREG),RWRK(LRWK), XWRK(NWRK), YWRK(NWRK)
      INTEGER MREG,NREG,IWRK(LIWK)
      INTEGER MAP(LMAP),IAREA(NOGRPS),IGRP(NOGRPS)
      
      EXTERNAL FILL
      EXTERNAL COLOR
C      
C Set up color table
C      
      CALL COLOR(IWKID)
C      
C Initialize Areas
C      
      CALL ARINAM(MAP, LMAP)
C      
C Set number of contour levels and initialize Conpack
C      
      CALL CPSETI('CLS - CONTOUR LEVEL SELECTION FLAG',NCL)
      CALL CPRECT(ZREG, MREG, MREG, NREG, RWRK, LRWK, IWRK, LIWK)
C      
C Add contours to area map
C      
      CALL CPCLAM(ZREG, RWRK, IWRK, MAP)
C      
C Set fill style to solid, and fill contours
C      
      CALL GSFAIS(1)
      CALL ARSCAM(MAP, XWRK, YWRK, NWRK, IAREA, IGRP, NOGRPS, FILL)
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
      CALL CPCLDR(ZREG,RWRK,IWRK)
      
      RETURN
      END
      
      SUBROUTINE FILL (XWRK,YWRK,NWRK,IAREA,IGRP,NGRPS)
C 
      DIMENSION XWRK(*),YWRK(*),IAREA(*),IGRP(*)
      
      DO 10, I=1,NGRPS
         IF (IGRP(I).EQ.3) IAREA3=IAREA(I)
 10   CONTINUE
      
      IF (IAREA3 .GT. 0) THEN
C      
C If the area is defined by 3 or more points, fill it
C      
         CALL GSFACI(IAREA3+1)
         CALL GFA(NWRK,XWRK,YWRK)
      ENDIF
C      
C Otherwise, do nothing
C      
      RETURN
      END
      
      SUBROUTINE GETDAT(XREG,YREG,ZREG,MREG,NREG)
      
      PARAMETER (NRAN=30,LRWK=5000,LIWK=5000)
      INTEGER MREG, NREG
      REAL XRAN(NRAN), YRAN(NRAN), ZRAN(NRAN)
      REAL XREG(MREG), YREG(NREG), ZREG(MREG,NREG), RWRK(LRWK)
      REAL XDELTA(50)
      INTEGER IWRK(LIWK)
      
      DATA XRAN /12., 60., 14., 33.,  8., 12., 43., 57., 22., 15.,
     1     19., 12., 64., 19., 15., 55., 31., 32., 33., 29.,
     2     18.,  1., 18., 42., 56.,  9.,  6., 12., 44., 19./
      DATA YRAN / 1.,  2.,  3., 53.,  7., 11., 13., 17., 19., 49.,
     1     1., 31., 37.,  5.,  7., 47., 61., 17.,  5., 23.,
     2     29.,  3.,  5., 41., 43.,  9., 13., 59.,  1., 67./
      DATA ZRAN /1.0, 1.5, 1.7, 1.4, 1.9, 1.0, 1.5, 1.2, 1.8, 1.4,
     1     1.8, 1.7, 1.9, 1.5, 1.2, 1.1, 1.3, 1.7, 1.2, 1.6,
     2     1.9, 1.0, 1.6, 1.3, 1.4, 1.8, 1.7, 1.5, 1.1, 1.0/
      DATA XDELTA/.00,.02,.04,.06,.08,.10,.12,.14,.16,.18,.20,
     +            .22,.24,.26,.28,.30,.32,.34,.36,.38,.40,.42,
     +            .44,.46,.48,.50,.52,.54,.56,.58,.60,.62,.64,
     +            .66,.68,.70,.72,.74,.76,.78,.80,.82,.84,.86,
     +            .88,.90,.92,.94,.96,.98/
C 
C Set the min and max data values.
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
         XREG(I)=XMIN + (XMAX - XMIN)*XDELTA(I)
 101  CONTINUE

      DO 102 I=1,NREG
         YREG(I)=YMIN + (YMAX - YMIN)*XDELTA(I)
 102  CONTINUE
C      
C Interpolate data onto a regular grid
C      
      CALL IDSFFT (1,NRAN,XRAN,YRAN,ZRAN,
     +     MREG,NREG,MREG,XREG,YREG,ZREG,IWRK,RWRK)
      
      RETURN
      END
      SUBROUTINE COLOR(IWKID)
C 
C BACKGROUND COLOR
C BLACK
C
      CALL GSCR(IWKID,0,0.,0.,0.)
C 
C FORGROUND COLORS
C White
C
      CALL GSCR(IWKID,  1, 1.0, 1.0, 1.0)
C      
C Orchid
C      
      CALL GSCR(IWKID,  2, 0.85, 0.45, 0.8)
C      
C Red
C      
      CALL GSCR(IWKID,  3, 0.9, 0.25, 0.0)
C      
C OrangeRed
C      
      CALL GSCR(IWKID,  4, 1.0, 0.0, 0.2)
C      
C Orange
C      
      CALL GSCR(IWKID,  5, 1.0, 0.65, 0.0)
C      
C Gold
C      
      CALL GSCR(IWKID,  6, 1.0, 0.85, 0.0)
C      
C Yellow
C      
      CALL GSCR(IWKID,  7, 1.0, 1.0, 0.0)
C      
C GreenYellow
C      
      CALL GSCR(IWKID,  8, 0.7, 1.0, 0.2)
C      
C Chartreuse
C      
      CALL GSCR(IWKID,  9, 0.5, 1.0, 0.0)
C      
C Celeste
C      
      CALL GSCR(IWKID, 10, 0.2, 1.0, 0.5)
C      
C Green
C      
      CALL GSCR(IWKID, 11, 0.2, 0.8, 0.2)
C      
C Aqua
C      
      CALL GSCR(IWKID, 12, 0.0, 0.9, 1.0)
C      
C DeepSkyBlue
C      
      CALL GSCR(IWKID, 13, 0.0, 0.75, 1.0)
C      
C RoyalBlue
C      
      CALL GSCR(IWKID, 14, 0.25, 0.45, 0.95)
C      
C SlateBlue
C      
      CALL GSCR(IWKID, 15, 0.4, 0.35, 0.8)
C      
C DarkViolet
C      
      CALL GSCR(IWKID, 16, 0.6, 0.0, 0.8)
C      
C Lavender
C      
      CALL GSCR(IWKID, 17, 0.8, 0.8, 1.0)
C      
C Done.
C 
      RETURN
C 
      END

        PARAMETER (MREG=50,NREG=50)
        REAL XREG(MREG),YREG(NREG),ZREG(MREG,NREG)

        EXTERNAL COLOR

C Get data array
        CALL GETDAT(XREG,YREG,ZREG,MREG,NREG)

C Open GKS and turn off clipping
        CALL OPNGKS
        CALL GSCLIP(0)

C Call Conpack color fill routine

        CALL CSFILL(ZREG,MREG,NREG,-15,COLOR)


C Close frame and close GKS
        CALL FRAME
        CALL CLSGKS

        STOP
        END

        SUBROUTINE CSFILL(ZREG,MREG,NREG,NCL,COLOR)

        PARAMETER (LRWK=5000,LIWK=5000,LMAP=50000,NWRK=5000,NOGRPS=5)
        REAL ZREG(MREG,NREG),RWRK(LRWK), XWRK(NWRK), YWRK(NWRK)
        INTEGER MREG,NREG,IWRK(LIWK)
        INTEGER MAP(LMAP),IAREA(NOGRPS),IGRP(NOGRPS)

        EXTERNAL SFILL
        EXTERNAL COLOR

C Set up color table
        CALL COLOR

C First draw B&W plot to left
	CALL CPSETR('VPL - VIEWPORT LEFT',0.)
	CALL CPSETR('VPR - VIEWPORT RIGHT',.49)

C Set number of contour levels and initialize Conpack
        CALL CPSETI('CLS - CONTOUR LEVEL SELECTION FLAG',NCL)
        CALL CPRECT(ZREG, MREG, MREG, NREG, RWRK, LRWK, IWRK, LIWK)

C Set up B&W fill options
        CALL SFSETI('TY - TYPE OF FILL',-4.)
C Draw Perimeter
        CALL CPBACK(ZREG, RWRK, IWRK)

C Initialize Areas
        CALL ARINAM(MAP, LMAP)

C Add contours to area map
        CALL CPCLAM(ZREG, RWRK, IWRK, MAP)

C Fill contours
        CALL ARSCAM(MAP, XWRK, YWRK, NWRK, IAREA, IGRP, NOGRPS, SFILL)

C Draw contours, masking label boxes
        CALL CPCLDR(ZREG, RWRK, IWRK)

C Second draw color plot to left
	CALL CPSETR('VPL - VIEWPORT LEFT',.51)
	CALL CPSETR('VPR - VIEWPORT RIGHT',1.)
        CALL CPRECT(ZREG, MREG, MREG, NREG, RWRK, LRWK, IWRK, LIWK)

C Set up color fill options
	CALL GSFAIS(1)
        CALL SFSETI('TY - TYPE OF FILL',0.)

C Draw Perimeter
        CALL CPBACK(ZREG, RWRK, IWRK)

C Initialize Areas
        CALL ARINAM(MAP, LMAP)

C Add contours to area map
        CALL CPCLAM(ZREG, RWRK, IWRK, MAP)

C Fill contours
        CALL ARSCAM(MAP, XWRK, YWRK, NWRK, IAREA, IGRP, NOGRPS, SFILL)

C Draw contours, masking label boxes
        CALL CPCLDR(ZREG, RWRK, IWRK)
        RETURN
        END

        SUBROUTINE SFILL (XWRK,YWRK,NWRK,IAREA,IGRP,NGRPS)
C
        REAL XWRK(*),YWRK(*),ISCR(5000)
        INTEGER IAREA(*),IGRP(*),RSCR(5000)

        DO 10, I=1,NGRPS
          IF (IGRP(I).EQ.3) IAREA3=IAREA(I)
 10     CONTINUE

        IF (IAREA3 .GE. 1) THEN
C If the area is defined by 3 or more points, fill it
           CALL SFSGFA(XWRK,YWRK,NWRK,RSCR,5000,ISCR,5000,IAREA3+1)
        ENDIF
        
C Otherwise, do nothing

        RETURN
        END

        SUBROUTINE GETDAT(XREG,YREG,ZREG,MREG,NREG)

        PARAMETER (NRAN=30,LRWK=5000,LIWK=5000)
        REAL XRAN(NRAN), YRAN(NRAN), ZRAN(NRAN)
        REAL XREG(MREG), YREG(NREG), ZREG(MREG,NREG), RWRK(LRWK)
        INTEGER IWRK(LIWK)

        DATA XRAN /12., 60., 14., 33.,  8., 12., 43., 57., 22., 15.,
     1             19., 12., 64., 19., 15., 55., 31., 32., 33., 29.,
     2             18.,  1., 18., 42., 56.,  9.,  6., 12., 44., 19./
        DATA YRAN / 1.,  2.,  3., 53.,  7., 11., 13., 17., 19., 49.,
     1              1., 31., 37.,  5.,  7., 47., 61., 17.,  5., 23.,
     2             29.,  3.,  5., 41., 43.,  9., 13., 59.,  1., 67./
        DATA ZRAN /1.0, 1.5, 1.7, 1.4, 1.9, 1.0, 1.5, 1.2, 1.8, 1.4,
     1             1.8, 1.7, 1.9, 1.5, 1.2, 1.1, 1.3, 1.7, 1.2, 1.6,
     2             1.9, 1.0, 1.6, 1.3, 1.4, 1.8, 1.7, 1.5, 1.1, 1.0/

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
      DO 101 I=1,MREG
        XREG(I)=XMIN + (XMAX - XMIN)* REAL(I-1)/MREG
  101 CONTINUE
C
      DO 102 I=1,NREG
        YREG(I)=YMIN + (YMAX - YMIN)* REAL(I-1)/NREG
  102 CONTINUE

C Interpolate data onto a regular grid
        CALL IDSFFT (1,NRAN,XRAN,YRAN,ZRAN,
     +          MREG,NREG,MREG,XREG,YREG,ZREG,IWRK,RWRK)

        RETURN
        END
      SUBROUTINE COLOR
C
C     BACKGROUND COLOR
C     BLACK
      CALL GSCR(1,0,0.,0.,0.)
C
C     FORGROUND COLORS
C White
      CALL GSCR(1,  1, 1.0, 1.0, 1.0)
C 
      CALL GSCR(1,  2, 0.85, 0.45, 0.4)
C Red
      CALL GSCR(1,  3, 0.9, 0.25, 0.0)
C OrangeRed
      CALL GSCR(1,  4, 1.0, 0.0, 0.2)
C Orange
      CALL GSCR(1,  5, 1.0, 0.65, 0.0)
C Gold
      CALL GSCR(1,  6, 1.0, 0.85, 0.0)
C Yellow
      CALL GSCR(1,  7, 1.0, 1.0, 0.0)
C GreenYellow
      CALL GSCR(1,  8, 0.7, 1.0, 0.2)
C Chartreuse
      CALL GSCR(1,  9, 0.5, 1.0, 0.0)
C Green
      CALL GSCR(1, 10, 0.2, 0.8, 0.2)
C Celeste
      CALL GSCR(1, 11, 0.2, 1.0, 0.7)
C Aqua
      CALL GSCR(1, 12, 0.0, 0.9, 1.0)
C DeepSkyBlue
      CALL GSCR(1, 13, 0.0, 0.75, 1.0)
C RoyalBlue
      CALL GSCR(1, 14, 0.25, 0.45, 0.95)
C SlateBlue
      CALL GSCR(1, 15, 0.4, 0.35, 0.8)
C DarkViolet
      CALL GSCR(1, 16, 0.6, 0.0, 0.8)
C Lavender
      CALL GSCR(1, 17, 0.8, 0.8, 1.0)
C Sienna
      CALL GSCR(1, 18, 0.63, 0.32, 0.18)
C Done.
C
        RETURN
C
      END


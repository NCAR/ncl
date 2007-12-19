      PROGRAM WMEX16
C
C  Example of drawing vectors.
C
C  Define the error file, the Fortran unit number, the workstation type,
C  and the workstation ID to be used in calls to GKS routines.
C
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)   ! NCGM
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=8,  IWKID=1)   ! X Windows
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=11, IWKID=1)   ! PDF
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=20, IWKID=1)   ! PostScript
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)
C
      D2R = 0.0174532925
C
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, 0)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C  Define a gray background color.
C
      CALL GSCR(IWKID, 0, 0.5, 0.5, 0.5)
C
C  Set up some vector control parameter values.
C
      CALL WMSETR('VRS - reference vector size',1.)
      CALL WMSETR('VRN - NDC size corresponding to VRS',0.275)
C
C  Define a color table going around the color hues.
C
      DO 25 I=1,36
        CALL HSVRGB(REAL((I+24)*10),1.,1.,R,G,B)
        CALL GSCR(IWKID,I,R,G,B)
   25 CONTINUE
C
C  Draw vectors radiating from a point having different colors
C  and vector arrow head sizes.
C
      KOUNT = 1
      BIGM  = 0.037
      SMALLM = 0.01
      CALL WMSETR('VCW - vector line width scale',3.)
      DO 10 I=0,350,10
        U = COS(REAL(I)*D2R)
        V = SIN(REAL(I)*D2R)
        CALL WMSETI('VCC - vector color',KOUNT)
        CALL WMSETR('VCH - arrow head size',(0.025/350.)*REAL(I)+0.01)       
C
C  Draw vector.
C
        CALL WMVECT(0.375, 0.4, U, V)
        KOUNT = KOUNT+1
   10 CONTINUE
C
C  Draw some vectors with differenc angles between the
C  arrow head and the tail, with different vector line widths,
C  and with different sizes.
C
      RR = 0.325
      DO 15 I=0,9
        ALPHA = D2R*10.*REAL(I)
        X = RR*COS(ALPHA) + 0.375
        Y = RR*SIN(ALPHA) + 0.4
        CALL WMSETR('VCW - width scale factor',REAL(I+1))
        CALL WMSETR('VVA - angle between arrow head and tail',40.-3.*I)
        CALL WMSETI('VCC - color index',I+1)
C
C  Size scale factor.
C
        RSCALE = 3.5-0.4*ABS((4.-I))
        CALL WMVECT(X,Y,RSCALE*RR*COS(ALPHA),RSCALE*RR*SIN(ALPHA))
   15 CONTINUE
C
C  Plot a vector label box at lower right.
C
      CALL WMSETI('VLF - vector label foreground color index',1)
      CALL WMVLBL(1.,0.)
C
C  Draw plot and close things out.
C
      CALL FRAME
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      END

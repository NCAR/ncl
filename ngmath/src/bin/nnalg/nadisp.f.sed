C
C $Id: nadisp.f.sed,v 1.2 2008-07-27 04:11:34 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      PROGRAM NADISP
C
C  Draw natural neighbor circles.
C
      PARAMETER (IPDIM=6001, ISDIM=2*IPDIM, ICDIM=6)
      PARAMETER (NCOLORS=10)
      CHARACTER*80 INFILE,INLINE
      DIMENSION PX(IPDIM),PY(IPDIM),PZ(IPDIM)
      DIMENSION PU(100),PV(100)
      DIMENSION CX(ISDIM),CY(ISDIM),R2(ISDIM)
      DIMENSION COLORS(3,NCOLORS)
      DIMENSION IPTS(ISDIM,3)
      DIMENSION XX(2),YY(2)
      DIMENSION VORX(ISDIM),VORY(ISDIM),IWORK(ISDIM),WORK(ISDIM)
C
C  Read in original points.  Read from the specified file if one is
C  supplied; read from stdin otherwise.
C
      NUMARG = IARGC()
      IF (NUMARG .EQ. 0) THEN
        WRITE(6,510) 
  510   FORMAT(' Usage: nnalg data_file_name')
        STOP
      ELSE
        IUNIT = 2
#ifdef  hpux
        CALL IGETARG(1, INFILE, 80)
#else
        CALL GETARG(1, INFILE)
#endif
        OPEN(IUNIT,FILE=INFILE)
      ENDIF
C
C  Read GKS workstation type.
C
      CALL NASKLN(IUNIT,INLINE)
      READ(INLINE,'(I5)') IWK
C
C  Read in the flag for whether axes should be drawn.
C
      CALL NASKLN(IUNIT,INLINE)
      READ(INLINE,'(I5)') IAXES
C
C  Read in whether to draw axes or a grid.
C
      CALL NASKLN(IUNIT,INLINE)
      READ(INLINE,'(I5)') IGRTYP
C
C  Read in whether to draw the triangulation.
C
      CALL NASKLN(IUNIT,INLINE)
      READ(INLINE,'(I5)') IDRTRI
C
C  Read in whether to draw a blue dot at (0.,0.).
C
      CALL NASKLN(IUNIT,INLINE)
      READ(INLINE,'(I5)') IREF
C
C  Read in whether the pseudo data should be included in the plot.
C
      CALL NASKLN(IUNIT,INLINE)
      READ(INLINE,'(I5)') IPSEUD
C
C  Read in whether the natural neighbor circles should be drawn.
C
      CALL NASKLN(IUNIT,INLINE)
      READ(INLINE,'(I5)') INN
C
C  Read in whether the centers of the natural neighbor circles should be 
C  marked.
C
      CALL NASKLN(IUNIT,INLINE)
      READ(INLINE,'(I5)') INNC
C
C  Read in whether the Voronoi polygons should be drawn.
C
      CALL NASKLN(IUNIT,INLINE)
      READ(INLINE,'(I5)') IVOR
C
C  Read in whether the original points should be marked.
C
      CALL NASKLN(IUNIT,INLINE)
      READ(INLINE,'(I5)') IMKPT
C
C  Read in the colors.
C
      DO 90 I=1,NCOLORS
        CALL NASKLN(IUNIT,INLINE)
        READ(INLINE,'(3F7.3)') COLORS(1,I),COLORS(2,I),COLORS(3,I)
   90 CONTINUE
C
C  Read in scale factor for dots at vertices.
C
      CALL NASKLN(IUNIT,INLINE)
      READ(INLINE,'(F7.3)') DSCALE
C
C  Read in scale factor for circumcircle centers.
C
      CALL NASKLN(IUNIT,INLINE)
      READ(INLINE,'(F7.3)') CSCALE
C
C  Read in scale factor for circle lines
C
      CALL NASKLN(IUNIT,INLINE)
      READ(INLINE,'(F7.3)') CLSCAL
C
C  Read in scale factor for Voronoi polygon lines
C
      CALL NASKLN(IUNIT,INLINE)
      READ(INLINE,'(F7.3)') VSCALE
C
C  Read in scale factor for for tringulation lines
C
      CALL NASKLN(IUNIT,INLINE)
      READ(INLINE,'(F7.3)') TSCALE
C
C  Read in scale factor for axes lines
C
      CALL NASKLN(IUNIT,INLINE)
      READ(INLINE,'(F7.3)') ASCALE
C
C  Read in scale factor for asterisk marking points where neighbors are
C  desired.
C
      CALL NASKLN(IUNIT,INLINE)
      READ(INLINE,'(F7.3)') ASTSCL
C
C  Read in scale factor for dots marking natural neighbors.
C
      CALL NASKLN(IUNIT,INLINE)
      READ(INLINE,'(F7.3)') DNNSCL
C
C  Read in the user coordinates for the SET call.
C
      CALL NASKLN(IUNIT,INLINE)
      READ(INLINE,'(4E15.3)') XL,XR,YB,YT
C
C  Read in number of original data points.
C
      CALL NASKLN(IUNIT,INLINE)
      READ(INLINE,'(I5)') NUMPNT
      IF (NUMPNT+3 .GT. IPDIM) THEN
        IMAX = IPDIM
        WRITE(6,520) NUMPNT,IMAX
  520   FORMAT(' Number of input data points = ',
     +I8/' This exceeds the maximum allowed, which is ',
     +I8)
        STOP
      ENDIF
C
C  Read in original data points.
C
      DO 50 I=1,NUMPNT
        CALL NASKLN(IUNIT,INLINE)
        READ (INLINE,'(I5,3E15.3)') NDUM,PX(I),PY(I),PZ(I)
   50 CONTINUE
C
C  Read in the pseudo point data.
C
      DO 51 I=1,3
        CALL NASKLN(IUNIT,INLINE)
        READ (INLINE,'(I5,3E15.3)') NDUM,PX(NUMPNT+I),PY(NUMPNT+I),
     +                              PZ(NUMPNT+I)       
   51 CONTINUE
C
C  Read in natural neighbor coordinates for center of circumcircles
C  and the radius squared.
C
      CALL NASKLN(IUNIT,INLINE)
      READ(INLINE,'(I5)') NUMSIM
      IF (NUMSIM .GT. ISDIM) THEN
        IMAX = ISDIM
        WRITE(6,530) NUMSIM,IMAX
  530   FORMAT(' Number of circumcircles = ',
     +I8/' This exceeds the maximum allowed, which is ',
     +I8)
        STOP
      ENDIF
      DO 60 I=1,NUMSIM
        CALL NASKLN(IUNIT,INLINE)
        READ (INLINE,'(3I5,3E15.3)') IPTS(I,1),IPTS(I,2),IPTS(I,3),
     +                               CX(I),CY(I),R2(I)
   60 CONTINUE
C
C  Read in the number of data points where natural neighbors are to
C  be displayed, as well as the first-order/second-order flag.
C
      CALL NASKLN(IUNIT,INLINE)
      READ(INLINE,'(2I5)') NUMNND,IFOFLG
C
C  Read in the points.
C
      IF (NUMNND .GT. 0) THEN
        IF (NUMNND .GT. 100) THEN
          WRITE(6,511)
  511     FORMAT(' More than 100 points for natural neighbor draws')
          STOP
        ENDIF
        DO 93 I=1,NUMNND
          CALL NASKLN(IUNIT,INLINE)
          READ(INLINE,'(2E15.3)') PU(I),PV(I)
   93   CONTINUE
      ENDIF
C
C  Open GKS, open and activate a workstation of the appropriate type.
C
      CALL GOPKS(6,0)
      CALL GOPWK(1,2,IWK)
      DO 91 I=1,NCOLORS
        CALL GSCR(1,I-1,COLORS(1,I),COLORS(2,I),COLORS(3,I))
   91 CONTINUE
      CALL GACWK(1)
C
C  Find the limits of the data.
C
      XMAX = -20000.
      XMIN =  20000.
      YMAX = -20000.
      YMIN =  20000.
      IF (IPSEUD .NE. 0) THEN
        LIMIT = NUMPNT+3
      ELSE
        LIMIT = NUMPNT
      ENDIF
      DO 73 I=1,LIMIT
        XMAX = MAX(XMAX,PX(I))
        YMAX = MAX(XMAX,PY(I))
        XMIN = MIN(XMIN,PX(I))
        YMIN = MIN(YMIN,PY(I))
   73 CONTINUE
      XMAXO = XMAX
      YMAXO = YMAX
      XMINO = XMIN
      YMINO = YMIN
C
C  Multiplier for extra space.
C
      ADDMLP = 0.3
      XMAX = XMAX+ADDMLP*(XMAX-XMIN)
      XMIN = XMIN-ADDMLP*(XMAX-XMIN)
      YMAX = YMAX+ADDMLP*(YMAX-YMIN)
      YMIN = YMIN-ADDMLP*(YMAX-YMIN)
      IF (XMAX-XMIN .GT. YMAX-YMIN) THEN
        IF (XL.NE.0. .OR. XR.NE.0. .OR. YB.NE.0. .OR. YT.NE.0.) THEN
          BREDTH = REAL(NINT(XR-XL)+1)
          XLP = XL
          XRP = XR
          YBP = YB
          YTP = YT
          CALL SET(0.0, 1.0, 0.0, 1.0, XLP, XRP, YBP, YTP, 1) 
        ELSE
          BREDTH = REAL(NINT(XMAX-XMIN)+1)
          XLP = XMIN
          XRP = XMAX
          YBP = XMIN
          YTP = XMAX
          CALL SET(0.0, 1.0, 0.0, 1.0, XLP, XRP, YBP, YTP, 1) 
        ENDIF
      ELSE
        IF (XL.NE.0. .OR. XR.NE.0. .OR. YB.NE.0. .OR. YT.NE.0.) THEN
          BREDTH = REAL(NINT(XR-XL)+1)
          XLP = XL
          XRP = XR
          YBP = YB
          YTP = YT
          CALL SET(0.0, 1.0, 0.0, 1.0, XLP, XRP, YBP, YTP, 1) 
        ELSE
          BREDTH = REAL(NINT(YMAX-YMIN)+1)
          XLP = YMIN
          XRP = YMAX
          YBP = YMIN
          YTP = YMAX
          CALL SET(0.0, 1.0, 0.0, 1.0, XLP, XRP, YBP, YTP, 1) 
        ENDIF
      ENDIF
C
C  Sizes
C
      SIZE  = 0.02*BREDTH*DSCALE
      BSIZE = 0.02*BREDTH*DNNSCL
      CSIZE = 0.02*ASTSCL
C
C  Draw axes
C
      IF (IAXES .NE. 0) THEN
        CALL GSLWSC(ASCALE)
        CALL GSPLCI(1)
        DO 72 I=1,10
          IF (I .EQ. 1) THEN
            ITEST = 1
          ELSE
            ITEST = ITEST*10
          ENDIF
          NUMGRD = NINT(BREDTH)/ITEST
          IF (NUMGRD .LE. 40) THEN
            IF (MOD(NUMGRD,2) .NE. 0) NUMGRD = NUMGRD+1
            XINC = (XMAXO-XMINO)/REAL(NUMGRD)
            YINC = (YMAXO-YMINO)/REAL(NUMGRD)
            XX(1) = 0.
            YY(1) = YBP
            XX(2) = 0.
            YY(2) = YTP
            CALL GPL(2,XX,YY)
            XX(1) = XLP
            YY(1) = 0.
            XX(2) = XRP
            YY(2) = 0.
            CALL GPL(2,XX,YY)
            NDG = 6*NUMGRD
            IF (IGRTYP .EQ. 0) THEN
              DO 76 J=1,NDG
                XPOS = REAL(J-1)*XINC
                IF (XPOS .LT. XRP) THEN
                  XX(1) = XPOS
                  YY(1) = 0.
                  XX(2) = XPOS
                  YY(2) = 0.5*SIZE
                  CALL GPL(2,XX,YY)
                ENDIF
                XPOS = -XPOS
                IF (XPOS .GT. XLP) THEN
                  XX(1) = XPOS
                  YY(1) = 0.
                  XX(2) = XPOS
                  YY(2) = 0.5*SIZE
                  CALL GPL(2,XX,YY)
                ENDIF
                YPOS = REAL(J-1)*YINC
                IF (YPOS .LT. YTP) THEN
                  XX(1) = 0.
                  YY(1) = YPOS
                  XX(2) = 0.5*SIZE
                  YY(2) = YPOS
                  CALL GPL(2,XX,YY)
                ENDIF
                YPOS = -YPOS
                IF (YPOS .GT. YBP) THEN
                  XX(1) = 0.
                  YY(1) = YPOS
                  XX(2) = 0.5*SIZE
                  YY(2) = YPOS
                  CALL GPL(2,XX,YY)
                ENDIF
   76         CONTINUE
            ELSE IF (IGRTYP .EQ. 1) THEN
              DO 74 J=1,NDG
                XPOS = REAL(J-1)*XINC
                IF (XPOS .LT. XRP) THEN
                  XX(1) = XPOS
                  YY(1) = YBP
                  XX(2) = XPOS
                  YY(2) = YTP
                  CALL GPL(2,XX,YY)
                ENDIF
                XPOS = -XPOS
                IF (XPOS .GT. XLP) THEN
                  XX(1) = XPOS
                  YY(1) = YBP
                  XX(2) = XPOS
                  YY(2) = YTP
                  CALL GPL(2,XX,YY)
                ENDIF
                YPOS = REAL(J-1)*YINC
                IF (YPOS .LT. YTP) THEN
                  XX(1) = XLP
                  YY(1) = YPOS
                  XX(2) = XRP
                  YY(2) = YPOS
                  CALL GPL(2,XX,YY)
                ENDIF
                YPOS = -YPOS
                IF (YPOS .GT. YBP) THEN
                  XX(1) = XLP
                  YY(1) = YPOS
                  XX(2) = XRP
                  YY(2) = YPOS
                  CALL GPL(2,XX,YY)
                ENDIF
   74         CONTINUE
            ENDIF
            CALL GSLWSC(1.)
            GO TO 77
          ENDIF
   72   CONTINUE
   77   CONTINUE
      ENDIF
C
C  Mark the points where the natural neighbors are to be marked.
C
      IF (NUMNND .GT. 0) THEN
        DO 121 L=1,NUMNND
          CALL PCSETI('CC',9)
          CALL PCSETI('FN',26)
          CALL PLCHHQ(PU(L),PV(L),'*',2.*CSIZE,0.,0.)
  121   CONTINUE
      ENDIF
C
C  Draw the natural neighbor circles.
C
      IF (INN .NE. 0) THEN
        CALL GSLWSC(CLSCAL)
C
C  Draw circumcircles.
C
        CALL NGSETI('CT',1)
        DO 70 I=1,NUMSIM
          IF (IPTS(I,1).LE.NUMPNT .OR. IPSEUD.EQ.1) THEN
            CALL NGDOTS(CX(I),CY(I),1,2.*SQRT(R2(I)),2)
          ENDIF
   70   CONTINUE
        CALL GSLWSC(1.)
      ENDIF
C
C  Draw the triangulation.
C
      IF (IDRTRI .NE. 0) THEN
        CALL GSPLCI(4)
        CALL GSLWSC(TSCALE)
        DO 110 I=1,NUMSIM
          IF (IPTS(I,1).LE.NUMPNT .OR. IPSEUD.EQ.1) THEN
            INDX = IPTS(I,1)
            XX(1) = PX(INDX)
            YY(1) = PY(INDX)
            INDX = IPTS(I,2)
            XX(2) = PX(INDX)
            YY(2) = PY(INDX)
            CALL GPL(2,XX,YY)
C
            INDX = IPTS(I,2)
            XX(1) = PX(INDX)
            YY(1) = PY(INDX)
            INDX = IPTS(I,3)
            XX(2) = PX(INDX)
            YY(2) = PY(INDX)
            CALL GPL(2,XX,YY)
C
            INDX = IPTS(I,3)
            XX(1) = PX(INDX)
            YY(1) = PY(INDX)
            INDX = IPTS(I,1)
            XX(2) = PX(INDX)
            YY(2) = PY(INDX)
            CALL GPL(2,XX,YY)
            CALL SFLUSH()
          ENDIF
  110   CONTINUE
        CALL GSLWSC(1.)
      ENDIF
C
C  Draw the voronoi polygons
C
      IF (IVOR .EQ. 1) THEN
        CALL GSLWSC(VSCALE)
        DO 80 I=1,NUMPNT
          NUMV = 0
C
C  Find the vertices of the Voronoi polygon around the Ith data point.
C
          DO 81 J=1,NUMSIM
            IF (IPTS(J,1).EQ.I .OR. IPTS(J,2).EQ.I .OR.
     +                              IPTS(J,3).EQ.I) THEN
              NUMV = NUMV+1
              VORX(NUMV) = CX(J)
              VORY(NUMV) = CY(J)
            ENDIF
   81     CONTINUE
        IF (NUMV .GT. 0) CALL NADRAW(NUMV,VORX,VORY,IWORK,WORK)
   80   CONTINUE
        CALL GSLWSC(1.)
      ENDIF
C
C  Draw dots at the center of all circles (including the pseudo
C  data if IPSEUD = 1).
C
      IF (INNC .NE. 0) THEN
        CALL NGSETI('CT',0)
        DO 92 I=1,NUMSIM
          IF (IPTS(I,1).LE.NUMPNT .OR. IPSEUD.EQ.1) THEN
            CALL NGDOTS(CX(I),CY(I),1,0.008*BREDTH*CSCALE,3)
          ENDIF
   92   CONTINUE
      ENDIF
C
C  Mark the original points.
C
      IF (IMKPT .NE. 0) THEN
        CALL NGSETI('CT',0)
        IF (IPSEUD .NE. 0) THEN
          LIMIT = NUMPNT+3
        ELSE
          LIMIT = NUMPNT
        ENDIF
        DO 115 I=1,LIMIT
          CALL NGDOTS(PX(I), PY(I), 1, 0.8*SIZE, 6)
          CALL SFLUSH()
  115   CONTINUE
      ENDIF
C
C  Draw a dot at point (0.,0.) if requested.
C
      IF (IREF .NE. 0) THEN
        CALL NGDOTS(0., 0., 1, 0.8*SIZE, 7)
      ENDIF
C
C  Draw the natural neighbors of the specified points.
C
      IF (IFOFLG .GE. 0) THEN
        DO 116 L=1,NUMNND
            CALL NGSETI('CT',0)
            DO 117 I=1,NUMSIM
              DIST = ( (PU(L)-CX(I))**2 + (PV(L)-CY(I))**2)
              IF (DIST .LE. R2(I)) THEN
                DO 118 J=1,3 
                  INDX = IPTS(I,J)
                  IF (IFOFLG .EQ. 0) THEN
                    CALL NGDOTS(PX(INDX),PY(INDX),1,0.8*BSIZE,8)
                  ELSE
                    DO 119 M=1,NUMSIM
                      IF (IPTS(M,1).EQ.INDX .OR. IPTS(M,2).EQ.INDX .OR.
     +                    IPTS(M,3).EQ.INDX) THEN
                        DO 120 N=1,3
                          INDX1 = IPTS(M,N)
                          CALL NGDOTS(PX(INDX1),PY(INDX1),1,0.8*BSIZE,8)
  120                   CONTINUE
                      ENDIF
  119               CONTINUE 
                  ENDIF
  118           CONTINUE
              ENDIF
  117       CONTINUE
  116   CONTINUE        
      ENDIF
C
C  re-mark the points where the natural neighbors are to be marked.
C
      IF (NUMNND .GT. 0) THEN
        DO 122 L=1,NUMNND
          CALL PCSETI('CC',9)
          CALL PCSETI('FN',26)
          CALL PLCHHQ(PU(L),PV(L),'*',2.*CSIZE,0.,0.)
  122   CONTINUE
      ENDIF
C
      CALL FRAME
C
      CALL GDAWK(1)
      CALL GCLWK(1)
      CALL GCLKS
C
      STOP
      END

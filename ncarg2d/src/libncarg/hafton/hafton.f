C
C	$Id: hafton.f,v 1.6 2008-07-27 00:17:14 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE HAFTON (Z,L,M,N,FLO,HI,NLEV,NOPT,NPRM,ISPV,SPVAL)
C
C     SUBROUTINE HAFTON (Z,L,M,N,FLO,HI,NLEV,NOPT,NPRM,ISPV,SPVAL)
C
C
C     DIMENSION OF           Z(L,M)
C     ARGUMENTS
C
C     PURPOSE                HAFTON draws a half-tone picture from data
C                            stored in a rectangular array with the
C                            intensity in the picture proportional to
C                            the data value.
C
C     USAGE                  If the following assumptions are met, use
C
C                                 CALL EZHFTN (Z,M,N)
C
C                              Assumptions:
C                                  .All of the array is to be drawn.
C                                  .Lowest value in Z will be at lowest
C                                   intensity on reader/printer output.
C                                  .Highest value in Z will be at
C                                   highest intensity.
C                                  .Values in between will appear
C                                   linearly spaced.
C                                  .Maximum possible number of
C                                   intensities are used.
C                                  .The picture will have a perimeter
C                                   drawn.
C                                  .FRAME will be called after the
C                                   picture is drawn.
C                                  .Z is filled with numbers that should
C                                   be used (no missing values).
C
C                            If these assumptions are not met, use
C
C                                 CALL HAFTON (Z,L,M,N,FLO,HI,NLEV,
C                                              NOPT,NPRM,ISPV,SPVAL)
C
C     ARGUMENTS
C
C     ON INPUT               Z
C     FOR EZHFTN               M by N array to be used to generate a
C                              half-tone plot.
C
C                            M
C                              First dimension of Z.
C
C                            N
C                              Second dimension of Z.
C
C     ON OUTPUT              All arguments are unchanged.
C     FOR EZHFTN
C
C     ON INPUT               Z
C     FOR HAFTON               The origin of the array to be plotted.
C
C                            L
C                              The first dimension of Z in the calling
C                              program.
C
C                            M
C                              The number of data values to be plotted
C                              in the x-direction (the first subscript
C                              direction).  When plotting all of an
C                              array, L = M.
C
C                            N
C                              The number of data values to be plotted
C                              in the y-direction (the second subscript
C                              direction).
C
C                            FLO
C                              The value of Z that corresponds to the
C                              lowest intensity.  (When NOPT.LT.0, FLO
C                              corresponds to the highest intensity.)
C                              If FLO=HI=0.0, MIN(Z) will be used for FLO.
C
C                            HI
C                              The value of Z that corresponds to the
C                              highest intensity.  (When NOPT.LT.0, HI
C                              corresponds to the lowest intensity.)  If
C                              HI=FLO=0.0, MAX(Z) will be used for HI.
C
C                            NLEV
C                              The number of intensity levels desired.
C                              16 maximum.  If NLEV = 0 or 1, 16 levels
C                              are used.
C
C                            NOPT
C                              Flag to control the mapping of Z onto the
C                              intensities.  The sign of NOPT controls
C                              the directness or inverseness of the
C                              mapping.
C
C                              . NOPT positive yields direct mapping.
C                                The largest value of Z produces the
C                                most dense dots.  On mechanical plotters,
C                                large values of Z will produce a dark
C                                area on the paper.  With the film
C                                development methods used at NCAR,
C                                large values of Z will produce many
C                                (white) dots on the film, also
C                                resulting in a dark area on
C                                reader-printer paper.
C                              . NOPT negative yields inverse mapping.
C                                The smallest values of Z produce the
C                                most dense dots resulting in dark
C                                areas on the paper.
C
C                              The absolute value of NOPT determines the
C                              mapping of Z onto the intensities.  For
C                              ABS(NOPT)
C                              = 0  The mapping is linear.  For
C                                   each intensity there is an equal
C                                   range in Z value.
C                              = 1  The mapping is linear.  For
C                                   each intensity there is an equal
C                                   range in Z value.
C                              = 2  The mapping is exponential.  For
C                                   larger values of Z, there is a
C                                   larger difference in intensity for
C                                   relatively close values of Z.  Details
C                                   in the larger values of Z are displayed
C                                   at the expense of the smaller values
C                                   of Z.
C                              = 3  The mapping is logrithmic, so
C                                   details of smaller values of Z are shown
C                                   at the expense of larger values of Z.
C                              = 4  Sinusoidal mapping, so mid-range values
C                                   of Z show details at the expense of
C                                   extreme values of Z.
C                              = 5  Arcsine mapping, so extreme values of
C                                   Z are shown at the expense of mid-range
C                                   values of Z.
C
C                            NPRM
C                              Flag to control the drawing of a
C                              perimeter around the half-tone picture.
C
C                              . NPRM=0:  The perimeter is drawn with
C                                ticks pointing at data locations.
C                                (Side lengths are proportional to number
C                                of data values.)
C                              . NPRM positive:  No perimeter is drawn.  The
C                                picture fills the frame.
C                              . NPRM negative:  The picture is within the
C                                confines of the user's current viewport
C                                setting.
C
C                            ISPV
C                              Flag to tell if the special value feature
C                              is being used.  The special value feature
C                              is used to mark areas where the data is
C                              not known or holes are wanted in the
C                              picture.
C
C                              . ISPV = 0:  Special value feature not in
C                                use.  SPVAL is ignored.
C                              . ISPV non-zero:  Special value feature
C                                in use.  SPVAL defines the special
C                                value.  Where Z contains the special
C                                value, no half-tone is drawn.  If ISPV
C                                = 0  Special value feature not in use.
C                                     SPVAL is ignored.
C                                = 1  Nothing is drawn in special value
C                                     area.
C                                = 2  Contiguous special value areas are
C                                     surrounded by a polygonal line.
C                                = 3  Special value areas are filled
C                                     with X(s).
C                                = 4  Special value areas are filled in
C                                     with the highest intensity.
C
C                            SPVAL
C                              The value used in Z to denote missing
C                              values.  This argument is ignored if
C                              ISPV = 0.
C
C     ON OUTPUT              All arguments are unchanged.
C     FOR HAFTON
C
C     NOTE                   This routine produces a huge number of
C                            plotter instructions per picture, averaging
C                            over 100,000 line-draws per frame when M = N.
C
C
C     ENTRY POINTS           EZHFTN, HAFTON, ZLSET, GRAY, BOUND, HFINIT,
C                            HFINITX
C
C     COMMON BLOCKS          HAFT01, HAFT02, HAFT03, HAFT04
C
C     REQUIRED LIBRARY       GRIDAL, the ERPRT77 package and the SPPS.
C     ROUTINES
C
C     REQUIRED GKS LEVEL     0A
C
C     I/O                    Plots half-tone picture.
C
C     PRECISION              Single
C
C     LANGUAGE               FORTRAN
C
C     HISTORY                Rewrite of PHOMAP originally written by
C                            M. Perry of High Altitude Observatory,
C                            NCAR.
C
C     ALGORITHM              Bi-linear interpolation on plotter
C                            (resolution-limited) grid of normalized
C                            representation of data.
C
C     PORTABILITY            ANSI FORTRAN 77.
C
C
C
C INTERNAL PARAMTERSs
C VALUES SET IN BLOCK DATA
C         NAME  DEFAULT                   FUNCTION
C         ----  -------                   ________
C
C         XLT     0.1    Left-hand edge of the plot when NSET=0.  (0.0=
C                        left edge of frame, 1.0=right edge of frame.)
C         YBT     0.1    Bottom edge of the plot when NSET=0.  (0.0=
C                        bottom of frame, 1.0=top of frame.)
C         SIDE    0.8    Length of longer edge of plot (see also EXT).
C         EXT     .25    Lengths of the sides of the plot are propor-
C                        tional to M and N (when NSET=0) except in
C                        extreme cases, namely, when MIN(M,N)/MAX(M,N)
C                        is less than EXT.  Then a square plot is pro-
C                        duced.  When a rectangular plot is produced,
C                        the plot is centered on the frame (as long as
C                        SIDE+2*XLT = SIDE+2*YBT=1., as with the
C                        defaults.)
C         ALPHA   1.6    A parameter to control the extremeness of the
C                        mapping function specified by NOPT.  (For
C                        ABS(NOPT)=0 or 1, the mapping function is
C                        linear and independent of ALPHA.)  For the non-
C                        linear mapping functions, when ALPHA is changed
C                        to a number closer to 1., the mapping function
C                        becomes more linear; when ALPHA is changed to
C                        a larger number, the mapping function becomes
C                        more extreme.
C         MXLEV   16     Maximum number of levels.  Limited by plotter.
C         NCRTG   8      Number of CRT units per gray-scale cell.
C                        Limited by plotter.
C         NCRTF   1024   Number of plotter address units per frame.
C         IL    (Below)  An array defining which of the available in-
C                        tensities are used when less than the maximum
C                        number of intensities are requested.
C
C
C   NLEV         Intensities Used
C   ____         ________________
C    2           5,11,
C    3           4, 8,12,
C    4           3, 6,10,13,
C    5           2, 5, 8,11,14,
C    6           1, 4, 7, 9,12,15,
C    7           1, 4, 6, 8,10,12,15,
C    8           1, 3, 5, 7, 9,11,13,15,
C    9           1, 3, 4, 6, 8,10,12,13,15
C   10           1, 3, 4, 6, 7, 9,10,12,13,15,
C   11           1, 2, 3, 5, 6, 8,10,11,13,14,15,
C   12           1, 2, 3, 5, 6, 7, 9,10,11,13,14,15,
C   13           1, 2, 3, 4, 6, 7, 8, 9,10,12,13,14,15
C   14           1, 2, 3, 4, 5, 6, 7, 9,10,11,12,13,14,15,
C   15           1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,
C   16           0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15
C
C

      SAVE
      DIMENSION       Z(L,N)      ,PX(2)       ,PY(2)
      DIMENSION       ZLEV(16)    ,VIEW(4)     ,WIND(4)  ,RECT(4)
      DIMENSION                    VIEW2(4)    ,WIND2(4)
      CHARACTER*11    IDUMMY
C
C
      COMMON /HAFTO1/ I          ,J          ,INTEN
      COMMON /HAFTO2/ GLO        ,HA         ,NOPTN      ,ALPHA      ,
     1                NSPV       ,SP         ,ICNST
      COMMON /HAFTO3/ XLT        ,YBT        ,SIDE       ,EXT        ,
     1                IOFFM      ,ALPH       ,MXLEV      ,NCRTG      ,
     2                NCRTF      ,IL(135)
      COMMON /HAFTO4/ NPTMAX     ,NPOINT     ,XPNT(50)  ,YPNT(50)
C
C Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
      CALL HFINIT
C
C THE FOLLOWING CALL IS FOR GATHERING STATISTICS ON LIBRARY USE AT NCAR
C
      CALL Q8QST4 ('GRAPHX','HAFTON','HAFTON','VERSION  1')
C
      NPOINT = 0
      ALPHA = ALPH
      GLO = FLO
      HA = HI
      NLEVL = MIN(ABS(NLEV),MXLEV)
      IF (NLEVL .LE. 1) NLEVL = MXLEV
      NOPTN = NOPT
      IF (NOPTN .EQ. 0) NOPTN = 1
      NPRIM = NPRM
      NSPV = MAX(MIN(ISPV,4),0)
      IF (NSPV .NE. 0) SP = SPVAL
      MX = L
      NX = M
      NY = N
      CRTF = NCRTF
      MSPV = 0
C
C SET INTENSITY BOUNDARY LEVELS
C
      CALL ZLSET (Z,MX,NX,NY,ZLEV,NLEVL)
C
C SET UP PERIMETER
C
      X3 = NX
      Y3 = NY
      CALL GETSET (VIEW(1),VIEW(2),VIEW(3),VIEW(4),
     +             WIND(1),WIND(2),WIND(3),WIND(4),IOLLS)
      IF (NPRIM.LT.0) THEN
C
         X1 = VIEW(1)
         X2 = VIEW(2)
         Y1 = VIEW(3)
         Y2 = VIEW(4)
      ELSE IF (NPRIM.EQ.0) THEN
         X1 = XLT
         X2 = XLT+SIDE
         Y1 = YBT
         Y2 = YBT+SIDE
         IF (MIN(X3,Y3)/MAX(X3,Y3) .GE. EXT) THEN
            IF (NX-NY.LT.0) THEN
               X2 =SIDE*X3/Y3+XLT
               X2 = (REAL(INT(X2*CRTF/REAL(NCRTG)))*REAL(NCRTG))/CRTF
            ELSE IF (NX-NY.GT.0) THEN
               Y2 = SIDE*Y3/X3+YBT
               Y2 = (REAL(INT(Y2*CRTF/REAL(NCRTG)))*REAL(NCRTG))/CRTF
            END IF
         END IF
      ELSE IF (NPRIM.GT.0) THEN
         X1 = 0.0
         X2 = 1.0
         Y1 = 0.0
         Y2 = 1.0
      END IF
      MX1 = X1*CRTF
      MX2 = X2*CRTF
      MY1 = Y1*CRTF
      MY2 = Y2*CRTF
      IF (NPRIM.GT.0) THEN
         MX1 = 1
         MY1 = 1
         MX2 = NCRTF
         MY2 = NCRTF
      END IF
C
C
C
C
C
C
C Define scaling for PERIM and draw perimeter if NPRIM is zero.
C
        CALL SET (X1,X2,Y1,Y2,1.0,X3,1.0,Y3,1)
        IF (NPRIM .EQ. 0) CALL PERIM (NX-1,1,NY-1,1)
        IF (ICNST .NE. 0) THEN
C
        CALL WTSTR((X3+1.)/2.,(Y3+1.)/2.,'CONSTANT FIELD',2,0,0)
        GO TO 132
      END IF
C
C FIND OFFSET FOR REFERENCE TO IL, WHICH IS TRIANGULAR
C
      IOFFST = NLEVL*((NLEVL-1)/2)+MOD(NLEVL-1,2)*(NLEVL/2)-1
C
C OUTPUT INTENSITY SCALE
C
      IF (NPRIM .GT. 0) GO TO 112
      LEV = 0
      KX = (1.1*XLT+SIDE)*CRTF
      KY = YBT*CRTF
      NNX = KX/NCRTG
  109 LEV = LEV+1
      ISUB = IOFFST+LEV
      INTEN = IL(ISUB)
      IF (NOPTN .LT. 0) INTEN = MXLEV-INTEN
      NNY = KY/NCRTG
      DO 111 JJ=1,3
         DO 110 II=1,10
            I = NNX+II
            J = NNY+JJ
            CALL GRAY
  110    CONTINUE
  111 CONTINUE
      IF (LEV .GE. NLEVL) GO TO 112
        WRITE(IDUMMY,'(G11.4)') ZLEV(LEV)
C     TKX = KX
C     TKY = KY+38
      CALL GQCLIP (IERR,ICLP,RECT)
      CALL GSCLIP (0)
      CALL WTSTR (CPUX(KX),CPUY(KY+38),IDUMMY,0,0,-1)
      CALL GSCLIP (ICLP)
C
C
C ADJUST 38 TO PLOTTER.
C
      KY = KY+52
C
C ADJUST 52 TO PLOTTER.
C
      GO TO 109
C
C STEP THROUGH PLOTTER GRID OF INTENSITY CELLS.
C
  112 IMIN = (MX1-1)/NCRTG+1
      IMAX = (MX2-1)/NCRTG
      JMIN = (MY1-1)/NCRTG+1
      JMAX = (MY2-1)/NCRTG
      XL = IMAX-IMIN+1
      YL = JMAX-JMIN+1
      XN = NX
      YN = NY
      LSRT = NLEVL/2
      DO 130 J=JMIN,JMAX
C
C FIND Y FOR THIS J AND Z FOR THIS Y.
C
       YJ = (REAL(J-JMIN)+.5)/YL*(YN-1.)+1.
         LOWY = YJ
         YPART = YJ-REAL(LOWY)
         IF (LOWY .NE. NY) GO TO 113
         LOWY = LOWY-1
         YPART = 1.
  113    IPEN = 0
         ZLFT = Z(1,LOWY)+YPART*(Z(1,LOWY+1)-Z(1,LOWY))
         ZRHT = Z(2,LOWY)+YPART*(Z(2,LOWY+1)-Z(2,LOWY))
         IF (NSPV .EQ. 0) GO TO 114
         IF (Z(1,LOWY).EQ.SP .OR. Z(2,LOWY).EQ.SP .OR.
     1       Z(1,LOWY+1).EQ.SP .OR. Z(2,LOWY+1).EQ.SP) IPEN = 1
  114    IF (IPEN .EQ. 1) GO TO 117
C
C FIND INT FOR THIS Z.
C
         IF (ZLFT .GT. ZLEV(LSRT+1)) GO TO 116
  115    IF (ZLFT .GE. ZLEV(LSRT)) GO TO 117
C
C LOOK LOWER
C
         IF (LSRT .LE. 1) GO TO 117
         LSRT = LSRT-1
         GO TO 115
C
C LOOK HIGHER
C
  116    IF (LSRT .GE. NLEVL) GO TO 117
         LSRT = LSRT+1
         IF (ZLFT .GT. ZLEV(LSRT+1)) GO TO 116
C
C OK
C
  117    IRHT = 2
         LAST = LSRT
         DO 129 I=IMIN,IMAX
C
C FIND X FOR THIS I AND Z FOR THIS X AND Y.
C
            IADD = 1
            XI = (REAL(I-IMIN)+.5)/XL*(XN-1.)+1.
            LOWX = XI
            XPART = XI-REAL(LOWX)
            IF (LOWX .NE. NX) GO TO 118
            LOWX = LOWX-1
            XPART = 1.
C
C TEST FOR INTERPOLATION POSITIONING
C
  118       IF (LOWX .LT. IRHT) GO TO 119
C
C MOVE INTERPOLATION ONE CELL TO THE RIGHT
C
            ZLFT = ZRHT
            IRHT = IRHT+1
            ZRHT = Z(IRHT,LOWY)+YPART*(Z(IRHT,LOWY+1)-Z(IRHT,LOWY))
            IF (NSPV .EQ. 0) GO TO 118
            IPEN = 0
            IF (Z(IRHT-1,LOWY).EQ.SP .OR. Z(IRHT,LOWY).EQ.SP .OR.
     1          Z(IRHT-1,LOWY+1).EQ.SP .OR. Z(IRHT,LOWY+1).EQ.SP)
     2          IPEN = 1
            GO TO 118
  119       IF (IPEN .NE. 1) GO TO 123
C
C SPECIAL VALUE AREA
C
            GO TO (129,120,121,122),NSPV
  120       MSPV = 1
            GO TO 129
  121       PX(1) = I*NCRTG
            PY(1) = J*NCRTG
            PX(2) = PX(1)+NCRTG-1
            PY(2) = PY(1)+NCRTG-1
            CALL GPL (2,PX,PY)
            PYTMP = PY(1)
            PY(1) = PY(2)
            PY(2) = PYTMP
            CALL GPL (2,PX,PY)
C
            GO TO 129
  122       INTEN = MXLEV
            GO TO 128
  123       ZZ = ZLFT+XPART*(ZRHT-ZLFT)
C
C TEST FOR SAME INT AS LAST TIME.
C
            IF (ZZ .GT. ZLEV(LAST+1)) GO TO 126
  124       IF (ZZ .GE. ZLEV(LAST)) GO TO 127
C
C LOOK LOWER
C
            IF (LAST .LE. 1) GO TO 125
            LAST = LAST-1
            GO TO 124
  125       IF (ZZ .LT. ZLEV(LAST)) IADD = 0
            GO TO 127
C
C LOOK HIGHER
C
  126       IF (LAST .GE. NLEVL) GO TO 127
            LAST = LAST+1
            IF (ZZ .GE. ZLEV(LAST+1)) GO TO 126
C
C OK
C
  127       ISUB = LAST+IOFFST+IADD
            INTEN = IL(ISUB)
            IF (NOPTN .LT. 0) INTEN = MXLEV-INTEN
  128       CALL GRAY
  129    CONTINUE
  130 CONTINUE
C
C PUT OUT ANY REMAINING BUFFERED POINTS.
      IF (NPOINT.GT.0) THEN
        CALL GETSET (VIEW2(1),VIEW2(2),VIEW2(3),VIEW2(4),
     +               WIND2(1),WIND2(2),WIND2(3),WIND2(4),IOLLS2)
        CALL SET (0.,1.,0.,1.,0.,1023.,0.,1023.,1)
        CALL POINTS (XPNT,YPNT,NPOINT,0,0)
        CALL SET (VIEW2(1),VIEW2(2),VIEW2(3),VIEW2(4),
     +            WIND2(1),WIND2(2),WIND2(3),WIND2(4),IOLLS2)
      ENDIF
C
C CALL BOUND IF ISPV=2 AND SPECIAL VALUES WERE FOUND.
C
      IF (MSPV .EQ. 1) THEN
        CALL SET (X1,X2,Y1,Y2,1.0,X3,1.0,Y3,1)
        CALL BOUND (Z,MX,NX,NY,SP)
      END IF
  132 CONTINUE
C
C Restore the original SET call.
C
      CALL SET (VIEW(1),VIEW(2),VIEW(3),VIEW(4),
     +          WIND(1),WIND(2),WIND(3),WIND(4),IOLLS)
C
C
      RETURN
C
      END

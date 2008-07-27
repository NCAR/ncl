C
C	$Id: wmdrft.f,v 1.16 2008-07-27 00:17:36 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE WMDRFT(N,X,Y)
C
C  This subroutine draws a front along the line determined by the
C  world coordinates  (X(I),Y(I)) for I=1,N.  A spline curve is
C  fit to the points, so only a few points need be supplied.
C
      include 'wmcomn.h'
C
      DIMENSION X(N),Y(N),OLDWN(4),OLDVP(4),OCLIP(4)
      DIMENSION IPOSIT(200)
C
      NPO = N
C
C  Check error status.
C
      IF (ICFELL('WMDRFT - Uncleared prior error',1) .NE. 0) RETURN
      IF (NPO .LE. 1) THEN
        CALL SETER ('WMDRFT - input line must have at least two points',       
     +              1, 1)
        RETURN
      ENDIF
C
C  If the original line has more than NPTS points, filter out NPTS
C  points from it.  Otherwise, keep the original points.
C
      IF (NPO .GT. NPTS) THEN
        NPO = NPTS
        CALL WMFPTS(N,X,Y,NPTS,XO,YO)
      ELSE
        DO 100 I=1,N
          XO(I) = X(I)
          YO(I) = Y(I)
  100   CONTINUE
      ENDIF
C
C  Cull out any duplicate points.
C
      NTOT = NPO
      I = 2
  120 CONTINUE
      CALL WMSQDP(XO,YO,NTOT,I,IFLG)
      IF (IFLG .EQ. 1) THEN
        NTOT = NTOT-1
        IF(I .LE. NTOT) GO TO 120
      ELSE
        I = I+1
        IF (I .GT. NTOT) THEN
          GO TO 110
        ELSE
          GO TO 120
        ENDIF
      ENDIF
  110 CONTINUE
      NPO = NTOT 
C
C  Construct a spline curve having NPTS points in it that represents
C  the curve in (X,Y).  We use Alan Cline's spline package for this.
C
      IF (TNSION .EQ. -1.) THEN
        TNSION = 1.
      ENDIF
C
C  Interpolate if ISMOTH=0; use a smoothing spline otherwise.
C
      IF (ISMOTH .EQ. 0) THEN
C
C  S is a common variable of maximal dimension of NPTS.
C
        CALL MSKRV1(NPO,XO,YO,SLOPE1,SLOPE2,XS,YS,TEMP,S,TNSION,ISLFLG)
        DO 10 I=1,NPTS
          T = REAL(I-1)/REAL(NPTS-1)
          IF (I .EQ. 1) THEN
            CALL MSKRV2(T,XOUT(I),YOUT(I),NPO,XO,YO,XS,YS,S,TNSION,1,
     +                  SLOPEL)
          ELSE IF (I .EQ. NPTS) THEN
            CALL MSKRV2(T,XOUT(I),YOUT(I),NPO,XO,YO,XS,YS,S,TNSION,0,
     +                  SLP)
          ELSE
            CALL MSKRV2(T,XOUT(I),YOUT(I),NPO,XO,YO,XS,YS,S,TNSION,1,
     +                  SLOPER)
          ENDIF
   10   CONTINUE
      ELSE
        TEPS = SQRT(2./REAL(NPO))
        IOBSET = 1
        IF (OBSERR .EQ. -1.) THEN
          IOBSET = 0
          XOMAX = WMCMAX(NPO,XO)
          YOMAX = WMCMAX(NPO,YO)
          OBSERR = 0.025*MAX(XOMAX,YOMAX)
          OBSRET = OBSERR
        ENDIF
        IRSSET = 1
        IF (RSMOTH .EQ. -1.) THEN
          IRSSET = 0
          RSMOTH = REAL(NPO)
          RSMRET = RSMOTH
        ENDIF 
        CALL CURVS1(NPO,XO,YO,OBSERR,1,RSMOTH,TEPS,TEMP(1,1),
     +              XS,YS,XSS,YSS,TNSION,TEMP(1,2),IER)
        DO 11 I=1,NPTS
          T = REAL(I-1)/REAL(NPTS-1)
          CALL CURVS2(T,N,TEMP(1,1),XS,YS,XSS,YSS,TNSION,XOUT(I),
     +                YOUT(I))
   11   CONTINUE
      ENDIF
C
C  Restore flags for unset values.
C
      IF(IOBSET .EQ. 0) OBSERR = -1.
      IF(IRSSET .EQ. 0) RSMOTH = -1.
C
C  Convert to NDC space.
C
      CALL WMW2NX(NPTS,XOUT,XS)
      CALL WMW2NY(NPTS,YOUT,YS)
      DO 20 I=1,NPTS
        XOUT(I) = XS(I)
        YOUT(I) = YS(I)
   20 CONTINUE
C
C  Save normalization transformation number 1 along with its
C  window and viewport and define a new transformation 1
C  that has as its window and viewport the current clip
C  rectangle.
C
      CALL GQCNTN(IER,NTRO)
      CALL GQCLIP(IER,ICLIP,OCLIP)
      CALL GQNT(1,IER,OLDWN,OLDVP)
C     
      CALL GSWN(1,OCLIP(1),OCLIP(2),OCLIP(3),OCLIP(4))
      CALL GSVP(1,OCLIP(1),OCLIP(2),OCLIP(3),OCLIP(4))
      CALL GSELNT(1)
C
C  Save the current value for fill area interior style, and set
C  interior style according to that needed.
C
      CALL GQFAIS(IER,INSTYO)
      CALL GSFAIS(1)
C
C  Save the current value for linewidth scale, and set to the current
C  value of RLINWD.
C
      CALL GQLWSC(IER,RLNSCO)
      CALL GSLWSC(RLINWD)
C
C  Save the current line and fill colors and set them to ICOLOR.
C
      CALL GQFACI(IER,IFCLRO)
      CALL GQPLCI(IER,ILCLRO)
      CALL GSFACI(ICOLOR)
      CALL GSPLCI(ICOLOR)
C
C  Calculate the distances along the curve.
C
      ALEN(1) = 0.
      DO 30 I=2,NPTS
        ALEN(I) = ALEN(I-1) +
     +              SQRT((XOUT(I)-XOUT(I-1))**2+(YOUT(I)-YOUT(I-1))**2)      
   30 CONTINUE
C
C  Just draw line if squall or tropical front.
C
      IF (IFRONT.EQ.5 .OR. IFRONT.EQ.6) THEN
        CALL WMLGPL(NPTS,XOUT,YOUT)
        GO TO 80
      ENDIF
C
C  Calculate the number of symbols along the curve.
C
      DSTBTW = BETDST
      CRVLEN = ALEN(NPTS)
      IF (NUMSYO .GE. 0) THEN
        NUMSYM = MIN(NUMSYO,MAXSYM)
      ELSE
        NUMSYM = 1
        DO 40 I=1,MAXSYM
          IF (REAL(I+1)*SYMWID+BEGDST+ENDDST+REAL(I)*DSTBTW .LE.
     +        CRVLEN) THEN
            NUMSYM = I+1
            IF (NUMSYM .EQ. MAXSYM) GO TO 50
            GO TO 40
          ELSE
            GO TO 50
          ENDIF
   40   CONTINUE
      ENDIF
   50 CONTINUE
C
C  Calculate distance between symbols for even spacing.
C
      IF (NUMSYM .EQ. 1) THEN
C
C  If NUMSYM is "1", and the front is occluded or stationary, see
C  if we can adjust the spacing between symbols so that two symbols
C  can be put out.  If not, issue a warning.
C
        IF (IFRONT.EQ.3. OR. IFRONT.EQ.4) THEN
          IF (BEGDST+ENDDST+2.*SYMWID .LT. CRVLEN) THEN
            NUMSYM = 2
            DSTBTW = CRVLEN-BEGDST-ENDDST-NUMSYM*SYMWID
          ELSE
            IF (IFRONT .EQ. 3) THEN
              PRINT *,    'WMDRFT - Warning: not enough space along the 
     +input curve to draw two symbols for a stationary front'
              NUMSYM = 0
C
C  For a stationary front, draw the line and return.  First find
C  the index that marks the midpoint of the line.
C
              HLFLEN = 0.5*ALEN(NPTS)
              IHLF = 2
              DO 90 I=1,NPTS
                IF (ALEN(I) .GE. HLFLEN) THEN
                  IHLF = I
                  GO TO 91
                ENDIF 
   90         CONTINUE
   91         CONTINUE
              ICOLOR = ICOLDC
              CALL WMLGPL(IHLF,XOUT,YOUT)
              ICOLOR = IWARMC
              IR = NPTS-IHLF+1
              CALL WMLGPL(IR,XOUT(IHLF),YOUT(IHLF))
              GO TO 80
            ELSE
              PRINT *,    'WMDRFT - Warning: not enough space along the 
     +input curve to draw two symbols for an occluded front'
              NUMSYM = 0
C
C  For an occluded front, draw the line and return.  First find
C  the index that marks the midpoint of the line.
C
              HLFLEN = 0.5*ALEN(NPTS)
              IHLF = 2
              DO 92 I=1,NPTS
                IF (ALEN(I) .GE. HLFLEN) THEN
                  IHLF = I
                  GO TO 93
                ENDIF 
   92         CONTINUE
   93         CONTINUE
              ICOLOR = ICOLDC
              CALL WMLGPL(IHLF,XOUT,YOUT)
              ICOLOR = IWARMC
              IR = NPTS-IHLF+1
              CALL WMLGPL(IR,XOUT(IHLF),YOUT(IHLF))
              GO TO 80
            ENDIF
          ENDIF
        ENDIF
      ELSE
        DSTBTW = (CRVLEN-BEGDST-ENDDST-NUMSYM*SYMWID)/(NUMSYM-1) 
      ENDIF
C
C  Draw the symbols along the front.
C
      IF (NUMSYM .GT. 0) THEN
        CALL WMDRSM(NUMSYM,DSTBTW,NPTS,XOUT,YOUT,ALEN,XS,YS,IPOSIT)
      ENDIF
C
   60 CONTINUE
C
C  Draw the spline curve if it is long enough.
C 
      IF (ALEN(NPTS) .GT. BEGDST+SYMWID+ENDDST) THEN
        CALL GQPLCI(IER,ICOLD)
        IF (NUMSYM .LE. 1) THEN
          IF (ABS(ISTYPE(1)) .EQ. 1) THEN
            ICOLOR = ICOLDC
          ELSE IF (ABS(ISTYPE(1)) .EQ. 2) THEN
            ICOLOR = IWARMC
          ENDIF
          CALL WMLGPL(NPTS,XOUT,YOUT)
        ELSE
C
C  Since the spline curve will be drawn in segments (so that 
C  the proper colors can be applied) we need to guard against 
C  possible notches in thick curves by drawing the entire spline
C  first.
C 
          IF (IFRONT.GE.1 .AND. IFRONT.LE.4) THEN
            ICOLOR = IWARMC
            IF (IFRONT .EQ. 1) THEN
              ICOLOR = ICOLDC
            ELSE
              ICOLOR = IWARMC
            ENDIF
            CALL WMLGPL(NPTS,XOUT,YOUT)
          ENDIF
C
C  Now draw the segments.
C
          IBL = 1
          DO 70 J=1,NUMSYM-1
            IF (ABS(ISTYPE(J)) .EQ. 1) THEN
              ICOLOR = ICOLDC
            ELSE IF (ABS(ISTYPE(J)) .EQ. 2) THEN
              ICOLOR = IWARMC
            ENDIF
            IBR = (IPOSIT(J+1)+IPOSIT(J))/2
            IF (IBR-IBL .GE. 1) THEN
              CALL WMLGPL(IBR-IBL+1,XOUT(IBL),YOUT(IBL))
            ENDIF
            IBL = IBR
   70     CONTINUE 
          IF (ABS(ISTYPE(NUMSYM)) .EQ. 1) THEN
            ICOLOR = ICOLDC
          ELSE IF (ABS(ISTYPE(NUMSYM)) .EQ. 2) THEN
            ICOLOR = IWARMC
          ENDIF
          IBR = NPTS
          CALL WMLGPL(IBR-IBL+1,XOUT(IBL),YOUT(IBL))
        ENDIF
        ICOLOR = ICOLD
      ENDIF
C
C  Restore normalization transformation 1, interior style,
C  linewidth, fill color, line color.
C
  80  CONTINUE
      CALL GSWN(1,OLDWN(1),OLDWN(2),OLDWN(3),OLDWN(4))
      CALL GSVP(1,OLDVP(1),OLDVP(2),OLDVP(3),OLDVP(4))
      CALL GSELNT(NTRO)
      CALL GSFAIS(INSTYO)
      CALL GSLWSC(RLNSCO)
      CALL GSFACI(IFCLRO)
      CALL GSPLCI(ILCLRO)
C
      RETURN
      END
      SUBROUTINE WMSQDP(X,Y,NTOT,I,IFLG)
C
C  If (X(I),Y(I)) equals some (X(J),Y(J)) for J .LT. I,
C  then squeeze (X(I),Y(I)) out of X and Y and set IFLG = 1.
C
      DIMENSION X(NTOT),Y(NTOT)
C
      DO 10 J=1,I-1
        IF (X(I).EQ.X(J) .AND. Y(I).EQ.Y(J)) THEN
          IF (I .EQ. NTOT) THEN
            IFLG = 1
            RETURN
          ENDIF
          DO 20 K=I,NTOT-1
            X(K) = X(K+1)
            Y(K) = Y(K+1)
            IFLG = 1
   20     CONTINUE
          RETURN
        ENDIF
   10 CONTINUE
C
      IFLG = 0
      RETURN
      END

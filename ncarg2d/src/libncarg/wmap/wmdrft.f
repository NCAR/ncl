C
C	$Id: wmdrft.f,v 1.4 1994-12-16 17:51:42 fred Exp $
C
      SUBROUTINE WMDRFT(N,X,Y)
C
C  This subroutine draws a front along the line determined by the
C  world coordinates  (X(I),Y(I)) for I=1,N.  A spline curve is
C  fit to the points, so only a few points need be supplied.
C
      include 'wmcomn.h'
C
      DIMENSION X(N),Y(N)
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
C  Construct a spline curve having NPTS points in it that represents
C  the curve in (X,Y).  We use Alan Cline's spline package for this.
C
      CALL MSKRV1(NPO,XO,YO,SLOPE1,SLOPE2,XS,YS,TEMP,S,TNSION,ISLFLG)
      DO 10 I=1,NPTS
        T = REAL(I-1)/REAL(NPTS-1)
        IF (I .EQ. 1) THEN
          CALL MSKRV2(T,XOUT(I),YOUT(I),NPO,XO,YO,XS,YS,S,TNSION,1,
     +                SLOPEL)
        ELSE IF (I .EQ. NPTS) THEN
          CALL MSKRV2(T,XOUT(I),YOUT(I),NPO,XO,YO,XS,YS,S,TNSION,0,SLP)
        ELSE
          CALL MSKRV2(T,XOUT(I),YOUT(I),NPO,XO,YO,XS,YS,S,TNSION,1,
     +                SLOPER)
        ENDIF
   10 CONTINUE
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
C  Save the current normalization transformation number, and select
C  transformation 0.
C
      CALL GQCNTN(IER,NTRO)
      CALL GSELNT(0)
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
C  can be put out.  If not, issue an error.
C
        IF (IFRONT.EQ.3. OR. IFRONT.EQ.4) THEN
          IF (BEGDST+ENDDST+2.*SYMWID .LT. CRVLEN) THEN
            NUMSYM = 2
            DSTBTW = CRVLEN-BEGDST-ENDDST-NUMSYM*SYMWID
          ELSE
            IF (IFRONT .EQ. 3) THEN
              CALL SETER ('WMDRFT - not enough space along the input cur
     +ve to draw two symbols for a stationary front', 2, 1)       
              RETURN
            ELSE
              CALL SETER ('WMDRFT - not enough space along the input cur
     +ve to draw two symbols for an occluded front', 3, 1)       
              RETURN
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
          IF (IABS(ISTYPE(1)) .EQ. 1) THEN
            ICOLOR = ICOLDC
          ELSE IF (IABS(ISTYPE(1)) .EQ. 2) THEN
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
            IF (IABS(ISTYPE(J)) .EQ. 1) THEN
              ICOLOR = ICOLDC
            ELSE IF (IABS(ISTYPE(J)) .EQ. 2) THEN
              ICOLOR = IWARMC
            ENDIF
            IBR = (IPOSIT(J+1)+IPOSIT(J))/2
            CALL WMLGPL(IBR-IBL+1,XOUT(IBL),YOUT(IBL))
            IBL = IBR
   70     CONTINUE 
          IF (IABS(ISTYPE(NUMSYM)) .EQ. 1) THEN
            ICOLOR = ICOLDC
          ELSE IF (IABS(ISTYPE(NUMSYM)) .EQ. 2) THEN
            ICOLOR = IWARMC
          ENDIF
          IBR = NPTS
          CALL WMLGPL(IBR-IBL+1,XOUT(IBL),YOUT(IBL))
        ENDIF
        ICOLOR = ICOLD
      ENDIF
C
C  Restore the original normalization transformation, interior style,
C  linewidth, fill color, line color.
C
  80  CONTINUE
      CALL GSELNT(NTRO)
      CALL GSFAIS(INSTYO)
      CALL GSLWSC(RLNSCO)
      CALL GSFACI(IFCLRO)
      CALL GSPLCI(ILCLRO)
C
      RETURN
      END

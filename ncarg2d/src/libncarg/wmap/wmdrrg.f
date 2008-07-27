C
C	$Id: wmdrrg.f,v 1.10 2008-07-27 00:17:36 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE WMDRRG(N,X,Y,ITYPE,NC,XC,YC)
C
C  This subroutine draws a closed region filled as per the
C  value of ITYPE.  Legal values for ITYPE are:
C
C    ITYPE = 'INDEXnnn' where "nnn" denotes a color index.
C          = 'T' for thunderstorms.
C          = 'SH' for showers.
C          = 'R' for rain.
C          = 'F' for flurries.
C          = 'SN' for snow.
C          = 'I' for ice.
C
C  If NC is greater than 2, then the region resulting from the
C  spline fit to (X(I),Y(I),I=1,N) will be clipped against the 
C  region in (XC(I),YC(I),I=1,NC) .
C
C  X,Y and XC,YC are world coordinate values.
C
      include 'wmcomn.h'
C
      DIMENSION X(N),Y(N),XC(NC),YC(NC)
      CHARACTER*(*) ITYPE
C
C  Define necessary arrays and functions for the clipping calls.
C
      COMMON /WMLGCM/IDRFLG
      EXTERNAL WMLGFA
C
      NPO = N
C
C  Check error status.
C
      IF (ICFELL('WMDRRG - Uncleared prior error',1) .NE. 0) RETURN
      IF (NPO .LE. 1) THEN
        CALL SETER (
     +       'WMDRRG - input region must have at least three points',       
     +       1, 1)
        RETURN
      ENDIF
      IF (NC.LT.1 .OR. NC.EQ.2) THEN
        CALL SETER (
     +    'WMDRRG - fifth argument must be either 1 or greater than 2',      
     +    1, 1)
        RETURN
      ENDIF
C
C  If the original region has more than NPTS points, filter out NPTS
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
      IF (TNSION .EQ. -1.) THEN
        TNSION = 0.001
      ENDIF
C
C  S is a common variable of maximal dimension NPTS.
C
      CALL MSKRV1(NPO,XO,YO,SLOPE1,SLOPE2,XS,YS,TEMP,S,TNSION,4)
      DO 10 I=1,NPTS
        T = REAL(I-1)/REAL(NPTS-1)
        IF (I.EQ.1) THEN
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
C  Convert spline curve to NDC space.
C
      CALL WMW2NX(NPTS,XOUT,XS)
      CALL WMW2NY(NPTS,YOUT,YS)
      DO 20 I=1,NPTS
        XOUT(I) = XS(I)
        YOUT(I) = YS(I)
   20 CONTINUE
C
C  Convert clipping region to NDC.
C
      IF (NC .GT. 2) THEN
        CALL WMW2NX(NC,XC,RWORK(1))
        CALL WMW2NY(NC,YC,RWORK(NC+1))
      ENDIF
C
C  Save the current normalization transformation number, and select
C  transformation 0.
C
      CALL GQCNTN(IER,NTRO)
      CALL GSELNT(0)
      CALL GQPLCI(IER,IPLLRO)
      CALL GQFACI(IER,IFCLRO)
      CALL GQFAIS(IER,INSTYO)
      CALL GSFAIS(1)
C
C  Draw the region.
C
      IF (ITYPE(1:5).EQ.'INDEX' .OR. ITYPE(1:5).EQ.'index') THEN
C
C  Get the color index.
C
        ICLEN = LEN(ITYPE)
C
C  Find the first non-blank character starting from the right
C  of ITYPE and moving to the left.  Adjust ICLEN accordingly.
C
        DO 400 I=ICLEN,1,-1
          IF (ITYPE(I:I) .NE. ' ') THEN
            ICLEN = I
            GO TO 405
          ENDIF
  400   CONTINUE   
  405   CONTINUE
        IF (ICLEN .EQ. 6) THEN
           READ(ITYPE(6:ICLEN),'(I1)') NDX
        ELSE IF (ICLEN .EQ. 7) THEN
           READ(ITYPE(6:ICLEN),'(I2)') NDX
        ELSE IF (ICLEN .EQ. 8) THEN
           READ(ITYPE(6:ICLEN),'(I3)') NDX
        ELSE IF (ICLEN .EQ. 9) THEN
           READ(ITYPE(6:ICLEN),'(I4)') NDX
        ELSE IF (ICLEN .EQ. 10) THEN
           READ(ITYPE(6:ICLEN),'(I5)') NDX
        ELSE
          NDX = 1
        ENDIF
        CALL GSFACI(NDX)
        CALL GSPLCI(NDX)
C
C  Draw the region.
C 
        IF (NC .EQ. 1) THEN
C
C  No clipping to be done.
C
          CALL GFA(NPTS,XOUT,YOUT)
        ELSE
C
C  Clip against the input poygon, set the value for IDRFLG so that
C  no outline is drawn.
C
          IDRFLG = 0
          CALL PPINPO(RWORK(1),RWORK(NC+1),NC,XOUT,YOUT,NPTS,
     +                RWORK(2*NC+1),IWORK(2*NC+1),NWRK-2*NC,WMLGFA,IER)       
        ENDIF
      ELSE IF (ITYPE(1:1).EQ.'T' .OR. ITYPE(1:1).EQ.'t') THEN
C
C  Thunderstorms.
C
        CALL GSFACI(ICOLOR)
        CALL GSPLCI(ICOLOR)
        CALL WMRGWT(NPTS,XOUT,YOUT,37,102)
      ELSE IF (ITYPE(1:2).EQ.'SH' .OR. ITYPE(1:2).EQ.'sh' .OR.
     +         ITYPE(1:2).EQ.'Sh') THEN
C
C  Showers.
C
        CALL GSFACI(ICOLOR)
        CALL GSPLCI(ICOLOR)
        CALL WMRGWT(NPTS,XOUT,YOUT,37,101)
      ELSE IF (ITYPE(1:1).EQ.'R' .OR. ITYPE(1:1).EQ.'r') THEN
C
C  Rain.
C
        CALL GSFACI(ICOLOR)
        CALL GSPLCI(ICOLOR)
        CALL WMRGWT(NPTS,XOUT,YOUT,37,103)
      ELSE IF (ITYPE(1:1).EQ.'F' .OR. ITYPE(1:1).EQ.'f') THEN
C
C  Flurries.
C
        CALL GSFACI(ICOLOR)
        CALL GSPLCI(ICOLOR)
        CALL WMRGWT(NPTS,XOUT,YOUT,37,106)
      ELSE IF (ITYPE(1:2).EQ.'SN' .OR. ITYPE(1:2).EQ.'sn' .OR.
     +         ITYPE(1:2).EQ.'Sn') THEN
C
C  Snow.
C
        CALL GSFACI(ICOLOR)
        CALL GSPLCI(ICOLOR)
        CALL WMRGWT(NPTS,XOUT,YOUT,37,104)
      ELSE IF (ITYPE(1:1).EQ.'I' .OR. ITYPE(1:1).EQ.'i') THEN
C
C  Ice.
C
        CALL GSFACI(ICOLOR)
        CALL GSPLCI(ICOLOR)
        CALL WMRGWT(NPTS,XOUT,YOUT,37,105)
      ENDIF
C
C  Restore original settings.
C
      CALL GSELNT(NTRO)
      CALL GSFAIS(INSTYO)
      CALL GSFACI(IFCLRO)
      CALL GSPLCI(IPLLRO)
C
      RETURN
      END

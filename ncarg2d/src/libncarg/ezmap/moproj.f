C
C $Id: moproj.f,v 1.4 2008-09-18 00:42:18 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MOPROJ (DLAT,DLON,UVAL,VVAL)
C
        DOUBLE PRECISION DLAT,DLON,UVAL,VVAL
C
C The subroutine MOPROJ implements the true Mollweide projection,
C using formulas identical to those given on various Web sites (for
C example, Wikipedia).
C
C DLAT and DLON are input values of latitude and longitude, in radians.
C UVAL and VVAL are output values of U and V, in the projection plane.
C
C The common block MAPCM0 contains mathematical constants.
C
        COMMON /MAPCM0/  COS1,DTOR,DTRH,OOPI,PI,PIOF,PIOT,RTDD,RTOD,
     +                   SROT,SIN1,TOPI,TSRT
        DOUBLE PRECISION COS1,DTOR,DTRH,OOPI,PI,PIOF,PIOT,RTDD,RTOD,
     +                   SROT,SIN1,TOPI,TSRT
        SAVE   /MAPCM0/
C
C Local variables.
C
        DOUBLE PRECISION THTA,THTP
C
C Near the poles, the following code doesn't work properly, so we have
C to deal with that case separately:
C
        IF (ABS(DLAT)-PIOT.GT.-1.D-12) THEN
          UVAL=0.D0
          VVAL=SIGN(SROT,DLAT)
          RETURN
        END IF
C
C We have to do a Newton-Raphson iteration to get a value for "theta".
C Set up the iteration loop.  THTP is an initial guess (per MathWorld).
C
        THTP=2.D0*ASIN(MAX(-1.D0,MIN(+1.D0,DLAT/PIOT)))
C
C NITR keeps track of the number of iterations performed.
C
        NITR=1
C
C Iteration loop.  First, compute terms that occur frequently.
C
  101   THTA=THTP-(THTP+SIN(THTP)-PI*SIN(DLAT))/(1.D0+COS(THTP))
C
C If the improved value is not sufficiently different from the previous
C value and fewer than the maximum number of iterations have been done,
C go back for another one.  Otherwise, we have our value of THTA.
C
        IF (ABS(THTA-THTP).GT.1.D-12) THEN
          IF (NITR.LT.20) THEN
            THTP=THTA
            NITR=NITR+1
            GO TO 101
          END IF
        END IF
C
C Halve the value of "theta".
C
        THTA=.5D0*THTA
C
C Compute the values of UVAL and VVAL.
C
        UVAL=SROT*COS(THTA)*DLON/PIOT
        VVAL=SROT*SIN(THTA)
C
        RETURN
C
      END

C
C $Id: haprin.f,v 1.4 2008-09-18 00:42:16 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE HAPRIN (UVAL,VVAL,DLAT,DLON)
C
        DOUBLE PRECISION UVAL,VVAL,DLAT,DLON
C
C The subroutine HAPRIN implements a closed form of the Hammer
C projection, using formulas given on "Wolfram MathWorld".
C
C UVAL and VVAL are input values of U and V, in the projection plane.
C DLAT and DLON are output values of latitude and longitude, in radians.
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
        DOUBLE PRECISION SVAL,TVAL
C
C First, we must check the point (UVAL,VVAL) to see if it lies in the
C ellipse occupied by the projection of the globe.  If not, we return
C the value "1.D12" to signal that fact.
C
        IF ((UVAL/TSRT)**2+(VVAL/SROT)**2-1.D0.GT.1.D-12) THEN
          DLAT=1.D12
          DLON=1.D12
          RETURN
        END IF
C
C Do the math.
C
        SVAL=1.D0-.0625D0*UVAL**2-.25D0*VVAL**2
        TVAL=SQRT(SVAL)
C
        DLAT=ASIN(MAX(-1.D0,MIN(+1.D0,VVAL*TVAL)))
        DLON=2.D0*ATAN2(UVAL*TVAL,4.D0*SVAL-2.D0)
C
        RETURN
C
      END

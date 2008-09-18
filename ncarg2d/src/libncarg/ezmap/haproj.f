C
C $Id: haproj.f,v 1.4 2008-09-18 00:42:16 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE HAPROJ (DLAT,DLON,UVAL,VVAL)
C
        DOUBLE PRECISION DLAT,DLON,UVAL,VVAL
C
C The subroutine HAPROJ implements a closed form of the Hammer
C projection, using formulas identical to those given on various Web
C sites.
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
C Local variable.
C
        DOUBLE PRECISION TVAL
C
C Do the math.
C
        TVAL=SQRT(1.D0+COS(DLAT)*COS(.5D0*DLON))
C
        UVAL=TSRT*COS(DLAT)*SIN(.5D0*DLON)/TVAL
        VVAL=SROT*SIN(DLAT)/TVAL
C
        RETURN
C
      END

C
C $Id: wtproj.f,v 1.4 2008-09-18 00:42:18 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE WTPROJ (DLAT,DLON,UVAL,VVAL,CSLT)
C
        DOUBLE PRECISION DLAT,DLON,UVAL,VVAL,CSLT
C
C The subroutine WTPROJ implements a closed form of the Winkel tripel
C projection, using formulas identical to those given on various Web
C sites (for example, Wikipedia), using the rather odd function SINC(X),
C the value of which is SIN(X)/X, when X is not zero, and 1, otherwise.
C For efficiency, this function is incorporated into the local code.
C
C DLAT and DLON are input values of latitude and longitude, in radians.
C UVAL and VVAL are output values of U and V, in the projection plane.
C CSLT is the cosine of the standard parallel of the Winkel tripel, for
C which Winkel used ACOS(2/PI); in EZMAP, though, it is allowed to vary.
C
C Local variable.
C
        DOUBLE PRECISION TVAL
C
        TVAL=ACOS(COS(DLAT)*COS(.5D0*DLON))
C
        IF (ABS(TVAL).GT.1.D-6) THEN
          TVAL=SIN(TVAL)/TVAL
        ELSE
          TVAL=1.D0
        END IF
C
        UVAL=2.D0*COS(DLAT)*SIN(.5D0*DLON)/TVAL
        VVAL=SIN(DLAT)/TVAL
C
        UVAL=.5D0*(DLON*CSLT+UVAL)
        VVAL=.5D0*(DLAT+VVAL)
C
        RETURN
C
      END

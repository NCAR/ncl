C
C $Id: aiproj.f,v 1.4 2008-09-18 00:42:16 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE AIPROJ (DLAT,DLON,UVAL,VVAL)
C
        DOUBLE PRECISION DLAT,DLON,UVAL,VVAL
C
C The subroutine AIPROJ implements a closed form of the Aitoff
C projection, using formulas identical to those given on various Web
C sites (for example, Wikipedia), using the rather odd function SINC(X),
C the value of which is SIN(X)/X, when X is not zero, and 1, otherwise.
C For efficiency, this function is incorporated into the local code.
C
C DLAT and DLON are input values of latitude and longitude, in radians.
C UVAL and VVAL are output values of U and V, in the projection plane.
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
        RETURN
C
      END

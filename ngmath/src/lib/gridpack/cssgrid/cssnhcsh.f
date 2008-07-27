C
C	$Id: cssnhcsh.f,v 1.5 2008-07-27 03:10:08 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CSSNHCSH (X, SINHM,COSHM,COSHMM)
      DOUBLE PRECISION X, SINHM, COSHM, COSHMM
C
C***********************************************************
C
C                                                From TSPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   11/20/96
C
C   This subroutine computes approximations to the modified
C hyperbolic functions defined below with relative error
C bounded by 3.4E-20 for a floating point number system with
C sufficient precision.
C
C   Note that the 21-digit constants in the data statements
C below may not be acceptable to all compilers.
C
C On input:
C
C       X = Point at which the functions are to be
C           evaluated.
C
C X is not altered by this routine.
C
C On output:
C
C       SINHM = sinh(X) - X.
C
C       COSHM = cosh(X) - 1.
C
C       COSHMM = cosh(X) - 1 - X*X/2.
C
C Modules required by CSSNHCSH:  None
C
C Intrinsic functions called by CSSNHCSH:  ABS, EXP
C
C***********************************************************
C
      DOUBLE PRECISION AX, EXPX, F, P, P1, P2, P3, P4, Q,
     .                 Q1, Q2, Q3, Q4, XC, XS, XSD2, XSD4
C
      DATA P1/-3.51754964808151394800D5/,
     .     P2/-1.15614435765005216044D4/,
     .     P3/-1.63725857525983828727D2/,
     .     P4/-7.89474443963537015605D-1/
      DATA Q1/-2.11052978884890840399D6/,
     .     Q2/3.61578279834431989373D4/,
     .     Q3/-2.77711081420602794433D2/,
     .     Q4/1.D0/
      AX = ABS(X)
      XS = AX*AX
      IF (AX .LE. .5D0) THEN
C
C Approximations for small X:
C
        XC = X*XS
        P = ((P4*XS+P3)*XS+P2)*XS+P1
        Q = ((Q4*XS+Q3)*XS+Q2)*XS+Q1
        SINHM = XC*(P/Q)
        XSD4 = .25D0*XS
        XSD2 = XSD4 + XSD4
        P = ((P4*XSD4+P3)*XSD4+P2)*XSD4+P1
        Q = ((Q4*XSD4+Q3)*XSD4+Q2)*XSD4+Q1
        F = XSD4*(P/Q)
        COSHMM = XSD2*F*(F+2.D0)
        COSHM = COSHMM + XSD2
      ELSE
C
C Approximations for large X:
C
        EXPX = EXP(AX)
        SINHM = -(((1.D0/EXPX+AX)+AX)-EXPX)/2.D0
        IF (X .LT. 0.D0) SINHM = -SINHM
        COSHM = ((1.D0/EXPX-2.D0)+EXPX)/2.D0
        COSHMM = COSHM - XS/2.D0
      ENDIF
      RETURN
      END

      SUBROUTINE CSSNHCSH (X, SINHM,COSHM,COSHMM)
      REAL X, SINHM, COSHM, COSHMM
C
C***********************************************************
C
C                                              From SSRFPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   03/18/90
C
C   This subroutine computes approximations to the modified
C hyperbolic functions defined below with relative error
C bounded by 4.7E-12 for a floating point number system with
C sufficient precision.  For IEEE standard single precision,
C the relative error is less than 1.E-5 for all x.
C
C   Note that the 13-digit constants in the data statements
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
      REAL AX, C1, C2, C3, C4, EXPX, F, XC, XS, XSD2, XSD4
C
      DATA C1/.1666666666659E0/,
     .     C2/.8333333431546E-2/,
     .     C3/.1984107350948E-3/,
     .     C4/.2768286868175E-5/
      AX = ABS(X)
      XS = AX*AX
      IF (AX .LE. .5) THEN
C
C Approximations for small X:
C
        XC = X*XS
        SINHM = XC*(((C4*XS+C3)*XS+C2)*XS+C1)
        XSD4 = .25*XS
        XSD2 = XSD4 + XSD4
        F = (((C4*XSD4+C3)*XSD4+C2)*XSD4+C1)*XSD4
        COSHMM = XSD2*F*(F+2.)
        COSHM = COSHMM + XSD2
      ELSE
C
C Approximations for large X:
C
        EXPX = EXP(AX)
        SINHM = -(((1./EXPX+AX)+AX)-EXPX)/2.
        IF (X .LT. 0.) SINHM = -SINHM
        COSHM = ((1./EXPX-2.)+EXPX)/2.
        COSHMM = COSHM - XS/2.
      ENDIF
      RETURN
      END

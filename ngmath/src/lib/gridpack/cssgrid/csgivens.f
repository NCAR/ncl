C
C	$Id: csgivens.f,v 1.5 2008-07-27 03:10:06 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CSGIVENS ( A,B, C,S)
      DOUBLE PRECISION A, B, C, S
C
C***********************************************************
C
C                                              From SSRFPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   05/09/92
C
C   This subroutine constructs the Givens plane rotation,
C
C           ( C  S)
C       G = (     ) , where C*C + S*S = 1,
C           (-S  C)
C
C which zeros the second component of the vector (A,B)**T
C (transposed).  Subroutine CSROTATE may be called to apply
C the transformation to a 2 by N matrix.
C
C   This routine is identical to Subroutine SROTG from the
C LINPACK BLAS (Basic Linear Algebra Subroutines).
C
C On input:
C
C       A,B = Components of the vector defining the rota-
C             tion.  These are overwritten by values R
C             and Z (described below) which define C and S.
C
C On output:
C
C       A = Signed Euclidean norm R of the input vector:
C           R = +/-SQRT(A*A + B*B)
C
C       B = Value Z such that:
C             C = SQRT(1-Z*Z) and S=Z if ABS(Z) .LE. 1, and
C             C = 1/Z and S = SQRT(1-C*C) if ABS(Z) > 1.
C
C       C = +/-(A/R) or 1 if R = 0.
C
C       S = +/-(B/R) or 0 if R = 0.
C
C Modules required by CSGIVENS:  None
C
C Intrinsic functions called by CSGIVENS:  ABS, SQRT
C
C***********************************************************
C
      DOUBLE PRECISION AA, BB, R, U, V
C
C Local parameters:
C
C AA,BB = Local copies of A and B
C R =     C*A + S*B = +/-SQRT(A*A+B*B)
C U,V =   Variables used to scale A and B for computing R
C
      AA = A
      BB = B
      IF (ABS(AA) .GT. ABS(BB)) THEN
C
C ABS(A) > ABS(B).
C
        U = AA + AA
        V = BB/U
        R = SQRT(.25 + V*V) * U
        C = AA/R
        S = V * (C + C)
C
C Note that R has the sign of A, C > 0, and S has
C   SIGN(A)*SIGN(B).
C
        B = S
        A = R
      ELSEIF (BB .NE. 0.) THEN
C
C ABS(A) .LE. ABS(B).
C
        U = BB + BB
        V = AA/U
C
C Store R in A.
C
        A = SQRT(.25 + V*V) * U
        S = BB/A
        C = V * (S + S)
C
C Note that R has the sign of B, S > 0, and C has
C   SIGN(A)*SIGN(B).
C
        B = 1.
        IF (C .NE. 0.) B = 1./C
      ELSE
C
C A = B = 0.
C
        C = 1.
        S = 0.
      ENDIF
      RETURN
      END

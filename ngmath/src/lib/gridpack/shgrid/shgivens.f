C
C       $Id: shgivens.f,v 1.4 2008-07-27 03:10:13 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE SHGIVENS ( A,B, C,S)
      REAL A, B, C, S
C
C***********************************************************
C
C                                               ROBERT RENKA
C                                       UNIV. OF NORTH TEXAS
C                                             (817) 565-2767
C
C  This routine constructs the Givens plane rotation --
C
C      ( C  S)
C  G = (     ) where C*C + S*S = 1 -- which zeros the second
C      (-S  C)
C
C  entry of the 2-vector (A B)-transpose.  A call to SHGIVENS
C  is normally followed by a call to SHROT which applieS
C  the transformation to a 2 by N matrix.  This routine was
C  taken from LINPACK.
C
C  ON INPUT --
C
C    A,B = Components of the 2-vector to be rotated.
C
C  ON OUTPUT --
C
C    A = value overwritten by R = +/-SQRT(A*A + B*B)
C
C    B = value overwritten by a value Z which allows C
C        and S to be recovered as follows --
C
C          C = SQRT(1-Z*Z), S=Z     if ABS(Z) .LE. 1.
C          C = 1/Z, S = SQRT(1-C*C) if ABS(Z) .GT. 1.
C
C    C = +/-(A/R)
C
C    S = +/-(B/R)
C
C  MODULES REQUIRED BY SHGIVENS -- NONE
C
C  INTRINSIC FUNCTIONS CALLED BY SHGIVENS - ABS, SQRT
C
C***********************************************************
C
      REAL AA, BB, R, U, V
C
C  Local parameters --
C
C  AA,BB = local copies of A and B
C  R =     C*A + S*B = +/-SQRT(A*A+B*B)
C  U,V =   variables used to scale A and B for computing R
C
      AA = A
      BB = B
      IF (ABS(AA) .LE. ABS(BB)) GO TO 1
C
C  ABS(A) .GT. ABS(B)
C
      U = AA + AA
      V = BB/U
      IF (ABS(V) .LT. 1.E-25) V = 0.
      R = SQRT(.25 + V*V) * U
      C = AA/R
      S = V * (C + C)
C
C  Note that R has the sign of A, C .GT. 0, and S has
C  SIGN(A)*SIGN(B).
C
      B = S
      A = R
      RETURN
C
C  ABS(A) .LE. ABS(B)
C
    1 IF (BB .EQ. 0.) GO TO 2
      U = BB + BB
      V = AA/U
C
C  Store R in A.
C
      IF (ABS(V) .LT. 1.E-25) V = 0.
      A = SQRT(.25 + V*V) * U
      S = BB/A
      C = V * (S + S)
C
C  Note that R has the sign of B, S .GT. 0, and C has
C  SIGN(A)*SIGN(B).
C
      B = 1.
      IF (C .NE. 0.) B = 1./C
      RETURN
C
C  A = B = 0.
C
    2 C = 1.
      S = 0.
      RETURN
      END

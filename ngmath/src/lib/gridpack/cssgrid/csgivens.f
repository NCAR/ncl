C
C	$Id: csgivens.f,v 1.3 2000-07-13 02:49:10 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU Lesser General Public License as
C published by the Free Software Foundation; either version 2.1 of the
C License, or (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C Lesser General Public License for more details.
C
C You should have received a copy of the GNU Lesser General Public
C License along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
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

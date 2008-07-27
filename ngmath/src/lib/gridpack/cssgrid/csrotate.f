C
C	$Id: csrotate.f,v 1.5 2008-07-27 03:10:07 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CSROTATE (N,C,S, X,Y )
      INTEGER N
      DOUBLE PRECISION C, S, X(N), Y(N)
C
C***********************************************************
C
C                                              From SSRFPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   09/01/88
C
C                                                ( C  S)
C   This subroutine applies the Givens rotation  (     )  to
C                                                (-S  C)
C                    (X(1) ... X(N))
C the 2 by N matrix  (             ) .
C                    (Y(1) ... Y(N))
C
C   This routine is identical to Subroutine SROT from the
C LINPACK BLAS (Basic Linear Algebra Subroutines).
C
C On input:
C
C       N = Number of columns to be rotated.
C
C       C,S = Elements of the Givens rotation.  Refer to
C             Subroutine CSGIVENS.
C
C The above parameters are not altered by this routine.
C
C       X,Y = Arrays of length .GE. N containing the compo-
C             nents of the vectors to be rotated.
C
C On output:
C
C       X,Y = Arrays containing the rotated vectors (not
C             altered if N < 1).
C
C Modules required by CSROTATE:  None
C
C***********************************************************
C
      INTEGER I
      DOUBLE PRECISION XI, YI
C
      DO 1 I = 1,N
        XI = X(I)
        YI = Y(I)
        X(I) = C*XI + S*YI
        Y(I) = -S*XI + C*YI
    1   CONTINUE
      RETURN
      END

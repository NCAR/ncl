C
C       $Id: shrot.f,v 1.4 2008-07-27 03:10:13 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE SHROT (N,C,S, X,Y )
      INTEGER N
      REAL    C, S, X(N), Y(N)
C
C***********************************************************
C
C                                               ROBERT RENKA
C                                       UNIV. OF NORTH TEXAS
C                                             (817) 565-2767
C
C                                           ( C  S)
C  This routine applies the Givens rotation (     ) to the
C                                           (-S  C)
C                (X(1) ... X(N))
C  2 by N matrix (             ).
C                (Y(1) ... Y(N))
C
C  ON INPUT --
C
C    N   = Number of columns to be rotated.
C
C    C,S = Elements of the Givens rotation.  These may be
C          determined by subroutine SHGIVENS.
C
C    X,Y = Arrays of length .GE. N containing the vectors
C          to be rotated.
C
C  Parameters N, C, and S are not altered by this routine.
C
C  ON OUTPUT --
C
C    X,Y = rotated vectors.
C
C  MODULES REQUIRED BY SHROT -- NONE
C
C***********************************************************
C
      INTEGER I
      REAL    XI, YI
C
C  Local parameters --
C
C    I =     DO-LOOP INDEX
C    XI,YI = X(I), Y(I)
C
      IF (N.LE.0 .OR. (C.EQ.1. .AND. S.EQ.0.)) RETURN
      DO 1 I = 1,N
        XI = X(I)
        YI = Y(I)
        X(I) =  C*XI + S*YI
        Y(I) = -S*XI + C*YI
    1   CONTINUE
      RETURN
      END

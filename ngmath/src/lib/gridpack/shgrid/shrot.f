C
C       $Id: shrot.f,v 1.2 2000-07-13 02:49:30 haley Exp $
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

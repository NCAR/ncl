C
C	$Id: csarclen.f,v 1.4 2000-07-13 02:49:06 haley Exp $
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
      DOUBLE PRECISION FUNCTION CSARCLEN (P,Q)
      DOUBLE PRECISION P(3), Q(3)
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
C   This function computes the arc-length (angle in radians)
C between a pair of points on the unit sphere.
C
C On input:
C
C       P,Q = Arrays of length 3 containing the X, Y, and Z
C             coordinates (in that order) of points on the
C             unit sphere.
C
C Input parameters are not altered by this function.
C
C On output:
C
C       CSARCLEN = Angle in radians between the unit vectors
C                P and Q.  0 .LE. CSARCLEN .LE. PI.
C
C Modules required by CSARCLEN:  None
C
C Intrinsic functions called by CSARCLEN:  ATAN, SQRT
C
C***********************************************************
C
      INTEGER I
      DOUBLE PRECISION D
C
C Local parameters:
C
C D = Euclidean norm squared of P+Q
C I = DO-loop index
C
      D = 0.
      DO 1 I = 1,3
        D = D + (P(I) + Q(I))**2
    1   CONTINUE
      IF (D .EQ. 0.) THEN
C
C P and Q are separated by 180 degrees.
C
        CSARCLEN = 4.D0*ATAN(1.D0)
      ELSEIF (D .GE. 4.D0) THEN
C
C P and Q coincide.
C
        CSARCLEN = 0.
      ELSE
        CSARCLEN = 2.D0*ATAN(SQRT((4.D0-D)/D))
      ENDIF
      RETURN
      END

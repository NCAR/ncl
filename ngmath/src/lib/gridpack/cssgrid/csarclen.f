C
C	$Id: csarclen.f,v 1.6 2008-07-27 03:10:05 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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

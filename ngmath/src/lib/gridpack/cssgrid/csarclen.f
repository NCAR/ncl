      REAL FUNCTION CSARCLEN (P,Q)
      REAL P(3), Q(3)
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
C                  P and Q.  0 .LE. CSARCLEN .LE. PI.
C
C Modules required by CSARCLEN:  None
C
C Intrinsic functions called by  CSARCLEN:  ATAN, SQRT
C
C***********************************************************
C
      INTEGER I
      REAL    D
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
        CSARCLEN = 4.*ATAN(1.)
      ELSEIF (D .GE. 4.) THEN
C
C P and Q coincide.
C
        CSARCLEN = 0.
      ELSE
        CSARCLEN = 2.*ATAN(SQRT((4.-D)/D))
      ENDIF
      RETURN
      END

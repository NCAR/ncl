C
C	$Id: css2cd.f,v 1.1 2000-01-27 18:57:56 fred Exp $
C
      SUBROUTINE CSS2CD (N,RLAT,RLON, X,Y,Z)
      INTEGER N
      DOUBLE PRECISION RLAT(N), RLON(N), X(N), Y(N), Z(N)
C
C   This subroutine transforms spherical coordinates into
C Cartesian coordinates on the unit sphere.
C
C On input:
C
C       N = Number of nodes (points on the unit sphere)
C           whose coordinates are to be transformed.
C
C       RLAT = Array of length N containing latitudinal
C              coordinates of the nodes in degrees.
C
C       RLON = Array of length N containing longitudinal
C              coordinates of the nodes in degrees.
C
C The above parameters are not altered by this routine.
C
C       X,Y,Z = Arrays of length at least N.
C
C On output:
C
C       X,Y,Z = Cartesian coordinates in the range -1 to 1.
C               X(I)**2 + Y(I)**2 + Z(I)**2 = 1 for I = 1
C               to N.
C
C Modules required by CSS2C:  None
C
C Intrinsic functions called by CSS2C:  SIN, COS
C
C***********************************************************
C
      PARAMETER (D2R=0.017453293D0)
      INTEGER I, NN
      DOUBLE PRECISION RLATD, RLOND, COSPHI, PHI, THETA
C
C Local parameters:
C
C COSPHI = cos(PHI)
C I =      DO-loop index
C NN =     Local copy of N
C PHI =    Latitude
C THETA =  Longitude
C
      NN = N
      DO 1 I = 1,NN
        RLATD = D2R*RLAT(I)
        RLOND = D2R*RLON(I)
        PHI = RLATD
        THETA = RLOND
        COSPHI = COS(PHI)
        X(I) = COSPHI*COS(THETA)
        Y(I) = COSPHI*SIN(THETA)
        Z(I) = SIN(PHI)
    1   CONTINUE
      RETURN
      END

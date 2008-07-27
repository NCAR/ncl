C
C	$Id: cstransd.f,v 1.4 2008-07-27 03:10:08 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CSTRANSD (N,RLAT,RLON, X,Y,Z)
      INTEGER N
      DOUBLE PRECISION RLAT(N), RLON(N), X(N), Y(N), Z(N)
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   04/08/90
C
C   This subroutine transforms spherical coordinates into
C Cartesian coordinates on the unit sphere for input to
C Subroutine CSTRMESH.  Storage for X and Y may coincide with
C storage for RLAT and RLON if the latter need not be saved.
C
C
C On input:
C
C       N = Number of nodes (points on the unit sphere)
C           whose coordinates are to be transformed.
C
C       RLAT = Array of length N containing latitudinal
C              coordinates of the nodes in radians.
C
C       RLON = Array of length N containing longitudinal
C              coordinates of the nodes in radians.
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
C Modules required by CSTRANS:  None
C
C Intrinsic functions called by CSTRANS:  COS, SIN
C
C***********************************************************
C
      INTEGER I, NN
      DOUBLE PRECISION COSPHI, PHI, THETA
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
        PHI = RLAT(I)
        THETA = RLON(I)
        COSPHI = COS(PHI)
        X(I) = COSPHI*COS(THETA)
        Y(I) = COSPHI*SIN(THETA)
        Z(I) = SIN(PHI)
    1   CONTINUE
      RETURN
      END

C
C	$Id: cstransd.f,v 1.2 2000-07-13 02:49:16 haley Exp $
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

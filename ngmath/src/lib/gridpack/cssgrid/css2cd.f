C
C	$Id: css2cd.f,v 1.3 2000-08-22 15:19:17 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published
C by the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
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

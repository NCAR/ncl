C
C	$Id: csleft.f,v 1.3 2000-07-13 02:49:11 haley Exp $
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
      LOGICAL FUNCTION CSLEFT (X1,Y1,Z1,X2,Y2,Z2,X0,Y0,Z0)
      DOUBLE PRECISION X1, Y1, Z1, X2, Y2, Z2, X0, Y0, Z0
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/15/96
C
C   This function determines whether node N0 is in the
C (closed) left hemisphere defined by the plane containing
C N1, N2, and the origin, where left is defined relative to
C an observer at N1 facing N2.
C
C
C On input:
C
C       X1,Y1,Z1 = Coordinates of N1.
C
C       X2,Y2,Z2 = Coordinates of N2.
C
C       X0,Y0,Z0 = Coordinates of N0.
C
C Input parameters are not altered by this function.
C
C On output:
C
C       CSLEFT = TRUE if and only if N0 is in the closed
C              left hemisphere.
C
C Modules required by CSLEFT:  None
C
C***********************************************************
C
C CSLEFT = TRUE iff <N0,N1 X N2> = det(N0,N1,N2) .GE. 0.
C
      CSLEFT = X0*(Y1*Z2-Y2*Z1) - Y0*(X1*Z2-X2*Z1) +
     .       Z0*(X1*Y2-X2*Y1) .GE. 0.D0
      RETURN
      END

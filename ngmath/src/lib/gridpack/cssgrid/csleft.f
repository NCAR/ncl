C
C	$Id: csleft.f,v 1.5 2008-07-27 03:10:07 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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

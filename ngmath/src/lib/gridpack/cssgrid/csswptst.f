C
C	$Id: csswptst.f,v 1.5 2008-07-27 03:10:08 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      LOGICAL FUNCTION CSSWPTST (N1,N2,N3,N4,X,Y,Z)
      INTEGER N1, N2, N3, N4
      DOUBLE PRECISION X(*), Y(*), Z(*)
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   03/29/91
C
C   This function decides whether or not to replace a
C diagonal arc in a quadrilateral with the other diagonal.
C The decision will be to swap (CSSWPTST = TRUE) if and only
C if N4 lies above the plane (in the half-space not contain-
C ing the origin) defined by (N1,N2,N3), or equivalently, if
C the projection of N4 onto this plane is interior to the
C circumcircle of (N1,N2,N3).  The decision will be for no
C swap if the quadrilateral is not strictly convex.
C
C
C On input:
C
C       N1,N2,N3,N4 = Indexes of the four nodes defining the
C                     quadrilateral with N1 adjacent to N2,
C                     and (N1,N2,N3) in counterclockwise
C                     order.  The arc connecting N1 to N2
C                     should be replaced by an arc connec-
C                     ting N3 to N4 if CSSWPTST = TRUE.  Refer
C                     to Subroutine CSSWAP.
C
C       X,Y,Z = Arrays of length N containing the Cartesian
C               coordinates of the nodes.  (X(I),Y(I),Z(I))
C               define node I for I = N1, N2, N3, and N4.
C
C Input parameters are not altered by this routine.
C
C On output:
C
C       CSSWPTST = TRUE if and only if the arc connecting N1
C                and N2 should be swapped for an arc con-
C                necting N3 and N4.
C
C Modules required by CSSWPTST:  None
C
C***********************************************************
C
      DOUBLE PRECISION DX1, DX2, DX3, DY1, DY2, DY3, DZ1,
     .                 DZ2, DZ3, X4, Y4, Z4
C
C Local parameters:
C
C DX1,DY1,DZ1 = Coordinates of N4->N1
C DX2,DY2,DZ2 = Coordinates of N4->N2
C DX3,DY3,DZ3 = Coordinates of N4->N3
C X4,Y4,Z4 =    Coordinates of N4
C
      X4 = X(N4)
      Y4 = Y(N4)
      Z4 = Z(N4)
      DX1 = X(N1) - X4
      DX2 = X(N2) - X4
      DX3 = X(N3) - X4
      DY1 = Y(N1) - Y4
      DY2 = Y(N2) - Y4
      DY3 = Y(N3) - Y4
      DZ1 = Z(N1) - Z4
      DZ2 = Z(N2) - Z4
      DZ3 = Z(N3) - Z4
C
C N4 lies above the plane of (N1,N2,N3) iff N3 lies above
C   the plane of (N2,N1,N4) iff Det(N3-N4,N2-N4,N1-N4) =
C   (N3-N4,N2-N4 X N1-N4) > 0.
C
      CSSWPTST = DX3*(DY2*DZ1 - DY1*DZ2)
     .        -DY3*(DX2*DZ1 - DX1*DZ2)
     .        +DZ3*(DX2*DY1 - DX1*DY2) .GT. 0.D0
      RETURN
      END

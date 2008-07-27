C
C	$Id: csaplyr.f,v 1.5 2008-07-27 03:10:05 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CSAPLYR (X,Y,Z,CX,SX,CY,SY, XP,YP,ZP)
      DOUBLE PRECISION X, Y, Z, CX, SX, CY, SY, XP, YP, ZP
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
C   This subroutine applies the rotation R defined by Sub-
C routine CSCONSTR to the unit vector (X Y Z)**T, i,e. (X,Y,Z)
C is rotated to (XP,YP,ZP).  If (XP,YP,ZP) lies in the
C southern hemisphere (ZP < 0), (XP,YP) are set to the
C coordinates of the nearest point of the equator, ZP re-
C maining unchanged.
C
C On input:
C
C       X,Y,Z = Coordinates of a point on the unit sphere.
C
C       CX,SX,CY,SY = Elements of the rotation defined by
C                     Subroutine CSCONSTR.
C
C Input parameters are not altered except as noted below.
C
C On output:
C
C       XP,YP,ZP = Coordinates of the rotated point on the
C                  sphere unless ZP < 0, in which case
C                  (XP,YP,0) is the closest point of the
C                  equator to the rotated point.  Storage
C                  for XP, YP, and ZP may coincide with
C                  storage for X, Y, and Z, respectively,
C                  if the latter need not be saved.
C
C Modules required by CSAPLYR:  None
C
C Intrinsic function called by CSAPLYR:  SQRT
C
C***********************************************************
C
      DOUBLE PRECISION T
C
C Local parameter:
C
C T = Temporary variable
C
      T = SX*Y + CX*Z
      YP = CX*Y - SX*Z
      ZP = SY*X + CY*T
      XP = CY*X - SY*T
      IF (ZP .GE. 0.) RETURN
C
C Move (XP,YP,ZP) to the equator.
C
      T = SQRT(XP*XP + YP*YP)
      IF (T .EQ. 0.) GO TO 1
      XP = XP/T
      YP = YP/T
      RETURN
C
C Move the south pole to an arbitrary point of the equator.
C
    1 XP = 1.
      YP = 0.
      RETURN
      END

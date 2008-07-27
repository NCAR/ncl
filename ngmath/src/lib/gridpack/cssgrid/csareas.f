C
C	$Id: csareas.f,v 1.5 2008-07-27 03:10:05 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      DOUBLE PRECISION FUNCTION CSAREAS (V1,V2,V3)
      DOUBLE PRECISION V1(3), V2(3), V3(3)
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   06/22/98
C
C   This function returns the area of a spherical triangle
C on the unit sphere.
C
C
C On input:
C
C       V1,V2,V3 = Arrays of length 3 containing the Carte-
C                  sian coordinates of unit vectors (the
C                  three triangle vertices in any order).
C                  These vectors, if nonzero, are implicitly
C                  scaled to have length 1.
C
C Input parameters are not altered by this function.
C
C On output:
C
C       CSAREAS = Area of the spherical triangle defined by
C               V1, V2, and V3 in the range 0 to 2*PI (the
C               area of a hemisphere).  CSAREAS = 0 (or 2*PI)
C               if and only if V1, V2, and V3 lie in (or
C               close to) a plane containing the origin.
C
C Modules required by CSAREAS:  None
C
C Intrinsic functions called by CSAREAS:  ACOS, SQRT
C
C***********************************************************
C
      DOUBLE PRECISION A1, A2, A3, CA1, CA2, CA3, S12, S23,
     .                 S31, U12(3), U23(3), U31(3)
      INTEGER          I
C
C Local parameters:
C
C A1,A2,A3 =    Interior angles of the spherical triangle
C CA1,CA2,CA3 = cos(A1), cos(A2), and cos(A3), respectively
C I =           DO-loop index and index for Uij
C S12,S23,S31 = Sum of squared components of U12, U23, U31
C U12,U23,U31 = Unit normal vectors to the planes defined by
C                 pairs of triangle vertices
C
C
C Compute cross products Uij = Vi X Vj.
C
      U12(1) = V1(2)*V2(3) - V1(3)*V2(2)
      U12(2) = V1(3)*V2(1) - V1(1)*V2(3)
      U12(3) = V1(1)*V2(2) - V1(2)*V2(1)
C
      U23(1) = V2(2)*V3(3) - V2(3)*V3(2)
      U23(2) = V2(3)*V3(1) - V2(1)*V3(3)
      U23(3) = V2(1)*V3(2) - V2(2)*V3(1)
C
      U31(1) = V3(2)*V1(3) - V3(3)*V1(2)
      U31(2) = V3(3)*V1(1) - V3(1)*V1(3)
      U31(3) = V3(1)*V1(2) - V3(2)*V1(1)
C
C Normalize Uij to unit vectors.
C
      S12 = 0.D0
      S23 = 0.D0
      S31 = 0.D0
      DO 2 I = 1,3
        S12 = S12 + U12(I)*U12(I)
        S23 = S23 + U23(I)*U23(I)
        S31 = S31 + U31(I)*U31(I)
    2   CONTINUE
C
C Test for a degenerate triangle associated with collinear
C   vertices.
C
      IF (S12 .EQ. 0.D0  .OR.  S23 .EQ. 0.D0  .OR.
     .    S31 .EQ. 0.D0) THEN
        CSAREAS = 0.D0
        RETURN
      ENDIF
      S12 = SQRT(S12)
      S23 = SQRT(S23)
      S31 = SQRT(S31)
      DO 3 I = 1,3
        U12(I) = U12(I)/S12
        U23(I) = U23(I)/S23
        U31(I) = U31(I)/S31
    3   CONTINUE
C
C Compute interior angles Ai as the dihedral angles between
C   planes:
C           CA1 = cos(A1) = -<U12,U31>
C           CA2 = cos(A2) = -<U23,U12>
C           CA3 = cos(A3) = -<U31,U23>
C
      CA1 = -U12(1)*U31(1)-U12(2)*U31(2)-U12(3)*U31(3)
      CA2 = -U23(1)*U12(1)-U23(2)*U12(2)-U23(3)*U12(3)
      CA3 = -U31(1)*U23(1)-U31(2)*U23(2)-U31(3)*U23(3)
      IF (CA1 .LT. -1.D0) CA1 = -1.D0
      IF (CA1 .GT. 1.D0) CA1 = 1.D0
      IF (CA2 .LT. -1.D0) CA2 = -1.D0
      IF (CA2 .GT. 1.D0) CA2 = 1.D0
      IF (CA3 .LT. -1.D0) CA3 = -1.D0
      IF (CA3 .GT. 1.D0) CA3 = 1.D0
      A1 = ACOS(CA1)
      A2 = ACOS(CA2)
      A3 = ACOS(CA3)
C
C Compute CSAREAS = A1 + A2 + A3 - PI.
C
      CSAREAS = A1 + A2 + A3 - ACOS(-1.D0)
      IF (CSAREAS .LT. 0.D0) CSAREAS = 0.D0
      RETURN
      END

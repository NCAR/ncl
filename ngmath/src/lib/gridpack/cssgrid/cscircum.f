C
C	$Id: cscircum.f,v 1.5 2008-07-27 03:10:06 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CSCIRCUM (V1,V2,V3, C,IER)
      INTEGER IER
      DOUBLE PRECISION V1(3), V2(3), V3(3), C(3)
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   06/29/95
C
C   This subroutine returns the circumcenter of a spherical
C triangle on the unit sphere:  the point on the sphere sur-
C face that is equally distant from the three triangle
C vertices and lies in the same hemisphere, where distance
C is taken to be arc-length on the sphere surface.
C
C
C On input:
C
C       V1,V2,V3 = Arrays of length 3 containing the Carte-
C                  sian coordinates of the three triangle
C                  vertices (unit vectors) in CCW order.
C
C The above parameters are not altered by this routine.
C
C       C = Array of length 3.
C
C On output:
C
C       C = Cartesian coordinates of the circumcenter unless
C           IER > 0, in which case C is not defined.  C =
C           (V2-V1) X (V3-V1) normalized to a unit vector.
C
C       IER = Error indicator:
C             IER = 0 if no errors were encountered.
C             IER = 1 if V1, V2, and V3 lie on a common
C                     line:  (V2-V1) X (V3-V1) = 0.
C             (The vertices are not tested for validity.)
C
C Modules required by CSCIRCUM:  None
C
C Intrinsic function called by CSCIRCUM:  SQRT
C
C***********************************************************
C
      INTEGER I
      DOUBLE PRECISION CNORM, CU(3), E1(3), E2(3)
C
C Local parameters:
C
C CNORM = Norm of CU:  used to compute C
C CU =    Scalar multiple of C:  E1 X E2
C E1,E2 = Edges of the underlying planar triangle:
C           V2-V1 and V3-V1, respectively
C I =     DO-loop index
C
      DO 1 I = 1,3
        E1(I) = V2(I) - V1(I)
        E2(I) = V3(I) - V1(I)
    1   CONTINUE
C
C Compute CU = E1 X E2 and CNORM**2.
C
      CU(1) = E1(2)*E2(3) - E1(3)*E2(2)
      CU(2) = E1(3)*E2(1) - E1(1)*E2(3)
      CU(3) = E1(1)*E2(2) - E1(2)*E2(1)
      CNORM = CU(1)*CU(1) + CU(2)*CU(2) + CU(3)*CU(3)
C
C The vertices lie on a common line if and only if CU is
C   the zero vector.
C
      IF (CNORM .NE. 0.D0) THEN
C
C   No error:  compute C.
C
        CNORM = SQRT(CNORM)
        DO 2 I = 1,3
          C(I) = CU(I)/CNORM
    2     CONTINUE
        IER = 0
      ELSE
C
C   CU = 0.
C
        IER = 1
      ENDIF
      RETURN
      END

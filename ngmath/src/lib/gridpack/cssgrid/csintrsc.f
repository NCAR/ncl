C
C	$Id: csintrsc.f,v 1.5 2008-07-27 03:10:07 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CSINTRSC (P1,P2,CN, P,IER)
      INTEGER IER
      DOUBLE PRECISION P1(3), P2(3), CN(3), P(3)
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/19/90
C
C   Given a great circle C and points P1 and P2 defining an
C arc A on the surface of the unit sphere, where A is the
C shorter of the two portions of the great circle C12 assoc-
C iated with P1 and P2, this subroutine returns the point
C of intersection P between C and C12 that is closer to A.
C Thus, if P1 and P2 lie in opposite hemispheres defined by
C C, P is the point of intersection of C with A.
C
C
C On input:
C
C       P1,P2 = Arrays of length 3 containing the Cartesian
C               coordinates of unit vectors.
C
C       CN = Array of length 3 containing the Cartesian
C            coordinates of a nonzero vector which defines C
C            as the intersection of the plane whose normal
C            is CN with the unit sphere.  Thus, if C is to
C            be the great circle defined by P and Q, CN
C            should be P X Q.
C
C The above parameters are not altered by this routine.
C
C       P = Array of length 3.
C
C On output:
C
C       P = Point of intersection defined above unless IER
C           .NE. 0, in which case P is not altered.
C
C       IER = Error indicator.
C             IER = 0 if no errors were encountered.
C             IER = 1 if <CN,P1> = <CN,P2>.  This occurs
C                     iff P1 = P2 or CN = 0 or there are
C                     two intersection points at the same
C                     distance from A.
C             IER = 2 if P2 = -P1 and the definition of A is
C                     therefore ambiguous.
C
C Modules required by CSINTRSC:  None
C
C Intrinsic function called by CSINTRSC:  SQRT
C
C***********************************************************
C
      INTEGER I
      DOUBLE PRECISION D1, D2, PP(3), PPN, T
C
C Local parameters:
C
C D1 =  <CN,P1>
C D2 =  <CN,P2>
C I =   DO-loop index
C PP =  P1 + T*(P2-P1) = Parametric representation of the
C         line defined by P1 and P2
C PPN = Norm of PP
C T =   D1/(D1-D2) = Parameter value chosen so that PP lies
C         in the plane of C
C
      D1 = CN(1)*P1(1) + CN(2)*P1(2) + CN(3)*P1(3)
      D2 = CN(1)*P2(1) + CN(2)*P2(2) + CN(3)*P2(3)
C
      IF (D1 .EQ. D2) THEN
        IER = 1
        RETURN
      ENDIF
C
C Solve for T such that <PP,CN> = 0 and compute PP and PPN.
C
      T = D1/(D1-D2)
      PPN = 0.D0
      DO 1 I = 1,3
        PP(I) = P1(I) + T*(P2(I)-P1(I))
        PPN = PPN + PP(I)*PP(I)
    1   CONTINUE
C
C PPN = 0 iff PP = 0 iff P2 = -P1 (and T = .5).
C
      IF (PPN .EQ. 0.D0) THEN
        IER = 2
        RETURN
      ENDIF
      PPN = SQRT(PPN)
C
C Compute P = PP/PPN.
C
      DO 2 I = 1,3
        P(I) = PP(I)/PPN
    2   CONTINUE
      IER = 0
      RETURN
      END

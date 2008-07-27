C
C	$Id: csintrc0.f,v 1.6 2008-07-27 03:10:07 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CSINTRC0 (N,PLAT,PLON,X,Y,Z,W,LIST,LPTR,
     .                   LEND, IST, PW,IER)
      INTEGER N, LIST(*), LPTR(*), LEND(N), IST, IER
      DOUBLE PRECISION PLAT, PLON, X(N), Y(N), Z(N), W(N),
     .                 PW
C
C***********************************************************
C
C                                              From SSRFPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   11/30/99
C
C   Given a triangulation of a set of nodes on the unit
C sphere, along with data values at the nodes, this sub-
C routine computes the value at a point P of a continuous
C function which interpolates the data values.  The interp-
C olatory function is linear on each underlying triangle
C (planar triangle with the same vertices as a spherical
C triangle).  If P is not contained in a triangle, an ex-
C trapolated value is taken to be the interpolated value at
C the nearest point of the triangulation boundary.
C
C On input:
C
C       N = Number of nodes in the triangulation.  N .GE. 3.
C
C       PLAT,PLON = Latitude and longitude of P in radians.
C
C       X,Y,Z = Arrays containing Cartesian coordinates of
C               the nodes.
C
C       W = Array containing data values at the nodes.  W(I)
C           is associated with (X(I),Y(I),Z(I)) for I =
C           1,...,N.
C
C       LIST,LPTR,LEND = Data structure defining the trian-
C                        gulation.  Refer to STRIPACK
C                        Subroutine CSTRMESH.
C
C       IST = Index of the starting node in the search for a
C             triangle containing P.  1 .LE. IST .LE. N.
C             The output value of IST from a previous call
C             may be a good choice.
C
C Input parameters other than IST are not altered by this
C   routine.
C
C On output:
C
C       IST = Index of one of the vertices of the triangle
C             containing P (or nearest P) unless IER = -1
C             or IER = -2.
C
C       PW = Value of the interpolatory function at P if
C            IER .GE. 0.
C
C       IER = Error indicator:
C             IER = 0 if interpolation was performed
C                     successfully.
C             IER = 1 if extrapolation was performed
C                     successfully.
C             IER = -1 if N < 3 or IST is outside its valid
C                      range.
C             IER = -2 if the nodes are collinear.
C             IER = -3 if P is not in a triangle and the
C                      angle between P and the nearest boun-
C                      dary point is at least 90 degrees.
C
C STRIPACK modules required by CSINTRC0:  CSJRAND, CSLSTPTR,
C                                         CSSTORE, CSTRFIND
C
C Intrinsic functions called by CSINTRC0:  COS, SIN
C
C***********************************************************
C
      INTEGER I1, I2, I3, LP, N1, N2
      DOUBLE PRECISION B1, B2, B3, P(3), PTN1, PTN2, S12,
     .                 SUM
C
C Local parameters:
C
C B1,B2,B3 = Barycentric coordinates of the central projec-
C              tion of P onto the underlying planar trian-
C              gle, or (B1 and B2) projection of Q onto the
C              underlying line segment N1-N2 when P is
C              exterior.  Unnormalized coordinates are
C              computed by CSTRFIND when P is in a triangle.
C I1,I2,I3 = Vertex indexes returned by CSTRFIND
C LP =       LIST pointer to N1 as a neighbor of N2 or N2
C              as a neighbor of N1
C N1,N2 =    Endpoints of a boundary arc which is visible
C              from P when P is not contained in a triangle
C P =        Cartesian coordinates of P
C PTN1 =     Scalar product (P,N1)
C PTN2 =     Scalar product (P,N2)
C S12 =      Scalar product (N1,N2)
C SUM =      Quantity used to normalize the barycentric
C              coordinates
C
      IF (N .LT. 3  .OR.  IST .LT. 1  .OR.  IST .GT. N)
     .    GO TO 11
C
C Transform (PLAT,PLON) to Cartesian coordinates.
C
      P(1) = COS(PLAT)*COS(PLON)
      P(2) = COS(PLAT)*SIN(PLON)
      P(3) = SIN(PLAT)
C
C Find the vertex indexes of a triangle containing P.
C
      CALL CSTRFIND(IST,P,N,X,Y,Z,LIST,LPTR,LEND, B1,B2,B3,
     .            I1,I2,I3)
      IF (I1 .EQ. 0) GO TO 12
      IST = I1
      IF (I3 .NE. 0) THEN
C
C P is contained in the triangle (I1,I2,I3).  Normalize the
C   barycentric coordinates.
C
        SUM = B1 + B2 + B3
        PW = (B1*W(I1) + B2*W(I2) + B3*W(I3))/SUM
        IER = 0
        RETURN
      ENDIF
C
C P is exterior to the triangulation, and I1 and I2 are
C   boundary nodes which are visible from P.  Set PW to the
C   interpolated value at Q, where Q is the closest boundary
C   point to P.
C
C Traverse the boundary starting from the rightmost visible
C   node I1.
C
      N1 = I1
      PTN1 = P(1)*X(N1) + P(2)*Y(N1) + P(3)*Z(N1)
      IF (I1 .NE. I2) GO TO 2
C
C All boundary nodes are visible from P.  Find a boundary
C   arc N1->N2 such that P Left (N2 X N1)->N1.
C
C Counterclockwise boundary traversal:
C   Set N2 to the first neighbor of N1.
C
    1 LP = LEND(N1)
        LP = LPTR(LP)
        N2 = LIST(LP)
C
C Compute inner products (N1,N2) and (P,N2), and compute
C   B2 = DET(P,N1,N2 X N1).
C
        S12 = X(N1)*X(N2) + Y(N1)*Y(N2) + Z(N1)*Z(N2)
        PTN2 = P(1)*X(N2) + P(2)*Y(N2) + P(3)*Z(N2)
        B2 = PTN2 - S12*PTN1
        IF (B2 .LE. 0.) GO TO 2
C
C P Right (N2 X N1)->N1 -- Iterate.
C
        N1 = N2
        I1 = N1
        PTN1 = PTN2
        GO TO 1
C
C P Left (N2 X N1)->N1, where N2 is the first neighbor of P1.
C   Clockwise boundary traversal:
C
    2 N2 = N1
        PTN2 = PTN1
C
C Set N1 to the last neighbor of N2 and test for
C   termination.
C
        LP = LEND(N2)
        N1 = -LIST(LP)
        IF (N1 .EQ. I1) GO TO 13
C
C Compute inner products (N1,N2) and (P,N1).
C
        S12 = X(N1)*X(N2) + Y(N1)*Y(N2) + Z(N1)*Z(N2)
        PTN1 = P(1)*X(N1) + P(2)*Y(N1) + P(3)*Z(N1)
C
C Compute B2 = DET(P,N1,N2 X N1) = DET(Q,N1,N2 X N1)*(P,Q).
C
        B2 = PTN2 - S12*PTN1
        IF (B2 .LE. 0.) GO TO 2
C
C Compute B1 = DET(P,N2 X N1,N2) = DET(Q,N2 X N1,N2)*(P,Q).
C
      B1 = PTN1 - S12*PTN2
      IF (B1 .LE. 0.) THEN
C
C Q = N2.
C
        PW = W(N2)
      ELSE
C
C P Strictly Left (N2 X N1)->N2 and P Strictly Left
C   N1->(N2 X N1).  Thus Q lies on the interior of N1->N2.
C   Normalize the coordinates and compute PW.
C
        SUM = B1 + B2
        PW = (B1*W(N1) + B2*W(N2))/SUM
      ENDIF
      IER = 1
      RETURN
C
C N or IST is outside its valid range.
C
   11 IER = -1
      RETURN
C
C Collinear nodes.
C
   12 IER = -2
      RETURN
C
C The angular distance between P and the closest boundary
C   point to P is at least 90 degrees.
C
   13 IER = -3
      RETURN
      END

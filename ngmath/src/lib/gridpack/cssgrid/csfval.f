C
C	$Id: csfval.f,v 1.5 2008-07-27 03:10:06 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      DOUBLE PRECISION FUNCTION CSFVAL (B1,B2,B3,V1,V2,V3,F1,
     .                                F2,F3,G1,G2,G3,CSSIG1,
     .                                CSSIG2,SIG3)
      DOUBLE PRECISION B1, B2, B3, V1(3), V2(3), V3(3), F1,
     .                 F2, F3, G1(3), G2(3), G3(3), CSSIG1,
     .                 CSSIG2, SIG3
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
C   Given data values and gradients at the three vertices of
C a spherical triangle containing a point P, this routine
C computes the value of F at P where F interpolates the ver-
C tex data.  Along the triangle sides, the interpolatory
C function F is the Hermite interpolatory tension spline
C defined by the values and tangential gradient components
C at the endpoints, and the gradient component normal to the
C triangle side varies linearly with respect to arc-length
C between the normal gradient components at the endpoints.
C A first-order C-1 blending method is used on the underly-
C ing planar triangle.  Since values and gradients on an arc
C depend only on the vertex data, the method results in C-1
C continuity when used to interpolate over a triangulation.
C
C   The blending method consists of taking F(P) to be a
C weighted sum of the values at PP of three univariate Her-
C mite interpolatory tension splines defined on the line
C segments which join the vertices to the opposite sides and
C pass through PP:  the central projection of P onto the
C underlying planar triangle.  The tension factors for these
C splines are obtained by linear interpolation between the
C pair of tension factors associated with the triangle sides
C which join at the appropriate vertex.
C
C   A tension factor SIGMA associated with a Hermite interp-
C olatory tension spline is a nonnegative parameter which
C determines the curviness of the spline.  SIGMA = 0 results
C in a cubic spline, and the spline approaches the linear
C interpolant as SIGMA increases.
C
C On input:
C
C       B1,B2,B3 = Barycentric coordinates of PP with re-
C                  spect to the (planar) underlying triangle
C                  (V1,V2,V3), where PP is the central
C                  projection of P onto this triangle.
C
C       V1,V2,V3 = Cartesian coordinates of the vertices of
C                  a spherical triangle containing P.  V3
C                  Left V1->V2.
C
C       F1,F2,F3 = Data values associated with the vertices.
C
C       G1,G2,G3 = Gradients associated with the vertices.
C                  Gi is orthogonal to Vi for i = 1,2,3.
C
C       CSSIG1,CSSIG2,SIG3 = Tension factors associated with the
C                        triangle sides opposite V1, V2, and
C                        V3, respectively.
C
C Input parameters are not altered by this function.
C
C On output:
C
C       CSFVAL = Interpolated value at P.
C
C Each vector V above contains X, Y, and Z components in
C   V(1), V(2), and V(3), respectively.
C
C SSRFPACK modules required by CSFVAL:  CSARCINT, CSARCLEN, CSHVAL
C
C Intrinsic function called by CSFVAL:  SQRT
C
C***********************************************************
C
      DOUBLE PRECISION CSHVAL
      INTEGER I
      DOUBLE PRECISION C1, C2, C3, DS, DUM, DV, F, G(3),
     .                 Q1(3), Q2(3), Q3(3), SIG, SUM, S1,
     .                 S2, S3, U1(3), U2(3), U3(3), U1N,
     .                 U2N, U3N, VAL
C
C Local parameters:
C
C C1,C2,C3 =    Coefficients (weight functions) of partial
C                 interpolants.  C1 = 1 on the edge opposite
C                 V1 and C1 = 0 on the other edges.  Simi-
C                 larly for C2 and C3.  C1+C2+C3 = 1.
C DS =          Directional derivative (scaled by distnace)
C                 at U1, U2, or U3:  DS = (G,U1-V1)/U1N =
C                 -(G,V1)/U1N on side opposite V1, where G/
C                 U1N (plus an orthogonal component) is the
C                 projection of G onto the planar triangle
C DUM =         Dummy variable for calls to CSARCINT
C DV =          Directional derivatives (scaled by distance)
C                 at a vertex:  D1 = (G1,U1-V1) = (G1,U1)
C F,G =         Value and gradient at Q1 Q2, or Q3 obtained
C                 by interpolation along one of the arcs of
C                 the spherical triangle
C I =           DO-loop index
C Q1,Q2,Q3 =    Central projections of U1, U2, and U3 onto
C                 the sphere and thus lying on an arc of the
C                 spherical triangle
C SIG =         Tension factor for a side-vertex (partial)
C                 interpolant:  obtained by linear interpo-
C                 lation applied to triangle side tensions
C SUM =         Quantity used to normalize C1, C2, and C3
C S1,S2,S3 =    Sums of pairs of barycentric coordinates:
C                 used to compute U1, U2, U3, and SIG
C U1,U2,U3 =    Points on the boundary of the planar trian-
C                 gle and lying on the lines containing PP
C                 and the vertices.  U1 is opposite V1, etc.
C U1N,U2N,U3N = Quantities used to compute Q1, Q2, and Q3
C                 (magnitudes of U1, U2, and U3)
C VAL =         Local variable used to accumulate the con-
C                 tributions to CSFVAL
C
C
C Compute weight functions C1, C2, and C3.
C
      C1 = B2*B3
      C2 = B3*B1
      C3 = B1*B2
      SUM = C1 + C2 + C3
      IF (SUM .LE. 0.) THEN
C
C P coincides with a vertex.
C
        CSFVAL = B1*F1 + B2*F2 + B3*F3
        RETURN
      ENDIF
C
C Normalize C1, C2, and C3.
C
      C1 = C1/SUM
      C2 = C2/SUM
      C3 = C3/SUM
C
C Compute (S1,S2,S3), (U1,U2,U3) and (U1N,U2N,U3N).
C
      S1 = B2 + B3
      S2 = B3 + B1
      S3 = B1 + B2
      U1N = 0.
      U2N = 0.
      U3N = 0.
      DO 1 I = 1,3
        U1(I) = (B2*V2(I) + B3*V3(I))/S1
        U2(I) = (B3*V3(I) + B1*V1(I))/S2
        U3(I) = (B1*V1(I) + B2*V2(I))/S3
        U1N = U1N + U1(I)*U1(I)
        U2N = U2N + U2(I)*U2(I)
        U3N = U3N + U3(I)*U3(I)
    1   CONTINUE
C
C Compute Q1, Q2, and Q3.
C
      U1N = SQRT(U1N)
      U2N = SQRT(U2N)
      U3N = SQRT(U3N)
      DO 2 I = 1,3
        Q1(I) = U1(I)/U1N
        Q2(I) = U2(I)/U2N
        Q3(I) = U3(I)/U3N
    2   CONTINUE
C
C Compute interpolated value (VAL) at P by looping on
C   triangle sides.
C
      VAL = 0.
C
C Contribution from side opposite V1:
C
C   Compute value and gradient at Q1 by interpolating
C     between V2 and V3.
C
      CALL CSARCINT (Q1,V2,V3,F2,F3,G2,G3,CSSIG1, F,G,DUM)
C
C   Add in the contribution.
C
      DV = G1(1)*U1(1) + G1(2)*U1(2) + G1(3)*U1(3)
      DS = -(G(1)*V1(1) + G(2)*V1(2) + G(3)*V1(3))/U1N
      SIG = (B2*SIG3 + B3*CSSIG2)/S1
      VAL = VAL + C1*CSHVAL(B1,F1,F,DV,DS,SIG)
C
C Contribution from side opposite V2:
C
C   Compute value and gradient at Q2 by interpolating
C     between V3 and V1.
C
      CALL CSARCINT (Q2,V3,V1,F3,F1,G3,G1,CSSIG2, F,G,DUM)
C
C   Add in the contribution.
C
      DV = G2(1)*U2(1) + G2(2)*U2(2) + G2(3)*U2(3)
      DS = -(G(1)*V2(1) + G(2)*V2(2) + G(3)*V2(3))/U2N
      SIG = (B3*CSSIG1 + B1*SIG3)/S2
      VAL = VAL + C2*CSHVAL(B2,F2,F,DV,DS,SIG)
C
C Contribution from side opposite V3:
C
C   Compute interpolated value and gradient at Q3
C     by interpolating between V1 and V2.
C
      CALL CSARCINT (Q3,V1,V2,F1,F2,G1,G2,SIG3, F,G,DUM)
C
C   Add in the final contribution.
C
      DV = G3(1)*U3(1) + G3(2)*U3(2) + G3(3)*U3(3)
      DS = -(G(1)*V3(1) + G(2)*V3(2) + G(3)*V3(3))/U3N
      SIG = (B1*CSSIG2 + B2*CSSIG1)/S3
      CSFVAL = VAL + C3*CSHVAL(B3,F3,F,DV,DS,SIG)
      RETURN
      END

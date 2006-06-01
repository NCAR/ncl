c
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c  .                                                             .
c  .                  copyright (c) 1998 by UCAR                 .
c  .                                                             .
c  .       University Corporation for Atmospheric Research       .
c  .                                                             .
c  .                      all rights reserved                    .
c  .                                                             .
c  .                                                             .
c  .                         SPHEREPACK                          .
c  .                                                             .
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
      SUBROUTINE DIHGEOD(M,IDP,JDP,X,Y,Z)
      DOUBLE PRECISION X
      DOUBLE PRECISION Y
      DOUBLE PRECISION Z
      DOUBLE PRECISION PI
      DOUBLE PRECISION DPHI
      DOUBLE PRECISION BETA
      DOUBLE PRECISION THETA1
      DOUBLE PRECISION THETA2
      DOUBLE PRECISION HDPHI
      DOUBLE PRECISION TDPHI
      DOUBLE PRECISION PHI
      DOUBLE PRECISION X1
      DOUBLE PRECISION Y1
      DOUBLE PRECISION Z1
      DOUBLE PRECISION X2
      DOUBLE PRECISION Y2
      DOUBLE PRECISION Z2
      DOUBLE PRECISION X3
      DOUBLE PRECISION Y3
      DOUBLE PRECISION Z3
      DOUBLE PRECISION DXI
      DOUBLE PRECISION DYI
      DOUBLE PRECISION DZI
      DOUBLE PRECISION DXJ
      DOUBLE PRECISION DYJ
      DOUBLE PRECISION DZJ
      DOUBLE PRECISION XS
      DOUBLE PRECISION YS
      DOUBLE PRECISION ZS
      DOUBLE PRECISION X4
      DOUBLE PRECISION Y4
      DOUBLE PRECISION Z4
      DOUBLE PRECISION X5
      DOUBLE PRECISION Y5
      DOUBLE PRECISION Z5
      DOUBLE PRECISION X6
      DOUBLE PRECISION Y6
      DOUBLE PRECISION Z6
      DOUBLE PRECISION RAD
      DOUBLE PRECISION THETA
      DIMENSION X(IDP,JDP,5),Y(IDP,JDP,5),Z(IDP,JDP,5)
c
c     m         is the number of points on the edge of a
c               single geodesic triangle
c
c     x,y,z     the coordinates of the geodesic points on
c               the sphere are x(i,j,k), y(i,j,k), z(i,j,k)
c               where i=1,...,m+m-1; j=1,...,m; and k=1,...,5.
c               the indices are defined on the unfolded
c               icosahedron as follows for the case m=3
c
c                north pole
c
c                 (5,1)          0      l
c        i     (4,1) (5,2)              a    (repeated for
c           (3,1) (4,2) (5,3)  theta1   t    k=2,3,4,5 in
c        (2,1) (3,2) (4,3)              i        -->
c     (1,1) (2,2) (3,3)        theta2   t    the longitudinal
c        (1,2) (2,3)                    u    direction)
c           (1,3)                pi     d
c      j                                e
c         south pole
c
c                total number of points is 10*(m-1)**2+2
c                total number of triangles is 20*(m-1)**2
c                total number of edges is 30*(m-1)**2
c
      PI = 4.D0*ATAN(1.D0)
      DPHI = .4D0*PI
      BETA = COS(DPHI)
      THETA1 = ACOS(BETA/ (1.D0-BETA))
      THETA2 = PI - THETA1
      HDPHI = DPHI/2.D0
      TDPHI = 3.D0*HDPHI
      DO K = 1,5
          PHI = (K-1)*DPHI
          CALL DSTOC(1.D0,THETA2,PHI,X1,Y1,Z1)
          CALL DSTOC(1.D0,PI,PHI+HDPHI,X2,Y2,Z2)
          CALL DSTOC(1.D0,THETA2,PHI+DPHI,X3,Y3,Z3)
          DXI = (X2-X1)/ (M-1)
          DYI = (Y2-Y1)/ (M-1)
          DZI = (Z2-Z1)/ (M-1)
          DXJ = (X3-X2)/ (M-1)
          DYJ = (Y3-Y2)/ (M-1)
          DZJ = (Z3-Z2)/ (M-1)
          DO I = 1,M
              XS = X1 + (I-1)*DXI
              YS = Y1 + (I-1)*DYI
              ZS = Z1 + (I-1)*DZI
              DO J = 1,I
                  X(J,I,K) = XS + (J-1)*DXJ
                  Y(J,I,K) = YS + (J-1)*DYJ
                  Z(J,I,K) = ZS + (J-1)*DZJ
              END DO
          END DO
          CALL DSTOC(1.D0,THETA1,PHI+HDPHI,X4,Y4,Z4)
          DXI = (X3-X4)/ (M-1)
          DYI = (Y3-Y4)/ (M-1)
          DZI = (Z3-Z4)/ (M-1)
          DXJ = (X4-X1)/ (M-1)
          DYJ = (Y4-Y1)/ (M-1)
          DZJ = (Z4-Z1)/ (M-1)
          DO J = 1,M
              XS = X1 + (J-1)*DXJ
              YS = Y1 + (J-1)*DYJ
              ZS = Z1 + (J-1)*DZJ
              DO I = 1,J
                  X(J,I,K) = XS + (I-1)*DXI
                  Y(J,I,K) = YS + (I-1)*DYI
                  Z(J,I,K) = ZS + (I-1)*DZI
              END DO
          END DO
          CALL DSTOC(1.D0,THETA1,PHI+TDPHI,X5,Y5,Z5)
          DXJ = (X5-X3)/ (M-1)
          DYJ = (Y5-Y3)/ (M-1)
          DZJ = (Z5-Z3)/ (M-1)
          DO I = 1,M
              XS = X4 + (I-1)*DXI
              YS = Y4 + (I-1)*DYI
              ZS = Z4 + (I-1)*DZI
              DO J = 1,I
                  X(J+M-1,I,K) = XS + (J-1)*DXJ
                  Y(J+M-1,I,K) = YS + (J-1)*DYJ
                  Z(J+M-1,I,K) = ZS + (J-1)*DZJ
              END DO
          END DO
          CALL DSTOC(1.D0,0.D0,PHI+DPHI,X6,Y6,Z6)
          DXI = (X5-X6)/ (M-1)
          DYI = (Y5-Y6)/ (M-1)
          DZI = (Z5-Z6)/ (M-1)
          DXJ = (X6-X4)/ (M-1)
          DYJ = (Y6-Y4)/ (M-1)
          DZJ = (Z6-Z4)/ (M-1)
          DO J = 1,M
              XS = X4 + (J-1)*DXJ
              YS = Y4 + (J-1)*DYJ
              ZS = Z4 + (J-1)*DZJ
              DO I = 1,J
                  X(J+M-1,I,K) = XS + (I-1)*DXI
                  Y(J+M-1,I,K) = YS + (I-1)*DYI
                  Z(J+M-1,I,K) = ZS + (I-1)*DZI
              END DO
          END DO
      END DO
      DO K = 1,5
          DO J = 1,M + M - 1
              DO I = 1,M
                  CALL DCTOS(X(J,I,K),Y(J,I,K),Z(J,I,K),RAD,THETA,PHI)
                  CALL DSTOC(1.D0,THETA,PHI,X(J,I,K),Y(J,I,K),Z(J,I,K))
              END DO
          END DO
      END DO
      RETURN
      END
      SUBROUTINE DCTOS(X,Y,Z,R,THETA,PHI)
      DOUBLE PRECISION X
      DOUBLE PRECISION Y
      DOUBLE PRECISION Z
      DOUBLE PRECISION R
      DOUBLE PRECISION THETA
      DOUBLE PRECISION PHI
      DOUBLE PRECISION R1

      R1 = X*X + Y*Y
      IF (R1.NE.0.D0) GO TO 10
      PHI = 0.D0
      THETA = 0.D0
      IF (Z.LT.0.D0) THETA = 4.D0*ATAN(1.D0)
      RETURN
   10 R = SQRT(R1+Z*Z)
      R1 = SQRT(R1)
      PHI = ATAN2(Y,X)
      THETA = ATAN2(R1,Z)
      RETURN
      END
      SUBROUTINE DSTOC(R,THETA,PHI,X,Y,Z)
      DOUBLE PRECISION R
      DOUBLE PRECISION THETA
      DOUBLE PRECISION PHI
      DOUBLE PRECISION X
      DOUBLE PRECISION Y
      DOUBLE PRECISION Z
      DOUBLE PRECISION ST

      ST = SIN(THETA)
      X = R*ST*COS(PHI)
      Y = R*ST*SIN(PHI)
      Z = R*COS(THETA)
      RETURN
      END

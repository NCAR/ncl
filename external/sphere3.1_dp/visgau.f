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
c ... file visgau.f
c
c     contains documentation and code for subroutine visgau
c
      SUBROUTINE DVISGAU(NLAT,NLON,H,LEN,EYER,EYELAT,EYELON,THETA,WK,
     +                  LWK,IWK,LIWK,IERROR)
      DOUBLE PRECISION H
      DOUBLE PRECISION EYER
      DOUBLE PRECISION EYELAT
      DOUBLE PRECISION EYELON
      DOUBLE PRECISION WK
      DOUBLE PRECISION PI
      DOUBLE PRECISION DTR
      DOUBLE PRECISION XEYE
      DOUBLE PRECISION YEYE
      DOUBLE PRECISION ZEYE
c
c     subroutine visgau produces a three dimensional visible rendering
c     of the function h(i,j) which is tabulated on a gauss distributed
c     colatitude grid.
c
c     requires  setgau alfk lfpt gaqd drst dintql dpytha
c               visgau embed intrpg sptcg diag stride
c               trigau vsurf vsurf1 prjct box icvmg projct
c
c     tvisgau uses the ncar graphics package.
c     compile with: ncargf77 (all programs above)
c
c     execute with:  a.out
c
c     on screen display with:  ctrans -d x11 gmeta
c
c     print with:  ctrans -d ps.color gmeta > gmeta.ps
c                  lpr -p(your printer) gmeta.ps
c
c     input parameters
c
c     nlat   the number of gauss colatitudes.
c            if nlat is odd the equator is located at
c            grid point i=(nlat+1)/2. if nlat is even the equator is
c            located half way between points i=nlat/2 and i=nlat/2+1.
c            nlat must be at least 3. note: on the half sphere, the
c            number of grid points in the colatitudinal direction is
c            nlat/2 if nlat is even or (nlat+1)/2 if nlat is odd.
c
c     nlon   the number of distinct londitude points.  nlon determines
c            the grid increment in longitude as 2*pi/nlon. for example
c            nlon = 72 for a five degree grid. nlon must be greater
c            than or equal to 4. the efficiency of the computation is
c            improved when nlon is a product of small prime numbers.
c
c
c     h      a two dimensional array that contains the discrete
c            function to be displayed. h(i,j) is the distance from the
c            center of the sphere to the surface at gauss colatitude
c            point theta(i) and longitude point
c            phi(j) = (j-1)*2*pi/nlon.
c
c     len    the first dimension of the array h as it appears in the
c            program that calls sphere.
c
c     eyer   the distance from the center of the sphere to the eye.
c
c     eyelat the colatitudinal coordinate of the eye (in degrees).
c
c     eyelon the longitudinal  coordinate of the eye (in degrees).
c
c     theta  a double precision  array with nlat gauss colatitudes
c            computed by subroutine gaqd
c
c     wk     a real work array
c
c     lwk    the dimension of the array wk as it appears in the
c            program that calls visgau. lwk must be at least
c                       46*(nlat+2)*(nlon+1).
c
c     iwk    an integer work array
c
c     liwk   the dimension of the array iwk as it appears in the
c            program that calls visgau. liwk must be at least
c                       14*(nlat+2)*(nlon+1).
c
c     ierror = 0    no error
c            = 1    the eye is positioned inside the sphere
c            = 2    lwk  is less than 46*(nlat+2)*(nlon+1)
c            = 3    liwk is less than 14*(nlat+2)*(nlon+1)
c
c
      DIMENSION H(LEN,NLON),WK(*)
      INTEGER IWK(*)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION THETA(NLAT)

      N = NLAT + 2
      M = NLON + 1
      MN = M*N
      IERROR = 2
      IF (LWK.LT.46*MN) RETURN
      IERROR = 3
      IF (LIWK.LT.14*MN) RETURN
      IERROR = 1
      DO 10 J = 1,NLON
          DO 10 I = 1,NLAT
              IF (EYER.LE.H(I,J)) RETURN
   10 CONTINUE
      IERROR = 0
c     ****     set up pointers to sub work arrays in wk
      NTRI = MN + MN
      NW1 = 1
      NW2 = NW1 + MN
      NCLAT = 1
      NSLAT = NCLAT + N
      NXP = 1
      NYP = NXP + MN
      NX1 = 1
      NY1 = NX1 + NTRI
      NZ1 = NY1 + NTRI
      NX2 = NZ1 + NTRI
      NY2 = NX2 + NTRI
      NZ2 = NY2 + NTRI
      NX3 = NZ2 + NTRI
      NY3 = NX3 + NTRI
      NZ3 = NY3 + NTRI
      NX = NZ3 + NTRI
      NY = NX + MN
      NZ = NY + MN
      NWRK = NX
      NITYPE = 1
      NIFLAG = NTRI + 1
      NMST = NIFLAG + MN
      NMFAC = NMST + N
c     **** embed h in a larger array
      CALL DEMBED(NLAT,NLON,H,LEN,WK(NZ1))
c     ****     mid-cell interpolation
      CALL DINTRPG(WK(NZ1),M,N,WK(NW1),WK(NW2),IWK(NIFLAG))
c     ****     transform grid points to cartesian coordinates
      CALL DSPTCG(WK(NZ1),M,N,THETA,WK(NCLAT),WK(NSLAT),WK(NX),WK(NY),
     +           WK(NZ))
c     ****     transform eye position to cartesian coordinates
      PI = 4.D0*ATAN(1.D0)
      DTR = PI/180.D0
      XEYE = EYER*SIN(DTR*EYELAT)
      YEYE = XEYE*SIN(DTR*EYELON)
      XEYE = XEYE*COS(DTR*EYELON)
      ZEYE = EYER*COS(DTR*EYELAT)
c     ****     project grid points
      CALL DPROJCT(M,N,XEYE,YEYE,ZEYE,WK(NX),WK(NY),WK(NZ),WK(NXP),
     +            WK(NYP))
c     ****     check for visibility of cell boundaries
      CALL DDIAG(M,N,WK(NXP),WK(NYP),IWK(NIFLAG))
c     ****     compute longitude stride as a function of latitude
      CALL DSTRIDE(M,N,IWK(NMST),IWK(NMFAC))
c     ****     perform triangulation
      CALL DTRIGAU(M,N,WK(NX),WK(NY),WK(NZ),ITRI,WK(NX1),WK(NY1),
     +             WK(NZ1),WK(NX2),WK(NY2),WK(NZ2),WK(NX3),WK(NY3),
     +             WK(NZ3),IWK(NITYPE),IWK(NIFLAG),IWK(NMST))
c     ****     call surface plotting routine
      CALL DVSURF(XEYE,YEYE,ZEYE,ITRI,WK(NX1),WK(NY1),WK(NZ1),WK(NX2),
     +           WK(NY2),WK(NZ2),WK(NX3),WK(NY3),WK(NZ3),IWK(NITYPE),
     +           WK(NWRK),IWK(NIFLAG))
      RETURN
      END
      SUBROUTINE DEMBED(NLAT,NLON,H,LEN,HG)
      DOUBLE PRECISION H
      DOUBLE PRECISION HG
      DOUBLE PRECISION SUMN
      DOUBLE PRECISION SUMS
      DIMENSION H(LEN,NLON),HG(NLAT+2,NLON+1)

      DO 10 I = 1,NLAT
          DO 10 J = 1,NLON
              HG(I+1,J) = H(I,J)
   10 CONTINUE
      SUMN = 0.D0
      SUMS = 0.D0
      DO 15 J = 1,NLON
          SUMN = SUMN + H(1,J)
          SUMS = SUMS + H(NLAT,J)
   15 CONTINUE
      SUMN = SUMN/NLON
      SUMS = SUMS/NLON
      DO 20 J = 1,NLON
          HG(1,J) = SUMN
          HG(NLAT+2,J) = SUMS
   20 CONTINUE
      DO 25 I = 1,NLAT + 2
          HG(I,NLON+1) = HG(I,1)
   25 CONTINUE
      RETURN
      END
      SUBROUTINE DINTRPG(H,M,N,W1,W2,IFLAG)
      DOUBLE PRECISION H
      DOUBLE PRECISION W1
      DOUBLE PRECISION W2
      DOUBLE PRECISION STEN
c     ****     interpolates to mid points of grid cells using second
c     ****     order formula
      DIMENSION H(N,M),W1(N,M),W2(N,M+2),IFLAG(N,M),STEN(4,4)
      DATA STEN/.015625D0,2*-.078125D0,.015625D0,-.078125D0,2*.390625D0,
     +     2*-.078125D0,2*.390625D0,-.078125D0,.015625D0,2*-.078125D0,
     +     .015625D0/
c     ****     copy h to w2
      MM1 = M - 1
      DO 1 I = 1,MM1
          DO 1 J = 1,N
              W2(J,I+1) = H(J,I)
    1 CONTINUE
c     ****     add periodic points
      DO 2 J = 1,N
          W2(J,1) = W2(J,M)
          W2(J,M+1) = W2(J,2)
          W2(J,M+2) = W2(J,3)
    2 CONTINUE
c     ****     perform interpolation
c     ****     set w1 to zero
      DO 7 I = 1,M
          DO 7 J = 1,N
              W1(J,I) = 0.D0
    7 CONTINUE
c     ****     interpolate
      DO 8 K = 1,4
          DO 8 L = 1,4
              DO 8 I = 1,M - 1
                  DO 8 J = 2,N - 2
                      W1(J,I) = W1(J,I) + W2(J+L-2,I+K-1)*STEN(K,L)
    8 CONTINUE
c     ****     set up iflag array
c     ****     iflag(j,i)=0  if diagonal is (j,i) to (j+1,i+1)
c     ****     iflag(j,i)=16 if diagonal is (j+1,i), (j,i+1)
      DO 9 I = 1,M - 1
          DO 9 J = 2,N - 2
              IFLAG(J,I) = ICVMG(16,0,ABS(.5D0* (W2(J,I+1)+W2(J+1,
     +                     I+2))-W1(J,I))-ABS(.5D0* (W2(J,I+2)+W2(J+1,
     +                     I+1))-W1(J,I)))
    9 CONTINUE
      RETURN
      END
      SUBROUTINE DSPTCG(R,M,N,THETA,CLAT,SLAT,X,Y,Z)
      DOUBLE PRECISION R
      DOUBLE PRECISION CLAT
      DOUBLE PRECISION SLAT
      DOUBLE PRECISION X
      DOUBLE PRECISION Y
      DOUBLE PRECISION Z
      DOUBLE PRECISION PI
      DOUBLE PRECISION DP
      DOUBLE PRECISION THET
      DOUBLE PRECISION CLON
      DOUBLE PRECISION SLON
c     ****     transforms from spherical to cartesian coordinates
      DIMENSION R(N,M),CLAT(N),SLAT(N),X(N,M),Y(N,M),Z(N,M)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION THETA(*)

      PI = 4.D0*ATAN(1.D0)
      DP = (PI+PI)/ (M-1)
      CLAT(1) = 1.D0
      SLAT(1) = 0.D0
      DO 10 J = 2,N - 1
          THET = THETA(J-1)
          CLAT(J) = COS(THET)
          SLAT(J) = SIN(THET)
   10 CONTINUE
      CLAT(N) = -1.D0
      SLAT(N) = 0.D0
      DO 20 I = 1,M - 1
          CLON = COS((I-1)*DP)
          SLON = SIN((I-1)*DP)
          DO 20 J = 1,N
              X(J,I) = R(J,I)*SLAT(J)
              Y(J,I) = X(J,I)*SLON
              X(J,I) = X(J,I)*CLON
              Z(J,I) = R(J,I)*CLAT(J)
   20 CONTINUE
      DO 30 J = 1,N
          X(J,M) = X(J,1)
          Y(J,M) = Y(J,1)
          Z(J,M) = Z(J,1)
   30 CONTINUE
      RETURN
      END
      SUBROUTINE DDIAG(M,N,XP,YP,IFLAG)
      DOUBLE PRECISION XP
      DOUBLE PRECISION YP
      DOUBLE PRECISION CP
c
c     ****     label visibility of cell sides
c
c     north side corresponds to j
c     south side corresponds to j+1
c     west  side corresponds to i
c     east  side corresponds to i+1
c
c     let iflag = b4 b3 b2 b1 b0 (in binary) then b0 through b3 are
c     either o or 1 depeending on whether the east, south, north
c     or west side is either invisible or visible, respectively.
c
c     b4 is o if the diagonal is from (i,j) to (i+1,j+1) and 1 if
c     the diagonal is from (i,j+1) to (i+1,j).
c
      DIMENSION XP(N,M),YP(N,M),IFLAG(N,M)
c     ****     arithmetic statement function
      CP(J1,I1,J2,I2,J3,I3) = ((XP(J1,I1)-XP(J2,I2))*
     +                        (YP(J3,I3)-YP(J2,I2))-
     +                        (XP(J3,I3)-XP(J2,I2))*
     +                        (YP(J1,I1)-YP(J2,I2)))
      DO 100 J = 2,N - 2
          DO 100 I = 1,M - 1
              IF (IFLAG(J,I).GE.16) GO TO 20
              IF (CP(J+1,I+1,J+1,I,J,I).LE.0) GO TO 10
c     west and south are visible
              IFLAG(J,I) = IFLAG(J,I) + 10
   10         IF (CP(J,I,J,I+1,J+1,I+1).LE.0) GO TO 100
c     east and north are visible
              IFLAG(J,I) = IFLAG(J,I) + 5
              GO TO 100
   20         IF (CP(J+1,I,J,I,J,I+1).LE.0) GO TO 30
c     west and north are visible
              IFLAG(J,I) = IFLAG(J,I) + 12
   30         IF (CP(J,I+1,J+1,I+1,J+1,I).LE.0) GO TO 100
c     east and south are visible
              IFLAG(J,I) = IFLAG(J,I) + 3
  100 CONTINUE
c
c     classify the poles
c
      DO 200 I = 1,M - 1
          IFLAG(1,I) = 0
          IF (CP(2,I+1,2,I,1,I).GT.0) IFLAG(1,I) = 15
          IFLAG(N-1,I) = 0
          IF (CP(N,I,N-1,I,N-1,I+1).GT.0) IFLAG(N-1,I) = 31
  200 CONTINUE
      DO 250 J = 1,N - 1
          IFLAG(J,M) = IFLAG(J,1)
  250 CONTINUE
      RETURN
      END
      SUBROUTINE DSTRIDE(M,N,MST,MFAC)
      DOUBLE PRECISION TPHI
      DOUBLE PRECISION PI
      DOUBLE PRECISION DT
      DOUBLE PRECISION THETA
      DOUBLE PRECISION ST
      DIMENSION MFAC(*),MTRYH(3),MST(N),ICL(8)
      DATA MTRYH(1),MTRYH(2),MTRYH(3)/2,3,5/
      DATA ICL(1),ICL(2),ICL(3),ICL(4),ICL(5),ICL(6),ICL(7),ICL(8)/0,1,
     +     2,12,3,13,23,123/
c
c     find prime factors of m-1
c
      ML = M - 1
      NF = 0
      J = 0
  101 J = J + 1
      IF (J-3) 102,102,103
  102 MTRY = MTRYH(J)
      GO TO 104
  103 MTRY = MTRY + 2
  104 MQ = ML/MTRY
      MR = ML - MTRY*MQ
      IF (MR) 101,105,101
  105 NF = NF + 1
      MFAC(NF) = MTRY
      ML = MQ
      IF (ML.NE.1) GO TO 104
      IF (MFAC(NF).GT.2) GO TO 106
      NF = NF - 1
      MFAC(NF) = 4
  106 TPHI = .707D0/DBLE(M-1)
      NS2 = N/2
      MF1 = MFAC(NF)
      MST(1) = (M-1)/MF1
      PI = 4.D0*ATAN(1.D0)
      DT = PI/DBLE(N-1)
      JF = NF - 1
      DO 110 JDO = 2,NS2
          J = JDO
          THETA = (J-1)*DT
          ST = SIN(THETA)
          MF2 = MF1*MFAC(JF)
          IF (ABS(ST/MF1-TPHI).GT.ABS(ST/MF2-TPHI)) GO TO 115
          MST(J) = MST(J-1)
          GO TO 110
  115     MST(J) = (M-1)/MF2
          MF1 = MF2
          JF = JF - 1
          IF (JF.EQ.0) GO TO 120
  110 CONTINUE
  120 DO 125 JDO = J,NS2
          MST(JDO) = 1
  125 CONTINUE
      DO 130 JDO = 1,NS2
          MST(N-JDO) = MST(JDO)
  130 CONTINUE
c      write (6,135) (mst(j),j=1,n)
  135 FORMAT (' colatitude strides',/, (15i5))
c
      RETURN
      END
      SUBROUTINE DTRIGAU(M,N,X,Y,Z,ITRI,X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,
     +                   ITYP,IFLAG,MST)
      DOUBLE PRECISION X
      DOUBLE PRECISION Y
      DOUBLE PRECISION Z
      DOUBLE PRECISION X1
      DOUBLE PRECISION Y1
      DOUBLE PRECISION Z1
      DOUBLE PRECISION X2
      DOUBLE PRECISION Y2
      DOUBLE PRECISION Z2
      DOUBLE PRECISION X3
      DOUBLE PRECISION Y3
      DOUBLE PRECISION Z3
c     ****     performs triangulation
      DIMENSION X(N,M),Y(N,M),Z(N,M),X1(1),Y1(1),Z1(1),X2(1),Y2(1),
     +          Z2(1),X3(1),Y3(1),Z3(1),ITYP(1),IFLAG(N,M),MST(N),ICL(8)
      DATA ICL(1),ICL(2),ICL(3),ICL(4),ICL(5),ICL(6),ICL(7),ICL(8)/0,1,
     +     2,12,3,13,23,123/

      ITRI = 0
      N1 = 2
      N2 = N - 2
      DO 100 J = N1,N2
          DO 100 I = 1,M - 1
              IF (IFLAG(J,I).GE.16) GO TO 50
              IF (MOD(IFLAG(J,I),16).LT.8) GO TO 70
              ITRI = ITRI + 1
              X1(ITRI) = X(J,I)
              Y1(ITRI) = Y(J,I)
              Z1(ITRI) = Z(J,I)
              X2(ITRI) = X(J+1,I)
              Y2(ITRI) = Y(J+1,I)
              Z2(ITRI) = Z(J+1,I)
              X3(ITRI) = X(J+1,I+1)
              Y3(ITRI) = Y(J+1,I+1)
              Z3(ITRI) = Z(J+1,I+1)
              ITYPH = 3
              IF (MOD(I-1,MST(J)).EQ.0) GO TO 60
              IF (MOD(IFLAG(J,I-1),2).EQ.0) GO TO 60
              ITYPH = ITYPH - 1
   60         IF (MOD(IFLAG(J,I),2).EQ.0) ITYPH = ITYPH + 4
              ITYP(ITRI) = ICL(ITYPH+1)
   70         IF (MOD(IFLAG(J,I),2).EQ.0) GO TO 100
              ITRI = ITRI + 1
              X1(ITRI) = X(J,I)
              Y1(ITRI) = Y(J,I)
              Z1(ITRI) = Z(J,I)
              X2(ITRI) = X(J+1,I+1)
              Y2(ITRI) = Y(J+1,I+1)
              Z2(ITRI) = Z(J+1,I+1)
              X3(ITRI) = X(J,I+1)
              Y3(ITRI) = Y(J,I+1)
              Z3(ITRI) = Z(J,I+1)
              ITYPH = 0
              IF (MOD(IFLAG(J,I),16).LT.8) ITYPH = ITYPH + 1
              IF (MOD(IFLAG(J,I+1),16).LT.8) ITYPH = ITYPH + 2
              IF (MOD(IFLAG(J-1,I),4).LT.2) ITYPH = ITYPH + 4
              ITYP(ITRI) = ICL(ITYPH+1)
              GO TO 100
   50         IF (MOD(IFLAG(J,I),16).LT.8) GO TO 20
              ITRI = ITRI + 1
              X1(ITRI) = X(J,I)
              Y1(ITRI) = Y(J,I)
              Z1(ITRI) = Z(J,I)
              X2(ITRI) = X(J+1,I)
              Y2(ITRI) = Y(J+1,I)
              Z2(ITRI) = Z(J+1,I)
              X3(ITRI) = X(J,I+1)
              Y3(ITRI) = Y(J,I+1)
              Z3(ITRI) = Z(J,I+1)
              ITYPH = 1
              IF (MOD(I-1,MST(J)).EQ.0) GO TO 10
              IF (MOD(IFLAG(J,I-1),2).EQ.0) GO TO 10
              ITYPH = 0
   10         IF (MOD(IFLAG(J,I),2).EQ.0) ITYPH = ITYPH + 2
              IF (MOD(IFLAG(J-1,I),4).LT.2) ITYPH = ITYPH + 4
              ITYP(ITRI) = ICL(ITYPH+1)
   20         IF (MOD(IFLAG(J,I),2).EQ.0) GO TO 100
              ITRI = ITRI + 1
              X1(ITRI) = X(J+1,I)
              Y1(ITRI) = Y(J+1,I)
              Z1(ITRI) = Z(J+1,I)
              X2(ITRI) = X(J+1,I+1)
              Y2(ITRI) = Y(J+1,I+1)
              Z2(ITRI) = Z(J+1,I+1)
              X3(ITRI) = X(J,I+1)
              Y3(ITRI) = Y(J,I+1)
              Z3(ITRI) = Z(J,I+1)
              ITYPH = 1
              IF (MOD(IFLAG(J,I+1),16).LT.8) ITYPH = ITYPH + 2
              IF (MOD(IFLAG(J,I),16).LT.8) ITYPH = ITYPH + 4
              ITYP(ITRI) = ICL(ITYPH+1)
  100 CONTINUE
c
c     ****     triangles around north and south poles
c
      DO 200 I = 1,M - 1
          IF (MOD(IFLAG(1,I),16).LT.8) GO TO 250
          ITRI = ITRI + 1
          X1(ITRI) = X(1,I)
          Y1(ITRI) = Y(1,I)
          Z1(ITRI) = Z(1,I)
          X2(ITRI) = X(2,I)
          Y2(ITRI) = Y(2,I)
          Z2(ITRI) = Z(2,I)
          X3(ITRI) = X(2,I+1)
          Y3(ITRI) = Y(2,I+1)
          Z3(ITRI) = Z(2,I+1)
          ITYP(ITRI) = ICL(3)
  250     IF (MOD(IFLAG(N-1,I),16).LT.8) GO TO 200
          ITRI = ITRI + 1
          X1(ITRI) = X(N-1,I)
          Y1(ITRI) = Y(N-1,I)
          Z1(ITRI) = Z(N-1,I)
          X2(ITRI) = X(N,I)
          Y2(ITRI) = Y(N,I)
          Z2(ITRI) = Z(N,I)
          X3(ITRI) = X(N-1,I+1)
          Y3(ITRI) = Y(N-1,I+1)
          Z3(ITRI) = Z(N-1,I+1)
          ITYP(ITRI) = ICL(1)
  200 CONTINUE
      RETURN
      END
      SUBROUTINE DVSURF(XEYE,YEYE,ZEYE,NTRI,X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,
     +                 ITYPE,WORK,IWORK)
      DOUBLE PRECISION XEYE
      DOUBLE PRECISION YEYE
      DOUBLE PRECISION ZEYE
      DOUBLE PRECISION X1
      DOUBLE PRECISION Y1
      DOUBLE PRECISION Z1
      DOUBLE PRECISION X2
      DOUBLE PRECISION Y2
      DOUBLE PRECISION Z2
      DOUBLE PRECISION X3
      DOUBLE PRECISION Y3
      DOUBLE PRECISION Z3
      DOUBLE PRECISION WORK
c
c    subroutine vsurf is like subroutine hidel except the triangles
c    are categorized. vsurf is also like solid except triangles rather
c    than lines are covered.
c
c     written by paul n. swarztrauber, national center for atmospheric
c     research, p.o. box 3000, boulder, colorado, 80307
c
c    this program plots visible lines for the surface defined
c    by the input 3-d triangles with corners at (x1,y1,z1), (x2,y2,z2)
c    and (x3,y3,z3). the sides of these these triangles may or
c    may not be plotted depending on itype. if itype is 1 then the
c    side between points (x1,y1,z1) and (x2,y2,z2) is plotted if it
c    is visible. if itype is 2 then the side between (x2,y2,z2)
c    and (x3,y3,z3) is plotted. if itype is 3 then the visible portion
c    of the side between (x3,y3,z3) and (x1,y1,z1) is plotted.
c    any combination is possible by specifying itype to be one
c    of the following values: 0,1,2,3,12,13,23,123.
c
c    the length of real    array  work must be at least 14*ntri
c
c    the length of integer array iwork must be at least  6*ntri
c
c
c    the vertices of the triangles are renumbered by vsurf so that
c    their projections are orientated counterclockwise. the user need
c    only be aware that the vertices may be renumbered by vsurf.
c
      DIMENSION X1(NTRI),Y1(NTRI),Z1(NTRI),X2(NTRI),Y2(NTRI),Z2(NTRI),
     +          X3(NTRI),Y3(NTRI),Z3(NTRI),ITYPE(NTRI),WORK(14*NTRI)
      INTEGER IWORK(6*NTRI)
c
      CALL DVSURF1(XEYE,YEYE,ZEYE,NTRI,X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,
     +             ITYPE,WORK,WORK(NTRI+1),WORK(2*NTRI+1),
     +             WORK(3*NTRI+1),WORK(4*NTRI+1),WORK(5*NTRI+1),
     +             WORK(6*NTRI+1),WORK(7*NTRI+1),WORK(8*NTRI+1),
     +             WORK(9*NTRI+1),WORK(10*NTRI+1),WORK(11*NTRI+1),
     +             WORK(12*NTRI+1),WORK(13*NTRI+1),IWORK,
     +             IWORK(NTRI+1),IWORK(2*NTRI+1),IWORK(4*NTRI+1))
      RETURN
      END
      SUBROUTINE DVSURF1(XEYE,YEYE,ZEYE,NTRI,X1,Y1,Z1,X2,Y2,Z2,X3,Y3,
     +                   Z3,ITYPE,PX1,PY1,PX2,PY2,PX3,PY3,VX1,VY1,VX2,
     +                   VY2,VX3,VY3,TL,TR,KH,NEXT,ISTART,IFINAL)
      DOUBLE PRECISION XEYE
      DOUBLE PRECISION YEYE
      DOUBLE PRECISION ZEYE
      DOUBLE PRECISION X1
      DOUBLE PRECISION Y1
      DOUBLE PRECISION Z1
      DOUBLE PRECISION X2
      DOUBLE PRECISION Y2
      DOUBLE PRECISION Z2
      DOUBLE PRECISION X3
      DOUBLE PRECISION Y3
      DOUBLE PRECISION Z3
      DOUBLE PRECISION PX1
      DOUBLE PRECISION PY1
      DOUBLE PRECISION PX2
      DOUBLE PRECISION PY2
      DOUBLE PRECISION PX3
      DOUBLE PRECISION PY3
      DOUBLE PRECISION VX1
      DOUBLE PRECISION VY1
      DOUBLE PRECISION VX2
      DOUBLE PRECISION VY2
      DOUBLE PRECISION VX3
      DOUBLE PRECISION VY3
      DOUBLE PRECISION TL
      DOUBLE PRECISION TR
      DOUBLE PRECISION FNTRI
      DOUBLE PRECISION X
      DOUBLE PRECISION Y
      DOUBLE PRECISION Z
      DOUBLE PRECISION DUM1
      DOUBLE PRECISION DUM2
      DOUBLE PRECISION CPROD
      DOUBLE PRECISION PX1H
      DOUBLE PRECISION PY1H
      DOUBLE PRECISION X1HOLD
      DOUBLE PRECISION Y1HOLD
      DOUBLE PRECISION Z1HOLD
      DOUBLE PRECISION PMAX
      DOUBLE PRECISION PMIN
      DOUBLE PRECISION XMIN
      DOUBLE PRECISION XMAX
      DOUBLE PRECISION YMIN
      DOUBLE PRECISION YMAX
      DOUBLE PRECISION DMX
      DOUBLE PRECISION DMY
      DOUBLE PRECISION C
      DOUBLE PRECISION D
      DOUBLE PRECISION XMID
      DOUBLE PRECISION HDY
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION YMID
      DOUBLE PRECISION HDX
      DOUBLE PRECISION HGR
      DOUBLE PRECISION DXT
      DOUBLE PRECISION HR
      DOUBLE PRECISION TL1
      DOUBLE PRECISION TL2
      DOUBLE PRECISION PX4
      DOUBLE PRECISION PY4
      DOUBLE PRECISION PX5
      DOUBLE PRECISION PY5
      DOUBLE PRECISION X4
      DOUBLE PRECISION Y4
      DOUBLE PRECISION Z4
      DOUBLE PRECISION X5
      DOUBLE PRECISION Y5
      DOUBLE PRECISION Z5
      DOUBLE PRECISION X54
      DOUBLE PRECISION Y54
      DOUBLE PRECISION C17
      DOUBLE PRECISION C27
      DOUBLE PRECISION C37
      DOUBLE PRECISION C14
      DOUBLE PRECISION C25
      DOUBLE PRECISION C36
      DOUBLE PRECISION TMIN
      DOUBLE PRECISION TMAX
      DOUBLE PRECISION XPL
      DOUBLE PRECISION YPL
      DOUBLE PRECISION ZPL
      DOUBLE PRECISION XPR
      DOUBLE PRECISION YPR
      DOUBLE PRECISION ZPR
      DOUBLE PRECISION VX1T
      DOUBLE PRECISION VY1T
      DOUBLE PRECISION VZ1T
      DOUBLE PRECISION VX2T
      DOUBLE PRECISION VY2T
      DOUBLE PRECISION VZ2T
      DOUBLE PRECISION APL
      DOUBLE PRECISION BPL
      DOUBLE PRECISION CPL
      DOUBLE PRECISION DPL
      DOUBLE PRECISION VX3T
      DOUBLE PRECISION VY3T
      DOUBLE PRECISION VZ3T
      DOUBLE PRECISION DEN
      DOUBLE PRECISION TIL
      DOUBLE PRECISION TIR
      DOUBLE PRECISION TIM
      DOUBLE PRECISION THOLD
      DOUBLE PRECISION TLH
      DOUBLE PRECISION TRH
      DOUBLE PRECISION XA
      DOUBLE PRECISION YA
      DOUBLE PRECISION XB
      DOUBLE PRECISION YB
c
      DIMENSION X1(NTRI),Y1(NTRI),Z1(NTRI),X2(NTRI),Y2(NTRI),Z2(NTRI),
     +          X3(NTRI),Y3(NTRI),Z3(NTRI),ITYPE(NTRI),PX1(NTRI),
     +          PY1(NTRI),PX2(NTRI),PY2(NTRI),PX3(NTRI),PY3(NTRI),
     +          VX1(NTRI),VY1(NTRI),VX2(NTRI),VY2(NTRI),VX3(NTRI),
     +          VY3(NTRI),TL(NTRI),TR(NTRI),NEXT(NTRI),KH(NTRI),
     +          ISTART(2*NTRI),IFINAL(2*NTRI),LTP(3),IRD(11),IP2(11),
     +          NCT(11),NCV(11),LAST(11)
c
      DOUBLE PRECISION L2E
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION LE2
C*PT*WARNING* Constant already double-precision
c
c     compute projections of 3-d points
c
      LE2 = .6931471805599453094172321d0
C*PT*WARNING* Constant already double-precision
      L2E = 1.d0/LE2
      FNTRI = NTRI
      IRMAX = .5D0*L2E*LOG(FNTRI)
      IRMAX = MIN(IRMAX,10)
      IRMP1 = IRMAX + 1
      DO 4 ICV = 1,11
          NCV(ICV) = 0
    4 CONTINUE
      NCT(1) = 0
      IP2(1) = 1
      IRD(1) = 0
      ISIZE = 4
      DO 7 IRP1 = 2,IRMP1
          IR = IRP1 - 1
          NCT(IRP1) = 0
          IP2(IRP1) = 2**IR
          IRD(IRP1) = IRD(IR) + ISIZE
          ISIZE = (IP2(IRP1)+1)**2
    7 CONTINUE
      ISXM = IRD(IRMP1) + ISIZE + 1
      DO 8 ISX = 1,ISXM
          ISTART(ISX) = 0
          IFINAL(ISX) = 0
    8 CONTINUE
      DO 6 I = 1,NTRI
          NEXT(I) = 0
    6 CONTINUE
      CALL DPRJCT(0,XEYE,YEYE,ZEYE,X,Y,Z,DUM1,DUM2)
c      write(6,127) ntri
  127 FORMAT (' ntri in hidel',i5)
      DO 86 K = 1,NTRI
          CALL DPRJCT(1,XEYE,YEYE,ZEYE,X1(K),Y1(K),Z1(K),PX1(K),PY1(K))
          CALL DPRJCT(1,XEYE,YEYE,ZEYE,X2(K),Y2(K),Z2(K),PX2(K),PY2(K))
          CALL DPRJCT(1,XEYE,YEYE,ZEYE,X3(K),Y3(K),Z3(K),PX3(K),PY3(K))
          IF (K.LT.3) THEN
c          write(6,333) xeye,yeye,zeye,x1(k),y1(k),z1(k),px1(k),py1(k)
  333         FORMAT (' xeye, etc.',8D8.1)
          END IF
   86 CONTINUE
c
c     orientate triangles counter clockwise
c
      DO 70 K = 1,NTRI
          CPROD = (PX2(K)-PX1(K))* (PY3(K)-PY1(K)) -
     +            (PY2(K)-PY1(K))* (PX3(K)-PX1(K))
c      if(cprod.eq.0.) write(6,79) k,px1(k),px2(k),px3(k),
c     -                              py1(k),py2(k),py3(k)
   79     FORMAT ('  cprod=0 at k=',i5,6D9.2)
          IF (CPROD.GE.0.D0) GO TO 70
          PX1H = PX1(K)
          PY1H = PY1(K)
          PX1(K) = PX2(K)
          PY1(K) = PY2(K)
          PX2(K) = PX1H
          PY2(K) = PY1H
          X1HOLD = X1(K)
          Y1HOLD = Y1(K)
          Z1HOLD = Z1(K)
          X1(K) = X2(K)
          Y1(K) = Y2(K)
          Z1(K) = Z2(K)
          X2(K) = X1HOLD
          Y2(K) = Y1HOLD
          Z2(K) = Z1HOLD
          ITYP = ITYPE(K)
          IF (ITYP.EQ.2) ITYPE(K) = 3
          IF (ITYP.EQ.3) ITYPE(K) = 2
          IF (ITYP.EQ.12) ITYPE(K) = 13
          IF (ITYP.EQ.13) ITYPE(K) = 12
   70 CONTINUE
c
c     set screen limits
c
      PMAX = PX1(1)
      PMIN = PX1(1)
      DO 87 K = 1,NTRI
          PMIN = DMIN1(PMIN,PX1(K),PY1(K),PX2(K),PY2(K),PX3(K),PY3(K))
          PMAX = DMAX1(PMAX,PX1(K),PY1(K),PX2(K),PY2(K),PX3(K),PY3(K))
   87 CONTINUE
      PMIN = 1.1D0*PMIN
      PMAX = 1.1D0*PMAX
      CALL SET(0.D0,1.D0,0.D0,1.D0,PMIN,PMAX,PMIN,PMAX,1)
      XMIN = DMIN1(PX1(1),PX2(1),PX3(1))
      XMAX = DMAX1(PX1(1),PX2(1),PX3(1))
      YMIN = DMIN1(PY1(1),PY2(1),PY3(1))
      YMAX = DMAX1(PY1(1),PY2(1),PY3(1))
      DO 1 I = 2,NTRI
          XMIN = DMIN1(XMIN,PX1(I),PX2(I),PX3(I))
          XMAX = DMAX1(XMAX,PX1(I),PX2(I),PX3(I))
          YMIN = DMIN1(YMIN,PY1(I),PY2(I),PY3(I))
          YMAX = DMAX1(YMAX,PY1(I),PY2(I),PY3(I))
    1 CONTINUE
      DMX = XMAX - XMIN
      DMY = YMAX - YMIN
      IF (DMX.GT.DMY) GO TO 2
      C = YMIN
      D = YMAX
      XMID = .5D0* (XMIN+XMAX)
      HDY = .5D0*DMY
      A = XMID - HDY
      B = XMID + HDY
      GO TO 3
    2 A = XMIN
      B = XMAX
      YMID = .5D0* (YMIN+YMAX)
      HDX = .5D0*DMX
      C = YMID - HDX
      D = YMID + HDX
    3 HGR = B - A
c
c     categorize triangles
c
      DO 100 I = 1,NTRI
          XMIN = DMIN1(PX1(I),PX2(I),PX3(I))
          XMAX = DMAX1(PX1(I),PX2(I),PX3(I))
          YMIN = DMIN1(PY1(I),PY2(I),PY3(I))
          YMAX = DMAX1(PY1(I),PY2(I),PY3(I))
          DXT = DMAX1(XMAX-XMIN,YMAX-YMIN)
          IF (DXT.GT.0.D0) GO TO 10
          IR = IRMAX
          GO TO 20
   10     IR = L2E*LOG(HGR/DXT)
          IR = MIN(IR,IRMAX)
   20     IRP1 = IR + 1
          NCT(IRP1) = NCT(IRP1) + 1
          HR = HGR/IP2(IRP1)
          XMID = .5D0* (XMIN+XMAX)
          ID = (XMID-A)/HR + 1.5D0
          YMID = .5D0* (YMIN+YMAX)
          JD = (YMID-C)/HR + 1.5D0
          IJD = IP2(IRP1) + 1
          ISX = ID + (JD-1)*IJD + IRD(IRP1)
          IFX = IFINAL(ISX)
          IF (IFX.GT.0) GO TO 50
          ISTART(ISX) = I
          GO TO 60
   50     NEXT(IFX) = I
   60     IFINAL(ISX) = I
  100 CONTINUE
c      write(6,106) tcat,(irp1,nct(irp1),irp1=1,irmp1)
  106 FORMAT (' time to categorize   ',D15.6,/, (' ir+1',i3,' ntri',i7))
c
c     sort triangles into boxes
c
      L = 0
      DO 30 IRP1 = 1,IRMP1
          IF (NCT(IRP1).EQ.0) GO TO 30
          IST = IRD(IRP1) + 1
          ISD = IP2(IRP1) + 1
          CALL DBOX(ISD,ISTART(IST),NEXT,L,IFINAL)
          LAST(IRP1) = L + 1
   30 CONTINUE
      DO 35 IRP1 = 1,IRMP1
          IL = IRD(IRP1) + (IP2(IRP1)+1)**2 + 1
          IF (ISTART(IL).EQ.0) ISTART(IL) = LAST(IRP1)
   35 CONTINUE
c      write(6,31) tsort,l,ntri
   31 FORMAT (' time to sort  ',D15.6,'   l',i8,'   ntri',i8)
      DO 90 K = 1,NTRI
          VX1(K) = PX2(K) - PX1(K)
          VY1(K) = PY2(K) - PY1(K)
          VX2(K) = PX3(K) - PX2(K)
          VY2(K) = PY3(K) - PY2(K)
          VX3(K) = PX1(K) - PX3(K)
          VY3(K) = PY1(K) - PY3(K)
   90 CONTINUE
      TL1 = 0.D0
      TL2 = 0.D0
      MAXS = 0
      DO 500 IR2 = 1,IRMP1
          IF (NCT(IR2).EQ.0) GO TO 500
          IST = IRD(IR2)
          ISD = IP2(IR2) + 1
          DO 490 J2 = 1,ISD
              DO 480 I2 = 1,ISD
                  IST = IST + 1
                  LS = ISTART(IST)
                  LF = ISTART(IST+1) - 1
                  IF (LF.LT.LS) GO TO 480
c
c     define coverings
c
                  KCV = 0
                  I2M = I2 - 1
                  J2M = J2 - 1
                  DO 300 IR1 = 1,IRMP1
                      IF (NCT(IR1).EQ.0) GO TO 300
                      IF (IR1.GE.IR2) GO TO 260
                      IRDP = 2** (IR2-IR1)
                      I1S = (I2M-1)/IRDP
                      I1F = (I2M+1)/IRDP
                      IF = I2M + 1 - I1F*IRDP
                      IF (IF.GT.0) I1F = I1F + 1
                      J1S = (J2M-1)/IRDP
                      J1F = (J2M+1)/IRDP
                      JF = J2M + 1 - J1F*IRDP
                      IF (JF.GT.0) J1F = J1F + 1
                      GO TO 270
  260                 IRDP = 2** (IR1-IR2)
                      I1S = IRDP* (I2M-1)
                      I1F = IRDP* (I2M+1)
                      J1S = IRDP* (J2M-1)
                      J1F = IRDP* (J2M+1)
  270                 IJD = IP2(IR1) + 1
                      I1S = MAX(I1S+1,1)
                      I1F = MIN(I1F+1,IJD)
                      J1S = MAX(J1S+1,1)
                      J1F = MIN(J1F+1,IJD)
                      IXH = (J1S-2)*IJD + IRD(IR1)
                      IXS = I1S + IXH
                      IXF = I1F + IXH
                      DO 290 J1 = J1S,J1F
                          IXS = IXS + IJD
                          KDS = ISTART(IXS)
                          IXF = IXF + IJD
                          KDF = ISTART(IXF+1) - 1
                          IF (KDF.LT.KDS) GO TO 290
                          DO 280 KD = KDS,KDF
                              KCV = KCV + 1
                              KH(KCV) = IFINAL(KD)
  280                     CONTINUE
  290                 CONTINUE
  300             CONTINUE
                  DO 310 ICV = 1,10
                      IF (KCV.LE.NCV(ICV)) GO TO 310
                      NCV(ICV) = KCV
                      GO TO 320
  310             CONTINUE
c
c
  320             DO 470 LDO = LS,LF
                      L = IFINAL(LDO)
                      ITH = ITYPE(L)
                      IF (ITH.EQ.0) GO TO 470
                      LTP(1) = 0
                      LTP(2) = 0
                      LTP(3) = 0
                      ID1 = ITH/100
                      ITH = ITH - 100*ID1
                      ID2 = ITH/10
                      ID3 = ITH - 10*ID2
                      IF (ID1.NE.0) LTP(ID1) = 1
                      IF (ID2.NE.0) LTP(ID2) = 1
                      IF (ID3.NE.0) LTP(ID3) = 1
c     if((ith.eq.123) .or. (ith.eq.12) .or.(ith.eq.13)) ltp(1) = 1
c     if((ith.eq.123) .or. (ith.eq.23) .or.(ith.eq.12)) ltp(2) = 1
c     if((ith.eq.123) .or. (ith.eq.13) .or.(ith.eq.23)) ltp(3) = 1
                      DO 460 NS = 1,3
                          GO TO (101,102,103) NS
  101                     IF (LTP(NS).EQ.0) GO TO 460
                          PX4 = PX1(L)
                          PY4 = PY1(L)
                          PX5 = PX2(L)
                          PY5 = PY2(L)
                          X4 = X1(L)
                          Y4 = Y1(L)
                          Z4 = Z1(L)
                          X5 = X2(L)
                          Y5 = Y2(L)
                          Z5 = Z2(L)
                          GO TO 105
  102                     IF (LTP(NS).EQ.0) GO TO 460
                          PX4 = PX2(L)
                          PY4 = PY2(L)
                          PX5 = PX3(L)
                          PY5 = PY3(L)
                          X4 = X2(L)
                          Y4 = Y2(L)
                          Z4 = Z2(L)
                          X5 = X3(L)
                          Y5 = Y3(L)
                          Z5 = Z3(L)
                          GO TO 105
  103                     IF (LTP(NS).EQ.0) GO TO 460
                          PX4 = PX1(L)
                          PY4 = PY1(L)
                          PX5 = PX3(L)
                          PY5 = PY3(L)
                          X4 = X1(L)
                          Y4 = Y1(L)
                          Z4 = Z1(L)
                          X5 = X3(L)
                          Y5 = Y3(L)
                          Z5 = Z3(L)
  105                     X54 = PX5 - PX4
                          Y54 = PY5 - PY4
                          NSEG = 0
                          DO 440 KD = 1,KCV
                              K = KH(KD)
                              C17 = VX1(K)*Y54 - VY1(K)*X54
                              C27 = VX2(K)*Y54 - VY2(K)*X54
                              C37 = VX3(K)*Y54 - VY3(K)*X54
                              C14 = VY1(K)* (PX4-PX1(K)) -
     +                              VX1(K)* (PY4-PY1(K))
                              C25 = VY2(K)* (PX4-PX2(K)) -
     +                              VX2(K)* (PY4-PY2(K))
                              C36 = VY3(K)* (PX4-PX3(K)) -
     +                              VX3(K)* (PY4-PY3(K))
                              TMIN = 0.D0
                              TMAX = 1.D0
                              IF (C17) 151,152,153
  151                         TMAX = DMIN1(C14/C17,TMAX)
                              GO TO 154
  152                         IF (C14) 154,440,440
  153                         TMIN = DMAX1(C14/C17,TMIN)
  154                         IF (C27) 155,156,157
  155                         TMAX = DMIN1(C25/C27,TMAX)
                              GO TO 158
  156                         IF (C25) 158,440,440
  157                         TMIN = DMAX1(C25/C27,TMIN)
  158                         IF (C37) 159,160,161
  159                         TMAX = DMIN1(C36/C37,TMAX)
                              GO TO 162
  160                         IF (C36) 162,440,440
  161                         TMIN = DMAX1(C36/C37,TMIN)
  162                         IF (TMAX-TMIN.LT..00001D0) GO TO 440
                              XPL = X4 + TMIN* (X5-X4)
                              YPL = Y4 + TMIN* (Y5-Y4)
                              ZPL = Z4 + TMIN* (Z5-Z4)
                              XPR = X4 + TMAX* (X5-X4)
                              YPR = Y4 + TMAX* (Y5-Y4)
                              ZPR = Z4 + TMAX* (Z5-Z4)
c
c     the projections of line and plane intersect
c     now determine if plane covers line
c
                              VX1T = X2(K) - X1(K)
                              VY1T = Y2(K) - Y1(K)
                              VZ1T = Z2(K) - Z1(K)
                              VX2T = X3(K) - X1(K)
                              VY2T = Y3(K) - Y1(K)
                              VZ2T = Z3(K) - Z1(K)
                              APL = VY1T*VZ2T - VY2T*VZ1T
                              BPL = VX2T*VZ1T - VX1T*VZ2T
                              CPL = VX1T*VY2T - VX2T*VY1T
                              DPL = APL*X1(K) + BPL*Y1(K) + CPL*Z1(K)
                              VX3T = XPL - XEYE
                              VY3T = YPL - YEYE
                              VZ3T = ZPL - ZEYE
                              DEN = APL*VX3T + BPL*VY3T + CPL*VZ3T
                              TIL = 0.D0
                              IF (DEN.EQ.0.D0) GO TO 410
                              TIL = (DPL-APL*XEYE-BPL*YEYE-CPL*ZEYE)/DEN
  410                         VX3T = XPR - XEYE
                              VY3T = YPR - YEYE
                              VZ3T = ZPR - ZEYE
                              DEN = APL*VX3T + BPL*VY3T + CPL*VZ3T
                              TIR = 0.D0
                              IF (DEN.EQ.0.D0) GO TO 412
                              TIR = (DPL-APL*XEYE-BPL*YEYE-CPL*ZEYE)/DEN
  412                         IF (TIL.GE..99999D0 .AND.
     +                            TIR.GE..99999D0) GO TO 440
                              IF (TIL.LT.1.D0 .AND.
     +                            TIR.LT.1.D0) GO TO 164
                              VX3T = XPR - XPL
                              VY3T = YPR - YPL
                              VZ3T = ZPR - ZPL
                              DEN = APL*VX3T + BPL*VY3T + CPL*VZ3T
                              TIM = 0.D0
                              IF (DEN.EQ.0.D0) GO TO 414
                              TIM = (DPL-APL*XPL-BPL*YPL-CPL*ZPL)/DEN
  414                         THOLD = TMIN + TIM* (TMAX-TMIN)
                              IF (TIL.GE.1.D0) GO TO 163
                              TMAX = THOLD
                              GO TO 164
  163                         TMIN = THOLD
  164                         NSEG = NSEG + 1
                              TL(NSEG) = TMIN
                              TR(NSEG) = TMAX
  440                     CONTINUE
                          MAXS = MAX0(MAXS,NSEG)
                          IF (NSEG-1) 171,180,172
  171                     CALL LINE(PX4,PY4,PX5,PY5)
                          GO TO 460
c
c     order the segments according to left end point tl(k)
c
  172                     DO 173 K = 2,NSEG
                              DO 173 I = K,NSEG
                                  IF (TL(K-1).LE.TL(I)) GO TO 173
                                  TLH = TL(K-1)
                                  TRH = TR(K-1)
                                  TL(K-1) = TL(I)
                                  TR(K-1) = TR(I)
                                  TL(I) = TLH
                                  TR(I) = TRH
  173                     CONTINUE
c
c     eliminate segment overlap
c
                          K1 = 1
                          K2 = 1
  174                     K2 = K2 + 1
                          IF (K2.GT.NSEG) GO TO 176
                          IF (TR(K1).LT.TL(K2)) GO TO 175
                          TR(K1) = DMAX1(TR(K1),TR(K2))
                          GO TO 174
  175                     K1 = K1 + 1
                          TL(K1) = TL(K2)
                          TR(K1) = TR(K2)
                          GO TO 174
  176                     NSEG = K1
c
c     plot all segments of the line
c
  180                     DO 181 KS = 1,NSEG
                              KB = NSEG - KS + 1
                              TL(KB+1) = TR(KB)
                              TR(KB) = TL(KB)
  181                     CONTINUE
                          TL(1) = 0.D0
                          TR(NSEG+1) = 1.D0
                          NSEGP = NSEG + 1
                          DO 450 K = 1,NSEGP
                              IF (ABS(TR(K)-TL(K)).LT.
     +                            .000001D0) GO TO 450
                              XA = PX4 + TL(K)* (PX5-PX4)
                              YA = PY4 + TL(K)* (PY5-PY4)
                              XB = PX4 + TR(K)* (PX5-PX4)
                              YB = PY4 + TR(K)* (PY5-PY4)
                              CALL LINE(XA,YA,XB,YB)
  450                     CONTINUE
  460                 CONTINUE
  470             CONTINUE
  480         CONTINUE
  490     CONTINUE
  500 CONTINUE
c      write(6,903) tl1,tl2
  903 FORMAT (' time to cover',D15.6,/,' time to test ',D15.6)
c      write(6,904) maxs
  904 FORMAT (' maximum number of segments',i5)
c      write(6,250) (ncv(icv),icv=1,10)
  250 FORMAT ('  the ten largest coverings',/, (10i5))
      CALL FRAME
      END
      SUBROUTINE DPRJCT(INIT,XEYE,YEYE,ZEYE,X,Y,Z,PX,PY)
      DOUBLE PRECISION XEYE
      DOUBLE PRECISION YEYE
      DOUBLE PRECISION ZEYE
      DOUBLE PRECISION X
      DOUBLE PRECISION Y
      DOUBLE PRECISION Z
      DOUBLE PRECISION PX
      DOUBLE PRECISION PY
      DOUBLE PRECISION RADS1
      DOUBLE PRECISION RADS2
      DOUBLE PRECISION D1
      DOUBLE PRECISION D2
      DOUBLE PRECISION CX1
      DOUBLE PRECISION CY1
      DOUBLE PRECISION CX2
      DOUBLE PRECISION CY2
      DOUBLE PRECISION CZ2
      DOUBLE PRECISION CX3
      DOUBLE PRECISION CY3
      DOUBLE PRECISION CZ3
      DOUBLE PRECISION X1
      DOUBLE PRECISION Y1
      DOUBLE PRECISION Z1
      DOUBLE PRECISION RATIO
c
c     subroutine prjct projects the point x,y,z onto a plane through
c     the origin that is perpendicular to a line between the origin
c     and the eye. the projection is along the line between the eye
c     and the point x,y,z. px and py are the coordinates of the
c     projection in the plane.
c     (version 2 , 12-10-82)
c
      SAVE

      IF (INIT.NE.0) GO TO 1
      RADS1 = XEYE**2 + YEYE**2
      RADS2 = RADS1 + ZEYE**2
      D1 = SQRT(RADS1)
      D2 = SQRT(RADS2)
      CX1 = -YEYE/D1
      CY1 = XEYE/D1
      CX2 = -XEYE*ZEYE/ (D1*D2)
      CY2 = -YEYE*ZEYE/ (D1*D2)
      CZ2 = D1/D2
      CX3 = XEYE/D2
      CY3 = YEYE/D2
      CZ3 = ZEYE/D2
      RETURN
    1 X1 = CX1*X + CY1*Y
      Y1 = CX2*X + CY2*Y + CZ2*Z
      Z1 = CX3*X + CY3*Y + CZ3*Z
      RATIO = D2/ (D2-Z1)
      PX = RATIO*X1
      PY = RATIO*Y1
      RETURN
      END
      SUBROUTINE DBOX(ISD,ISTART,NEXT,L,LIST)
      DIMENSION ISTART(ISD,ISD),NEXT(1),LIST(1)

      DO 30 JD = 1,ISD
          DO 10 ID = 1,ISD
              IDX = ISTART(ID,JD)
              ISTART(ID,JD) = L + 1
              IF (IDX.EQ.0) GO TO 10
   20         L = L + 1
              LIST(L) = IDX
              IF (NEXT(IDX).EQ.0) GO TO 10
              IDX = NEXT(IDX)
              GO TO 20
   10     CONTINUE
   30 CONTINUE
      RETURN
      END
      INTEGER FUNCTION ICVMG(I1,I2,R)
      INTEGER I1,I2
      DOUBLE PRECISION R
c
c     returns i1 if i3.ge.0 and returns i2 if i3.lt.0 .
c
      ICVMG = I1
      IF (R.LT.0.D0) ICVMG = I2
      RETURN
      END
      SUBROUTINE DPROJCT(M,N,XEYE,YEYE,ZEYE,X,Y,Z,PX,PY)
      DOUBLE PRECISION XEYE
      DOUBLE PRECISION YEYE
      DOUBLE PRECISION ZEYE
      DOUBLE PRECISION X
      DOUBLE PRECISION Y
      DOUBLE PRECISION Z
      DOUBLE PRECISION PX
      DOUBLE PRECISION PY
      DOUBLE PRECISION RDUM1
      DOUBLE PRECISION RDUM2
      DOUBLE PRECISION RDUM3
      DOUBLE PRECISION RDUM4
      DOUBLE PRECISION RDUM5
c     ****     projects point (x,y,z) onto plane thru origin and perp
c     ****     to line joining origin and eye
      DIMENSION X(N,M),Y(N,M),Z(N,M),PX(N,M),PY(N,M)

      CALL DPRJCT(0,XEYE,YEYE,ZEYE,RDUM1,RDUM2,RDUM3,RDUM4,RDUM5)
      DO 100 I = 1,M
          DO 100 J = 1,N
              CALL DPRJCT(1,XEYE,YEYE,ZEYE,X(J,I),Y(J,I),Z(J,I),
     +                    PX(J,I),PY(J,I))
  100 CONTINUE
      RETURN
      END

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
c  .                         SPHEREPACK                       .
c  .                                                             .
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c ... visgeo.f
c
c     contains documentation and code for subroutine visgeo
c
      SUBROUTINE DVISGEO(M,IDP,JDP,X,Y,Z,H,EYER,EYELAT,EYELON,WORK,
     +                  LWORK,IWORK,LIWORK,IERROR)
      DOUBLE PRECISION X
      DOUBLE PRECISION Y
      DOUBLE PRECISION Z
      DOUBLE PRECISION H
      DOUBLE PRECISION EYER
      DOUBLE PRECISION EYELAT
      DOUBLE PRECISION EYELON
      DOUBLE PRECISION WORK
c
c     subroutine visgeo will display a function on the sphere
c     as a solid. ie. as a "lumpy" sphere. visgeo calls subroutine
c     vsurf to produce the visible surface rendering. X, Y, and Z
c     are the points on an icosahedral geodesic computed by
c     subroutine geopts available in spherepack.
c
c     requires routines visgeo1 ctos stoc vsurf vsurf1
c                       prjct box
c
c     visgeo uses the ncar graphics package.
c     compile with: ncargf77 (all programs above)
c
c     execute with:  a.out
c
c     on screen display with:  ctrans -d x11 gmeta
c
c     print with:  ctrans -d ps.color gmeta > gmeta.ps
c                  lpr -P(your printer) gmeta.ps
c
c
c     input parameters
c
c     m        the number of points on one edge of the icosahedron
c
c     idp,jdp  the first and second dimensions of the three
c              dimensional arrays x, y, z, and h.
c
c     x,y,z    the coordinates of the geodesic points on
c              the unit sphere computed by subroutine geopts.
c              the indices are defined on the unfolded
c              icosahedron as follows for the case m=3
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
c            total number of vertices is  10*(m-1)**2+2
c            total number of triangles is 20*(m-1)**2
c
c     h      a three dimensional array that contains the discrete
c            function to be displayed. h(i,j,k) is the distance from
c            the center of the sphere to the "lumpy" surface at the
c             point [x(i,j,k),y(i,j,k),z(i,j,k)] on the unit sphere.
c
c     eyer   the distance from the center of the sphere to the eye.
c
c     eyelat the colatitudinal coordinate of the eye (in degrees).
c
c     eyelon the longitudinal  coordinate of the eye (in degrees).
c
c     idp    the first dimension of the array h as it appears in
c            the program that calls visgeo
c
c     jdp    the second dimension of the array h as it appears in
c            the program that calls visgeo
c
c     work   a real work array
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls visgeo. lwork must be at least
c                       480*(m-1)**2.
c
c     iwork  an integer work array
c
c     liwork the dimension of the array iwork as it appears in the
c            program that calls visgeo. liwork must be at least
c                       140*(m-1)**2.
c
c     input parameter
c
c     ierror = 0    no error
c            = 1    h(i,j,k) is less than zero for some i,j,k.
c            = 2    eyer is less than h(i,j,k) for some i,k,k.
c            = 3    lwork  is less than 480*(m-1)**2
c            = 4    liwork is less than 140*(m-1)**2
c
      DIMENSION H(IDP,JDP,5),X(IDP,JDP,5),Y(IDP,JDP,5),Z(IDP,JDP,5),
     +          WORK(*)
      INTEGER IWORK(*)

      MMSQ = (M-1)**2
      IERROR = 3
      IF (LWORK.LT.480*MMSQ) RETURN
      IERROR = 4
      IF (LIWORK.LT.140*MMSQ) RETURN
      DO 10 K = 1,5
          DO 10 J = 1,M
              DO 10 I = 1,M + M - 1
                  IF (H(I,J,K).GE.0.D0) GO TO 15
                  IERROR = 1
                  RETURN
   15             IF (EYER.GT.H(I,J,K)) GO TO 10
                  IERROR = 2
                  RETURN
   10 CONTINUE
      IERROR = 0
      LT = 20* (M-1)**2
      LG = 5*M* (M+M-1)
      I1 = 1
      I2 = I1 + LT
      I3 = I2 + LT
      I4 = I3 + LT
      I5 = I4 + LT
      I6 = I5 + LT
      I7 = I6 + LT
      I8 = I7 + LT
      I9 = I8 + LT
      I10 = I9 + LT
      I11 = I10 + LT
      I12 = I11
      I13 = I12 + LG
      I14 = I13 + LG
      CALL DVISGEO1(M,IDP,JDP,H,EYER,EYELAT,EYELON,X,Y,Z,WORK(I1),
     +             WORK(I2),WORK(I3),WORK(I4),WORK(I5),WORK(I6),
     +             WORK(I7),WORK(I8),WORK(I9),IWORK(1),WORK(I11),
     +             WORK(I12),WORK(I13),WORK(I14),IWORK(LT+1))
      RETURN
      END
      SUBROUTINE DVISGEO1(M,IDP,JDP,H,EYER,EYELAT,EYELON,XI,YI,ZI,X1,
     +                    Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,ITYPE,WORK,X,Y,Z,
     +                    IWORK)
      DOUBLE PRECISION H
      DOUBLE PRECISION EYER
      DOUBLE PRECISION EYELAT
      DOUBLE PRECISION EYELON
      DOUBLE PRECISION XI
      DOUBLE PRECISION YI
      DOUBLE PRECISION ZI
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
      DOUBLE PRECISION X
      DOUBLE PRECISION Y
      DOUBLE PRECISION Z
      DOUBLE PRECISION RAD
      DOUBLE PRECISION THETA
      DOUBLE PRECISION ELAMBDA
      DOUBLE PRECISION PI
      DOUBLE PRECISION DTR
      DOUBLE PRECISION XEYE
      DOUBLE PRECISION YEYE
      DOUBLE PRECISION ZEYE
      DIMENSION H(IDP,JDP,5),XI(IDP,JDP,5),YI(IDP,JDP,5),ZI(IDP,JDP,5),
     +          X1(*),Y1(*),Z1(*),X2(*),Y2(*),Z2(*),X3(*),Y3(*),Z3(*),
     +          ITYPE(*),WORK(*),X(M+M-1,M,5),Y(M+M-1,M,5),Z(M+M-1,M,5)
      INTEGER IWORK(*)
c
c     the * above refers to 20*(m-1)**2 locations which is the
c     number of triangles
c
      DO 10 K = 1,5
          DO 10 J = 1,M
              DO 10 I = 1,M + M - 1
                  CALL DCTOS(XI(I,J,K),YI(I,J,K),ZI(I,J,K),RAD,THETA,
     +                      ELAMBDA)
                  CALL DSTOC(H(I,J,K),THETA,ELAMBDA,X(I,J,K),Y(I,J,K),
     +                      Z(I,J,K))
   10 CONTINUE
      NTRI = 0
      DO 20 K = 1,5
          DO 20 J = 1,M - 1
              DO 20 I = 1,M + M - 2
                  NTRI = NTRI + 1
                  X1(NTRI) = X(I,J,K)
                  Y1(NTRI) = Y(I,J,K)
                  Z1(NTRI) = Z(I,J,K)
                  X2(NTRI) = X(I+1,J+1,K)
                  Y2(NTRI) = Y(I+1,J+1,K)
                  Z2(NTRI) = Z(I+1,J+1,K)
                  X3(NTRI) = X(I+1,J,K)
                  Y3(NTRI) = Y(I+1,J,K)
                  Z3(NTRI) = Z(I+1,J,K)
                  ITYPE(NTRI) = 13
                  NTRI = NTRI + 1
                  X1(NTRI) = X(I,J,K)
                  Y1(NTRI) = Y(I,J,K)
                  Z1(NTRI) = Z(I,J,K)
                  X2(NTRI) = X(I+1,J+1,K)
                  Y2(NTRI) = Y(I+1,J+1,K)
                  Z2(NTRI) = Z(I+1,J+1,K)
                  X3(NTRI) = X(I,J+1,K)
                  Y3(NTRI) = Y(I,J+1,K)
                  Z3(NTRI) = Z(I,J+1,K)
                  ITYPE(NTRI) = 3
   20 CONTINUE
c      write(6,22) ntri
   22 FORMAT (i10)
c      write(6,23) (x1(l2),y1(l2),z1(l2),x2(l2),y2(l2),z2(l2),
c     1             x3(l2),y3(l2),z3(l2),l2=1,ntri)
c 23   format(9f10.7)
c
      PI = 4.D0*ATAN(1.D0)
      DTR = PI/180.D0
      XEYE = EYER*SIN(DTR*EYELAT)
      YEYE = XEYE*SIN(DTR*EYELON)
      XEYE = XEYE*COS(DTR*EYELON)
      ZEYE = EYER*COS(DTR*EYELAT)
      CALL DVSURF(XEYE,YEYE,ZEYE,NTRI,X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,ITYPE,
     +           WORK,IWORK)
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
      IF (D1.NE.0.D0) GO TO 2
      CX1 = 1.D0
      CY1 = 0.D0
      CX2 = 0.D0
      CY2 = 1.D0
      CZ2 = 0.D0
      CX3 = 0.D0
      CY3 = 0.D0
      CZ3 = 1.D0
      RETURN
    2 CX1 = -YEYE/D1
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

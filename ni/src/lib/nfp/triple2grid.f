C NCLFORTSTART
      SUBROUTINE TRIPLE2GRID(KZ,XI,YI,ZI,ZMSG,MX,NY,GX,GY,GRID,DOMAIN,
     +                       STRICT,IER)
      IMPLICIT NONE

c NCL:  grid = triple2grid(xi,yi,zi,gx,gy,option)

      INTEGER MX,NY,KZ,IER
      DOUBLE PRECISION GRID(MX,NY),GX(MX),GY(NY),DOMAIN
      DOUBLE PRECISION XI(KZ),YI(KZ),ZI(KZ),ZMSG
      LOGICAL STRICT
C NCLEND

      INTEGER M,N,K,KOUT,KPTS
      DOUBLE PRECISION X(KZ),Y(KZ),Z(KZ)
      DOUBLE PRECISION GBIG(0:MX+1,0:NY+1),GXBIG(0:MX+1),GYBIG(0:NY+1)
      DOUBLE PRECISION DD,XX,YY,SLPY,SLPX
cdbug real     t0, t1, t2, second
cdbug logical  debug

      IER = 0
cdbug debug = .false.
cdbug t0    = second()
c                     strip out missing data (kpts)
c                     count the number of pts outside the grid (kout)
      KPTS = 0
      KOUT = 0
      DO K = 1,KZ
          IF (ZI(K).NE.ZMSG) THEN
              KPTS = KPTS + 1
              X(KPTS) = XI(K)
              Y(KPTS) = YI(K)
              Z(KPTS) = ZI(K)
              IF (XI(K).LT.GX(1) .OR. XI(K).GT.GX(MX) .OR.
     +            YI(K).LT.GY(1) .OR. YI(K).GT.GY(NY)) KOUT = KOUT + 1
          END IF
      END DO

cdbug if (debug) then
cdbug     print *, "STRIP MSG: kout=",kout,":  telapse=", (second()-t0)
cdbug end if
c                     return if no valid pts
      IF (KPTS.EQ.0) THEN
          DO N = 1,NY
              DO M = 1,MX
                  GRID(M,N) = ZMSG
              END DO
          END DO
          RETURN
      END IF

      IF (DOMAIN.LE.0.D0 .OR. KOUT.EQ.0) THEN
          CALL TRIP2GRD(KPTS,X,Y,Z,ZMSG,MX,NY,GX,GY,GRID,STRICT,IER)
      ELSE
c                          create an oversized (big) grid
c                          allows outliers to influence grid
          DO N = 1,NY
              GYBIG(N) = GY(N)
          END DO

          DO M = 1,MX
              GXBIG(M) = GX(M)
          END DO
c                           domain is arbitrary
          GYBIG(0) = GY(1) - DOMAIN* (GY(2)-GY(1))
          GYBIG(NY+1) = GY(NY) + DOMAIN* (GY(NY)-GY(NY-1))
          GXBIG(0) = GX(1) - DOMAIN* (GX(2)-GX(1))
          GXBIG(MX+1) = GX(MX) + DOMAIN* (GX(MX)-GX(MX-1))

          CALL TRIP2GRD(KPTS,X,Y,Z,ZMSG,MX+2,NY+2,GXBIG,GYBIG,GBIG,
     +                  STRICT,IER)

c                           store interior of gbig in return array
          DO N = 1,NY
              DO M = 1,MX
                  GRID(M,N) = GBIG(M,N)
              END DO
          END DO

      END IF
cdbug if (debug) then
cdbug     print *, "TOTAL TIME    :  telapse=", (second()-t0)
cdbug end if

      RETURN
      END
C ------------
      SUBROUTINE TRIP2GRD(KZ,X,Y,Z,ZMSG,MX,NY,GXOUT,GYOUT,GOUT,STRICT,
     +                    IER)

      IMPLICIT NONE
      INTEGER MX,NY,KZ,IER
      DOUBLE PRECISION GXOUT(MX),GYOUT(NY),GOUT(MX,NY)
      DOUBLE PRECISION X(KZ),Y(KZ),Z(KZ),ZMSG
      LOGICAL STRICT
c                          local
      INTEGER M,N,K,MM,NN,KSUM,KPTS
      DOUBLE PRECISION DOUT(MX,NY),DD,XX,YY,SLPY,SLPX
cdbug real     t0, t1, t2, second
cdbug logical  debug

cdbug debug = .false.
cdbug t0    = second()

      IER = 0
      KPTS = KZ
c                          initialize
      DO N = 1,NY
          DO M = 1,MX
              DOUT(M,N) = 1.D20
              GOUT(M,N) = ZMSG
          END DO
      END DO
c                     exact matches
cdbug t1  = second()

      KSUM = 0
      DO N = 1,NY
          DO M = 1,MX

              DO K = 1,KPTS
                  IF (X(K).EQ.GXOUT(M) .AND. Y(K).EQ.GYOUT(N)) THEN
                      GOUT(M,N) = Z(K)
                      DOUT(M,N) = 0.0D0
                      KSUM = KSUM + 1
                      GO TO 10
                  END IF
              END DO
   10         CONTINUE

          END DO
      END DO

cdbug if (debug) then
cdbug     print *, "EXACT MATCH: ksum=",ksum,":  telapse=",(second()-t1)
cdbug end if

c  Did all kpts input get assigned to the gout?
c  ier = -1 is not really an error; it is just informational
c           meaning that no 'nearest neighbor' stuuf was used.

      IF (KSUM.EQ.KPTS) THEN
          IER = -1
          RETURN
      END IF

cdbug t1  = second()
c                     gout interior:crude and lewd
      KSUM = 0
      DO N = 1,NY
          DO M = 1,MX
c c c     if (dout(m,n).ne.0.0) then

              DO K = 1,KPTS
                  IF ((X(K).GE.GXOUT(M).AND.X(K).LT.GXOUT(M+1)) .AND.
     +                (Y(K).GE.GYOUT(N).AND.Y(K).LT.GYOUT(N+1))) THEN

                      SLPX = (X(K)-GXOUT(M))/ (GXOUT(M+1)-GXOUT(M))
                      SLPY = (Y(K)-GYOUT(N))/ (GYOUT(N+1)-GYOUT(N))
                      XX = M + SLPX
                      YY = N + SLPY
                      IF (STRICT) THEN
c                                     mm,nn =>  nearest grid pt
                          MM = M + NINT(XX)
                          NN = N + NINT(YY)
                          DD = (XX-MM)**2 + (YY-NN)**2
                          IF (DD.LT.DOUT(MM,NN)) THEN
                              KSUM = KSUM + 1
                              GOUT(MM,NN) = Z(K)
                              DOUT(MM,NN) = DD
                          END IF
                      ELSE
c                                    allow value to influence nearby pts
                          DO NN = N,N + 1
                              DO MM = M,M + 1
                                  DD = (XX-MM)**2 + (YY-NN)**2
                                  IF (DD.LT.DOUT(MM,NN)) THEN
                                      KSUM = KSUM + 1
                                      GOUT(MM,NN) = Z(K)
                                      DOUT(MM,NN) = DD
                                  END IF
                              END DO
                          END DO
                      END IF

                  END IF
              END DO

c c c     end if
          END DO
      END DO

cdbug if (debug) then
cdbug     print *, "gout interior :  kpts=",kpts,":  ksum=",ksum
cdbug+                                 ,": telapse=", (second()-t1)
cdbug end if

      RETURN
      END

C NCLFORTSTART
      SUBROUTINE TRIPLE2GRID1(KZ,XI,YI,ZI,ZMSG,MX,NY,GX,GY,GRID,
     +                        DOMAIN,LOOP,METHOD,DISTMX,MX2,NY2,
     +                        X,Y,Z,GBIGX,GBIGY,GBIGXY,IER)
      IMPLICIT NONE

c NCL:  grid = triple2grid(xi,yi,zi,gx,gy,option)

      INTEGER MX,NY,KZ,IER,MX2,NY2, LOOP,METHOD
      DOUBLE PRECISION GRID(MX,NY),GX(MX),GY(NY),DISTMX,DOMAIN
      DOUBLE PRECISION XI(KZ),YI(KZ),ZI(KZ),ZMSG
      DOUBLE PRECISION X(KZ),Y(KZ),Z(KZ)

C C C WRAPIT can not handle semantics like:  0:MX2-1 
C C C DOUBLE PRECISION GXBIG(0:MX2-1),GYBIG(0:NY2-1)
C C C DOUBLE PRECISION GBIG(0:MX2-1,0:NY2-1)
      DOUBLE PRECISION GBIGX(MX2),GBIGY(NY2)
      DOUBLE PRECISION GBIGXY(MX2,NY2)
C NCLEND
      INTEGER M,N,K,KOUT,KPTS, MFLAG,NFLAG
      DOUBLE PRECISION DD,DDEPS,DDCRIT

c c c real     t0, t1, t2, second
      logical  debug

      IER   = 0
      DDEPS = 1D-3
      IF (DISTMX.LE.0.0D0) THEN
          DDCRIT = 1D20
      ELSE
          DDCRIT = DISTMX
      END IF
          
      debug = .false.
c c c t0    = second()
c                     strip out missing data (kpts)
c                     count the number of pts outside the grid (KOUT)
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

c c c if (debug) then
c c c     print *, "STRIP MSG: kout=",kout,":  telapse=", (second()-t0)
c c c end if
c                     return if no valid pts
      IF (KPTS.EQ.0) THEN
          DO N = 1,NY
              DO M = 1,MX
                  GRID(M,N) = ZMSG
              END DO
          END DO
          RETURN
      END IF

c To facilitate faster subscripting, check to see if the grid is equally spaced
c                    equally spaced in x ???
      MFLAG = 1
      DD    = ABS( GX(2)-GX(1) )
      DO M=2,MX-1
         IF (DD.LT.(GX(M+1)-GX(M))-DDEPS   .OR.
     +       DD.GT.(GX(M+1)-GX(M))+DDEPS ) THEN
             MFLAG = 0
             GO TO 10 
         END IF
      END DO

c                    equally spaced in y ???
   10 NFLAG = 1
      DD    = ABS( GY(2)-GY(1) )
      DO N=2,NY-1
         IF (DD.LT.(GY(N+1)-GY(N))-DDEPS   .OR.
     +       DD.GT.(GY(N+1)-GY(N))+DDEPS ) THEN
             NFLAG = 0
             GO TO 20 
         END IF
      END DO
   20 CONTINUE

C  MH Note: 02/01/2017
C
C  There's a serious bug in TRIP2GRD3 that sometimes causes the return 
C  values to be all missing or just flat out wrong. This bug is 
C  elusive; it appears to change in behavior if you run this code
C  in debug mode, or even on different systems (Mac versus Linux).
C  Since TRIP2GRD2 seems to be working for now and is faster, we
C  will have the NCL C driver code set LOOP=0 no matter what.
C
      IF (KOUT.EQ.0) THEN
          IF (LOOP.EQ.0) THEN
              CALL TRIP2GRD2(KPTS,X,Y,Z,ZMSG,MX,NY,GX,GY,GRID
     +                      ,MFLAG,NFLAG,METHOD,DDCRIT,IER)
          ELSE
              CALL TRIP2GRD3(KPTS,X,Y,Z,ZMSG,MX,NY,GX,GY,GRID
     +                      ,MFLAG,NFLAG,METHOD,DDCRIT,IER)
          END IF
      ELSE
c          create an oversized (big) grid
c          allows outliers to influence grid
          DO N = 1,NY
             GBIGY(N+1) = GY(N)
          END DO

          DO M = 1,MX
             GBIGX(M+1) = GX(M)
          END DO
c          domain is arbitrary
          GBIGY(1)   = GY(1)  - DOMAIN*(GY(2)-GY(1))
          GBIGY(NY2) = GY(NY) + DOMAIN*(GY(NY)-GY(NY-1))
          GBIGX(1)   = GX(1)  - DOMAIN*(GX(2)-GX(1))
          GBIGX(MX2) = GX(MX) + DOMAIN*(GX(MX)-GX(MX-1))

C  See MH note above about TRIP2GRD3 and LOOP.
          IF (LOOP.EQ.0) THEN
              CALL TRIP2GRD2(KPTS,X,Y,Z,ZMSG,MX2,NY2,GBIGX,GBIGY
     +                      ,GBIGXY,MFLAG,NFLAG,METHOD,DDCRIT,IER)
          ELSE
              CALL TRIP2GRD3(KPTS,X,Y,Z,ZMSG,MX2,NY2,GBIGX,GBIGY
     +                      ,GBIGXY,MFLAG,NFLAG,METHOD,DDCRIT,IER)
          END IF
c           store interior of gbig in return array
          DO N = 1,NY
            DO M = 1,MX
               GRID(M,N) = GBIGXY(M+1,N+1)
            END DO
          END DO
      END IF

c c c if (debug) then
c c c     print *, "TOTAL TIME    :  telapse=", (second()-t0)
c c c end if

      RETURN
      END
C ------------
      SUBROUTINE TRIP2GRD2(KZ,X,Y,Z,ZMSG,MX,NY,GXOUT,GYOUT,GOUT
     +                     ,MFLAG,NFLAG,METHOD,DDCRIT,IER)

c This sub assigns each "Z" to a grid point.
c It is possible that the number of grid points having values assigned
c .   will be less than the number KZ due to over writing the same grid pt.

      IMPLICIT NONE
      INTEGER MX,NY,KZ,IER, MFLAG,NFLAG,METHOD
      DOUBLE PRECISION GXOUT(MX),GYOUT(NY),GOUT(MX,NY)
      DOUBLE PRECISION X(KZ),Y(KZ),Z(KZ),ZMSG,DDCRIT
c                          local
      INTEGER M,N,K,MM,NN,KSUM,KPTS
      DOUBLE PRECISION DOUT(MX,NY)
      DOUBLE PRECISION DD,XX,YY,SLPY,SLPX,DX,DY,ATMP,YLAT,RE,RAD 

c c c real     t0, t1, t2, second
      logical  debug

      IER   = 0
      debug = .false.
c c c t0    = second()

      RE   = 6371.2200D0 
      RAD  = 4.D0*ATAN(1.0D0)/180.D0

c                     strip out missing data (kpts)
c                     count the number of pts outside the grid (kout)
      IER  = 0
      KPTS = KZ
      DX   = ABS( GXOUT(3)-GXOUT(2) )
      DY   = ABS( GYOUT(3)-GYOUT(2) )

c                          initialize
      DO N = 1,NY
        DO M = 1,MX
           DOUT(M,N) = 1D20
           GOUT(M,N) = ZMSG
        END DO
      END DO

c                     EXACT MATCHES ONLY 
c c c t1  = second()
      KSUM = 0
      DO K = 1,KPTS
        DO N=1,NY
           IF (Y(K).EQ.GYOUT(N)) THEN
               DO M=1,MX
                  IF (X(K).EQ.GXOUT(M)) THEN
                      GOUT(M,N) = Z(K)
                      DOUT(M,N) = 0.0D0
                      KSUM = KSUM + 1
                      GO TO 10
                   END IF
                END DO
            END IF
        END DO
   10   CONTINUE
      END DO

c  Did all kpts input get assigned to the gout?
c  ier = -1 is not really an error; it is just informational
c           meaning that no 'nearest neighbor' stuff was used.

      IF (KSUM.EQ.KPTS) THEN
          IER = -1
          RETURN
      END IF
c                     LOOP OVER THE X/Y/Z POINTS
c                     ASSIGN TO NEARBY GRID POINT
      KSUM = 0
      DO K = 1,KPTS
c                     determine subscripts to nearest grid pt
         NN = -1
         IF (NFLAG.EQ.1) THEN
             NN = INT(((Y(K)-GYOUT(1))/DY))+2  
         ELSE
             DO N=1,NY-1
                IF (Y(K).GE.GYOUT(N) .AND. Y(K).LT.GYOUT(N+1) ) THEN   
                    DY   = Y(K)-GYOUT(N)
                    SLPY = DY/(GYOUT(N+1)-GYOUT(N))
                    YY   = N + SLPY
                    NN   = NINT(YY)
                    GO TO 20
                END IF
             END DO
         END IF

   20    MM = -1
         IF (MFLAG.EQ.1) THEN
             MM = INT(((X(K)-GXOUT(1))/DX))+2
         ELSE
             DO M=1,MX-1
                IF (X(K).GE.GXOUT(M) .AND. X(K).LT.GXOUT(M+1) ) THEN   
                    DX   = X(K)-GXOUT(M)
                    SLPX = DX/(GXOUT(M+1)-GXOUT(M))
                    XX   = M + SLPX
                    MM   = NINT(XX)
                    GO TO 30
                END IF
             END DO
         END IF

  30     IF (MM.GE.1 .AND. MM.LE.MX .AND.
     +       NN.GE.1 .AND. NN.LE.NY) THEN

c                                     mm,nn =>  nearest grid pt
             IF (METHOD.EQ.0) THEN
                 DD = SQRT( SLPX**2 + SLPY**2 )
             ELSE
                 YLAT = Y(K)*RAD
                 ATMP = SIN(YLAT)*SIN(GXOUT(MM)*RAD)
     +                       + COS(YLAT)*COS(GYOUT(NN)*RAD)*
     +                         COS((X(K)-GXOUT(MM))*RAD)
                 ATMP  = MIN(1.D0,MAX(-1.D0,ATMP))
                 DD    = ACOS(ATMP)*RE
             END IF

             IF (DD.LT.DOUT(MM,NN) .AND. DD.LT.DDCRIT) THEN
                 KSUM = KSUM + 1
                 GOUT(MM,NN) = Z(K)
                 DOUT(MM,NN) = DD
             END IF
         END IF
   
      END DO

c     if (debug) then
c c c     print *, "gout interior :  kpts=",kpts,":  ksum=",ksum
c c c+                                 ,": telapse=", (second()-t1)
c     end if

      RETURN
      END
C ------------
      SUBROUTINE TRIP2GRD3(KZ,X,Y,Z,ZMSG,MX,NY,GXOUT,GYOUT,GOUT
     +                    ,MFLAG,NFLAG,METHOD,DDCRIT,IER)

      IMPLICIT NONE
      INTEGER MX,NY,KZ,IER, MFLAG,NFLAG,METHOD
      DOUBLE PRECISION GXOUT(MX),GYOUT(NY),GOUT(MX,NY)
      DOUBLE PRECISION X(KZ),Y(KZ),Z(KZ),ZMSG, DDCRIT
c                          local
      INTEGER M,N,K,KSUM,KPTS
      DOUBLE PRECISION DOUT(MX,NY), DIST(KZ)
     +                ,RE,RAD,RLAT,ATMP 

c c c real     t0, t1, t2, second
      logical  debug

      debug = .true.
c c c t0    = second()

      IER = 0
      KPTS = KZ
c                          initialize
      DO N = 1,NY
          DO M = 1,MX
              DOUT(M,N) = 1.D20
              GOUT(M,N) = ZMSG
          END DO
      END DO
c                     EXACT MATCHES ONLY 
c c c t1  = second()
      KSUM = 0
      DO K = 1,KPTS
         DO N=1,NY
            IF (Y(K).EQ.GYOUT(N)) THEN
                DO M=1,MX
                   IF (X(K).EQ.GXOUT(M)) THEN
                      GOUT(M,N) = Z(K)
                      DOUT(M,N) = 0.0D0
                      KSUM = KSUM + 1
                      GO TO 10
                   END IF
                END DO
            END IF
         END DO
   10    CONTINUE
      END DO

c c c if (debug) then
c c c     print *, "EXACT MATCH: ksum=",ksum,":  telapse=",(second()-t1)
c c c end if

c  Did all kpts input get assigned to the gout?
c  ier = -1 is not really an error; it is just informational
c           meaning that no 'nearest neighbor' stuuf was used.

      IF (KSUM.EQ.KPTS) THEN
          IER = -11
          RETURN
      END IF

c c c t1  = second()

      RE   = 6371.2200D0 
      RAD  = 4.D0*ATAN(1.0D0)/180.D0
C                                        LOOP OVER EACH GRID POINT
      DO N = 1,NY
         DO M = 1,MX
            IF (DOUT(M,N).NE.0.D0) then
C                                        LOOP OVER EACH OBSERVATION
C                                        DISTANCE TO CURRENT GRID PT
                IF (METHOD.EQ.0) THEN
                    DO K = 1,KPTS
                       DIST(K) = SQRT( (X(K)-GXOUT(M))**2 
     +                               + (Y(K)-GYOUT(N))**2)
                    END DO
                ELSE
                    RLAT = GYOUT(N)*RAD
                    DO K = 1,KPTS
C
C The ATMP variable is necessary to make sure the
C value passed to ACOS is between -1 and 1.
C (Otherwise, you might get "NaN".)
C
                        ATMP = SIN(RLAT)*SIN(Y(K)*RAD)
     +                       + COS(RLAT)*COS(Y(K)*RAD)*
     +                         COS((X(K)-GXOUT(M))*RAD)
                        ATMP  = MIN(1.D0,MAX(-1.D0,ATMP))
                        DIST(K) = ACOS(ATMP)*RE
                    END DO
                END IF
c                              assign z(k) to nearest grid point
                DO K = 1,KPTS
                   IF (Z(K).NE.ZMSG) THEN
                       IF (DIST(K).LT.DOUT(M,N) .AND. 
     +                     DIST(K) .LT.DDCRIT)   THEN
                           DOUT(M,N) = DIST(K)
                           GOUT(M,N) = Z(K)
                       END IF
                   END IF
                END DO

            END IF

         END DO
      END DO

      RETURN
      END

c ------------------
C TRIP2GRID4 ... not used ....keep for historical reasons
c ------------------
cc    SUBROUTINE TRIP2GRD4(KZ,X,Y,Z,ZMSG,MX,NY,GXOUT,GYOUT,GOUT,STRICT,
cc   +                    ,METHOD,IER)

cc    IMPLICIT NONE
cc    INTEGER MX,NY,KZ,IER, METHOD
cc    DOUBLE PRECISION GXOUT(MX),GYOUT(NY),GOUT(MX,NY)
cc    DOUBLE PRECISION X(KZ),Y(KZ),Z(KZ),ZMSG
cc    LOGICAL STRICT
c                          local
cc    INTEGER M,N,K,MM,NN,KSUM,KPTS
cc    DOUBLE PRECISION DOUT(MX,NY),DD,XX,YY,SLPY,SLPX

cc    real     t0, t1, t2, second
cc    logical  debug

cc    debug = .true.
cc    t0    = second()

cc    IER = 0
cc    KPTS = KZ
c                          initialize
cc    DO N = 1,NY
cc        DO M = 1,MX
cc            DOUT(M,N) = 1.D20
cc            GOUT(M,N) = ZMSG
cc        END DO
cc    END DO
c                     EXACT MATCHES ONLY 
cc    t1  = second()
cc    KSUM = 0
cc    DO K = 1,KPTS
cc       DO N=1,NY
cc          IF (Y(K).EQ.GYOUT(N)) THEN
cc              DO M=1,MX
cc                 IF (X(K).EQ.GXOUT(M)) THEN
cc                    GOUT(M,N) = Z(K)
cc                    DOUT(M,N) = 0.0D0
cc                    KSUM = KSUM + 1
cc                    GO TO 10
cc                 END IF
cc              END DO
cc          END IF
cc       END DO
cc 10    CONTINUE
cc    END DO

cc    if (debug) then
cc        print *, "EXACT MATCH: ksum=",ksum,":  telapse=",(second()-t1)
cc    end if

c  Did all kpts input get assigned to the gout?
c  ier = -1 is not really an error; it is just informational
c           meaning that no 'nearest neighbor' stuuf was used.

cc    IF (KSUM.EQ.KPTS) THEN
cc        IER = -1
cc        RETURN
cc    END IF

cc    t1  = second()
c                     LOOP OVER POINTS
cc    KSUM = 0
cc    DO N = 1,NY
cc        DO M = 1,MX
cc        IF (DOUT(M,N).NE.0.0) THEN

cc            DO K = 1,KPTS
cc                IF ((X(K).GE.GXOUT(M).AND.X(K).LT.GXOUT(M+1)) .AND.
cc   +                (Y(K).GE.GYOUT(N).AND.Y(K).LT.GYOUT(N+1))) THEN

cc                    SLPX = (X(K)-GXOUT(M))/ (GXOUT(M+1)-GXOUT(M))
cc                    SLPY = (Y(K)-GYOUT(N))/ (GYOUT(N+1)-GYOUT(N))
cc                    XX = M + SLPX
cc                    YY = N + SLPY
cc                    IF (STRICT) THEN
c                                     mm,nn =>  nearest grid pt
cc                        MM = NINT(XX)
cc                        NN = NINT(YY)
cc                        DD = SQRT(X-MM)**2 + (YY-NN)**2)
cc                        IF (DD.LT.DOUT(MM,NN)) THEN
cc                            KSUM = KSUM + 1
cc                            GOUT(MM,NN) = Z(K)
cc                            DOUT(MM,NN) = DD
cc                        END IF
cc                    ELSE
c                                    allow value to influence nearby pts
cc                        DO NN = N,N + 1
cc                            DO MM = M,M + 1
cc                                DD = (XX-MM)**2 + (YY-NN)**2
cc                                IF (DD.LT.DOUT(MM,NN)) THEN
cc                                    KSUM = KSUM + 1
cc                                    GOUT(MM,NN) = Z(K)
cc                                    DOUT(MM,NN) = DD
cc                                END IF
cc                            END DO
cc                        END DO
cc                    END IF

cc                END IF
cc            END DO

c c c     end if
cc        END DO
cc    END DO

cc    if (debug) then
cc        print *, "gout interior :  kpts=",kpts,":  ksum=",ksum
cc   +                                 ,": telapse=", (second()-t1)
cc    end if

cc    RETURN
cc    END

c NCLFORTSTART
      SUBROUTINE DVRFIDF(U,V,GLAT,GLON,MLON,NLAT,XMSG,IOPT,RV,IER)
      IMPLICIT NONE

c relative vorticity via finite differences
c NCL: rv = uv2vr_cfd (u,v,lat,lon,iopt)

c            xmsg = u@_FillValue
c            nlat,mlon = dimsizes (u)
c            iopt= cyclic

C*PL*ERROR* Comment line too long
c relative vorticity via centered finite difference approach approach (rv)
c .   rv = dv/dx-du/dy   where "d" means partial derivitive

c assumptions:
c .   (1) latitudes  monotonically increasing  [eg: glat(2) > glat(1)]
c .                  latitudes may be unequally spaced
c .   (2) longitudes monotonically increasing  [eg: glon(2) > glon(1)]
c .                  longitudes may be unequally spaced
c .   (3) if iopt=1 then the grids are cyclic in x
c .                  eg: for T42 grid mlon=128, nlat=64
c .                      cyclic point should NOT be included
c .

c                                          ! INPUT
      INTEGER MLON,NLAT
      DOUBLE PRECISION U(MLON,NLAT),V(MLON,NLAT),GLAT(NLAT),GLON(MLON),
     +                 XMSG
      INTEGER IOPT

c                                          ! OUTPUT
      DOUBLE PRECISION RV(MLON,NLAT)
      INTEGER IER
C NCLEND
c                                          ! LOCAL  (dynamic/adjustable)
      DOUBLE PRECISION CLAT(NLAT),DY(NLAT),RE,RAD,RCON,DX,DX2
      INTEGER ML,NL,MLSTRT,MLEND,MLM1,MLP1,JOPT

c                                          ! error chk stuff
      IER = 0
      IF (MLON.LT.1 .OR. NLAT.LT.1) THEN
          IER = 1
          RETURN
      END IF

      RE = 6.37122D06
      RAD = 4.D0*ATAN(1.D0)/180.D0
      RCON = RE*RAD
      JOPT = ABS(IOPT)
c                                 ! clat=1 produces results
c                                 ! MUCH closer to spherical harm
      DO NL = 1,NLAT
          IF (IOPT.GE.0) THEN
              CLAT(NL) = 1.0D0
          ELSE
              CLAT(NL) = COS(RAD*GLAT(NL))
          END IF
      END DO
c                                          ! initialize to msg
      DO NL = 1,NLAT
          DO ML = 1,MLON
              RV(ML,NL) = XMSG
          END DO
      END DO
c                                          ! calculate "1/dy"
      DY(1) = 1.D0/ (RCON* (GLAT(2)-GLAT(1)))
      DY(NLAT) = 1.D0/ (RCON* (GLAT(NLAT)-GLAT(NLAT-1)))
c                                          ! calculate "1/(2*dy)"
      DO NL = 2,NLAT - 1
          DY(NL) = 1.D0/ (RCON* (GLAT(NL+1)-GLAT(NL-1)))
      END DO
c                                          ! calculate "1/dx"
      DX = 1.D0/ (RCON* (GLON(2)-GLON(1)))
c                                          ! calculate "1/(2*dx)"
      DX2 = 1.D0/ (RCON* (GLON(3)-GLON(1)))
c                                          ! set subscript range
      IF (JOPT.EQ.1 .OR. JOPT.EQ.3) THEN
          MLSTRT = 1
          MLEND = MLON
      ELSE
          MLSTRT = 2
          MLEND = MLON - 1
      END IF
c                                          ! rv in grid body
      DO ML = MLSTRT,MLEND
c                                          ! iopt=0 or 1
          MLM1 = ML - 1
          MLP1 = ML + 1
          IF (ML.EQ.1) MLM1 = MLON
          IF (ML.EQ.MLON) MLP1 = 1

          DO NL = 2,NLAT - 1
              IF (V(MLP1,NL).NE.XMSG .AND. V(MLM1,NL).NE.XMSG .AND.
     +            U(ML,NL+1).NE.XMSG .AND. U(ML,NL-1).NE.XMSG) THEN

                  RV(ML,NL) = (V(MLP1,NL)-V(MLM1,NL))*DX2 -
     +                        (U(ML,NL+1)*CLAT(NL+1)-
     +                        U(ML,NL-1)*CLAT(NL-1))*DY(NL)

              END IF

              IF (JOPT.EQ.2) THEN
c                                   ! left bound (ml=1)
                  IF (V(2,NL).NE.XMSG .AND. V(1,NL).NE.XMSG .AND.
     +                U(1,NL+1).NE.XMSG .AND. U(1,NL-1).NE.XMSG) THEN

                      RV(1,NL) = (V(2,NL)-V(1,NL))*DX -
     +                           (U(1,NL+1)*CLAT(NL+1)-
     +                           U(1,NL-1)*CLAT(NL-1))*DY(NL)
                  END IF
c                                   ! right bound (ml=mlon)
                  IF (V(MLON,NL).NE.XMSG .AND.
     +                V(MLON-1,NL).NE.XMSG .AND.
     +                U(MLON,NL+1).NE.XMSG .AND.
     +                U(MLON,NL-1).NE.XMSG) THEN

                      RV(MLON,NL) = (V(MLON,NL)-V(MLON-1,NL))*DX -
     +                              (U(MLON,NL+1)*CLAT(NL+1)-
     +                              U(MLON,NL-1)*CLAT(NL-1))*DY(NL)
                  END IF
              END IF
          END DO

          IF (JOPT.GE.2) THEN
c                                   ! bottom bound (nl=1)
              IF (V(MLP1,1).NE.XMSG .AND. V(MLM1,1).NE.XMSG .AND.
     +            U(ML,2).NE.XMSG .AND. U(ML,1).NE.XMSG) THEN

                  RV(ML,1) = (V(MLP1,1)-V(MLM1,1))*DX2 -
     +                       (U(ML,2)*CLAT(2)-U(ML,1)*CLAT(1))*DY(1)
              END IF
c                                   ! top bound   (nl=nlat)
              IF (V(MLP1,NLAT).NE.XMSG .AND. V(MLM1,NLAT).NE.XMSG .AND.
     +            U(ML,NLAT).NE.XMSG .AND. U(ML,NLAT-1).NE.XMSG) THEN

                  RV(ML,NLAT) = (V(MLP1,NLAT)-V(MLM1,NLAT))*DX2 -
     +                          (U(ML,NLAT)*CLAT(NLAT)-
     +                          U(ML,NLAT-1)*CLAT(NLAT-1))*DY(NLAT)
              END IF
          END IF
      END DO
c                                   ! special for corners (jopt=2 only)
c                                   ! use linear extrapolation from
c                                   ! two directions
      IF (JOPT.EQ.2) THEN
          CALL DLNEXTRP(RV,MLON,NLAT,XMSG)
      END IF

      RETURN
      END

c NCLFORTSTART
      SUBROUTINE DDVFIDF(U,V,GLAT,GLON,MLON,NLAT,XMSG,IOPT,DV,IER)
      IMPLICIT NONE

c divergence via centered finite differences
c .   div = x/dy+du/dx   where "d" means partial derivitive

c NCL: dv = uv2dv_cfd(u,v,lat,lon,iopt)
c            xmsg = u@_FillValue
c            nlat,mlon = dimsizes (u)
c            iopt= cyclic

c assumptions:
c .   (1) latitudes  monotonically increasing  [eg: glat(2) > glat(1)]
c .                  latitudes may be unequally spaced
c .   (2) longitudes monotonically increasing  [eg: glon(2) > glon(1)]
c .                  longitudes may be unequally spaced
c .   (3) if iopt=1 then the grids are cyclic in x
c .                  eg: for T42 grid mlon=128, nlat=64
c .                      cyclic point should NOT be included
c .

c                                          ! INPUT
      INTEGER MLON,NLAT
      DOUBLE PRECISION U(MLON,NLAT),V(MLON,NLAT),GLAT(NLAT),GLON(MLON),
     +                 XMSG
      INTEGER IOPT

c                                          ! OUTPUT
      DOUBLE PRECISION DV(MLON,NLAT)
      INTEGER IER
C NCLEND
c                                          ! LOCAL  (dynamic/adjustable)
      DOUBLE PRECISION CLAT(NLAT),DY(NLAT),DX,RE,RAD,RCON,DX2
      INTEGER ML,NL,MLSTRT,MLEND,MLM1,MLP1,JOPT

c                                          ! error chk stuff
      IER = 0
      IF (MLON.LT.1 .OR. NLAT.LT.1) THEN
          IER = 1
          RETURN
      END IF

      RE = 6.37122D06
      RAD = 4.D0*ATAN(1.D0)/180.D0
      RCON = RE*RAD
      JOPT = ABS(IOPT)
c                                          ! computing 1./cos(glat)
      DO NL = 1,NLAT
          IF (IOPT.GE.0) THEN
              CLAT(NL) = 1.0D0
          ELSE
              CLAT(NL) = COS(RAD*GLAT(NL))
          END IF
      END DO
c                                          ! initialize to msg
      DO NL = 1,NLAT
          DO ML = 1,MLON
              DV(ML,NL) = XMSG
          END DO
      END DO
c                                          ! calculate "1/dy"
      DY(1) = 1.D0/ (RCON* (GLAT(2)-GLAT(1)))
      DY(NLAT) = 1.D0/ (RCON* (GLAT(NLAT)-GLAT(NLAT-1)))
c                                          ! calculate "1/(2*dy)"
      DO NL = 2,NLAT - 1
          DY(NL) = 1.D0/ (RCON* (GLAT(NL+1)-GLAT(NL-1)))
      END DO
c                                          ! calculate "1/dx"
      DX = 1.D0/ (RCON* (GLON(2)-GLON(1)))
c                                          ! calculate "1/(2*dx)"
      DX2 = 1.D0/ (RCON* (GLON(3)-GLON(1)))

c                                          ! set subscript range
      IF (JOPT.EQ.1 .OR. JOPT.EQ.3) THEN
          MLSTRT = 1
          MLEND = MLON
      ELSE
          MLSTRT = 2
          MLEND = MLON - 1
      END IF
c                                          ! rv in grid body
      DO ML = MLSTRT,MLEND
          MLM1 = ML - 1
          MLP1 = ML + 1
          IF (ML.EQ.1) MLM1 = MLON
          IF (ML.EQ.MLON) MLP1 = 1

          DO NL = 2,NLAT - 1
              IF (V(MLP1,NL).NE.XMSG .AND. V(MLM1,NL).NE.XMSG .AND.
     +            U(ML,NL+1).NE.XMSG .AND. U(ML,NL-1).NE.XMSG) THEN

                  DV(ML,NL) = (V(ML,NL+1)*CLAT(NL+1)-
     +                        V(ML,NL-1)*CLAT(NL-1))*DY(NL) +
     +                        (U(MLP1,NL)-U(MLM1,NL))*DX2
              END IF

              IF (JOPT.EQ.2) THEN
c                                   ! left bound (ml=1)
                  IF (V(1,NL+1).NE.XMSG .AND. V(1,NL-1).NE.XMSG .AND.
     +                U(2,NL).NE.XMSG .AND. U(1,NL).NE.XMSG) THEN

                      DV(1,NL) = (V(1,NL+1)*CLAT(NL+1)-
     +                           V(1,NL-1)*CLAT(NL-1))*DY(NL) +
     +                           (U(2,NL)-U(1,NL))*DX
                  END IF
c                                   ! right bound (ml=mlon)
                  IF (V(MLON,NL+1).NE.XMSG .AND.
     +                V(MLON,NL-1).NE.XMSG .AND.
     +                U(MLON,NL).NE.XMSG .AND.
     +                U(MLON-1,NL).NE.XMSG) THEN

                      DV(MLON,NL) = (V(MLON,NL+1)*CLAT(NL+1)-
     +                              V(MLON,NL-1)*CLAT(NL-1))*DY(NL) +
     +                              (U(MLON,NL)-U(MLON-1,NL))*DX
                  END IF
              END IF
          END DO

          IF (JOPT.GE.2) THEN
c                                   ! bottom bound (nl=1)
              IF (V(ML,2).NE.XMSG .AND. V(ML,1).NE.XMSG .AND.
     +            U(MLP1,2).NE.XMSG .AND. U(MLM1,1).NE.XMSG) THEN

                  DV(ML,1) = (V(ML,2)*CLAT(2)-V(ML,1)*CLAT(1))*DY(1) +
     +                       (U(MLP1,1)-U(MLM1,1))*DX2
              END IF
c                                   ! top bound   (nl=nlat)
              IF (V(ML,NLAT).NE.XMSG .AND. V(ML,NLAT-1).NE.XMSG .AND.
     +            U(MLP1,NLAT).NE.XMSG .AND. U(MLM1,NLAT).NE.XMSG) THEN

                  DV(ML,NLAT) = (V(ML,NLAT)*CLAT(NLAT)-
     +                          V(ML,NLAT-1)*CLAT(NLAT-1))*DY(NLAT) +
     +                          (U(MLP1,NLAT)-U(MLM1,NLAT))*DX2
              END IF
          END IF
      END DO
c                                   ! special for corners (jopt=2 only)
c                                   ! use linear extrapolation from
c                                   ! two directions
      IF (JOPT.EQ.2) THEN
          CALL DLNEXTRP(DV,MLON,NLAT,XMSG)
      END IF

      RETURN
      END

      SUBROUTINE DLNEXTRP(X,MLON,NLAT,XMSG)
      IMPLICIT NONE

c linearly extrapolate from two direction [tkae average]
c .   for the corner pts

      INTEGER MLON,NLAT
      DOUBLE PRECISION X(MLON,NLAT),XMSG

      INTEGER ML,NL

      DO NL = 1,NLAT,NLAT - 1
          DO ML = 1,MLON,MLON - 1
              IF (X(ML,NL).EQ.XMSG) THEN
                  IF (NL.EQ.1 .AND. ML.EQ.1) THEN
                      IF (X(ML,NL+1).NE.XMSG .AND.
     +                    X(ML,NL+2).NE.XMSG .AND.
     +                    X(ML+1,NL).NE.XMSG .AND.
     +                    X(ML+1,NL).NE.XMSG) THEN
                          X(ML,NL) = (2.D0*X(ML,NL+1)-X(ML,NL+2)+
     +                               2.D0*X(ML+1,NL)-X(ML+2,NL))*0.5D0
                      END IF
                  END IF
                  IF (NL.EQ.1 .AND. ML.EQ.MLON) THEN
                      IF (X(ML,NL+1).NE.XMSG .AND.
     +                    X(ML,NL+2).NE.XMSG .AND.
     +                    X(ML-1,NL).NE.XMSG .AND.
     +                    X(ML-1,NL).NE.XMSG) THEN
                          X(ML,NL) = (2.D0*X(ML,NL+1)-X(ML,NL+2)+
     +                               2.D0*X(ML-1,NL)-X(ML-2,NL))*0.5D0
                      END IF
                  END IF
                  IF (NL.EQ.NLAT .AND. ML.EQ.1) THEN
                      IF (X(ML,NL-1).NE.XMSG .AND.
     +                    X(ML,NL-2).NE.XMSG .AND.
     +                    X(ML+1,NL).NE.XMSG .AND.
     +                    X(ML+1,NL).NE.XMSG) THEN
                          X(ML,NL) = (2.D0*X(ML,NL-2)-X(ML,NL-2)+
     +                               2.D0*X(ML+1,NL)-X(ML+2,NL))*0.5D0
                      END IF
                  END IF
                  IF (NL.EQ.NLAT .AND. ML.EQ.MLON) THEN
                      IF (X(ML,NL-1).NE.XMSG .AND.
     +                    X(ML,NL-2).NE.XMSG .AND.
     +                    X(ML-1,NL).NE.XMSG .AND.
     +                    X(ML-1,NL).NE.XMSG) THEN
                          X(ML,NL) = (2.D0*X(ML,NL-2)-X(ML,NL-2)+
     +                               2.D0*X(ML-1,NL)-X(ML-2,NL))*0.5D0
                      END IF
                  END IF
              END IF
          END DO
      END DO

      RETURN
      END

c -----------------------------------------------------------
C NCLFORTSTART
      SUBROUTINE DRCM2RGRID(NYI,NXI,YI,XI,FI,NYO,YO,NXO,XO,FO,XMSG,OPT,
     +                      IER)
      IMPLICIT NONE
      INTEGER NXI,NYI,NXO,NYO,IER
      DOUBLE PRECISION XI(NXI,NYI),YI(NXI,NYI),FI(NXI,NYI),OPT
      DOUBLE PRECISION XO(NXO),YO(NYO),FO(NXO,NYO),XMSG
C NCLEND

C This is written  with GNU f77 acceptable extensions
c .   This could be improved considerably with f90

c NCL:  fo = rcm2rgrid (lat2d,lon2d,fi, lat, lon iopt)
c                        yi    xi   fi  yo   xo
c
c            fo is the same size xo, yo and same type as "fi"
c            xmsg = fi@_FillValue
c            opt unused option
c
c            The NCL wrapper should allow for multiple datasets
c            so the user need only make one call to the function.

c perform 2D interpolation allowing for missing data:  nothing fancy

c nomenclature:
c .   nxi,nyi - lengths of xi,yi and dimensions of fi (must be >= 2)
c .   xi      - coordinates of fi (eg, lon [2D] )
c .   yi      - coordinates of fi (eg, lat [2D] )
c .   fi      - functional input values [2D]
c .   nxo,nyo - lengths of xo,yo and dimensions of fo (must be >= 1)
c .   xo      - coordinates of fo (eg, lon [1D])
c .             must be monotonically increasing
c .   yo      - coordinates of fo (eg, lat [1D])
c .             must be monotonically increasing
c .   fo      - functional output values [interpolated]
c .   xmsg    - missing code
c .   opt     - unused
c .   ier     - error code
c .             =0;   no error
c .             =1;   not enough points in input/output array
c .             =2/3; xi or yi are not monotonically increasing
c .             =4/5; xo or yo are not monotonically increasing
c
c                              local
      INTEGER NX,NY,NEXACT,IX,IY,M,N,NW,NER,K
      DOUBLE PRECISION FW(2,2),W(2,2),SUMF,SUMW,CHKLAT(NYO),CHKLON(NXO)
      DOUBLE PRECISION DGCDIST
c                              error checking
      IER = 0
      IF (NXI.LE.1 .OR. NYI.LE.1 .OR. NXO.LE.1 .OR. NYO.LE.1) THEN
          IER = 1
          RETURN
      END IF
      IF (IER.NE.0) RETURN

      CALL DMONOINC(YO,NYO,IER,NER)
      IF (IER.NE.0) RETURN
      CALL DMONOINC(XO,NXO,IER,NER)
      IF (IER.NE.0) RETURN

      DO NY = 1,NYI
          CHKLAT(NY) = YI(1,NY)
c c c    print *,"chklat: ny=",ny,"  chklat=",chklat(ny)
      END DO
      CALL DMONOINC(CHKLAT,NYI,IER,NER)
      IF (IER.NE.0) RETURN

      DO NX = 1,NXI
          CHKLON(NX) = XI(NX,1)
c c c    print *,"chklon: nx=",nx,"  chklon=",chklon(nx)
      END DO
      CALL DMONOINC(CHKLAT,NYI,IER,NER)
      IF (IER.NE.0) RETURN

      K = 2
c c c k = opt

      DO NY = 1,NYO
          DO NX = 1,NXO
              FO(NX,NY) = XMSG
          END DO
      END DO
c                              main loop [exact matches]
      NEXACT = 0
      DO NY = 1,NYO
          DO NX = 1,NXO

              DO IY = 1,NYI
                  DO IX = 1,NXI
                      IF (XO(NX).EQ.XI(IX,IY) .AND.
     +                    YO(NY).EQ.YI(IX,IY)) THEN
                          FO(NX,NY) = FI(IX,IY)
                          NEXACT = NEXACT + 1
                          GO TO 10
                      END IF
                  END DO
              END DO

   10         CONTINUE
          END DO
      END DO

c c c print *, "nexact=",nexact
c                              main loop [interpolation]
      DO NY = 1,NYO
          DO NX = 1,NXO
              IF (FO(NX,NY).EQ.XMSG) THEN

                  DO IY = 1,NYI - K
                      DO IX = 1,NXI - K
                          IF (XO(NX).GE.XI(IX,IY) .AND.
     +                        XO(NX).LE.XI(IX+K,IY) .AND.
     +                        YO(NY).GE.YI(IX,IY) .AND.
     +                        YO(NY).LE.YI(IX,IY+K)) THEN


                              W(1,1) = (1.D0/DGCDIST(YO(NY),XO(NX),
     +                                 YI(IX,IY),XI(IX,IY),2))**2
                              W(2,1) = (1.D0/DGCDIST(YO(NY),XO(NX),
     +                                 YI(IX+K,IY),XI(IX+K,IY),2))**2
                              W(1,2) = (1.D0/DGCDIST(YO(NY),XO(NX),
     +                                 YI(IX,IY+K),XI(IX,IY+K),2))**2
                              W(2,2) = (1.D0/DGCDIST(YO(NY),XO(NX),
     +                                 YI(IX+K,IY+K),XI(IX+K,IY+K),2))**
     +                                 2

                              FW(1,1) = FI(IX,IY)
                              FW(2,1) = FI(IX+K,IY)
                              FW(1,2) = FI(IX,IY+K)
                              FW(2,2) = FI(IX+K,IY+K)

                              NW = 0
                              SUMF = 0.0D0
                              SUMW = 0.0D0
                              DO N = 1,2
                                  DO M = 1,2
                                      IF (FW(M,N).NE.XMSG) THEN
                                          SUMF = SUMF + FW(M,N)*W(M,N)
                                          SUMW = SUMW + W(M,N)
                                          NW = NW + 1
                                      END IF
                                  END DO
                              END DO
c                                             nw >=3 arbitrary
                              IF (NW.GE.3 .AND. SUMW.GT.0.D0) THEN
                                  FO(NX,NY) = SUMF/SUMW
                              END IF
                              GO TO 20
                          END IF
                      END DO
                  END DO

   20             CONTINUE
              END IF
          END DO
      END DO

      RETURN
      END
c -----------------------------------------------------------
C NCLFORTSTART
      SUBROUTINE DRGRID2RCM(NYI,NXI,YI,XI,FI,NYO,NXO,YO,XO,FO,XMSG,OPT,
     +                      IER)
      IMPLICIT NONE
      INTEGER NXI,NYI,NXO,NYO,IER
      DOUBLE PRECISION XI(NXI),YI(NYI),FI(NXI,NYI),OPT
      DOUBLE PRECISION XO(NXO,NYO),YO(NXO,NYO),FO(NXO,NYO),XMSG
C NCLEND

C This is written  with GNU f77 acceptable extensions
c .   This could be improved considerably with f90

c NCL:  fo = rcm2rgrid (lat2d,lon2d,fi, lat, lon iopt)
c                        yi    xi   fi  yo   xo
c
c            fo is the same size xo, yo and same type as "fi"
c            xmsg = fi@_FillValue
c            opt unused option
c
c            The NCL wrapper should allow for multiple datasets
c            so the user need only make one call to the function.

c perform 2D interpolation allowing for missing data:  nothing fancy

c nomenclature:
c .   nxi,nyi - lengths of xi,yi and dimensions of fi (must be >= 2)
c .   xi      - coordinates of fi (eg, lon [1D])
c .   yi      - coordinates of fi (eg, lat [1D])
c .   fi      - functional input values [2D]
c .   nxo,nyo - lengths of xo,yo and dimensions of fo (must be >= 1)
c .   xo      - coordinates of fo (eg, lon [2D])
c .             must be monotonically increasing
c .   yo      - coordinates of fo (eg, lat [2D])
c .             must be monotonically increasing
c .   fo      - functional output values [interpolated]
c .   xmsg    - missing code
c .   opt     - unused
c .   ier     - error code
c .             =0;   no error
c .             =1;   not enough points in input/output array
c .             =2/3; xi or yi are not monotonically increasing
c .             =4/5; xo or yo are not monotonically increasing
c
c                              local
      INTEGER NX,NY,NEXACT,IX,IY,M,N,NW,NER,K
      DOUBLE PRECISION FW(2,2),W(2,2),SUMF,SUMW
      DOUBLE PRECISION DGCDIST

c                              in-line functions (bilinear interp)
      DOUBLE PRECISION Z1,Z2,Z3,Z4,SLOPE,SLPX,SLPY,FLI,FBLI

      FLI(Z1,Z2,SLOPE) = Z1 + SLOPE* (Z2-Z1)
      FBLI(Z1,Z2,Z3,Z4,SLPX,SLPY) = FLI(Z1,Z2,SLPX) +
     +                              SLPY* (FLI(Z3,Z4,SLPX)-
     +                              FLI(Z1,Z2,SLPX))

c                              error checking
      IER = 0
      IF (NXI.LE.1 .OR. NYI.LE.1 .OR. NXO.LE.1 .OR. NYO.LE.1) THEN
          IER = 1
          RETURN
      END IF
      IF (IER.NE.0) RETURN

      CALL DMONOINC(YI,NYI,IER,NER)
      IF (IER.NE.0) RETURN
      CALL DMONOINC(XI,NXI,IER,NER)
      IF (IER.NE.0) RETURN

      DO NY = 1,NYO
          DO NX = 1,NXO
              FO(NX,NY) = XMSG
          END DO
      END DO
c                              main loop [exact matches]
      NEXACT = 0
      DO NY = 1,NYO
          DO NX = 1,NXO

              DO IY = 1,NYI
                  DO IX = 1,NXI
                      IF (XO(NX,NY).EQ.XI(IX) .AND.
     +                    YO(NX,NY).EQ.YI(IY)) THEN
                          FO(NX,NY) = FI(IX,IY)
                          NEXACT = NEXACT + 1
                          GO TO 10
                      END IF
                  END DO
              END DO

   10         CONTINUE
          END DO
      END DO

c c c print *, "nexact=",nexact

      K = 1
c c c k = opt

c                              main loop [interpolation]
      DO NY = 1,NYO
          DO NX = 1,NXO

              DO IY = 1,NYI - K
                  DO IX = 1,NXI - K
                      IF (XO(NX,NY).GT.XI(IX) .AND.
     +                    XO(NX,NY).LT.XI(IX+K) .AND.
     +                    YO(NX,NY).GT.YI(IY) .AND.
     +                    YO(NX,NY).LT.YI(IY+K)) THEN

                          IF (FI(IX,IY).NE.XMSG .AND.
     +                        FI(IX+K,IY).NE.XMSG .AND.
     +                        FI(IX,IY+K).NE.XMSG .AND.
     +                        FI(IX+K,IY+K).NE.XMSG) THEN

                              FO(NX,NY) = FBLI(FI(IX,IY),FI(IX+K,IY),
     +                                    FI(IX,IY+K),FI(IX+K,IY+K),
     +                                    (XO(NX,NY)-XI(IX))/
     +                                    (XI(IX+K)-XI(IX)),
     +                                    (YO(NX,NY)-YI(IY))/
     +                                    (YI(IY+K)-YI(IY)))

                          ELSE
c                                            OVERKILL
                              FW(1,1) = FI(IX,IY)
                              FW(2,1) = FI(IX+K,IY)
                              FW(1,2) = FI(IX,IY+K)
                              FW(2,2) = FI(IX+K,IY+K)

                              W(1,1) = (1.D0/DGCDIST(YO(NX,NY),XO(NX,
     +                                 NY),YI(IY),XI(IX),2))**2
                              W(2,1) = (1.D0/DGCDIST(YO(NX,NY),XO(NX,
     +                                 NY),YI(IY),XI(IX+K),2))**2
                              W(1,2) = (1.D0/DGCDIST(YO(NX,NY),XO(NX,
     +                                 NY),YI(IY+K),XI(IX),2))**2
                              W(2,2) = (1.D0/DGCDIST(YO(NX,NY),XO(NX,
     +                                 NY),YI(IY+K),XI(IX+K),2))**2

                              NW = 0
                              SUMF = 0.0D0
                              SUMW = 0.0D0
                              DO N = 1,2
                                  DO M = 1,2
                                      IF (FW(M,N).NE.XMSG) THEN
                                          SUMF = SUMF + FW(M,N)*W(M,N)
                                          SUMW = SUMW + W(M,N)
                                          NW = NW + 1
                                      END IF
                                  END DO
                              END DO
c                                             nw >=3 arbitrary
                              IF (NW.GE.3 .AND. SUMW.GT.0.D0) THEN
                                  FO(NX,NY) = SUMF/SUMW
                              END IF
                              GO TO 20

                          END IF
                      END IF
                  END DO
              END DO

   20         CONTINUE
          END DO
      END DO

      RETURN
      END

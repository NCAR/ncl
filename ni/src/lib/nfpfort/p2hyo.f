C NCLFORTSTART
      SUBROUTINE P2HYO(PI,MLON,NLAT,KLEVI,XI,PSFC,P0,HYAO,HYBO,KLEVO,XO)
      IMPLICIT NONE

c NCL: xo = p2hyo (p,xi,psfc,hyao,hybo)

c this routine interploates one hybrid level to another
c     formula for the pressure of a hybrid surface is;
c          phy(k) = hya(k)*p0 + hyb(k)*psfc

c     input  ["i" input ... "o" output]
c          pi     - pressure level                     [input]
c          psfc   - is the surface pressure Pa         [input]
c          mlon   - longitude dimension
c          nlat   - latitude  dimension
c          klevi  - number of input  levels
c          hyao   - is the "a" or pressure hybrid coef
c          hybo   - is the "b" or sigma coeficient
c          klevo  - number of output levels
c     output
c          xo     - pressure at hybrid levels [Pa]
c                                                 ! input
      INTEGER MLON,NLAT,KLEVI,KLEVO
      DOUBLE PRECISION P0,PI(KLEVI),PSFC(MLON,NLAT),XI(MLON,NLAT,KLEVI),
     +                 HYAO(KLEVO),HYBO(KLEVO)
c                                                 ! output
      DOUBLE PRECISION XO(MLON,NLAT,KLEVO)
C NCLEND
c                                                 ! local (automatic)
      DOUBLE PRECISION PO(KLEVO)
      INTEGER IFLAG

      IFLAG = 0

      CALL P2HYOX(PI,MLON,NLAT,KLEVI,XI,PSFC,P0,HYAO,HYBO,KLEVO,XO,PO,
     +            IFLAG)
      RETURN
      END

C NCLFORTSTART
      SUBROUTINE P2HYOX(PI,MLON,NLAT,KLEVI,XI,PSFC,P0,HYAO,HYBO,KLEVO,
     +                  XO,PO,IFLAG)
      IMPLICIT NONE
      INTEGER MLON,NLAT,KLEVI,KLEVO,IFLAG
      DOUBLE PRECISION P0,HYAI(KLEVI),HYBI(KLEVI),HYAO(KLEVO),
     +                 HYBO(KLEVO),PSFC(MLON,NLAT),XI(MLON,NLAT,KLEVI),
     +                 PI(KLEVI),PO(KLEVO)
c                                                 ! output
      DOUBLE PRECISION XO(MLON,NLAT,KLEVO)
C NCLEND
c                                                 ! local
      INTEGER NL,ML,KI,KO
c f77
      DO NL = 1,NLAT
          DO ML = 1,MLON

              DO KO = 1,KLEVO
                  PO(KO) = HYAO(KO)*P0 + HYBO(KO)*PSFC(ML,NL)
              END DO

              DO KO = 1,KLEVO
                  XO(ML,NL,KO) = 1.D20
                  DO KI = 1,KLEVI - 1
                      IF (PI(KI).LE.PSFC(ML,NL)) THEN
                          IF (PO(KO).GE.PI(KI) .AND.
     +                        PO(KO).LT.PI(KI+1)) THEN
                              XO(ML,NL,KO) = XI(ML,NL,KI) +
     +                                       (XI(ML,NL,KI+1)-
     +                                       XI(ML,NL,KI))*
     +                                       (DLOG(PO(KO))-
     +                                       DLOG(PI(KI)))/
     +                                       (DLOG(PI(KI+1))-
     +                                       DLOG(PI(KI)))
                          ELSE
                              IFLAG = 1
                          END IF
                      END IF
                  END DO
              END DO

          END DO
      END DO

      RETURN
      END

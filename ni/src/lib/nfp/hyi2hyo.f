C NCLFORTSTART
      SUBROUTINE DHYI2HYOA(P0,HYAI,HYBI,PSFC,MLON,NLAT,KLEVI,XI,HYAO,
     +                     HYBO,KLEVO,XO)
      IMPLICIT NONE

c NCL: xo = hybrid2hybrid (p0,hyai,hybi,psfc,xi,  hyao,hybo)

c this routine interploates one hybrid level to another
c     formula for the pressure of a hybrid surface is;
c          phy(k) = hya(k)*p0 + hyb(k)*psfc

c     input  ["i" input ... "o" output]
c          hyai   - is the "a" or pressure hybrid coef [input]
c          hybi   - is the "b" or sigma coeficient     [input]
c          p0     - is the base pressure in Pa         [input]
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
      DOUBLE PRECISION P0,HYAI(KLEVI),HYBI(KLEVI),HYAO(KLEVO),
     +                 HYBO(KLEVO),PSFC(MLON,NLAT),XI(MLON,NLAT,KLEVI)
c                                                 ! output
      DOUBLE PRECISION XO(MLON,NLAT,KLEVO)
C NCLEND
c                                                 ! local (automatic)
      DOUBLE PRECISION PI(KLEVI),PO(KLEVO)

      CALL DHYI2HYOB(P0,HYAI,HYBI,PSFC,MLON,NLAT,KLEVI,XI,HYAO,HYBO,
     +               KLEVO,XO,PI,PO)
      RETURN
      END

C NCLFORTSTART
      SUBROUTINE DHYI2HYOB(P0,HYAI,HYBI,PSFC,MLON,NLAT,KLEVI,XI,HYAO,
     +                     HYBO,KLEVO,XO,PI,PO)
      IMPLICIT NONE
      INTEGER MLON,NLAT,KLEVI,KLEVO
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

              DO KI = 1,KLEVI
                  PI(KI) = HYAI(KI)*P0 + HYBI(KI)*PSFC(ML,NL)
              END DO

              DO KO = 1,KLEVO
                  PO(KO) = HYAO(KO)*P0 + HYBO(KO)*PSFC(ML,NL)
              END DO

              DO KO = 1,KLEVO
                  DO KI = 1,KLEVI - 1
                      IF (PO(KO).GE.PI(KI) .AND.
     +                    PO(KO).LT.PI(KI+1)) THEN
                          XO(ML,NL,KO) = XI(ML,NL,KI) +
     +                                   (XI(ML,NL,KI+1)-XI(ML,NL,KI))*
     +                                   (DLOG(PO(KO))-DLOG(PI(KI)))/
     +                                   (DLOG(PI(KI+1))-DLOG(PI(KI)))
                      END IF
                  END DO
              END DO

          END DO
      END DO

      RETURN
      END

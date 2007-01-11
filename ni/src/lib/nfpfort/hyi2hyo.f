      SUBROUTINE DHYI2HYOB(P0,HYAI,HYBI,PSFC,MLON,NLAT,KLEVI,XI,HYAO,
     +                     HYBO,KLEVO,XO,PI,PO,INTFLG, MSGFLG, XMSG)
      IMPLICIT NONE
c NCL: xo = hyi2hyo (p0,hyai,hybi,psfc,xi,hyao,hybo)

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
c          xmsg   = missing code; if none present set to
c                   some bogus value [eg, 1d20]
c          intflg - if  0 set all values outside of input P to xmsg
c                   if  1 set output values to closest input pressure level
c     output
c          xo     - pressure at hybrid levels [Pa]
c          msgflg - if  0 no missing values upon return
c                   if -1 missing values are present.

c                                                 ! input
      INTEGER MLON,NLAT,KLEVI,KLEVO,INTFLG
      DOUBLE PRECISION P0,HYAI(KLEVI),HYBI(KLEVI),HYAO(KLEVO),
     +                 HYBO(KLEVO),PSFC(MLON,NLAT),XI(MLON,NLAT,KLEVI),
     +                 PI(KLEVI),PO(KLEVO), XMSG
c                                                 ! output
      DOUBLE PRECISION XO(MLON,NLAT,KLEVO)
c                                                 ! local
      INTEGER NL,ML,KI,KO, MSGFLG
      DOUBLE PRECISION PEPS, PILOW
c f77
      PEPS   = 1.0D-3
      MSGFLG = 0

      DO NL = 1,NLAT
          DO ML = 1,MLON
              DO KI = 1,KLEVI
                  PI(KI) = HYAI(KI)*P0 + HYBI(KI)*PSFC(ML,NL)
              END DO
              PILOW = PI(KLEVI) - PEPS

              DO KO = 1,KLEVO
                  PO(KO) = HYAO(KO)*P0 + HYBO(KO)*PSFC(ML,NL)
              END DO

              DO KO = 1,KLEVO
C outlier check: set flag for interface routine and value    
                 IF (PO(KO).LT.PI(1) .OR. PO(KO).GT.PILOW ) THEN
C must be outside input pressure range
                     IF (INTFLG.EQ.0) THEN 
C default is to set to xmsg
                         MSGFLG       = -1
                         XO(ML,NL,KO) = XMSG
                     ELSEIF (INTFLG.EQ.1) THEN 
C set to nearest input value
                         IF (PO(KO).LT.PI(1)) THEN
                             XO(ML,NL,KO) = XI(ML,NL,1) 
                         ELSEIF (PO(KO).GT.PILOW) THEN
                             XO(ML,NL,KO) = XI(ML,NL,KLEVI) 
                         END IF
                     END IF
                 ELSE
C This was the original code
                     DO KI = 1,KLEVI-1
                        IF ((PO(KO).GE.PI(KI) .AND. 
     +                       PO(KO).LT.PI(KI+1)))THEN
                             XO(ML,NL,KO) = XI(ML,NL,KI) +
     +                                  (XI(ML,NL,KI+1)-XI(ML,NL,KI))*
     +                                  (DLOG(PO(KO))-DLOG(PI(KI)))/
     +                                  (DLOG(PI(KI+1))-DLOG(PI(KI)))
                             GO TO 20
                        END IF
                     END DO
                 END IF
   20            CONTINUE
              END DO

          END DO
      END DO

      RETURN
      END

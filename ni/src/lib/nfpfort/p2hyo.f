C NCLFORTSTART
      SUBROUTINE P2HYO(PI,MLON,NLAT,KLEVI,XI,PSFC
     +                 ,P0,HYAO,HYBO,KLEVO,XO,XMSG
     +                 ,IFLAG,KFLAG,IER)    
      IMPLICIT NONE

c NCL: xo = p2hyo (p,xi,psfc,hyao,hybo)

c this routine interploates constant pres levels to hybrid
c     the formula for the pressure of a hybrid surface is;
c          phy(k) = pOut(k) = hya(k)*p0 + hyb(k)*psfc
c
c     input  ["i" input ... "o" output]
c          pi     - pressure level                     [input]
c          psfc   - is the surface pressure Pa         [input]
c          mlon   - longitude dimension
c          nlat   - latitude  dimension
c          klevi  - number of input  levels
c          hyao   - is the "a" or pressure hybrid coef
c          hybo   - is the "b" or sigma coeficient
c          klevo  - number of output levels
c          kflag  - specify how values outside the "pi" will be handled
c                   By "outside" I mean [pOut < pi(1)] or [pOut > pi(klevi)]
c                   Extrapolation is via log-linear extrapolation.
c                   =0   no extrapolation. Values set to _FillValue
c                   =1   values set to nearest valid value
c                   =2   values at pOut less    than pi(1) set to nearest value
c                        values at pOut greater than pi(klevi) are extrapolated 
c                   =3   values at pOut less    than pi(1) are extrapolated
c                        values at pOut greater than pi(klevi) set to nearest value 
c                   =4   values at pOut less    than pi(1) are extrapolated
c                        values at pOut greater than pi(klevi) are extrapolated
c               
c          ier    - error code  [=0 no error detected]
c                               [.ne.0 error detected: one or both
c                                      pressure arrays not top->bot order]
c     output
c          iflag  - indicates whether missing values in output
c          xo     - pressure at hybrid levels [Pa]
c                                                 ! input
      INTEGER MLON,NLAT,KLEVI,KLEVO,IFLAG,KFLAG,IER
      DOUBLE PRECISION P0,PI(KLEVI),PSFC(MLON,NLAT),XI(MLON,NLAT,KLEVI),
     +                 HYAO(KLEVO),HYBO(KLEVO),XMSG
c                                                 ! output
      DOUBLE PRECISION XO(MLON,NLAT,KLEVO)
C NCLEND
c                                                 ! local (automatic)
      DOUBLE PRECISION PO(KLEVO)

      IFLAG = 0
c                                                 ! ? input asending order     
      IER   = 0
      IF (PI(1).GT.PI(KLEVI)) THEN
          IER = 1
      END IF

      PO(1)     = HYAO(1)*P0     + HYBO(1)*PSFC(1,1)
      PO(KLEVO) = HYAO(KLEVO)*P0 + HYBO(KLEVO)*PSFC(1,1)
c                                                 ! ? output ascending order     
      IF (PO(1).GT.PO(KLEVO)) THEN
          IER = 20 + IER
      END IF

      IF (IER.NE.0) RETURN

      CALL P2HYB(PI,MLON,NLAT,KLEVI,XI,PSFC,P0,HYAO,HYBO,KLEVO,XO,PO 
     +          ,IFLAG, KFLAG, XMSG)
      RETURN
      END

C NCLFORTSTART
      SUBROUTINE P2HYB(PI,MLON,NLAT,KLEVI,XI,PSFC,P0,HYAO,HYBO,KLEVO 
     +                ,XO,PO,IFLAG, KFLAG, XMSG)
      IMPLICIT NONE
c                                                 ! input
      INTEGER MLON,NLAT,KLEVI,KLEVO,IFLAG, KFLAG
      DOUBLE PRECISION P0,HYAI(KLEVI),HYBI(KLEVI),HYAO(KLEVO),
     +                 HYBO(KLEVO),PSFC(MLON,NLAT),XI(MLON,NLAT,KLEVI),
     +                 PI(KLEVI),PO(KLEVO),XMSG
c                                                 ! output
      DOUBLE PRECISION XO(MLON,NLAT,KLEVO)
C NCLEND
c                                                 ! local
      INTEGER NL,ML,KI,KO
      DOUBLE PRECISION PIMIN, PIMAX, POMIN, POMAX, DXDP
c f77
      PIMIN = PI(1)   
      PIMAX = PI(KLEVI)

      DO NL = 1,NLAT
        DO ML = 1,MLON

          DO KO = 1,KLEVO
             PO(KO) = HYAO(KO)*P0 + HYBO(KO)*PSFC(ML,NL)
          END DO

          POMIN = PO(1)
          POMAX = PO(KLEVO)

          DO KO = 1,KLEVO
             XO(ML,NL,KO) = XMSG
            DO KI = 1,KLEVI-1
               IF (PO(KO).GE.PIMIN .AND. PO(KO).LE.PIMAX ) THEN     

                   IF (PO(KO).GE.PI(KI) .AND. PO(KO).LT.PI(KI+1)) THEN
                       XO(ML,NL,KO) = XI(ML,NL,KI) 
     +                              +(XI(ML,NL,KI+1)-XI(ML,NL,KI))*
     +                               (DLOG(PO(KO))  -DLOG(PI(KI)))/
     +                               (DLOG(PI(KI+1))-DLOG(PI(KI)))
                   END IF
               ELSE
                   IF (KFLAG.EQ.0) THEN
                       IFLAG = 1
                       GO TO 100
                   END IF

                   IF (KFLAG.EQ.1) THEN
                       IF (PO(KO).LT.PIMIN) THEN
                           XO(ML,NL,KO) = XI(ML,NL,1)
                       END IF
                       IF (PO(KO).GT.PIMAX) THEN
                           XO(ML,NL,KO) = XI(ML,NL,KLEVI)
                       END IF
                       GO TO 100
                   END IF

                   IF (KFLAG.EQ.2) THEN
                       IF (PO(KO).LT.PIMIN) THEN
                           XO(ML,NL,KO) = XI(ML,NL,1)   
                       END IF
                       IF (PO(KO).GT.PIMAX) THEN
                           DXDP = (XI(ML,NL,KLEVI)-XI(ML,NL,KLEVI-1))* 
     +                            (DLOG(PI(KLEVI))-DLOG(PI(KLEVI-1)))
                           XO(ML,NL,KO) = XI(ML,NL,KLEVI)
     +                            + (DLOG(PO(KO))-DLOG(PI(KLEVI)))*DXDP   
                       END IF
                       GO TO 100
                   END IF

                   IF (KFLAG.EQ.3) THEN
                       IF (PO(KO).LT.PIMIN) THEN
                           DXDP = (XI(ML,NL,2)-XI(ML,NL,1))* 
     +                            (DLOG(PI(2))-DLOG(PI(1)))
                           XO(ML,NL,KO) = XI(ML,NL,1)
     +                            + (DLOG(PO(KO))-DLOG(PI(1)))*DXDP   
                       END IF
                       IF (PO(KO).GT.PIMAX) THEN
                           XO(ML,NL,KO) = XI(ML,NL,KLEVI)   
                       END IF
                       GO TO 100
                   END IF

                   IF (KFLAG.EQ.4) THEN
                       IF (PO(KO).LT.PIMIN) THEN
                           DXDP = (XI(ML,NL,2)-XI(ML,NL,1))* 
     +                            (DLOG(PI(2))-DLOG(PI(1)))
                           XO(ML,NL,KO) = XI(ML,NL,1)
     +                            + (DLOG(PO(KO))-DLOG(PI(1)))*DXDP   
                       END IF
                       IF (PO(KO).GT.PIMAX) THEN
                           DXDP = (XI(ML,NL,KLEVI)-XI(ML,NL,KLEVI-1))* 
     +                            (DLOG(PI(KLEVI))-DLOG(PI(KLEVI-1)))
                           XO(ML,NL,KO) = XI(ML,NL,KLEVI)
     +                            + (DLOG(PO(KO))-DLOG(PI(KLEVI)))*DXDP   
                       END IF
c c c                  GO TO 100
                   END IF

  100              CONTINUE
               END IF
             END DO
          END DO


        END DO
      END DO

      RETURN
      END

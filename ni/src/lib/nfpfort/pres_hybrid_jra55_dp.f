C NCLFORTSTART
      SUBROUTINE DPHYBRIDJRA55(HYA,HYB,PSFC,MLON,NLAT,KLEV,PHY,PMSG
     +                        ,KLEVI,PI)  

c NCL: phy = pres_hybrid_jra55 (psfc,hya,hyb)

c this routine iuses the JRA55 hybrid interface coordinate data
c     to derive pressure coordinates.

c source: http://rda.ucar.edu/datasets/ds628.0/docs/JRA-55.handbook_TL319_en.pdf
c         pgs 18-19

c     input
c          hya    - is the "a" *interface* pressure hybrid coef
c          hyb    - is the "b" *interface* sigma coeficient
c          psfc   - is the surface pressure in pascals [Pa]
c          mlon   - longitude dimension
c          nlat   - latitude  dimension
c          klev   - number of levels
c          pi     - work array containing pressure at interface levels
c          klevi  - number of interface levels (klev+1)
c     output
c          phy    - mid-layer pressures at hybrid levels [Pa]

      IMPLICIT NONE
      INTEGER MLON,NLAT,KLEV,KLEVI
      DOUBLE PRECISION HYA(KLEVI),HYB(KLEVI),PSFC(MLON,NLAT),PMSG
      DOUBLE PRECISION PHY(MLON,NLAT,KLEV), PI(MLON,NLAT,KLEVI)
C NCLEND

      DOUBLE PRECISION DP
      INTEGER KL,NL,ML

c pressure at interfaces levels
     
      DO KL = 1,KLEVI
          DO NL = 1,NLAT
              DO ML = 1,MLON
                 IF (PSFC(ML,NL).NE.PMSG) then
                     PI(ML,NL,KL) = HYA(KL) + HYB(KL)*PSFC(ML,NL)
                 ELSE
                     PI(ML,NL,KL) = PMSG
                 END IF
              END DO
          END DO
      END DO

c f90 do kl=1,klev
c f90    pi(:,:,kl) = hya(kl) + hyb(kl)*psfc(:,:)
c f90 end do
c ---
c use formula in the above pdf doc to get mid-layer pressures
c ---

      DO NL = 1,NLAT
        DO ML = 1,MLON
          DO KL = 1,KLEV
             IF (PSFC(ML,NL).NE.PMSG) THEN
                 DP = PI(ML,NL,KL)-PI(ML,NL,KL+1)
                 PHY(ML,NL,KL) = 
     +               EXP((1/DP)*(PI(ML,NL,KL  )*LOG(PI(ML,NL,KL  )) 
     +                          -PI(ML,NL,KL+1)*LOG(PI(ML,NL,KL+1)))-1)      
             ELSE
                 PHY(ML,NL,KL) = PMSG
             END IF
          END DO
        END DO
      END DO

c force 

      DO NL = 1,NLAT
        DO ML = 1,MLON
           PHY(ML,NL,KLEV) = 10d0
        END DO
      END DO

      RETURN
      END

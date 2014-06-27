C NCLFORTSTART
      SUBROUTINE DMIXHUM1(P,TK,RH,ISWIT,QW)
      IMPLICIT NONE
C NCL: qw = mixhum_ptrh(p:numeric,tk:numeric,rh:numeric,iswit:integer)

c input
      INTEGER ISWIT
      DOUBLE PRECISION P,TK,RH
c output
      DOUBLE PRECISION QW
c NCLEND

c computes qw (specific humidity or mixing ratio) from p, t and rh

c definition of mixing ratio
c if,
c    es  - is the saturation mixing ratio
c    ep  - is the ratio of the molecular weights of water vapor to 
c          dry air
c    p   - is the atmospheric pressure
c    rh  - is the relative humidity (given as a percent)

c    q =   rh * ( (ep*es)/(p-es) )

c input-
c    p      - pressure (hPa or mb)
c    tk     - temperature (k)
c    rh     - rel hum in percent
c    iswit  - if iswit=1 qw will contain mix ratio
c                iswit=2 qw will contain spc humidity
c                if iswit is negative the units will be kg/kg else g/kg

c output-
c    qw     - mixing ratio or specific hum

c local
      DOUBLE PRECISION T0,EP,ONEMEP,ES0,A,B,EST,QST
      DATA T0,EP,ONEMEP,ES0,A,B/273.15D0,0.622D0,0.378D0,6.11D0,
     +     17.269D0,35.86D0/

      EST = ES0*EXP((A* (TK-T0))/ (TK-B))
      QST = (EP*EST)/ (P-ONEMEP*EST)
c                                    mix ratio
      QW = QST* (RH*.01D0)
c                                    ?specific hum?
      IF (ABS(ISWIT).EQ.2) THEN
          QW = QW/ (1.D0+QW)
      END IF

      IF (ISWIT.LT.0) THEN
          QW = QW*1000.D0
      END IF

      RETURN
      END

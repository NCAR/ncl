C NCLFORTSTART
      SUBROUTINE DRELHUMI(P,TK,QW, RH)
      IMPLICIT NONE
C NCL: qw = relhum_ice(p:numeric,tk:numeric,qw:numeric)

c"Improved Magnus' Form Approx. of Saturation Vapor pressure"
c Oleg A. Alduchov and Robert E. Eskridge
c  http://www.osti.gov/scitech/servlets/purl/548871/
c  One of Three best approx for +50 to -80 C max relative errors of (0.337/0.823) ?

c input
      DOUBLE PRECISION P,TK,QW
c output
      DOUBLE PRECISION RH
c NCLEND

c computes rh (relative humidity) from p, t and qw (mixing ratio)

c definition of mixing ratio
c if,
c    es  - is the saturation mixing ratio
c    ep  - is the ratio of the molecular weights of water vapor to
c          dry air
c    p   - is the atmospheric pressure
c    rh  - is the relative humidity (given as a percent)

c    rh =  100*  q / ( (ep*es)/(p-es) )

c input-
c    p      - pressure (Pa) (this function was originally written for 
c             hPa/mb, so we have to do conversion below)
c    tk     - temperature (k)
c    qw     - mixing ratio (kg/kg)

c output-
c    rh     - relative humidity as %

c local
      DOUBLE PRECISION T0,EP,ONEMEP,ES0,A,B,EST,QST
      DATA T0,EP,ONEMEP,ES0,A,B/273.15D0,0.622D0,0.378D0,6.1128D0,
     +     22.571D0,273.71D0/

      EST = ES0*EXP((A* (TK-T0))/ ((TK-T0)+B))
      QST = (EP*EST)/ ((P*0.01D0)-ONEMEP*EST)
c                                    mix ratio
      RH =  100* (QW / QST)

      RETURN
      END

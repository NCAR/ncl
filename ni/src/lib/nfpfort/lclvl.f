C NCLFORTSTART
      SUBROUTINE DLCLPRS(P,TK,TDK,PLCL)
      IMPLICIT NONE

C NCL:  plcl = lclvl (p,tk,tdk)

c input
      DOUBLE PRECISION P,TK,TDK
c output
      DOUBLE PRECISION PLCL
c NCLEND

c     g.s. stipanuk     1973      	  original version.
c     reference stipanuk paper entitled:
c            "algorithms for generating a skew-t, log p
c          diagram and computing selected meteorological
c          quantities."
c          atmospheric sciences laboratory
c          u.s. army electronics command
c          white sands missile range, new mexico 88002
c          33 pages
c
c   this function returns the pressure plcl (mb/hPa) of the lifting
c   condensation level (lcl) for a parcel initially at temperature
c   tk (K) dew point tdk (K) and pressure p (mb/hPa). plcl is computed
c   by an iterative procedure described by eqs. 8-12 in stipanuk
c   (1973), pp.13-14.

      INTEGER I
      DOUBLE PRECISION TNOT,RCP,TC,TDC,ES,ES0,WS,PTK,X,PI
      DOUBLE PRECISION C1,C2,C3,C4,C5,C6,TDAC,TMRK,TMRC,Z

      DATA TNOT/273.15D0/
      DATA RCP/0.286D0/
      DATA ES0/6.1121D0/
      DATA C1/.0498646455D0/,C2/2.4082965D0/,C3/7.07475D0/
      DATA C4/38.9114D0/,C5/.0915D0/,C6/1.2035D0/

      TC = TK - TNOT
      TDC = TDK - TNOT

c   determine the mixing ratio line through td and p.
c
c   the "es" statement returns the saturation vapor pressure es (mb)
c   over liquid water given a temperature t (celsius). the formula
c   appears bolton, david, 1980: "the computation of equivalent
c   potential temperature," monthly weather review, vol. 108,
c   no. 7 (july), p. 1047, eq.(10). the quoted accuracy is 0.3%
c   or better for -35 < t < 35c.

      ES = ES0*EXP(17.67D0*TDC/ (TDC+243.5D0))

c   saturation mix ratio

      WS = 622.D0*ES/ (P-ES)

c   determine the dry adiabat through t and p (ie: potential
c   temperature)

      PTK = TK* ((1000.D0/P)**RCP)

c   iterate to locate pressure pi at the intersection of the two
c   curves. pi has been set to p for the initial guess.

c   tdac - temp [C] on a dry adiabat at p [mb/hPa] where the dry
c          adibat is given by the potential temperature.
c   tmrc - temperature on a mix ratio line [ws] at pressure p [hPa]

    3 PI = P
      DO I = 1,10
          TDAC = PTK* ((PI/1000.D0)**RCP) - TNOT

          Z = DLOG10(WS*PI/ (622.D0+WS))
          TMRK = 10.D0** (C1*Z+C2) - C3 +
     +           C4* ((10.D0** (C5*Z)-C6)**2.D0)
          TMRC = TMRK - TNOT

          X = .02D0* (TMRC-TDAC)
          IF (ABS(X).LT.0.01D0) GO TO 5
          PI = PI* (2.D0** (X))
      END DO

    5 PLCL = PI
      RETURN

c       entry alclm(t,td,p)
c   for entry alclm only, t is the mean potential temperature (celsius)
c   and td is the mean mixing ratio (g/kg) of the layer containing the
c   parcel.

c       aw = td
c       ao = t
c       go to 3
      END

c ---------------------------------------------------------------
      SUBROUTINE DHYDRO(P,TKV,ZSFC,NLVL,ZH,IER)
      IMPLICIT NONE

c NCL: zh = hydro (p,tkv,zsfc)

c use the hydrostatic eqn to get geopotential height
c .   it is assumed that p(1) and tkv(1) contain the
c .   surface quantities corresponding to zsfc.

c .   missing data not allowed
C input

C no. levels; error code
      INTEGER NLVL,IER
C pres (Pa)
C temp (K) at each "p"
C sfc geopotential height (gpm)
      DOUBLE PRECISION P(NLVL),TKV(NLVL),ZSFC
C output

C calculated geopotential (gpm)
      DOUBLE PRECISION ZH(NLVL)
C local

      INTEGER NL
      DOUBLE PRECISION G,RDAIR,RDAG,TVBAR

c     --------
      IER = 0
      IF (NLVL.LT.1) THEN
C unrealistic nlvl value
          IER = -1
          RETURN
      END IF

C gravity at 45 deg lat used by the WMO
      G = 9.80665D0
C gas const dry air (j/{kg-k})
      RDAIR = 287.04D0
      RDAG = RDAIR/G

c calculate geopotential height if initial z available [hydrostatic eq]

      ZH(1) = ZSFC
      DO NL = 2,NLVL
C same as [ln(p(nl)+ln(p(nl-1)]
          TVBAR = (TKV(NL)*DLOG(P(NL))+TKV(NL-1)*DLOG(P(NL-1)))/
     +            DLOG(P(NL)*P(NL-1))
          ZH(NL) = ZH(NL-1) + RDAG*TVBAR*DLOG(P(NL-1)/P(NL))
      END DO

      RETURN
      END

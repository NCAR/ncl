C NCLFORTSTART
      SUBROUTINE DDEWTEMP(TK,RH,TDK)
C     NCL: tdk = dewtemp_trh (tk:numeric, rh:numeric)

c calculate the dew pt temperature [k]:
c input:
c     rh - relative humidty [%]
c     tk - temperature [k]
c output:
c     tdk- dew point temperature
c          equation used is Dutton p274 (6) [k]

      IMPLICIT NONE
      DOUBLE PRECISION RH,TK
      DOUBLE PRECISION TDK
c NCLEND

c local:
c     gc  - gas constant for water vapor [j/{kg-k}]
c           gcx [cal/{g-k}]
c     lhv - latent heat vap (Dutton top p273)

      DOUBLE PRECISION GC,GCX,LHV
      PARAMETER (GC=461.5D0)
      PARAMETER (GCX=GC/ (1000.D0*4.186D0))


      LHV = (597.3D0-0.57D0* (TK-273.D0))/GCX
      TDK = TK*LHV/ (LHV-TK*DLOG(RH*0.01D0))

      RETURN
      END

C NCLFORTSTART
      SUBROUTINE DWMRQ (P,TD,TDMSG,NMAX,WMR,ISWIT)
      IMPLICIT NONE
      INTEGER  NMAX, ISWIT
      DOUBLE PRECISION P(NMAX), TD(NMAX),TDMSG
      DOUBLE PRECISION WMR(NMAX)
C NCLEND    
      DOUBLE PRECISION DWMRSKEWT 
      EXTERNAL DWMRSKEWT

C ncl:   q = mixhum_ptd (p,td,option)   [ q=g/kg ]
c                        p  - PA
c                        td - K
C local
      INTEGER  N  
      DOUBLE PRECISION T0, PA2MB
      DATA     T0    /273.15d0/
      DATA     PA2MB /0.01d0  /

C mixing ratio (kg/kg)
c the function wants hPA (mb) and degrees centigrade
      
      DO N=1,NMAX
         IF (TD(N).NE.TDMSG) then
             WMR(N) = DWMRSKEWT(P(N)*PA2MB,(TD(N)-T0))*0.001d0  
         ELSE
             WMR(N) = TDMSG
         END IF
      END DO

c if ISWIT=2 calculate specific humidity (kg/kg)
      
      IF (ABS(ISWIT).EQ.2) THEN
          DO N=1,NMAX
             IF (WMR(N).NE.TDMSG) THEN
                 WMR(N) = (WMR(N)/(WMR(N)+1.d0))
             END IF 
         END DO
      END IF

c if ISWIT < 0 then return g/kg
      
      IF (ISWIT.LT.0) THEN
          DO N=1,NMAX
             IF (WMR(N).NE.TDMSG) THEN
                 WMR(N) = WMR(N)*1000.d0
             END IF 
         END DO
      END IF
      
      RETURN
      END


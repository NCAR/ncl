C Copyright 1981-2007 ECMWF
C 
C Licensed under the GNU Lesser General Public License which
C incorporates the terms and conditions of version 3 of the GNU
C General Public License.
C See LICENSE and gpl-3.0.txt for details.
C

 
      SUBROUTINE SCM0   (PDL,PDR,PFL,PFR,KLG)
C
C---->
C**** SCM0   - Apply SCM0 limiter to derivative estimates.
C
C    M. HORTAL    ECMWF February 1991  closely following D. WILLIAMSON
C
C    Apply SCM0 limiter to derivative estimates.
C
C   output:
C     pdl   = the limited derivative at the left edge of the interval
C     pdr   = the limited derivative at the right edge of the interval
C
C   inputs
C     pdl   = the original derivative at the left edge
C     pdr   = the original derivative at the right edge
C     pfl   = function value at the left edge of the interval
C     pfr   = function value at the right edge of the interval
C     klg  = number of intervals where the derivatives are limited
C
C----<
      INTEGER KLG
C
      REAL PDL(KLG), PDR(KLG), PFL(KLG), PFR(KLG)
C
      INTEGER JL
C      
      REAL ZALPHA, ZBETA, ZEPS, ZFAC
C
  100 CONTINUE
C
C    define constants
C
      ZEPS=1.E-12
      ZFAC=3.*(1.-ZEPS)
C
      DO 200 JL=1,KLG
      IF(ABS(PFR(JL)-PFL(JL)).GT.ZEPS) THEN
          ZALPHA=PDL(JL)/(PFR(JL)-PFL(JL))
          ZBETA =PDR(JL)/(PFR(JL)-PFL(JL))
          IF(ZALPHA.LE.0.) PDL(JL)=0.
          IF(ZBETA .LE.0.) PDR(JL)=0.
          IF(ZALPHA.GT.ZFAC) PDL(JL)=ZFAC*(PFR(JL)-PFL(JL))
          IF(ZBETA .GT.ZFAC) PDR(JL)=ZFAC*(PFR(JL)-PFL(JL))
      ELSE
          PDL(JL)=0.
          PDR(JL)=0.
      ENDIF
  200 CONTINUE
      END

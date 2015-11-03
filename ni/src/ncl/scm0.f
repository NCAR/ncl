C Copyright 1981-2012 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
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

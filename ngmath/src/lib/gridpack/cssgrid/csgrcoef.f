C
C	$Id: csgrcoef.f,v 1.5 2008-07-27 03:10:07 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CSGRCOEF (SIGMA, D,SD)
      DOUBLE PRECISION SIGMA, D, SD
C
C***********************************************************
C
C                                              From SSRFPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   11/21/96
C
C   This subroutine computes factors involved in the linear
C systems solved by Subroutines CSGRADG and CSSMSGS.
C
C On input:
C
C       SIGMA = Nonnegative tension factor associated with a
C               triangulation arc.
C
C SIGMA is not altered by this routine.
C
C On output:
C
C       D = Diagonal factor.  D = SIG*(SIG*Coshm(SIG) -
C           Sinhm(SIG))/E where E = SIG*Sinh(SIG) - 2*
C           Coshm(SIG).  D > 0, and D = 4 at SIG = 0.
C
C       SD = Off-diagonal factor.  SD = SIG*Sinhm(SIG)/E.
C            SD > 0, and SD = 2 at SIG = 0.
C
C SSRFPACK module required by CSGRCOEF:  CSSNHCSH
C
C Intrinsic function called by CSGRCOEF:  EXP
C
C***********************************************************
C
      DOUBLE PRECISION COSHM, COSHMM, E, EMS, SCM, SIG,
     .                 SINHM, SSINH, SSM
      SIG = SIGMA
      IF (SIG .LT. 1.D-9) THEN
C
C Cubic function:
C
        D = 4.
        SD = 2.
      ELSEIF (SIG .LE. .5) THEN
C
C 0 < SIG .LE. .5.
C
C Use approximations designed to avoid cancellation error
C   in the hyperbolic functions when SIGMA is small.
C
        CALL CSSNHCSH (SIG, SINHM,COSHM,COSHMM)
        E = SIG*SINHM - COSHMM - COSHMM
        D = SIG*(SIG*COSHM-SINHM)/E
        SD = SIG*SINHM/E
      ELSE
C
C SIG > .5.
C
C Scale SINHM, COSHM, and E by 2*EXP(-SIG) in order to
C   avoid overflow when SIGMA is large.
C
        EMS = EXP(-SIG)
        SSINH = 1. - EMS*EMS
        SSM = SSINH - 2.*SIG*EMS
        SCM = (1.-EMS)*(1.-EMS)
        E = SIG*SSINH - SCM - SCM
        D = SIG*(SIG*SCM-SSM)/E
        SD = SIG*SSM/E
      ENDIF
      RETURN
      END

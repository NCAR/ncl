C
C	$Id: mstrms.f,v 1.1.1.1 1992-04-17 22:32:39 ncargd Exp $
C
      SUBROUTINE MSTRMS (DIAG,SDIAG,SIGMA,DEL)
C
C ---------------------------------------------------------------------
C Note:  This routine comes from a proprietary package called FITPACK.
C It is used in the NCAR graphics package by permission of the author,
C Alan Cline.
C ---------------------------------------------------------------------
C
C                                            CODED BY ALAN KAYLOR CLINE
C                                         FROM FITPACK -- JUNE 22, 1986
C                                   A CURVE AND SURFACE FITTING PACKAGE
C                                 A PRODUCT OF PLEASANT VALLEY SOFTWARE
C                             8603 ALTUS COVE, AUSTIN, TEXAS 78759, USA
C
C ---------------------------------------------------------------------
C
C THIS SUBROUTINE COMPUTES THE DIAGONAL AND SUPERDIAGONAL
C TERMS OF THE TRIDIAGONAL LINEAR SYSTEM ASSOCIATED WITH
C SPLINE UNDER TENSION INTERPOLATION.
C
C ON INPUT--
C
C   SIGMA CONTAINS THE TENSION FACTOR.
C
C   DEL CONTAINS THE STEP SIZE.
C
C ON OUTPUT--
C
C                SIGMA*DEL*COSH(SIGMA*DEL) - SINH(SIGMA*DEL)
C   DIAG = DEL*--------------------------------------------.
C                     (SIGMA*DEL)**2 * SINH(SIGMA*DEL)
C
C                   SINH(SIGMA*DEL) - SIGMA*DEL
C   SDIAG = DEL*----------------------------------.
C                (SIGMA*DEL)**2 * SINH(SIGMA*DEL)
C
C   SIGMA AND DEL ARE UNALTERED.
C
C THIS SUBROUTINE REFERENCES PACKAGE MODULE MSSHCH.
C
C-----------------------------------------------------------
C
      IF (SIGMA .NE. 0.) GO TO 1
      DIAG = DEL/3.
      SDIAG = DEL/6.
      RETURN
    1 SIGDEL = SIGMA*DEL
      CALL MSSHCH (SINHM,COSHM,SIGDEL,0)
      DENOM = SIGMA*SIGDEL*(1.+SINHM)
      DIAG = (COSHM-SINHM)/DENOM
      SDIAG = SINHM/DENOM
      RETURN
      END

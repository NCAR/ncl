C
C $Id: slbkgd.f,v 1.1 1993-01-14 00:29:39 kennison Exp $
C
      SUBROUTINE SLBKGD
C
C  Plot background if the user has set the BGC parameter.
C
C The labeled common block SLCOMN holds all of the internal parameters
C for the STITLE package.
C
      COMMON /SLCOMN/ ICU,ICO,PCHSZ,GAPSZ,T1,T2,NXST,NXFIN,ICRTJP,
     +                LIM(4),IBKG,LND,BGCLR(3),FGCLR(3),IFST,IWK,FIN,
     +                FOU,ISPB,ISPF,IDEN,IWU,IMAP,OORV
      SAVE   /SLCOMN/
C
      DIMENSION XL(4),YL(4)
C
      XL(1) = REAL(LIM(1)/32767.)
      XL(2) = REAL(LIM(2)/32767.)
      XL(3) = REAL(LIM(2)/32767.)
      XL(4) = REAL(LIM(1)/32767.)
      YL(1) = REAL(LIM(3)/32767.)
      YL(2) = REAL(LIM(3)/32767.)
      YL(3) = REAL(LIM(4)/32767.)
      YL(4) = REAL(LIM(4)/32767.)
C
      CALL GQFAIS(IERR,ISFS)
      IF (IERR.NE.0) THEN
        WRITE(I1MACH(4),1001) IERR
        STOP
      END IF
      CALL GSFAIS(1)
C
      CALL GQFACI(IERR,ISCI)
      IF (IERR.NE.0) THEN
        WRITE(I1MACH(4),1002) IERR
        STOP
      END IF
      CALL GSFACI(IBKG)
C
      CALL GFA(4,XL,YL)
C
      CALL GSFAIS(ISFS)
      CALL GSFACI(ISCI)
C
      RETURN
C
 1001 FORMAT (' SLBKGD - Error flag returned by GQFAIS - ',I8)
 1002 FORMAT (' SLBKGD - Error flag returned by GQFACI - ',I8)
C
      END

C
C	$Id: bkgnd.f,v 1.1.1.1 1992-04-17 22:33:00 ncargd Exp $
C
      SUBROUTINE BKGND
C
C  Plot background if the user has set the BGC parameter.
C
C
C The labeled common block SCRLDT holds all of the internal parameters
C for the STITLE package.
C
      COMMON /SCRLDT/ ICU,ICO,PCHSZ,GAPSZ,T1,T2,NXST,NXFIN,ICRTJP,
     +                LIM(4),MXOLD,MYOLD,LOLD,IBKG,LND,BGCLR(3),
     +                FGCLR(3),IFST,IWK,FIN,FOU,ISPB,ISPF,IDEN,IWU
      SAVE   /SCRLDT/
C
      DIMENSION XL(4),YL(4)
      XL(1) = REAL(LIM(1)/32767.)
      XL(2) = REAL(LIM(2)/32767.)
      XL(3) = REAL(LIM(2)/32767.)
      XL(4) = REAL(LIM(1)/32767.)
      YL(1) = REAL(LIM(3)/32767.)
      YL(2) = REAL(LIM(3)/32767.)
      YL(3) = REAL(LIM(4)/32767.)
      YL(4) = REAL(LIM(4)/32767.)
      CALL GSFAIS(1)
      CALL GSFACI(IBKG)
      CALL GFA(4,XL,YL)
C
      RETURN
      END

C
C $Id: slownd.f,v 1.1 1993-01-14 00:29:54 kennison Exp $
C
      SUBROUTINE SLOWND
C
C Draw the outline of the windowed area.
C
C The labeled common block SLCOMN holds all of the internal parameters
C for the STITLE package.
C
      COMMON /SLCOMN/ ICU,ICO,PCHSZ,GAPSZ,T1,T2,NXST,NXFIN,ICRTJP,
     +                LIM(4),IBKG,LND,BGCLR(3),FGCLR(3),IFST,IWK,FIN,
     +                FOU,ISPB,ISPF,IDEN,IWU,IMAP,OORV
      SAVE   /SLCOMN/
C
      CALL PLOTIT (LIM(1),LIM(3),0)
      CALL PLOTIT (LIM(2),LIM(3),1)
      CALL PLOTIT (LIM(2),LIM(4),1)
      CALL PLOTIT (LIM(1),LIM(4),1)
      CALL PLOTIT (LIM(1),LIM(3),1)
      RETURN
      END

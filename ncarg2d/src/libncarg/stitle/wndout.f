C
C	$Id: wndout.f,v 1.1.1.1 1992-04-17 22:33:02 ncargd Exp $
C
      SUBROUTINE WNDOUT
C
C Draw the outline of the windowed area.
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
      CALL PLOTIT (LIM(1),LIM(3),0)
      CALL PLOTIT (LIM(2),LIM(3),1)
      CALL PLOTIT (LIM(2),LIM(4),1)
      CALL PLOTIT (LIM(1),LIM(4),1)
      CALL PLOTIT (LIM(1),LIM(3),1)
      RETURN
      END

C
C $Id: slinit.f,v 1.2 1993-01-14 00:29:50 kennison Exp $
C
      SUBROUTINE SLINIT
C
C Does initialization.
C
      COMMON /SLCOMN/ ICU,ICO,PCHSZ,GAPSZ,T1,T2,NXST,NXFIN,ICRTJP,
     +                LIM(4),IBKG,LND,BGCLR(3),FGCLR(3),IFST,IWK,FIN,
     +                FOU,ISPB,ISPF,IDEN,IWU,IMAP,OORV
      SAVE   /SLCOMN/
C
C Get background color and store in BGCLR, this color is taken from
C the first active workstation.
C
      CALL GQACWK(1,IER,IOL,IWK)
      IF (IOL .LT. 1) THEN
        WRITE(I1MACH(4),500)
        STOP
      ENDIF
      CALL GQCR(IWK,0,0,IER,BGCLR(1),BGCLR(2),BGCLR(3))
C
C If the background color has not been set or another error state
C obtains, set the background color to black.
C
      IF (IER .NE. 0) THEN
        BGCLR(1) = 0.
        BGCLR(2) = 0.
        BGCLR(3) = 0.
      ENDIF
C
C Get foreground color and store in FGCLR, this color is taken from
C the first active workstation.
C
      CALL GQCR(IWK,1,0,IER,FGCLR(1),FGCLR(2),FGCLR(3))
C
C If the foreground color has not been set or another error state obtains,
C set the foreground color to white.
C
      IF (IER .NE. 0) THEN
        FGCLR(1) = 1.
        FGCLR(2) = 1.
        FGCLR(3) = 1.
      ENDIF
C
      RETURN
C
  500 FORMAT(' SLINIT -- No active workstations.')
C
      END

C
C $Id: slblda.f,v 1.1 1993-01-14 00:29:42 kennison Exp $
C
      BLOCK DATA SLBLDA
C
C The labeled common block SLCOMN holds all of the internal parameters
C for the STITLE package.
C
      COMMON /SLCOMN/ ICU,ICO,PCHSZ,GAPSZ,T1,T2,NXST,NXFIN,ICRTJP,
     +                LIM(4),IBKG,LND,BGCLR(3),FGCLR(3),IFST,IWK,FIN,
     +                FOU,ISPB,ISPF,IDEN,IWU,IMAP,OORV
      SAVE   /SLCOMN/
C
C Define the card-input unit for FTITLE.
C
      DATA ICU    /   5 /
C
C Define the centering option for lines written by FTITLE.
C
      DATA ICO    /   1 /
C
C Define the character size assumed by FTITLE.
C
      DATA PCHSZ  / 21. /
C
C Define the interline spacing assumed by FTITLE.
C
      DATA GAPSZ  / 40. /
C
C Define the blank-frame gap lengths used by FTITLE.
C
      DATA T1     / 1.0 /
      DATA T2     / 0.5 /
C
C Define the start and finish coordinates for STITLE in the
C X direction.
C
      DATA NXST   / 512 /
      DATA NXFIN  / 512 /
C
C Define the distance between centers of practice frames.
C
      DATA ICRTJP / 300 /
C
C Define the viewport.
C
      DATA LIM(1),LIM(2),LIM(3),LIM(4) / 0 , 32767 , 0 , 32767 /
C
C Define the default background color index.
C
      DATA IBKG/0/
C
C Define the flag to control the drawing of alignment frames with dots
C in the corners.
C
      DATA LND/0/
C
C Flag to indicate if SLINIT has been called.
C
      DATA IFST/0/
C
C Set default fade-in, fade-out times.
C
      DATA FIN,FOU/0.,0./
C
C Suppression flags for background and foreground fades.
C
      DATA ISPB,ISPF/0,0/
C
C Workstation ID and logical unit for WISS.
C
      DATA IDEN,IWU/9,4/
C
C PLOTCHAR mapping flag and out-of-range indicator.
C
      DATA IMAP,OORV / 100 , 1.E12 /
C
      END

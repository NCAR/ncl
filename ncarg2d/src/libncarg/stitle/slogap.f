C
C $Id: slogap.f,v 1.1 1993-01-14 00:29:52 kennison Exp $
C
      SUBROUTINE SLOGAP (T,MOVIE)
C
C The labeled common block SLCOMN holds all of the internal parameters
C for the STITLE package.
C
      COMMON /SLCOMN/ ICU,ICO,PCHSZ,GAPSZ,T1,T2,NXST,NXFIN,ICRTJP,
     +                LIM(4),IBKG,LND,BGCLR(3),FGCLR(3),IFST,IWK,FIN,
     +                FOU,ISPB,ISPF,IDEN,IWU,IMAP,OORV
      SAVE   /SLCOMN/
C
C Provide a T-second gap in the movie (or a practice frame saying so).
C
      CHARACTER*16 IDUM
C
C NFRM is the number of blank frames required for a T-second gap.
C
      NFRM = INT(24.*T+.5)
C
C Nothing to do if a non-positive number of frames is specified.
C
      IF (NFRM.LE.0) RETURN
C
C Output depends on whether this is a real run or a practice run.
C
      IF (MOVIE.EQ.0) THEN
C
C Real run - output NFRM blank frames.
C
        IBKGO = IBKG
        IF (ISPB .EQ. 0) IBKG = 0
C
        DO 101 IFRM=1,NFRM
          CALL SLBKGD
          CALL FRAME
  101   CONTINUE
C
        IBKG = IBKGO
C
      ELSE
C
C Practice run - output a single frame saying how many blank frames it
C stands for.
C
        WRITE(IDUM,'(I3)') NFRM
        IDUM(4:16) = ' BLANK FRAMES'
        CALL PWRIT  (.5,.5,IDUM,16,2,0,0)
        CALL FRAME
C
      END IF
C
C Done.
C
      RETURN
C
      END

C
C	$Id: gap.f,v 1.1.1.1 1992-04-17 22:33:00 ncargd Exp $
C
      SUBROUTINE GAP (T,MOVIE)
C
C The labeled common block SCRLDT holds all of the internal parameters
C for the STITLE package.
C
      COMMON /SCRLDT/ ICU,ICO,PCHSZ,GAPSZ,T1,T2,NXST,NXFIN,ICRTJP,
     +                LIM(4),MXOLD,MYOLD,LOLD,IBKG,LND,BGCLR(3),
     +                FGCLR(3),IFST,IWK,FIN,FOU,ISPB,ISPF,IDEN,IWU
      SAVE   /SCRLDT/
C
C Provide a T-second gap in the movie (or a practice frame saying so).
C
      CHARACTER*16 IDUM
C
C NFRM is the number of blank frames required for a T-second gap.
C
      NFRM = IFIX(24.*T+.5)
C
C Nothing to do if a non-positive number of frames is specified.
C
      IF (NFRM.LE.0) RETURN
C
C Output depends on whether this is a real run or a practice run.
C
      IF (MOVIE.NE.0) GO TO 102
C
C Real run - output NFRM blank frames.
C
         IBKGO = IBKG
         IF (ISPB .EQ. 0) IBKG = 0
         DO 101 IFRM=1,NFRM
         CALL BKGND
         CALL FRAME
  101    CONTINUE
         IBKG = IBKGO
C
      RETURN
C
C Practice run - output a single frame saying how many blank frames it
C stands for.
C
  102 CONTINUE
      WRITE(IDUM,200) NFRM
  200 FORMAT(I3)
      IDUM(4:16) = ' BLANK FRAMES'
      CALL PWRIT  (.5,.5,IDUM,16,2,0,0)
      CALL FRAME
C
      RETURN
C
      END

C
C $Id: ftitle.f,v 1.2 1993-01-14 00:29:33 kennison Exp $
C
      SUBROUTINE FTITLE (MOVIE)
C
C For a description of this routine, see the comments at the beginning
C of the routine STITLE.
C
C This subroutine limits the number of lines on a given frame to 120.
C If more lines than this are desired, reset the parameter MAXLIN
C in the following PARAMETER statement.
C
      PARAMETER (MAXLIN=120)
      PARAMETER (IBDIM = 5*MAXLIN+14)
      CHARACTER*100 CARDS(MAXLIN)
C
C The labeled common block SLCOMN holds all of the internal parameters
C for the STITLE package.
C
      COMMON /SLCOMN/ ICU,ICO,PCHSZ,GAPSZ,T1,T2,NXST,NXFIN,ICRTJP,
     +                LIM(4),IBKG,LND,BGCLR(3),FGCLR(3),IFST,IWK,FIN,
     +                FOU,ISPB,ISPF,IDEN,IWU,IMAP,OORV
      SAVE   /SLCOMN/
C
C Initialize variables if this is the first user call.
C
      IF (IFST .EQ. 0) THEN
        CALL SLINIT
        IFST = 1
      ENDIF
C
C Output an initial gap of T1 seconds.
C
      CALL SLOGAP (T1,MOVIE)
C
C Read up the first card of the next batch (quit on zero card-count
C or end-of-file).
C
  101 NCARDS = 0
      READ (ICU,'(I5,2F5.1)',END=102) NCARDS,T,SIZE
      IF (NCARDS .EQ. 0) GO TO 102
      IF (NCARDS .GT. MAXLIN) GO TO 901
C
C Compute the vertical size of a line and of the gap between lines.
C
      ISIZ = INT(SIZE*PCHSZ)
      IGAP = INT(SIZE*GAPSZ)
C
      ISUM = ISIZ+IGAP
C
C Set the X/Y coordinates of the first line.
C
      IX = 64+448*ICO
      IY = 512+((NCARDS-1)*ISUM)/2
C
C Set up CARDS array for input to STITLE.
C
      DO 103 I=1,NCARDS
        WRITE(CARDS(I)(1:20),'(3I5,F5.1)') IX,IY,ICO,SIZE
        READ (ICU,'(A80)') CARDS(I)(21:100)
        IY = IY-ISUM
  103 CONTINUE
C
C Call STITLE to actually produce the desired plots.
C
      CALL STITLE (CARDS,NCARDS,512,512,T,0.,0.,MOVIE)
C
C Output a gap and go get the next batch of input cards.
C
      CALL SLOGAP (T2,MOVIE)
C
      GO TO 101
C
C Normal return if end-of-file read.
C
  102 RETURN
C
C Error exit on too many cards in a single batch.
C
  901 WRITE(I1MACH(4),
     +      '('' FTITLE -- NUMBER OF INPUT CARDS EXCEEDS 120'')')
      STOP
C
      END

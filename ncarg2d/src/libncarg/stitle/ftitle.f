C
C	$Id: ftitle.f,v 1.1.1.1 1992-04-17 22:33:05 ncargd Exp $
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
      CHARACTER*80 CARDS(MAXLIN),CTMP
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
C Initialize variables if this is the first user call.
C
      IF (IFST .EQ. 0) THEN
        CALL SLINIT
        IFST = 1
      ENDIF
C
C Output an initial gap of T1 seconds.
C
      CALL GAP (T1,MOVIE)
C
C Read up the first card of the next batch (quit on zero card-count
C or end-of-file).
C
  101 NCARDS = 0
      READ (ICU,1001,END=902) NCARDS,T,SIZE
      IF (NCARDS .EQ. 0) RETURN
      IF (NCARDS .GT. MAXLIN) GO TO 901
C
C Compute the vertical size of a line and of the gap between lines.
C
      ISIZ = IFIX(SIZE*PCHSZ)
      IGAP = IFIX(SIZE*GAPSZ)
C
      ISUM = ISIZ+IGAP
C
C Set the X/Y cOordinates of the first line.
C
      IX = 64+448*ICO
      IY = 512+((NCARDS-1)*ISUM)/2
C
C Set up CARDS array for input to STITLE.
C
         DO 103 I=1,NCARDS
         WRITE(CARDS(I)( 1: 5),1010) IX
         WRITE(CARDS(I)( 6:10),1010) IY
         WRITE(CARDS(I)(11:15),1010) ICO
         WRITE(CARDS(I)(16:20),1011) SIZE
         READ(ICU,1002) CTMP
         CARDS(I)(21:80) = CTMP(1:60)
         IY = IY-ISUM
  103    CONTINUE
C
C Call STITLE to actually produce the desired plots.
C
      CALL STITLE (CARDS,NCARDS,512,512,T,0.,0.,MOVIE)
C
C Output a gap and go get the next batch of input cards.
C
      CALL GAP (T2,MOVIE)
      GO TO 101
C
  901 CONTINUE
      WRITE(I1MACH(4),1003)
      STOP
  902 CONTINUE
      RETURN
C
 1001 FORMAT (I5,2F5.1)
 1002 FORMAT (A80)
 1003 FORMAT (' FTITLE -- NUMBER OF INPUT CARDS EXCEEDS 120')
 1010 FORMAT (I5)
 1011 FORMAT (F5.1)
C
      END

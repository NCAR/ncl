C
C	$Id: gwilod.f,v 1.1 1993-01-09 02:09:34 fred Exp $
C
      SUBROUTINE GWILOD(LIST,BITS,COUNT,GKSERR)
C
C  Load the bit string contained in the low order part of each word.
C
C  INPUT
C    LIST   -  A list of words which have bit strings right justified.
C    BITS   -  Number of bits per word to move.
C    COUNT  -  The repitition count (number of words in the list).
C
C  OUTPUT
C    GKSERR -  The error status.
C
C  All data is type INTEGER unless otherwise indicated.
C
      IMPLICIT INTEGER (A-Z)
C
      DIMENSION LIST(*)
C
C  Common for metafile buffer.
C
      include 'gwiio.h'
C
      SAVE
C
      DATA  ALLOK/0/
C
      IF (COUNT.LE.0)  RETURN
      CTEMP = COUNT
      STRT = 1
C
C  Determine the number of packets of size bits left in buffer.
C
 10   CONTINUE
      BLEFT = (WXBITS-WBFPOS)
      REPLFT = BLEFT/BITS
C
C  Compute how may packets to move into the buffer.
C
      IF (CTEMP .LE. REPLFT) THEN
C
C  Room for all.
C
        CMOVE = CTEMP
        CTEMP = 0
      ELSE
C
C  Not enough room for all bits.
C
        CMOVE = REPLFT
        CTEMP = CTEMP - REPLFT
      END IF
C
C  Move the current bit packets.
C
      CALL SBYTES(WOUTBF,LIST(STRT),WBFPOS,BITS,0,CMOVE)
      WBFPOS = WBFPOS + (BITS * CMOVE)
C
C  Check if more bit packets remaining.
C
      IF (CTEMP.NE.0) THEN
C
C  More remaining, flush the current buffer.
C
        CALL GWIFLB(GKSERR)
        IF (GKSERR .NE. ALLOK) RETURN
        STRT = STRT + CMOVE
        GO TO 10
      END IF
C
      RETURN
      END

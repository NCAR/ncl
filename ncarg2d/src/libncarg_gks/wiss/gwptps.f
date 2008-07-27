C
C	$Id: gwptps.f,v 1.5 2008-07-27 00:21:08 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GWPTPS (BUFFER, COUNT1, COUNT2, CONTIN, GKSERR)
C
C  Put a character string into the metafile buffer.  This subroutine
C  can be called successively to put out long strings--on the
C  first call the total count is put out and on subsequent calls
C  just the characters are put out.
C
C  INPUT
C    BUFFER -- The character string to move (must be type CHARACTER).
C    COUNT1 -- Total number of characters to be processed for the
C              entire sequence.
C    COUNT2 -- Number of characters to be processed this call.
C    CONTIN -- If 0, COUNT1 is put out prior to the string itself;
C              if 1, only the character string is put out.
C  OUTPUT
C    GKSERR -- Error status.
C
C  All data is type integer unless otherwise indicated.
C
      IMPLICIT INTEGER (A-Z)
      DIMENSION CHARS(256)
      CHARACTER*(*) BUFFER
C
      INTEGER  BITSPC
      INTEGER  GKASCI
C
      include 'gwiins.h'
C
      SAVE
C
C  Maximum number of characters allowed in the temporary buffer.
C
      DATA  MXCH/256/
C
C  CGM uses 8-bit ASCII.
C
      DATA  BITSPC/8/,  LSFLG/255/
C
C  First call processing, set up count (or flag/count).
C
      IF (CONTIN .EQ. 0)  THEN
C
C  The number of bytes for the  COUNT+STRING  depends on
C  whether the string is encoded in the long or short form.
C
        IF (COUNT1 .LE. 254)  THEN
          NBCCNT = 8
        ELSE
          NBCCNT = 16
          CALL GWILOD (LSFLG, 8, 1, GKSERR)
          IF (GKSERR .NE. 0)  RETURN
        END IF
        CALL GWILOD (COUNT1, NBCCNT, 1, GKSERR)
        IF (GKSERR .NE. 0)  RETURN
      END IF
C
      CTEMP = COUNT2
      STRT = 1
C
   10 CONTINUE
C
C  Determine the number of characters that will fit
C  in the current partition.
C
      WCBYT = 1 + (MCCBYT*8-1)/BITSPC
C
C  Compute and move the allowed number of characters.
C
      MOVIT = MIN(WCBYT,CTEMP,MXCH)
C
C  Move the character codes to the integer buffer.
C
      DO 20 II = 1,MOVIT
        NP = STRT + II - 1
C
C  Get the ASCII equivalent of the character code.
C
        CHARS(II) = GKASCI (ICHAR(BUFFER(NP:NP)))
   20 CONTINUE
C
      CALL GWILOD (CHARS, BITSPC, MOVIT, GKSERR)
      IF (GKSERR .NE. 0) RETURN
C
C  Check if another partition has to be started.
C
      CTEMP = CTEMP - MOVIT
      MCCBYT = MCCBYT - (MOVIT*BITSPC)/8
      IF (CTEMP .NE. 0) THEN
C
C  Check if more room in partition.
C
        IF (MCCBYT .NE. 0) THEN
C
C  More room in the current partition.
C
          STRT = STRT + MOVIT
        ELSE
C
C  New partition required, so set up the instruction.
C
          STRT = STRT + MOVIT
C  Take the remainder of the bytes left in the current partition
C  (they must be used).
          TCBYT = MCNBYT + MCCBYT
          CALL GWPART (TCBYT, GKSERR)
          IF (GKSERR .NE. 0) RETURN
        END IF
C
C  Move more operands into new partition.
C
        GO TO 10
      END IF
C
      RETURN
      END

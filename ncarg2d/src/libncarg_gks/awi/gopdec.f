C
C	$Id: gopdec.f,v 1.4 2008-07-27 00:20:58 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GOPDEC(OPRNS,OPNLEN,COUNT,IOS,STATUS)
C
C  This subroutine extracts the requested number of operands from the
C  input segment and decodes them.
C
C  INPUT
C    OPNLEN --  Length of each operand in bits.
C    COUNT  --  Number of operands to fetch.
C
C  Note that this routine will decode according to the operand length
C  flag.  Errors will result if this value is greater than the machine
C  word length.
C
      include 'trbufr.h'
C
      INTEGER OPNLEN, COUNT, OPRNS(*), IOS, STATUS
      INTEGER ZERO, BCOUNT, TCOUNT, SCOUNT
C
C  Define the 8 bit byte size.
C
      DATA ZERO/0/
C
      STATUS = 0
C
C  Initialize the operand pointers.
C
      BCOUNT = 1
      TCOUNT = COUNT
 10   CONTINUE
C
C  Calculate the amount of operands in the buffer.
C
      IF ((OPNLEN * TCOUNT + MOPRST) .GT. MRECLN) THEN
C
C  Not enough room for the operand list, so set up for a partial move.
C
        SCOUNT = ((MRECLN-MOPRST)) / OPNLEN
        TCOUNT = TCOUNT - SCOUNT
      ELSE
C
C  Room for all the remaining operands.
C
        SCOUNT = TCOUNT
        TCOUNT = ZERO
      END IF
C
C  Get the operands.
C
      IF (SCOUNT .NE. ZERO) THEN
        CALL GBYTES(MBUFER,OPRNS(BCOUNT),MOPRST,OPNLEN,ZERO,SCOUNT)
      END IF
C
C  Increment the operand pointer.
C
      MOPRST = MOPRST + (OPNLEN * SCOUNT)
C
C  Test if we have all the operands.
C
      IF (TCOUNT .NE. ZERO) THEN
C
C  Get another record.
C
        CALL GSEGRD(IOS,STATUS)
        IF (STATUS .NE. 0) RETURN
C
        MOPRST = METBIT
        BCOUNT = BCOUNT + SCOUNT
        GO TO 10
      END IF
C
C  Move the next instruction pointer past the current operands fetched.
C
      METBIT = MOPRST
C
      RETURN
      END

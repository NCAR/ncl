C
C	$Id: gopdec.f,v 1.3 2000-08-22 15:08:02 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published
C by the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
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

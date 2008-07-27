C
C	$Id: gwelod.f,v 1.4 2008-07-27 00:21:07 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GWELOD(GKSERR)
C
C  This routine loads the current instruction into the segment.
C  The current instruction includes the opcode class and id,
C  and the size filled which may be short or long.
C
C    OUTPUT
C      GKSERR -- The error status flag.
C
C
C  All data is type INTEGER unless otherwise indicated.
C
      IMPLICIT INTEGER (A-Z)
C
C  COMMON for communication of instruction and length.
C
      include 'gwiins.h'
      include 'gwiio.h'
C
      SAVE
C
C  Define the ALLOK status and the opcode class and id lengths.
C
      DATA ALLOK,OPCLLN,OPIDLN/0,4,7/
C
C  Define the short format length, short format count, long format flag,
C  continue flag on, continue flag off, continue length, long format
C  length.
C
      DATA SHFMLN,SHTFMT,LFMFLG,CONON,CONOFF,CFMLNG,LFMLNG
     1    /     5,    30,    31,    1,     0,     1,    15/
      DATA SHORT/0/, LONG/1/
C
      GKSERR = ALLOK
C
C  Make sure the instruction starts on a 16 bit boundry.
C
      TEMP = MOD(WBFPOS,16)
      IF (TEMP .NE. 0) WBFPOS = WBFPOS + (16-TEMP)
C
C  Load the opcode class and id into the buffer.
C
      CALL GWILOD(MCOPCL,OPCLLN,1,GKSERR)
      IF (GKSERR .NE. ALLOK) RETURN
      CALL GWILOD(MCOPID,OPIDLN,1,GKSERR)
      IF (GKSERR .NE. ALLOK) RETURN
C
C  Determine if a long format or short format instruction.
C
      IF (MCCBYT .LE. SHTFMT) THEN
C
C  Short format instruction.
C
        MSLFMT = SHORT
        CALL GWILOD (MCCBYT,SHFMLN,1,GKSERR)
        IF (GKSERR .NE. ALLOK) RETURN
      ELSE
C
C  Long format instruction, set the long format flag.
C
        MSLFMT = LONG
        CALL GWILOD (LFMFLG,SHFMLN,1,GKSERR)
        IF (GKSERR .NE. ALLOK) RETURN
C
C  Set the continue flag.
C
        IF (MCNBYT .NE. 0) THEN
C
C  There is another partition.
C
          CALL GWILOD(CONON,CFMLNG,1,GKSERR)
        ELSE
C
C  Last partition.
C
          CALL GWILOD(CONOFF,CFMLNG,1,GKSERR)
        END IF
C
        IF (GKSERR .NE. ALLOK) RETURN
C
C  Set the long format operand list size.
C
        CALL GWILOD(MCCBYT,LFMLNG,1,GKSERR)
      END IF
C
      RETURN
      END

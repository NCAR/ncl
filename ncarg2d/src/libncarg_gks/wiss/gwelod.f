C
C	$Id: gwelod.f,v 1.2 2000-07-12 16:54:39 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU Lesser General Public License as
C published by the Free Software Foundation; either version 2.1 of the
C License, or (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C Lesser General Public License for more details.
C
C You should have received a copy of the GNU Lesser General Public
C License along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
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

C
C	$Id: gwpart.f,v 1.2 2000-07-12 16:54:44 haley Exp $
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
      SUBROUTINE GWPART(NBYTES,GKSERR)
C
C  This subroutine loads the next partition of an active instruction.
C
C  OUTPUT
C    GKSERR - The error status flag.
C
C  All data is type INTEGER unless otherwise indicated.
C
      IMPLICIT INTEGER (A-Z)
C
      include 'gwiins.h'
      include 'gwiio.h'
C
      SAVE
C
C  Define the CGM data element partition size.  To make the arithmetic
C  come out right in other parts of the code, this should be a multiple
C  of 256.
C
      DATA PARSIZ/32256/
C
      DATA ALLOK/0/
C
C  Define the short format length, short format count, long format flag,
C  continue flag on, continue flag off, continue length, long format
C  length.
C
      DATA CONON,CONOFF,CFMLNG,LFMLNG
     +    /    1,     0,     1,    15/
C
      GKSERR = ALLOK
C
C  Set the current partition byte count and the remainder byte count.
C
      IF (NBYTES .GT. PARSIZ) THEN
        MCCBYT = PARSIZ
        MCNBYT = NBYTES - PARSIZ
      ELSE
        MCCBYT = NBYTES
        MCNBYT = 0
      END IF
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
C
      RETURN
      END

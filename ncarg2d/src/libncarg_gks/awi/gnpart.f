C
C	$Id: gnpart.f,v 1.2 2000-07-12 16:39:43 haley Exp $
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
      SUBROUTINE GNPART(IOS,STATUS)
C
C  Get next partitiion of an instruction.
C
      include 'trpars.h'
      include 'trinst.h'
      include 'trbufr.h'
C
      INTEGER IOS,STATUS
      INTEGER CONVAL
C
      STATUS = 0
C
C  Parse the partition.
C
      METBIT = MOPRST
C
C  Make sure that the instruction starts on a 16 bit boundry.
C
      TEMP = MOD(METBIT,16)
      IF (TEMP .NE. 0) METBIT = METBIT + (16-TEMP)
C
C  First test if there is enough room in the buffer.
C
      IF (MRECLN .LT. (METBIT+MCOLEN+MLOLEN)) THEN
C
C  Read in a new record.
C
        CALL GSEGRD (IOS,STATUS)
        IF (STATUS .NE. 0) RETURN
      END IF
C
C  Get the continue flag value.
C
      CALL GBYTES(MBUFER,CONVAL,METBIT,MCOLEN,0,1) 
      METBIT = METBIT + MCOLEN
      IF (CONVAL .EQ. CONFLG) THEN
        CNTINU = .TRUE.
      ELSE
        CNTINU = .FALSE.
      END IF
C
C  Get the long operand 8 bit byte count field.
C
      CALL GBYTES(MBUFER,LEN,METBIT,MLOLEN,0,1)
      METBIT = METBIT + MLOLEN
C
      MOPRST = METBIT
      METBIT = METBIT + (LEN*BYTSIZ)
C
      RETURN
      END

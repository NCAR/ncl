C
C	$Id: g01msg.f,v 1.2 2000-07-12 16:50:45 haley Exp $
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
      SUBROUTINE G01MSG(IACT)
C
C  Put out MESSAGE elements.
C
      include 'g01prm.h'
      include 'gksin.h'
      include 'g01ins.h'
C
C
C  IACT = 0  for no action, or
C       = 1  for action
C       
      INTEGER IACT,G01PBL
C
C  Find the first non-blank, non-null and set the length.
C
      ILEN = 1
      DO 10 I=80,1,-1
        IF (STR(I:I).NE.' ' .AND. STR(I:I).NE.CHAR(0)) THEN
          ILEN = I
          GO TO 20
        ENDIF
   10 CONTINUE
   20 CONTINUE
C
C  Put out opcode (class and id) and length.
C
      NBYTES = G01PBL (ILEN, 1+(MEFW-1)/8 )
      CALL GPUTNI (7, 1, NBYTES, IERR)
C
C  Put out action/no-action flag.
C
      CALL GPUTPR (IACT, MEFW, 1, RERR)
C
C  Put out the message string.
C
      CALL GPUTPS (STR(1:ILEN), ILEN, ILEN, 0, IERR)
C
      RETURN
      END

C
C	$Id: g01esc.f,v 1.5 2000-08-22 15:09:33 haley Exp $
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
      SUBROUTINE G01ESC
C
C  Process ESCAPE elements.
C
      include 'g01prm.h'
      include 'gksin.h'
      include 'g01ins.h'
C
      INTEGER  KALL, NBYTES, G01PBL
      SAVE KALL
      DATA KALL/0/
      KALL = KALL+1
C
C  Treat the first call -- put out the opcode, FCTID, and first data
C  record if there is one.
C
      IF (KALL .EQ. 1) THEN
        IF (STRL1 .GT. 0) THEN
          NBYTES = G01PBL(STRL1,1+(MINTFW-1)/8)
        ELSE
C
C  Two bytes for the function identifier and one for the string
C  count of the empty string.
C
          NBYTES = 3
        ENDIF
C
C  Put out class, id, length
C
        CALL GPUTNI (6, 1, NBYTES, RERR)
        IF (RERR .NE. 0)  RETURN
C
C  Put out function identifier (2-byte integer).
C
        CALL GPUTPR (ID(1),MINTFW,1,RERR)
        IF (RERR .NE. 0)  RETURN
C
C  Put out first data record if there is one.
C
        IF (STRL2 .EQ. 0) THEN
          CALL GPUTPS (STR, STRL1,  0, 0, RERR)
        ELSE IF (STRL2 .GT. 0) THEN
          CALL GPUTPS (STR, STRL1, 80, 0, RERR)
        ENDIF
        IF (RERR .NE. 0)  RETURN
C
C  If there is to be no continuation, check for consistency, and
C  reset the parameter "KALL".
C
        IF (CONT .EQ. 0) THEN
          IF (MOD(STRL1,80) .EQ. 0) THEN
            KALL = 0
            RETURN
          ELSE
            RERR = 325
            RETURN
          ENDIF
        ENDIF
      ENDIF
C
C  Treat the continuation calls.
C
      IF (KALL .GT. 1) THEN
        IF (CONT .EQ. 0) THEN
          CALL GPUTPS (STR, 80, 80, 1, RERR)
          IF (RERR .NE. 0)  RETURN
          KALL = 0
        ELSE
          CALL GPUTPS (STR, 80, 80, 1, RERR)
          IF (RERR .NE. 0)  RETURN
        ENDIF
      ENDIF
C
      RETURN
      END

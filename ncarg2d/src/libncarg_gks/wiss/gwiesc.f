C
C	$Id: gwiesc.f,v 1.2 2000-07-12 16:54:41 haley Exp $
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
      SUBROUTINE GWIESC
C
C  Process ESCAPE elements.
C
      include 'gksin.h'
      include 'gwiins.h'
C
      INTEGER  KALL, NBYTES, G01PBL
      SAVE
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
          NBYTES = 2
        ENDIF
C
C  Put out class, id, length
C
        CALL GWPTNI (6, 1, NBYTES, RERR)
        IF (RERR .NE. 0)  RETURN
C
C  Put out function identifier (2-byte integer).
C
        CALL GWPTPR (ID(1),MINTFW,1,RERR)
        IF (RERR .NE. 0)  RETURN
C
C  Put out first data record if there is one.
C
        IF (STRL2 .GT. 0) THEN
          CALL GWPTPS (STR, STRL1, 80, 0, RERR)
          IF (RERR .NE. 0)  RETURN
        ENDIF
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
          CALL GWPTPS (STR, 80, 80, 1, RERR)
          IF (RERR .NE. 0)  RETURN
          KALL = 0
        ELSE
          CALL GWPTPS (STR, 80, 80, 1, RERR)
          IF (RERR .NE. 0)  RETURN
        ENDIF
      ENDIF
C
      RETURN
      END

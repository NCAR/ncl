C
C	$Id: ngckop.f,v 1.3 2000-08-22 15:05:10 haley Exp $
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
      INTEGER FUNCTION NGCKOP(IWKID)
C
C  Function to check if the workstation, whose workstation ID is
C  given in IWKID, is currently open.  If it is open, then the
C  function returns a 1, otherwise the function returns a 0.
C
      CALL GQOPWK(0,IER,NOPN,LID)
      IF (NOPN .EQ. 0) THEN
        NGCKOP = 0
        RETURN
      ELSE
        DO 10 I=1,NOPN
          CALL GQOPWK(I,IER,NOPN,LID)
          IF (IWKID .EQ. LID) THEN
            NGCKOP = 1
            RETURN
          ENDIF
   10   CONTINUE
      ENDIF
C
      NGCKOP = 0
      RETURN
      END

C
C	$Id: gescfn.f,v 1.2 2000-07-12 16:39:41 haley Exp $
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
      SUBROUTINE GESCFN (IOS, STATUS)
C
C  ESCAPE elements from segments.
C       
      include 'trinst.h'
      include 'trpars.h'
C
      INTEGER IOS, STATUS
      INTEGER BUFS
      PARAMETER (BUFS=100)
      INTEGER BUFF(BUFS), COUNT, CURR
C
      STATUS = 0
C
C  Get the argument count.
C
    5 CONTINUE
      CALL GOPDEC(COUNT,MWHCPR,1,IOS,STATUS)
      IF (STATUS .NE. 0) RETURN
C
C  Loop until the instruction is processed.
C
   10 CONTINUE
      IF (COUNT .GT. BUFS) THEN
        COUNT = COUNT - BUFS
        CURR = BUFS
      ELSE
        CURR = COUNT
        COUNT = 0
      END IF
C
      CALL GOPDEC(BUFF, BYTSIZ, CURR, IOS, STATUS)
      IF (STATUS .NE. 0) RETURN
C
      IF (COUNT.NE.0) GO TO 10
C
C  Get the next part of the instruction if a continue.
C
      IF (CNTINU) THEN
        CALL GNPART(IOS,STATUS)
        IF (STATUS .NE. 0) RETURN
        GO TO 5
      END IF
C
      RETURN
      END

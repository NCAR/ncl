C
C	$Id: gqsgus.f,v 1.4 2000-07-12 16:39:53 haley Exp $
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
      SUBROUTINE GQSGUS(N,ERRIND,OL,SGNA)
C
C  INQUIRE SET member OF SEGMENT NAMES IN USE
C
      include 'gkscom.h'
C
      INTEGER N,ERRIND,OL,SGNA
C
C  Check that GKS is in the proper state.
C
      CALL GZCKST(7,-1,ERRIND)
      IF (ERRIND .NE. 0) GOTO 100
C
C  Check if N is in bounds.
C
      IF (N.LT.0 .OR. N.GT.NUMSEG) THEN
        ERRIND = 2002
        GOTO 100
      ENDIF
      IF (NUMSEG .EQ. 0) GO TO 100
      OL   = NUMSEG
      IF (N .EQ. 0) RETURN
      SGNA = SEGS(N)
      RETURN
C
  100 CONTINUE
      OL   = NUMSEG
      SGNA = -1
      RETURN
      END

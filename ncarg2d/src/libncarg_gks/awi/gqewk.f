C
C	$Id: gqewk.f,v 1.3 2000-07-12 16:39:47 haley Exp $
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
      SUBROUTINE GQEWK(N,ERRIND,NUMBER,WKTYP)
C
C  INQUIRE LIST element OF AVAILABLE WORKSTATION TYPES
C
      include 'gkscom.h'
C
      INTEGER N,ERRIND,NUMBER,WKTYP
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .NE. 0) GOTO 100
C
C  Check if N is in bounds.
C
      IF (N.LT.0 .OR. N.GT.WK) THEN
        ERRIND = 2002
        GOTO 100
      ENDIF
      NUMBER = WK
      IF (N .EQ. 0) RETURN
      WKTYP = LSWK(N)
      RETURN
C
  100 CONTINUE
      NUMBER = WK
      WKTYP = -1
      RETURN
      END

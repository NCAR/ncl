C
C	$Id: gqentn.f,v 1.4 2000-08-22 15:08:06 haley Exp $
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
      SUBROUTINE GQENTN(N,ERRIND,OL,NPRIO)
C
C  INQUIRE LIST element OF NORMALIZATION TRANSFORMATION NUMBERS
C
      include 'gkscom.h'
C
      INTEGER N,ERRIND,OL,NPRIO
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .NE. 0) GOTO 100
C
C  Check that N is in range.
C
      IF (N.LT.0 .OR. N.GT.MNT) THEN
        ERRIND = 2002
        GOTO 100
      ENDIF
      NPRIO = N
      OL    = MNT+1
      RETURN
C
  100 NPRIO = -1
      OL    = -1
      RETURN
      END

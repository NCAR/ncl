C
C	$Id: gqtxfp.f,v 1.4 2000-08-22 15:08:14 haley Exp $
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
      SUBROUTINE GQTXFP(ERRIND,FONT,PREC)
C
C  INQUIRE TEXT FONT AND PRECISION
C
      include 'gkscom.h'
C
      INTEGER ERRIND,FONT,PREC
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .EQ. 0) THEN
        FONT = CTXFP(1)
        PREC = CTXFP(2)
      ELSE
        FONT = -1
        PREC = -1
      ENDIF
C
      RETURN
      END

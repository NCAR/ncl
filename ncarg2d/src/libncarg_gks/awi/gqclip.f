C
C	$Id: gqclip.f,v 1.4 2000-08-22 15:08:05 haley Exp $
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
      SUBROUTINE GQCLIP(ERRIND,CLSW,CLRECT)
C
C  INQUIRE CLIPPING INDICATOR
C
      include 'gkscom.h'
C
      INTEGER ERRIND,CLSW
      REAL CLRECT(4)
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .EQ. 0) THEN
        CLSW = CCLIP
        INT = CNT+1
        CLRECT(1) = NTVP(INT,1)
        CLRECT(2) = NTVP(INT,2)
        CLRECT(3) = NTVP(INT,3)
        CLRECT(4) = NTVP(INT,4)
      ELSE
        CLSW = -1
        CLRECT(1) = -1.
        CLRECT(2) = -1.
        CLRECT(3) = -1.
        CLRECT(4) = -1.
      ENDIF
C
      RETURN
      END

C
C	$Id: gqpa.f,v 1.4 2000-08-22 15:08:10 haley Exp $
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
      SUBROUTINE GQPA(ERRIND,PWX,PWY,PHX,PHY)
C
C  INQUIRE PATTERN SIZE
C
      include 'gkscom.h'
C
      INTEGER ERRIND
      REAL    PWX,PWY,PHX,PHY
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND.EQ.0) THEN
C
C  Pattern width and height vectors will always be along the 
C  coordinate axes since no metafile interpretation functions 
C  are in the package.
C
        PWX = CPA(1)
        PWY = 0.
        PHX = 0.
        PHY = CPA(2)
      ELSE
        PWX = -1.
        PWY = -1.
        PHX = -1.
        PHY = -1.
      ENDIF
C
      RETURN
      END

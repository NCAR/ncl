C
C	$Id: wmgtln.f,v 1.2 2000-07-12 16:27:04 haley Exp $
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
      INTEGER FUNCTION WMGTLN(LAB,LABLEN,ILR)
C
C  Given a character variable LAB of length LABLEN, calculate
C  the character position in LAB that marks the final non-blank
C  if ILR=0, and marks the first non-blank if ILR=1.
C  
      CHARACTER*(*) LAB
C
      IF (ILR .EQ. 0) THEN
        DO 10 I=LABLEN,1,-1
          IF (LAB(I:I) .NE. ' ') THEN
            WMGTLN = I
            RETURN
          ENDIF
   10   CONTINUE
        WMGTLN = 1
        RETURN
      ELSE 
        DO 20 I=1,LABLEN
          IF (LAB(I:I) .NE. ' ') THEN
            WMGTLN = I
            RETURN
          ENDIF
   20   CONTINUE
        WMGTLN = LABLEN
        RETURN
      ENDIF
C
      END

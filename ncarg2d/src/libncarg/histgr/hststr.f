C
C $Id: hststr.f,v 1.4 2000-07-12 16:24:24 haley Exp $
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
C *****************************************************
C
        SUBROUTINE HSTSTR(LABEL,FIRST,LAST)
C
C *****************************************************
C
C  THIS CALCULATES THE POSITION OF THE FIRST NON-BLANK CHARACTER
C  AND THE POSITION OF THE LAST NON-BLANK CHARACTER IN LABEL
C
      INTEGER   FIRST, LAST
        CHARACTER*(*) LABEL
C
        LAST = LEN(LABEL)
        DO 860 I = 1,LAST
            IF (LABEL(I:I) .NE. ' ') GOTO 870
 860  CONTINUE
 870  FIRST = I
        IF (FIRST .NE. LAST) THEN
            DO 880 J = LAST,FIRST,-1
                  IF (LABEL(J:J) .NE. ' ') THEN
                  LAST = J
                  GOTO 890
                  ENDIF
 880      CONTINUE
 890  CONTINUE
        ENDIF
      RETURN
        END

C
C $Id: galbex.f,v 1.6 2000-08-22 15:04:35 haley Exp $
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
      SUBROUTINE GALBEX (LABL,MLBL,NLBL)
C
        CHARACTER*(*) LABL
C
C Given a character string LABL, of arbitrary length, find the index of
C the first non-blank character in it (MLBL) and the index of the last
C non-blank character following that (NLBL).
C
        LLBL=LEN(LABL)
C
        DO 101 I=1,LLBL
          IF (LABL(I:I).NE.' ') THEN
            MLBL=I
            GO TO 102
          END IF
  101   CONTINUE
        MLBL=1
C
  102   DO 103 I=MLBL,LLBL-1
          IF (LABL(I+1:I+1).EQ.' ') THEN
            NLBL=I
            GO TO 104
          END IF
  103   CONTINUE
        NLBL=LLBL
C
C Done.
C
  104   RETURN
C
      END

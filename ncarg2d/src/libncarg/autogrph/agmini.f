C
C $Id: agmini.f,v 1.3 2000-07-12 16:21:59 haley Exp $
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
      FUNCTION AGMINI (SVAL,ZLOW,ZDRA,NVIZ,IIVZ,NEVZ,IIEZ)
C
      DIMENSION ZDRA(1)
C
C The routine AGMINI returns the mimimum value of the elements in ZDRA
C specified by NVIZ, IIVZ, NEVZ, and IIEZ, skipping elements having the
C special value SVAL (or less than ZLOW, if ZLOW is not equal to SVAL).
C
C -- NVIZ is the number of vectors of data stored in ZDRA.
C
C -- IIVZ is the index increment from one data vector to the next.
C
C -- NEVZ is the number of elements per vector to be examined.
C
C -- IIEZ is the index increment from one vector element to the next.
C    If IIEZ is 0, the array is ignored and 1. is returned.
C
      AGMINI=1.
      IF (IIEZ.EQ.0) RETURN
C
      AGMINI=SVAL
      INDZ=1-IIEZ
C
      DO 103 I=1,NVIZ
        IF (ZLOW.EQ.SVAL) THEN
          DO 101 J=1,NEVZ
            INDZ=INDZ+IIEZ
            IF (ZDRA(INDZ).EQ.SVAL) GO TO 101
            IF (AGMINI.EQ.SVAL) AGMINI=ZDRA(INDZ)
            AGMINI=AMIN1(AGMINI,ZDRA(INDZ))
  101     CONTINUE
        ELSE
          DO 102 J=1,NEVZ
            INDZ=INDZ+IIEZ
            IF (ZDRA(INDZ).EQ.SVAL.OR.ZDRA(INDZ).LT.ZLOW) GO TO 102
            IF (AGMINI.EQ.SVAL) AGMINI=ZDRA(INDZ)
            AGMINI=AMIN1(AGMINI,ZDRA(INDZ))
  102     CONTINUE
        END IF
        INDZ=INDZ-NEVZ*IIEZ+IIVZ
  103 CONTINUE
C
      RETURN
C
      END

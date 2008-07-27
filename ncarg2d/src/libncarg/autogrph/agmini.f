C
C $Id: agmini.f,v 1.6 2008-07-27 00:14:35 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
            AGMINI=MIN(AGMINI,ZDRA(INDZ))
  101     CONTINUE
        ELSE
          DO 102 J=1,NEVZ
            INDZ=INDZ+IIEZ
            IF (ZDRA(INDZ).EQ.SVAL.OR.ZDRA(INDZ).LT.ZLOW) GO TO 102
            IF (AGMINI.EQ.SVAL) AGMINI=ZDRA(INDZ)
            AGMINI=MIN(AGMINI,ZDRA(INDZ))
  102     CONTINUE
        END IF
        INDZ=INDZ-NEVZ*IIEZ+IIVZ
  103 CONTINUE
C
      RETURN
C
      END

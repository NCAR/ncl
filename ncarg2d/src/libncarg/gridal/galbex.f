C
C $Id: galbex.f,v 1.7 2008-07-27 00:17:13 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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

C
C $Id: icloem.f,v 1.1 1993-09-23 17:21:27 kennison Exp $
C
      FUNCTION ICLOEM (MESSG)
C
        CHARACTER*(*) MESSG
C
C ICLOEM(MESSG) is the index of the last non-blank character in MESSG.
C
        DO 101 I=LEN(MESSG),1,-1
         IF (MESSG(I:I).NE.' ') THEN
           ICLOEM=I
           RETURN
         END IF
  101   CONTINUE
C
        ICLOEM=1
C
        RETURN
C
      END

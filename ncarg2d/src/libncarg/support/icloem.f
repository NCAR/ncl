C
C $Id: icloem.f,v 1.2 1994-03-16 00:42:47 kennison Exp $
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

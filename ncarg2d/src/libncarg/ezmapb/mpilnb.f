C
C $Id: mpilnb.f,v 1.3 1998-05-24 00:40:48 kennison Exp $
C
      INTEGER FUNCTION MPILNB (CHRS)
C
        CHARACTER*(*) CHRS
C
C The value of MPILNB(CHRS) is the index of the last non-blank in the
C character string CHRS.
C
        DO 101 I=LEN(CHRS),1,-1
          IF (CHRS(I:I).NE.' ') THEN
            MPILNB=I
            RETURN
          END IF
  101   CONTINUE
C
        MPILNB=1
C
        RETURN
C
      END

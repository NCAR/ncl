C
C $Id: mpifnb.f,v 1.1 1998-04-16 20:45:40 kennison Exp $
C
      INTEGER FUNCTION MPIFNB (CHRS)
C
        CHARACTER*(*) CHRS
C
C The value of MPIFNB(CHRS) is the index of the first non-blank in the
C character string CHRS.
C
        DO 101 I=1,LEN(CHRS)
          IF (CHRS(I:I).NE.' ') THEN
            MPIFNB=I
            RETURN
          END IF
  101   CONTINUE
C
        MPIFNB=1
C
        RETURN
C
      END

C
C	$Id: gzn2wy.f,v 1.3 1995-03-30 22:01:22 fred Exp $
C
      SUBROUTINE GZN2WY(N,P,Q)
C
C  This subroutine takes the N points in the real array P,
C  which are assumed to be in NDC coordinates, and, using
C  the current normalization transformation, converts them
C  into world coordinates and stores them in Q.
C  This subroutine operates on Y-coordinates.
C
C
      include 'gkscom.h'
C
      INTEGER N
      REAL P(N),Q(N)
      ICNT  = CNT+1
      TMP1 = NTWN(ICNT,3)
      TMP2 = NTWN(ICNT,4)
      TMP3 = NTVP(ICNT,3)
      TMP4 = NTVP(ICNT,4)
      IF ((TMP3.EQ.TMP1) .AND. (TMP4.EQ.TMP2)) THEN
        DO 210 I=1,N
          Q(I) = P(I)
  210   CONTINUE
      ELSE
        SCALE = (TMP2-TMP1)/(TMP4-TMP3)
        DO 200 I=1,N
          Q(I) = TMP1+(P(I)-TMP3)*SCALE
  200   CONTINUE
      ENDIF
C
      RETURN
      END

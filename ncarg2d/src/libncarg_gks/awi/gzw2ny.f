C
C	$Id: gzw2ny.f,v 1.3 1995-03-30 22:01:25 fred Exp $
C
      SUBROUTINE GZW2NY(N,P,Q)
C
C  This subroutine takes the N points in the real array P,
C  which are assumed to be in world coordinates, and, using
C  the current normalization transformation, converts them
C  into NDC coordinates and stores them in Q.  This subroutine 
C  operates on Y-coordinates.
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
      IF ((TMP1.EQ.TMP3) .AND. (TMP2.EQ.TMP4)) THEN
        DO 210 I=1,N
          Q(I) = P(I)
  210   CONTINUE
      ELSE
        SCALE = (TMP4-TMP3)/(TMP2-TMP1)
        DO 200 I=1,N
          Q(I) = TMP3+(P(I)-TMP1)*SCALE
  200   CONTINUE
      ENDIF
C
      RETURN
      END

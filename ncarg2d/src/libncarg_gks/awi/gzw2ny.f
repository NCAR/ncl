C
C	$Id: gzw2ny.f,v 1.2 1993-01-09 02:04:41 fred Exp $
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
      SCALE = (TMP4-TMP3)/(TMP2-TMP1)
      DO 200 I=1,N
        Q(I) = TMP3+(P(I)-TMP1)*SCALE
  200 CONTINUE
C
      RETURN
      END

C
C	$Id: gzn2wx.f,v 1.2 1993-01-09 02:04:20 fred Exp $
C
      SUBROUTINE GZN2WX(N,P,Q)
C
C  This subroutine takes the N points in the real array P,
C  which are assumed to be in NDC coordinates, and, using
C  the current normalization transformation, converts them
C  into world coordinates and stores them in Q.
C  This subroutine operates on X-coordinates.
C
C
      include 'gkscom.h'
C
      INTEGER N
      REAL P(N),Q(N)
      ICNT  = CNT+1
      TMP1 = NTWN(ICNT,1)
      TMP2 = NTWN(ICNT,2)
      TMP3 = NTVP(ICNT,1)
      TMP4 = NTVP(ICNT,2)
      SCALE = (TMP2-TMP1)/(TMP4-TMP3)
      DO 200 I=1,N
        Q(I) = TMP1+(P(I)-TMP3)*SCALE
  200 CONTINUE
C
      RETURN
      END

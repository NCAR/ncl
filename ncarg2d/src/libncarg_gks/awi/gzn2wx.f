C
C	$Id: gzn2wx.f,v 1.6 2008-07-27 00:21:04 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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

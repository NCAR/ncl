C
C $Id: hstmed.f,v 1.7 2008-07-27 00:17:15 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
C *****************************************************
C
      SUBROUTINE HSTMED(DATARR,NPTS2,WRK2,MED)
C
C *****************************************************
C
C  FINDS MEDIAN OF ALL DATA POINTS USING SHELL SORT
C
      REAL DATARR(NPTS2), WRK2(NPTS2), MED, TEMP
      INTEGER FLAG, D, HALFN
C
C  COPY DATA ARRAY TO WORK ARRAY
C
        DO 790 I = 1,NPTS2
  790     WRK2(I) = DATARR(I)
C
C  DO SHELL SORT
C
        HALFN = NPTS2/2
        D = NPTS2
  810   D = (D+1)/2
        FLAG = 0
        DO 830 I=1, NPTS2-D
          IF(WRK2(I) .GT. WRK2(I+D)) THEN
            TEMP = WRK2(I)
            WRK2(I) = WRK2(I+D)
            WRK2(I+D) = TEMP
            FLAG = 1
          ENDIF
  830   CONTINUE
        IF (FLAG .EQ. 1 .OR. D .GT. 1)GOTO 810
C
  850 IF (REAL(NPTS2)/2. .GT. REAL(HALFN)) THEN
        MED = WRK2(MIN(HALFN + 1,NPTS2))
      ELSE
        MED = (WRK2(MAX(1,HALFN)) + WRK2(MIN(HALFN + 1,NPTS2)))/2.
      ENDIF
      RETURN
      END

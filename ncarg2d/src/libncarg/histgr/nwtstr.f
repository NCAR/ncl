C
C $Id: nwtstr.f,v 1.8 2008-07-27 00:17:15 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
C *************************************************************
C
      SUBROUTINE NWTSTR (PX,PY,CH)
C
C *************************************************************
C
C This subroutine is a routine which converts calls to
C GTX to calls to PCHIQU.  Direct calls to GTX produce
C distortions in the characters due to the current
C normalization transformation.  PCHIQU avoids this
C problem by using a uniform transformation.
C
      CHARACTER*(*) CH
      REAL VP(4),WN(4)
C
C Determine the centering option.
C
      CALL GQCNTN(IER,INT)
      CALL GQNT(INT,IER,WN,VP)
      CALL GQTXAL(IER,IHZ,IDUM)
C
        ICENT = IHZ-2
	IF (IHZ .EQ. 0) ICENT = -1
	CNTR = REAL(ICENT)
C
C Determine character height.
C
      CALL GQCHH (IER,CHARH)
C
	 YPORT  = VP(4) - VP(3)
	SIZE = CHARH * YPORT
C
C Determine character orientation.
C
      CALL GQCHUP(IER,XV,YV)
      ANGD = 57.29634*ATAN2(-XV,YV)
      IANG = ANGD + .01
      IF (IANG.LT. 0) IANG = IANG+360
      ANGD = REAL(IANG)
C
C Invoke PCHIQU (formerly PLCHHQ) of the Plotchar utility.
C
      CALL PCHIQU (PX,PY,CH,SIZE,ANGD,CNTR)
C
      RETURN
      END

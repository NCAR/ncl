C
C	$Id: nwtstr.f,v 1.1.1.1 1992-04-17 22:31:55 ncargd Exp $
C
C
C *************************************************************
C
      SUBROUTINE NWTSTR (PX,PY,CH)
C
C *************************************************************
C
C This subroutine is a routine which converts calls to
C GTX to calls to WTSTR.  Direct calls to GTX produce
C distortions in the characters due to the current
C normalization transformation.  WTSTR avoids this
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
      IF (IHZ .EQ. 0) THEN
        ICENT = 0
      ELSE
        ICENT = IHZ-2
      ENDIF
C
C Determine character height.
C
      CALL GQCHH (IER,CHI)
      ISZ = KUPY(WN(3)+CHI)-KUPY(WN(3))
C
C Determine character orientation.
C
      CALL GQCHUP(IER,XV,YV)
      IDEG = 57.29634*ATAN2(-XV,YV)
      IF (IDEG .LT. 0) IDEG = IDEG+360
C
C Invoke WTSTR.
C
      CALL WTSTR(PX,PY,CH,ISZ,IDEG,ICENT)
C
      RETURN
      END

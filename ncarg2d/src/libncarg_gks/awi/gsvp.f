C
C	$Id: gsvp.f,v 1.2 1993-01-09 02:03:31 fred Exp $
C
      SUBROUTINE GSVP(TNR,XMIN,XMAX,YMIN,YMAX)
C
C  SET VIEWPORT
C
      INTEGER ESVP
      PARAMETER (ESVP=50)
C
      include 'gkscom.h'
C
      INTEGER TNR
      REAL XMIN,XMAX,YMIN,YMAX
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,ESVP,IER)
      IF (IER .NE. 0) RETURN
C
C  Check that the normalization transformation number is valid.
C
      IF (TNR.LT.1 .OR. TNR.GT.MNT) THEN
        ERS = 1
        CALL GERHND(50,ESVP,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Check that the rectangle definition is valid.
C
      IF (XMAX.LE.XMIN .OR. YMAX.LE.YMIN) THEN
        ERS = 1
        CALL GERHND(51,ESVP,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Check that viewport lies in NDC space.
C
      IF (XMIN.LT.0. .OR. XMAX.GT.1. .OR.
     +    YMIN.LT.0. .OR. YMAX.GT.1.) THEN
        ERS = 1
        CALL GERHND(52,ESVP,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Set the normalization transformation viewport in the GKS state list.
C
      INR = TNR+1
      NTVP(INR,1) = XMIN
      NTVP(INR,2) = XMAX
      NTVP(INR,3) = YMIN
      NTVP(INR,4) = YMAX
C
C  Reestablish character height and up vector, and
C  pattern size and reference point, and clipping rectangle
C  if the normalization number TNR is the current one.
C
      CALL GQCNTN(IER,ICUR)
      IF (TNR .EQ. ICUR) THEN
        CALL GSCHH(CCHH)
        CALL GSCHUP(CCHUP(1),CCHUP(2))
C
C  Suppress issuing warning of unsupported elements.
C
        IFLGO  = CUFLAG
        CUFLAG = -2
        CALL GSPA(CPA(1),CPA(2))
        CALL GSPARF(CPARF(1),CPARF(2))
        CUFLAG = IFLGO
        CALL GSCLIP(CCLIP)
      ENDIF
C
      RETURN
      END

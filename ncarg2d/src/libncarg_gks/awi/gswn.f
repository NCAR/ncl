C
C	$Id: gswn.f,v 1.3 1994-05-19 19:26:56 fred Exp $
C
      SUBROUTINE GSWN (TNR,XMIN,XMAX,YMIN,YMAX)
C
C  SET WINDOW
C
      INTEGER ESWN
      PARAMETER (ESWN=49)
C
      include 'gkscom.h'
C
      INTEGER TNR
      REAL XMIN,XMAX,YMIN,YMAX
      DATA IFRST/0/
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,ESWN,IER)
      IF (IER .NE. 0) RETURN
C
C  Check that the normalization transformation number is valid.
C
      IF (TNR.LT.1 .OR. TNR.GT.MNT) THEN
        ERS = 1
        CALL GERHND(50,ESWN,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Check that the rectangle definition is valid.
C
      IF (XMAX.LE.XMIN .OR. YMAX.LE.YMIN) THEN
        IF (IFRST.EQ.0) THEN
          WRITE(ERF,333)
  333     FORMAT(' WARNING - OTHER GKS IMPLEMENTATIONS MAY NOT ALLOW WIN 
     +DOW XMIN > XMAX OR WINDOW YMIN > YMAX (AS THIS ONE DOES)')
        ENDIF
      ENDIF
C
C  Set the normalization transformation window in the GKS state list.
C
      INR = TNR+1
      NTWN(INR,1) = XMIN
      NTWN(INR,2) = XMAX
      NTWN(INR,3) = YMIN
      NTWN(INR,4) = YMAX
C
C  Set the world coordinate limits to clip to NDC space.
C
      ICNTO = CNT
      CNT = TNR
      CALL GZN2WX(1,0.,WRLDCP(INR,1))
      CALL GZN2WX(1,1.,WRLDCP(INR,2))
      CALL GZN2WY(1,0.,WRLDCP(INR,3))
      CALL GZN2WY(1,1.,WRLDCP(INR,4))
      CNT = ICNTO
C
C  Re-establish the character height and up vector, and pattern size 
C  and reference point if the normalization transformatio number TNR 
C  is the current one.
C
      CALL GQCNTN(IER,ICUR)
      IF (TNR.EQ.ICUR) THEN
        CALL GSCHH(CCHH)
        CALL GSCHUP(CCHUP(1),CCHUP(2))
C
C  Suppress issuing warning of unsupported elements.
C
        IFLGO = CUFLAG
        CUFLAG = -2
        CALL GSPA(CPA(1),CPA(2))
        CALL GSPARF(CPARF(1),CPARF(2))
        CUFLAG = IFLGO
      ENDIF
C
C  Set flag to indicate that GSWN has been called.
C
      IFRST = 1
C
      RETURN
      END

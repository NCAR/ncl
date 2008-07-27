C
C	$Id: gswn.f,v 1.9 2008-07-27 00:21:02 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
        IF (IFRST .EQ. 0) THEN
          CALL GERHND(-113,ESWN,ERF)
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
C  Re-establish the character height and up vector, and pattern size 
C  and reference point if the normalization transformation number TNR 
C  is the current one.
C
      CALL GQCNTN(IER,ICUR)
      IF (TNR.EQ.ICUR) THEN
        CALL GSCHH(CCHH)
        CALL GSCHUP(CCHUP(1),CCHUP(2))
C
C  Pattern size and pattern reference point currently not implemented.
C
C       CALL GSPA(CPA(1),CPA(2))
C       CALL GSPARF(CPARF(1),CPARF(2))
      ENDIF
C
C  Set flag to indicate that GSWN has been called.
C
      IFRST = 1
C
      RETURN
      END

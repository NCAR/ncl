C
C	$Id: gselnt.f,v 1.6 2008-07-27 00:21:01 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GSELNT (TNR)
C
C  SELECT NORMALIZATION TRANSFORMATION
C
      INTEGER ESELNT
      PARAMETER (ESELNT=52)
C
      include 'gkscom.h'
C
      INTEGER TNR
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,ESELNT,IER)
      IF (IER .NE. 0) RETURN
C
C  Check that the normalization transformation number is valid.
C
      IF (TNR.LT.0 .OR. TNR.GT.MNT) THEN
        ERS = 1
        CALL GERHND(50,ESELNT,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Set the current normalization transformation variable in
C  the GKS state list.
C
      CNT = TNR
C
C  Reestablish character height and up vector, and
C  pattern size and reference point, and clipping rectangle.
C
      CALL GSCHH(CCHH)
      CALL GSCHUP(CCHUP(1),CCHUP(2))
C
C  Pattern size and pattern reference point currently not implemented.
C
C     CALL GSPA(CPA(1),CPA(2))
C     CALL GSPARF(CPARF(1),CPARF(2))
      CALL GSCLIP(CCLIP)
C
      RETURN
      END

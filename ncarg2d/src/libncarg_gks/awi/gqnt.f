C
C	$Id: gqnt.f,v 1.5 2008-07-27 00:20:59 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GQNT(NTNR,ERRIND,WINDOW,VIEWPT)
C
C  INQUIRE NORMALIZATION TRANSFORMATION
C
      include 'gkscom.h'
C
      INTEGER NTNR,ERRIND
      REAL    WINDOW(4),VIEWPT(4)
C
C  Check that GKS in in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .NE. 0) GOTO 100
C
C  Check that the transformation number is in range.
C
      IF (NTNR.LT.0 .OR. NTNR.GT.MNT) THEN
        ERRIND = 50
        GO TO 100
      ENDIF
C
      INDX = NTNR+1
      WINDOW(1) = NTWN(INDX,1)
      WINDOW(2) = NTWN(INDX,2)
      WINDOW(3) = NTWN(INDX,3)
      WINDOW(4) = NTWN(INDX,4)
      VIEWPT(1) = NTVP(INDX,1)
      VIEWPT(2) = NTVP(INDX,2)
      VIEWPT(3) = NTVP(INDX,3)
      VIEWPT(4) = NTVP(INDX,4)
      RETURN
C
  100 CONTINUE
      WINDOW(1) = 0.
      WINDOW(2) = 0.
      WINDOW(3) = 0.
      WINDOW(4) = 0.
      VIEWPT(1) = -1.
      VIEWPT(2) = -1.
      VIEWPT(3) = -1.
      VIEWPT(4) = -1.
      RETURN
      END

C
C	$Id: wmgtln.f,v 1.4 2008-07-27 00:17:36 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      INTEGER FUNCTION WMGTLN(LAB,LABLEN,ILR)
C
C  Given a character variable LAB of length LABLEN, calculate
C  the character position in LAB that marks the final non-blank
C  if ILR=0, and marks the first non-blank if ILR=1.
C  
      CHARACTER*(*) LAB
C
      IF (ILR .EQ. 0) THEN
        DO 10 I=LABLEN,1,-1
          IF (LAB(I:I) .NE. ' ') THEN
            WMGTLN = I
            RETURN
          ENDIF
   10   CONTINUE
        WMGTLN = 1
        RETURN
      ELSE 
        DO 20 I=1,LABLEN
          IF (LAB(I:I) .NE. ' ') THEN
            WMGTLN = I
            RETURN
          ENDIF
   20   CONTINUE
        WMGTLN = LABLEN
        RETURN
      ENDIF
C
      END

C
C $Id: naskln.f,v 1.4 2008-07-27 03:11:53 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE NASKLN(IUNIT,LINE)
C
C  Return the next non-comment line on IUNIT in LINE.
C
      CHARACTER*80 LINE
C
    5 CONTINUE
      READ(IUNIT,100,END=200) LINE
  100 FORMAT(A80)
      IF (LINE(1:2) .EQ. '/*') GO TO 5
C
      RETURN
  200 CONTINUE
      WRITE(6,500)
  500 FORMAT(' Unexpected end of file on input')
      STOP
C
      END

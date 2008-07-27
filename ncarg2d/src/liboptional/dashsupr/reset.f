C
C	$Id: reset.f,v 1.4 2008-07-27 00:23:04 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE RESET
C
C USER ENTRY POINT.
C RESET INITIALIZES ALL POINTS IN THE MODEL PICTURE TO UNMARKED.
C
      LOGICAL LDUMMY
      SAVE
C
      CALL REMOVE (0,0,LDUMMY,1)
      RETURN
      END

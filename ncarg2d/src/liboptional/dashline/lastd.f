C
C	$Id: lastd.f,v 1.4 2008-07-27 00:23:02 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE LASTD
      SAVE
C USER ENTRY POINT. SEE DOCUMENTATION FOR PURPOSE.
      DATA IDUMMY /0/
      CALL CFVLD (3,IDUMMY,IDUMMY)
      CALL PLOTIT (0,0,0)
      RETURN
      END

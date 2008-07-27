C
C	$Id: gqops.f,v 1.5 2008-07-27 00:20:59 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GQOPS(OPSTA)
C
C  INQUIRE OPERATING STATE VALUE
C
      include 'gkscom.h'
C
      INTEGER OPSTA
      OPSTA = OPS
C
      RETURN
      END

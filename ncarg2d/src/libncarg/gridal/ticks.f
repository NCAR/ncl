C
C $Id: ticks.f,v 1.7 2008-07-27 00:17:14 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE TICKS (LMJR,LMNR)
        IF (ICFELL('TICKS - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
        CALL TICK4 (LMJR,LMNR,LMJR,LMNR)
        IF (ICFELL('TICKS',2).NE.0) RETURN
        RETURN
      END

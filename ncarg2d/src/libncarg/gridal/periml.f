C
C $Id: periml.f,v 1.7 2008-07-27 00:17:14 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE PERIML (MJRX,MNRX,MJRY,MNRY)
        IF (ICFELL('PERIML - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
        CALL GRIDAL (MJRX,MNRX,MJRY,MNRY,1,1,5,0.,0.)
        IF (ICFELL('PERIML',2).NE.0) RETURN
        RETURN
      END

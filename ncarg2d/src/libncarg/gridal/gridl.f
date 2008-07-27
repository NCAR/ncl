C
C $Id: gridl.f,v 1.7 2008-07-27 00:17:14 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GRIDL (MJRX,MNRX,MJRY,MNRY)
        IF (ICFELL('GRIDL - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
        CALL GRIDAL (MJRX,MNRX,MJRY,MNRY,1,1,0,0.,0.)
        IF (ICFELL('GRIDL',2).NE.0) RETURN
        RETURN
      END

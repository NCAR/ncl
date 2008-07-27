C
C $Id: halfax.f,v 1.7 2008-07-27 00:17:14 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE HALFAX (MJRX,MNRX,MJRY,MNRY,XINT,YINT,IXLB,IYLB)
        IF (ICFELL('HALFAX - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
        CALL GRIDAL (MJRX,MNRX,MJRY,MNRY,IXLB,IYLB,10,XINT,YINT)
        IF (ICFELL('HALFAX',2).NE.0) RETURN
        RETURN
      END

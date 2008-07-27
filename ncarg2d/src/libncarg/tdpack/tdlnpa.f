C
C $Id: tdlnpa.f,v 1.4 2008-07-27 00:17:32 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE TDLNPA (XCP1,YCP1,XCP2,YCP2)
C
C This routine draws a line joining the projections of two points in
C 3-space.  The points are defined by their positions within the
C parallelogram defined by the last call to TDPARA.
C
C Project the two points.
C
        CALL TDPRPA (XCP1,YCP1,XPP1,YPP1)
        CALL TDPRPA (XCP2,YCP2,XPP2,YPP2)
C
C Draw the line joining the projected points.
C
        CALL LINE   (XPP1,YPP1,XPP2,YPP2)
C
C Done.
C
        RETURN
C
      END

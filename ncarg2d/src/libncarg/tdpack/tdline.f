C
C $Id: tdline.f,v 1.4 2008-07-27 00:17:32 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE TDLINE (UCP1,VCP1,WCP1,UCP2,VCP2,WCP2)
C
C This routine draws a line joining the projections of two points in
C 3-space.  The points are defined by their positions in 3-space.
C
C Project the two points.
C
        CALL TDPRPT (UCP1,VCP1,WCP1,XCP1,YCP1)
        CALL TDPRPT (UCP2,VCP2,WCP2,XCP2,YCP2)
C
C Draw the line joining the projected points.
C
        CALL LINE   (XCP1,YCP1,XCP2,YCP2)
C
C Done.
C
        RETURN
C
      END

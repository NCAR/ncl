C
C $Id: tdlndp.f,v 1.1 1997-06-18 22:08:40 kennison Exp $
C
      SUBROUTINE TDLNDP (UCP1,VCP1,WCP1,UCP2,VCP2,WCP2)
C
C This routine draws a line joining the projections of two points in
C 3-space (just like TDLINE), but it calls the DASHPACK routine DPLINE
C to do it and thus can be made to draw a dashed line.  The points are
C defined by their positions in 3-space.
C
C Project the two points.
C
        CALL TDPRPT (UCP1,VCP1,WCP1,XCP1,YCP1)
        CALL TDPRPT (UCP2,VCP2,WCP2,XCP2,YCP2)
C
C Draw the (possibly dashed) line joining the projected points.
C
        CALL DPLINE (XCP1,YCP1,XCP2,YCP2)
C
C Done.
C
        RETURN
C
      END

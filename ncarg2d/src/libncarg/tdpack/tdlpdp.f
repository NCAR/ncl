C
C $Id: tdlpdp.f,v 1.1 1997-06-18 22:08:44 kennison Exp $
C
      SUBROUTINE TDLPDP (XCP1,YCP1,XCP2,YCP2)
C
C This routine draws a line joining the projections of two points in
C 3-space (just like TDLNPA), but it calls the DASHPACK routine DPLINE
C to do it and thus can be made to draw a dashed line.  The points are
C defined by their positions within the parallelogram defined by the
C last call to TDPARA.
C
C Project the two points.
C
        CALL TDPRPA (XCP1,YCP1,XPP1,YPP1)
        CALL TDPRPA (XCP2,YCP2,XPP2,YPP2)
C
C Draw the (possibly dashed) line joining the projected points.
C
        CALL DPLINE (XPP1,YPP1,XPP2,YPP2)
C
C Done.
C
        RETURN
C
      END

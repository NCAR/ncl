C
C	$Id: cpchll.f,v 1.1.1.1 1992-04-17 22:32:43 ncargd Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE CPCHLL (IFLG)
C
C This routine is a dummy.  It is called just before and just after
C each action involving a contour line label.  A user version may
C change color and/or line width.
C
C IFLG is positive if an action is about to be taken, negative if an
C action has just been completed.  The action in question is defined
C by the absolute value of IFLG, as follows:
C
C   1 - computing the size of the line label
C   2 - filling the box around the line label
C   3 - drawing the line label
C   4 - outlining the box around the line label
C
C When CPCHLL is called, the internal parameter 'PAI' will have been
C set to the index of the appropriate contour level.  Thus, parameters
C associated with that level may easily be retrieved by calls to CPGETx.
C
      RETURN
C
      END

C
C $Id: cpchll.f,v 1.6 1996-02-29 17:44:12 kennison Exp $
C
      SUBROUTINE CPCHLL (IFLG)
C
C This routine is a dummy.  It is called just before and just after
C each action involving a contour line label.  A user version may
C take action to change the label.
C
C IFLG is positive if an action is about to be taken, negative if an
C action has just been completed.  The action in question is implied
C by the absolute value of IFLG, as follows:
C
C   1 - deciding whether to put a line label at a given point
C   2 - filling the box around a line label
C   3 - drawing a line label
C   4 - outlining the box around a line label
C
C When CPCHLL is called, the internal parameter 'PAI' will have been
C set to the index of the appropriate contour level.  Thus, parameters
C associated with that level may easily be retrieved by calls to CPGETx.
C
C CPCHLL may retrieve the value of the internal parameter 'ZDV', which
C is the contour level associated with the contour line being labelled.
C
C CPCHLL may retrieve the values of the internal parameters 'LBX' and
C 'LBY', which are the coordinates of the center point of the label,
C in the current user coordinate system.
C
C When IFLG is 1 or 3, CPCHLL is permitted to change the value of the
C internal parameter 'CTM' (a character string); if IFLG is 1 and 'CTM'
C is made blank, the label is suppressed; otherwise, the new value of
C 'CTM' will replace whatever CONPACK was about to use.  If this is
C done for either IFLG = 1 or IFLG = 3, it must be done for both, and
C the same replacement label must be supplied in both cases.
C
C When IFLG = 2, 3, or 4, CPCHLL may make GKS calls to change color
C or line width; during the following call with IFLG = -2, -3, or -4,
C such changes should be undone.
C
      RETURN
C
      END

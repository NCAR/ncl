C
C $Id: cpchil.f,v 1.6 1996-02-29 17:44:12 kennison Exp $
C
      SUBROUTINE CPCHIL (IFLG)
C
C This routine is a dummy.  It is called just before and just after
C each action involving the informational label.  A user version may
C take action to change the label.
C
C IFLG is positive if an action is about to be taken, negative if an
C action has just been completed.  The action in question is implied
C by the absolute value of IFLG, as follows:
C
C   1 - deciding whether to put the informational label at a given point
C   2 - filling the box around the informational label
C   3 - drawing the informational label
C   4 - outlining the box around the informational label
C
C CPCHIL may retrieve the values of the internal parameters 'LBX' and
C 'LBY', which are the coordinates of the center point of the label,
C in the current user coordinate system.
C
C When IFLG is 1 or 3, CPCHIL is permitted to change the value of the
C internal parameter 'CTM' (a character string); if IFLG is 1 and 'CTM'
C is made blank, the label is suppressed; otherwise, the new value of
C 'CTM' will replace whatever CONPACK was about to use.  If this is
C done for either IFLG = 1 or IFLG = 3, it must be done for both, and
C the same replacement label must be supplied in both cases.
C
C When IFLG = 2, 3, or 4, CPCHIL may make GKS calls to change color
C or line width; during the following call with IFLG = -2, -3, or -4,
C such changes should be undone.
C
      RETURN
C
      END

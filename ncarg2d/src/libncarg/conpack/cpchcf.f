C
C $Id: cpchcf.f,v 1.6 1996-02-29 17:44:11 kennison Exp $
C
      SUBROUTINE CPCHCF (IFLG)
C
C This routine is a dummy.  It is called just before and just after
C each action involving a constant-field label.  A user version may
C take action to change the label.
C
C IFLG is positive if an action is about to be taken, negative if an
C action has just been completed.  The action in question is implied
C by the absolute value of IFLG, as follows:
C
C   1 - computing the size of the constant-field label
C   2 - filling the box around the constant-field label
C   3 - drawing the constant-field label
C   4 - outlining the box around the constant-field label
C
C When IFLG = 2, 3, or 4, CPCHCF may make GKS calls to change color
C or line width; during the following call with IFLG = -2, -3, or -4,
C such changes should be undone.
C
      RETURN
C
      END

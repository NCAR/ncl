C
C $Id: cpchil.f,v 1.5 1995-04-26 22:44:29 kennison Exp $
C
      SUBROUTINE CPCHIL (IFLG)
C
C This routine is a dummy.  It is called just before and just after
C each action involving the informational label.  A user version may
C change color and/or line width.
C
C IFLG is positive if an action is about to be taken, negative if an
C action has just been completed.  The action in question is defined
C by the absolute value of IFLG, as follows:
C
C   1 - computing the size of the informational label
C   2 - filling the box around the informational label
C   3 - drawing the informational label
C   4 - outlining the box around the informational label
C
      RETURN
C
      END

C
C $Id: cpchcf.f,v 1.4 1994-09-12 22:10:14 kennison Exp $
C
      SUBROUTINE CPCHCF (IFLG)
C
C This routine is a dummy.  It is called just before and just after
C each action involving a constant-field label.  A user version may
C change color and/or line width.
C
C IFLG is positive if an action is about to be taken, negative if an
C action has just been completed.  The action in question is defined
C by the absolute value of IFLG, as follows:
C
C   1 - computing the size of the constant-field label
C   2 - filling the box around the constant-field label
C   3 - drawing the constant-field label
C   4 - outlining the box around the constant-field label
C
      RETURN
C
      END

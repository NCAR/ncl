C
C $Id: cpchhl.f,v 1.3 1994-05-18 16:16:04 kennison Exp $
C
      SUBROUTINE CPCHHL (IFLG)
C
C This routine is a dummy.  It is called just before and just after
C each action involving a high/low label.  A user version may change
C color and/or line width.
C
C IFLG is positive if an action is about to be taken, negative if the
C action has just been completed.  The action in question is defined
C by the absolute value of IFLG, as follows:
C
C   1 - computing the size of the label for a high
C   2 - filling the box around the label for a high
C   3 - drawing the label for a high
C   4 - outlining the box around the label for a high
C   5 - computing the size of the label for a low
C   6 - filling the box around the label for a low
C   7 - drawing the label for a low
C   8 - outlining the box around the label for a low
C
C When CPCHHL is called, the internal parameter 'ZDL' will have been
C set to the value of the high or low being labelled.
C
      RETURN
C
      END

C
C $Id: ctchhl.f,v 1.1 2003-05-28 15:44:27 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published
C by the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
      SUBROUTINE CTCHHL (IFLG)
C
C This routine is a dummy.  It is called just before and just after
C each action involving a high/low label.  A user version may take
C action to change the label.
C
C IFLG is positive if an action is about to be taken, negative if the
C action has just been completed.  The action in question is implied
C by the absolute value of IFLG, as follows:
C
C   1 - deciding whether to put a high label at a given point
C   2 - filling the box around the label for a high
C   3 - drawing the label for a high
C   4 - outlining the box around the label for a high
C   5 - deciding whether to put a low label at a given point
C   6 - filling the box around the label for a low
C   7 - drawing the label for a low
C   8 - outlining the box around the label for a low
C
C CTCHHL may retrieve the value of the internal parameter 'DVA', which
C is the value associated with the high or low being labelled.
C
C CTCHHL may retrieve the values of the internal parameters 'LBX' and
C 'LBY', which are the coordinates of the center point of the label,
C in the current user coordinate system.
C
C When IFLG is 1, 3, 5, or 7, CTCHHL is permitted to change the value
C of the internal parameter 'CTM' (a character string); if IFLG is 1 or
C 5 and 'CTM' is made blank, the label is suppressed; otherwise, the
C new value of 'CTM' will replace whatever CONPACKT was about to use.
C If this is done for either IFLG = 1 or IFLG = 3, it must be done for
C both, and the same replacement label must be supplied in both cases.
C Similarly, if it is done for either IFLG = 5 or IFLG = 7, it must be
C done for both, and the same replacement label must be specified in
C both cases.
C
C When IFLG = 2, 3, 4, 6, 7, or 8, CTCHHL may make GKS calls to change
C color or line width; during the following call with IFLG = -2, -3,
C -4, -6, -7, or -8, such changes should be undone.
C
      RETURN
C
      END

C
C $Id: ctchll.f,v 1.2 2004-03-19 22:51:53 kennison Exp $
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
      SUBROUTINE CTCHLL (IFLG)
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
C When CTCHLL is called, the internal parameter 'PAI' will have been
C set to the index of the appropriate contour level.  Thus, parameters
C associated with that level may easily be retrieved by calls to CTGETx.
C
C CTCHLL may retrieve the value of the internal parameter 'DVA', which
C is the contour level associated with the contour line being labelled.
C
C CTCHLL may retrieve the values of the internal parameters 'LBX' and
C 'LBY', which are the coordinates of the center point of the label,
C in the current user coordinate system.
C
C When IFLG is 1 or 3, CTCHLL is permitted to change the value of the
C internal parameter 'CTM' (a character string); if IFLG is 1 and 'CTM'
C is made blank, the label is suppressed; otherwise, the new value of
C 'CTM' will replace whatever CONPACKT was about to use.  If this is
C done for either IFLG = 1 or IFLG = 3, it must be done for both, and
C the same replacement label must be supplied in both cases.
C
C When IFLG = 2, 3, or 4, CTCHLL may make GKS calls to change color
C or line width; during the following call with IFLG = -2, -3, or -4,
C such changes should be undone.
C
      RETURN
C
      END

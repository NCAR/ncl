C
C $Id: cpchcl.f,v 1.6 2000-07-12 16:22:25 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU Lesser General Public License as
C published by the Free Software Foundation; either version 2.1 of the
C License, or (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C Lesser General Public License for more details.
C
C You should have received a copy of the GNU Lesser General Public
C License along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
      SUBROUTINE CPCHCL (IFLG)
C
C This routine is a dummy.  It is called just before and just after each
C contour line is drawn.  A user version may be substituted to change
C dash pattern, color, and/or line width.
C
C IFLG is +1 if a contour line is about to be drawn, -1 if a contour
C line has just been drawn.
C
C When CPCHCL is called, the internal parameter 'PAI' will have been
C set to the index of the appropriate contour level.  Thus, parameters
C associated with that level may easily be retrieved by calls to CPGETx.
C
      RETURN
C
      END

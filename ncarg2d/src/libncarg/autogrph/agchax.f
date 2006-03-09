C
C $Id: agchax.f,v 1.5 2006-03-09 22:56:03 kennison Exp $
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
      SUBROUTINE AGCHAX (IFLG,IAXS,IPRT,VILS)
C
C The routine AGCHAX is called by AGAXIS just before and just after each
C of a selected set of objects on the axes are drawn.  A user may supply
C a version to change the appearance of these objects.  The arguments
C are as follows:
C
C - IFLG is zero if a particular object is about to be drawn, non-zero
C   if it has just been drawn.
C
C - IAXS is the number of the axis in question.  The values 1, 2, 3, and
C   4 imply the right, left, bottom, and top axes, respectively.
C
C - IPRT is an integer implying which part of the axis is being drawn.
C   The value 1 implies the line itself, 2 a major tick, 3 a minor tick,
C   4 the mantissa of a label, and 5 the exponent of a label.
C
C - VILS is the value, in the label coordinate system along the axis,
C   associated with the position of the object being drawn.  IPRT=1
C   implies VILS=0.
C
C Done.
C
      RETURN
C
      END

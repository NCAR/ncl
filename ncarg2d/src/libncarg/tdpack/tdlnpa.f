C
C $Id: tdlnpa.f,v 1.2 2000-07-12 16:26:34 haley Exp $
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
      SUBROUTINE TDLNPA (XCP1,YCP1,XCP2,YCP2)
C
C This routine draws a line joining the projections of two points in
C 3-space.  The points are defined by their positions within the
C parallelogram defined by the last call to TDPARA.
C
C Project the two points.
C
        CALL TDPRPA (XCP1,YCP1,XPP1,YPP1)
        CALL TDPRPA (XCP2,YCP2,XPP2,YPP2)
C
C Draw the line joining the projected points.
C
        CALL LINE   (XPP1,YPP1,XPP2,YPP2)
C
C Done.
C
        RETURN
C
      END

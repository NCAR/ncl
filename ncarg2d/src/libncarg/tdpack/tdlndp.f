C
C $Id: tdlndp.f,v 1.3 2000-08-22 15:07:10 haley Exp $
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
      SUBROUTINE TDLNDP (UCP1,VCP1,WCP1,UCP2,VCP2,WCP2)
C
C This routine draws a line joining the projections of two points in
C 3-space (just like TDLINE), but it calls the DASHPACK routine DPLINE
C to do it and thus can be made to draw a dashed line.  The points are
C defined by their positions in 3-space.
C
C Project the two points.
C
        CALL TDPRPT (UCP1,VCP1,WCP1,XCP1,YCP1)
        CALL TDPRPT (UCP2,VCP2,WCP2,XCP2,YCP2)
C
C Draw the (possibly dashed) line joining the projected points.
C
        CALL DPLINE (XCP1,YCP1,XCP2,YCP2)
C
C Done.
C
        RETURN
C
      END

C
C $Id: tdprpa.f,v 1.4 2008-04-04 21:02:56 kennison Exp $
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
      SUBROUTINE TDPRPA (XIPA,YIPA,XI2D,YI2D)
C
C This routine, given the X and Y coordinates of a point in the
C parallelogram defined by the last call to TDPARA, return the X
C and Y coordinates of its image in the image plane.
C
C The variables in the following common block define the parallelogram.
C
        COMMON /TDCOM2/ XACP,YACP,ZACP,XCDX,YCDX,ZCDX,XCDY,YCDY,ZCDY
        SAVE   /TDCOM2/
C
C Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
        CALL TDBLDA
C
C Check for an uncleared prior error.
C
        IF (ICFELL('TDPRPA - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Compute the coordinates of the point in 3-space.
C
        XI3D=XACP+XIPA*XCDX+YIPA*XCDY
        YI3D=YACP+XIPA*YCDX+YIPA*YCDY
        ZI3D=ZACP+XIPA*ZCDX+YIPA*ZCDY
C
C Pass the buck to the routine TDPRPT.
C
        CALL TDPRPT (XI3D,YI3D,ZI3D,XI2D,YI2D)
        IF (ICFELL('TDPRPA',2).NE.0) RETURN
C
C Done.
C
        RETURN
C
      END

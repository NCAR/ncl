C
C $Id: tdpara.f,v 1.4 2008-04-04 21:02:56 kennison Exp $
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
      SUBROUTINE TDPARA (ARG1,ARG2,ARG3,ARG4,ARG5,ARG6,ARG7,ARG8,ARG9)
C
C This routine is called to define a 3-D parallelogram in which 2-D
C objects may be drawn for projection to the image plane.  Its
C arguments are simply copied to a labeled common block.  The first
C three arguments are the X, Y, and Z coordinates of that corner of
C the parallelogram with 2-D coordinates (0.,0.).  The next three
C arguments are the X, Y, and Z components of a unit vector in the
C direction of that corner of the parallelogram with 2-D coordinates
C (1.,0.) and the last three arguments are the X, Y, and Z components
C of a unit vector in the direction of that corner of the parallelogram
C with 2-D coordinates (0.,1.).
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
        IF (ICFELL('TDPARA - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Copy values from the argument list to the common block.
C
        XACP=ARG1
        YACP=ARG2
        ZACP=ARG3
C
        XCDX=ARG4
        YCDX=ARG5
        ZCDX=ARG6
C
        XCDY=ARG7
        YCDY=ARG8
        ZCDY=ARG9
C
C Done.
C
        RETURN
C
      END

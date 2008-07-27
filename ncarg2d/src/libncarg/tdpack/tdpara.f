C
C $Id: tdpara.f,v 1.5 2008-07-27 00:17:33 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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

C
C $Id: tdprpa.f,v 1.1 1992-11-17 18:47:22 kennison Exp $
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
C Declare the BLOCK DATA routine external to force it to load.
C
        EXTERNAL TDBLDA
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
C
C Done.
C
        RETURN
C
      END

C
C $Id: dpsmth.f,v 1.6 2008-07-27 00:16:59 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE DPSMTH (XCPF,YCPF,IFVL)
C
C DPSMTH is called just like DPDRAW, but it arranges for the curve
C defined by a series of calls - a "first-point" call followed by one
C or more "vector" calls and then a terminating "last-point" call - to
C be smoothed; it then interpolates points on the smoothed curve and
C passes them on to DPDRAW.  Like DPDRAW, DPSMTH tries to be forgiving
C when "first point" and "last point" calls are omitted; in such cases,
C instead of issuing an error message and stopping, it does whatever
C its internal state implies would be the logical action.
C
C Define the lengths of the segment buffers, in which the coordinates
C of points are collected before smoothing takes place, and therefore
C the lengths of the scratch arrays needed by the smoothing routines.
C
        PARAMETER (LSGB=100)
C
C Declare the real/integer common block.
C
        COMMON /DPCMRI/ ANGF,DBPI,EPSI,IDPI,IDPS,ILTL,INDP,IPCF,ISBF,
     +                  ISCF,LCDP,RLS1,RLS2,RMFS,TENS,WCHR,WGAP,WSLD
        SAVE   /DPCMRI/
C
C Declare scratch arrays needed by the smoothing routines.
C
        DIMENSION SCR1(LSGB),SCR2(LSGB),SCR3(LSGB),SCR4(LSGB)
C
C Declare the arrays for the segment buffer, in which the coordinates
C of points defining curves to be smoothed are collected.
C
        DIMENSION XSGB(LSGB),YSGB(LSGB)
C
C Declare which quantities must be preserved from call to call.
C
        SAVE IFSF,NSGB,SBFS,SCR1,SCR2,SCR3,SCR4,SELS,XBFS,XSGB,XELS,
     +       YBFS,YSGB,YELS
C
C Initialize the first-segment flag, which says whether we're working
C on the first segment of a curve or not.
C
        DATA IFSF / 1 /
C
C Initialize the count of the number of points in the segment buffer.
C
        DATA NSGB / 0 /
C
C Initialize some quantities that will otherwise cause the code to blow
C up on some machines with some compilers (because FORTRAN insists on
C evaluating the entire contents of an IF clause, even when it doesn't
C need to).
C
        DATA XBFS,XELS,YBFS,YELS / 4*0. /
C
C Check for an uncleared prior error.
C
        IF (ICFELL('DPSMTH - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C See if the segment buffer needs to be processed and dumped.
C
        IF ((IFVL.NE.1.AND.NSGB.GE.   2).OR.
     +      (IFVL.EQ.1.AND.NSGB.GE.LSGB)) THEN
C
C Yes.  First, determine how slopes are to be computed.
C
          IF (IFSF.NE.0.AND.NSGB.GT.3.AND.
     +        ABS(XSGB(NSGB)-XSGB(1)).LT.EPSI.AND.
     +        ABS(YSGB(NSGB)-YSGB(1)).LT.EPSI) THEN
            ISLP=4
          ELSE IF (IFSF.EQ.0.AND.
     +             ABS(XSGB(1)-XELS).LT.EPSI.AND.
     +             ABS(YSGB(1)-YELS).LT.EPSI) THEN
            ISLP=1
            SLP1=SELS
            IF (ABS(XSGB(NSGB)-XBFS).LT.EPSI.AND.
     +          ABS(YSGB(NSGB)-YBFS).LT.EPSI) THEN
              ISLP=0
              SLPN=SBFS
            END IF
          ELSE
            ISLP=3
          END IF
C
C Then, call MSKRV1 to compute coefficients of the smoothed curve.
C
          CALL MSKRV1 (NSGB,XSGB,YSGB,SLP1,SLPN,
     +                 SCR1,SCR2,SCR3,SCR4,MAX(0.,TENS),ISLP)
          IF (ICFELL('DPSMTH',2).NE.0) RETURN
C
C Determine how many points to interpolate along the curve.
C
          RNIN=REAL(MAX(3,1+INT(SCR4(NSGB)/DBPI)))
C
C Interpolate points along the curve and deliver them to the drawing
C routine.
C
          RIIN=1.
C
  101     IF (RIIN.LT.RNIN) THEN
            CALL MSKRV2 (RIIN/RNIN,XTMP,YTMP,NSGB,
     +                   XSGB,YSGB,SCR1,SCR2,SCR4,MAX(0.,TENS),0,DUMI)
            IF (ICFELL('DPSMTH',3).NE.0) RETURN
          ELSE
            XTMP=XSGB(NSGB)
            YTMP=YSGB(NSGB)
          END IF
C
          CALL DPDRAW (XTMP,YTMP,1)
          IF (ICFELL('DPSMTH',4).NE.0) RETURN
C
          RIIN=RIIN+1.
          IF (RIIN.LE.RNIN) GO TO 101
C
C If we just did the first segment of a curve, save the coordinates
C of the point at its beginning, save the slope that the smoother used
C there, and then turn off the first-segment flag.
C
          IF (IFSF.NE.0) THEN
            XBFS=XSGB(1)
            YBFS=YSGB(1)
            CALL MSKRV2 (0.,XTMP,YTMP,NSGB,
     +                   XSGB,YSGB,SCR1,SCR2,SCR4,MAX(0.,TENS),1,SBFS)
            IF (ICFELL('DPSMTH',5).NE.0) RETURN
            IFSF=0
          END IF
C
C Save the coordinates of the point at the end of the segment just
C done and the slope that the smoother used there.
C
          XELS=XSGB(NSGB)
          YELS=YSGB(NSGB)
          CALL MSKRV2 (1.,XTMP,YTMP,NSGB,
     +                 XSGB,YSGB,SCR1,SCR2,SCR4,MAX(0.,TENS),1,SELS)
          IF (ICFELL('DPSMTH',6).NE.0) RETURN
C
C Copy the last point of the segment to the beginning of the segment
C buffer and reset the number of points in the segment buffer to one.
C
          XSGB(1)=XSGB(NSGB)
          YSGB(1)=YSGB(NSGB)
          NSGB=1
C
        END IF
C
C If the point defined by this call is an initial point, make it the
C only point in the segment buffer, turn on the first-segment flag,
C and deliver the point to the drawing routine.
C
        IF (IFVL.LE.0.OR.(IFVL.EQ.1.AND.NSGB.EQ.0)) THEN
C
          XSGB(1)=XCPF
          YSGB(1)=YCPF
          NSGB=1
C
          IFSF=1
C
          CALL DPDRAW (XCPF,YCPF,0)
          IF (ICFELL('DPSMTH',7).NE.0) RETURN
C
C If the point is not an initial point, just make it the next point in
C the segment buffer (unless the point is too close to the previous
C one, in which case it is just ignored.)
C
        ELSE IF (IFVL.EQ.1.AND.NSGB.NE.0) THEN
C
          IF (ABS(XCPF-XSGB(NSGB)).GT.EPSI.OR.
     +        ABS(YCPF-YSGB(NSGB)).GT.EPSI) THEN
C
            NSGB=NSGB+1
            XSGB(NSGB)=XCPF
            YSGB(NSGB)=YCPF
C
          END IF
C
C If the object of the call was to dump the segment buffer, clear the
C count of points in it, reset the first-segment flag, and re-initialize
C the drawing routine.
C
        ELSE
C
          NSGB=0
          IFSF=1
C
          CALL DPDRAW (0.,0.,2)
          IF (ICFELL('DPSMTH',8).NE.0) RETURN
C
        END IF
C
C Done.
C
        RETURN
C
      END

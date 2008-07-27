C
C $Id: tdinit.f,v 1.5 2008-07-27 00:17:32 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE TDINIT (XMID,YMID,ZMID,XORI,YORI,ZORI,XTHI,YTHI,ZTHI,
     +                   OTEP)
C
C This routine initializes the projection package.  The arguments are
C as follows:
C
C   (XMID,YMID,ZMID) is a point in three-space that is midway between
C   the eyes of the observer.
C
C   (XORI,YORI,ZORI) is a point in three-space at the origin of a plane
C   called "plane 1", which is the image plane; plane 1 passes through
C   the point (XORI,YORI,ZORI) and is perpendicular to the line joining
C   (XMID,YMID,ZMID) to (XORI,YORI,ZORI).
C
C   (XTHI,YTHI,ZTHI) is the third of three points in three-space [the
C   other two being (XMID,YMID,ZMID) and (XORI,YORI,ZORI)] required to
C   define a plane called "plane 2", which is the plane of bilateral
C   symmetry of the user.  The intersection of planes 1 and 2 is the Y
C   axis of the image plane.  A plane called "plane 3" is defined as
C   passing through the point (XORI,YORI,ZORI) and being perpendicular
C   to each of planes 1 and 2.  The intersection of planes 1 and 3 is
C   the X axis of the image plane.
C
C   OTEP is the desired offset to the position of the eye to be used.
C   If the distance between the eyes is "s", then use OTEP = "-s/2" for
C   a left-eye view and OTEP = "+s/2" for a right-eye view.  OTEP = 0
C   gives a Cyclopean view from the point (XMID,YMID,ZMID).
C
C If the common variable IH has the value zero, the 3D coordinate system
C is right-handed; if it has a non-zero value, the coordinate system is
C left-handed.
C
C If the common variable IS has the value zero, no SET call is done;
C otherwise, a SET call is done, defining a field of view of FV degrees,
C where FV is another common variable.
C
C If one half of a stereo pair is being done and the common variable
C IT is non-zero, planes 1, 2, and 3 are redefined (by rotation about
C the Y axis) so that plane 1 is perpendicular to the line from the
C eye being used to the point (XORI,YORI,ZORI); planes 2 and 3 also
C pass through the eye being used.
C
C The variables in the following common block define the mapping from
C 3-space to 2-space.
C
        COMMON /TDCOM1/ IH,IT,XM,YM,ZM,XO,YO,ZO,XT,YT,ZT,OE,XE,YE,ZE
        COMMON /TDCOM1/ A1,B1,C1,D1,E1,A2,B2,C2,D2,E2,A3,B3,C3,D3,E3
        COMMON /TDCOM1/ IS,FV,VL,VR,VB,VT,WL,WR,WB,WT
        SAVE   /TDCOM1/
C
C Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
        CALL TDBLDA
C
C Check for an uncleared prior error.
C
        IF (ICFELL('TDINIT - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Copy the coordinates of the midpoint of the line joining the eyes
C into labelled common.
C
        XM=XMID
        YM=YMID
        ZM=ZMID
C
C Copy the coordinates of the origin of the image plane into labelled
C common.
C
        XO=XORI
        YO=YORI
        ZO=ZORI
C
C Copy the coordinates of the third point into labelled common.
C
        XT=XTHI
        YT=YTHI
        ZT=ZTHI
C
C Copy the offset to the eye position into labelled common.
C
        OE=OTEP
C
C Compute the coefficients in the equation defining plane 1.
C
        A1=XO-XM
        B1=YO-YM
        C1=ZO-ZM
        D1=XO*XM+YO*YM+ZO*ZM-XO*XO-YO*YO-ZO*ZO
        T1=SQRT(A1*A1+B1*B1+C1*C1)
        A1=A1/T1
        B1=B1/T1
        C1=C1/T1
        D1=D1/T1
C
C Compute the coefficients in the equation defining plane 2.
C
        A2=YM*(ZO-ZT)+YO*(ZT-ZM)+YT*(ZM-ZO)
        B2=XM*(ZT-ZO)+XO*(ZM-ZT)+XT*(ZO-ZM)
        C2=XM*(YO-YT)+XO*(YT-YM)+XT*(YM-YO)
        D2=XM*YT*ZO+XO*YM*ZT+XT*YO*ZM-XM*YO*ZT-XT*YM*ZO-XO*YT*ZM
        T2=SQRT(A2*A2+B2*B2+C2*C2)
        IF (IH.NE.0) T2=-T2
        A2=A2/T2
        B2=B2/T2
        C2=C2/T2
        D2=D2/T2
C
C Compute the coefficients in the equation defining plane 3.
C
        A3=B2*(ZO-ZM)+C2*(YM-YO)
        B3=A2*(ZM-ZO)+C2*(XO-XM)
        C3=A2*(YO-YM)+B2*(XM-XO)
        D3=A2*(YM*ZO-YO*ZM)+B2*(XO*ZM-XM*ZO)+C2*(XM*YO-XO*YM)
        T3=SQRT(A3*A3+B3*B3+C3*C3)
        IF (IH.NE.0) T3=-T3
        A3=A3/T3
        B3=B3/T3
        C3=C3/T3
        D3=D3/T3
C
C Call SET, if that is to be done.
C
        IF (IS.NE.0) THEN
          IF (VR-VL.GT.VT-VB) THEN
            WL=-ABS(A1*XM+B1*YM+C1*ZM+D1)*TAN(.017453292519943*FV/2.)
            WR=-WL
            WB=((VT-VB)/(VR-VL))*WL
            WT=-WB
          ELSE
            WB=-ABS(A1*XM+B1*YM+C1*ZM+D1)*TAN(.017453292519943*FV/2.)
            WT=-WB
            WL=((VR-VL)/(VT-VB))*WB
            WR=-WL
          END IF
          CALL SET (VL,VR,VB,VT,WL,WR,WB,WT,1)
          IF (ICFELL('TDINIT',2).NE.0) RETURN
        END IF
C
C Compute the coordinates of the eye position.
C
        XE=XM+A2*OE
        YE=YM+B2*OE
        ZE=ZM+C2*OE
C
C If a stereo view is being done and the user has requested that the
C image plane for each view be perpendicular to the line of sight from
C the eye in use, recompute all quantities to make it so.
C
        IF (OE.NE.0..AND.IT.NE.0) THEN
          TT=ABS(A1*XE+B1*YE+C1*ZE+D1)
          XS=XO+TT*A3
          YS=YO+TT*B3
          ZS=ZO+TT*C3
          A1=XO-XE
          B1=YO-YE
          C1=ZO-ZE
          D1=XO*XE+YO*YE+ZO*ZE-XO*XO-YO*YO-ZO*ZO
          T1=SQRT(A1*A1+B1*B1+C1*C1)
          A1=A1/T1
          B1=B1/T1
          C1=C1/T1
          D1=D1/T1
          A2=YE*(ZO-ZS)+YO*(ZS-ZE)+YS*(ZE-ZO)
          B2=XE*(ZS-ZO)+XO*(ZE-ZS)+XS*(ZO-ZE)
          C2=XE*(YO-YS)+XO*(YS-YE)+XS*(YE-YO)
          D2=XE*YS*ZO+XO*YE*ZS+XS*YO*ZE-XE*YO*ZS-XS*YE*ZO-XO*YS*ZE
          T2=SQRT(A2*A2+B2*B2+C2*C2)
          IF (IH.NE.0) T2=-T2
          A2=A2/T2
          B2=B2/T2
          C2=C2/T2
          D2=D2/T2
          A3=B2*(ZO-ZE)+C2*(YE-YO)
          B3=A2*(ZE-ZO)+C2*(XO-XE)
          C3=A2*(YO-YE)+B2*(XE-XO)
          D3=A2*(YE*ZO-YO*ZE)+B2*(XO*ZE-XE*ZO)+C2*(XE*YO-XO*YE)
          T3=SQRT(A3*A3+B3*B3+C3*C3)
          IF (IH.NE.0) T3=-T3
          A3=A3/T3
          B3=B3/T3
          C3=C3/T3
          D3=D3/T3
        END IF
C
C Compute quantities needed by TDPRPT.
C
        E1=A1*XE+B1*YE+C1*ZE+D1
        E2=A2*XE+B2*YE+C2*ZE+D2
        E3=A3*XE+B3*YE+C3*ZE+D3
C
C Done.
C
        RETURN
C
      END

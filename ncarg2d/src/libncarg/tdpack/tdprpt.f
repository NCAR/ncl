C
C $Id: tdprpt.f,v 1.5 2008-07-27 00:17:33 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE TDPRPT (XI3D,YI3D,ZI3D,XI2D,YI2D)
C
C This routine finds the image of the point (XI3D,YI3D,ZI3D) in the
C image plane and returns in XI2D and YI2D the distances of that point
C from each of two planes perpendicular to the image plane, which are
C the desired X and Y coordinates in the image plane.
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
        IF (ICFELL('TDPRPT - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C What's involved is just algebra.
C
        DX=XI3D-XE
        DY=YI3D-YE
        DZ=ZI3D-ZE
C
        TT=-E1/(A1*DX+B1*DY+C1*DZ)
C
        XP=XE+DX*TT
        YP=YE+DY*TT
        ZP=ZE+DZ*TT
C
        XI2D=A2*XP+B2*YP+C2*ZP+D2
        YI2D=A3*XP+B3*YP+C3*ZP+D3
C
C Done.
C
        RETURN
C
      END

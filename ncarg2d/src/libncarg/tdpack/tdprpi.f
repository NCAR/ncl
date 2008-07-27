C
C $Id: tdprpi.f,v 1.5 2008-07-27 00:17:33 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE TDPRPI (XI2D,YI2D,XIPA,YIPA)
C
C This routine, given the projected coordinates of a point (XI2D,YI2D),
C returns, in XIPA and YIPA, the X and Y coordinates of the point, in
C the parallelogram defined by the last call to TDPARA, that projected
C into (XI2D,YI2D).
C
C The variables in the following common block define the mapping from
C 3-space to 2-space.
C
        COMMON /TDCOM1/ IH,IT,XM,YM,ZM,XO,YO,ZO,XT,YT,ZT,OE,XE,YE,ZE
        COMMON /TDCOM1/ A1,B1,C1,D1,E1,A2,B2,C2,D2,E2,A3,B3,C3,D3,E3
        COMMON /TDCOM1/ IS,FV,VL,VR,VB,VT,WL,WR,WB,WT
        SAVE   /TDCOM1/
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
        IF (ICFELL('TDPRPI - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C What's involved is just algebra.
C
        F2=E2-XI2D
        F3=E3-YI2D
C
        A12=A2-F2*A1/E1
        B12=B2-F2*B1/E1
        C12=C2-F2*C1/E1
C
        A13=A3-F3*A1/E1
        B13=B3-F3*B1/E1
        C13=C3-F3*C1/E1
C
        P1=A12*XCDX+B12*YCDX+C12*ZCDX
        Q1=A12*XCDY+B12*YCDY+C12*ZCDY
        R1=A12*(XACP-XE)+B12*(YACP-YE)+C12*(ZACP-ZE)
C
        P2=A13*XCDX+B13*YCDX+C13*ZCDX
        Q2=A13*XCDY+B13*YCDY+C13*ZCDY
        R2=A13*(XACP-XE)+B13*(YACP-YE)+C13*(ZACP-ZE)
C
        XIPA=(Q1*R2-Q2*R1)/(P1*Q2-P2*Q1)
        YIPA=(P2*R1-P1*R2)/(P1*Q2-P2*Q1)
C
C Done.
C
        RETURN
C
      END

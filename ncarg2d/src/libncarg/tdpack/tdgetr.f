C
C $Id: tdgetr.f,v 1.5 2008-07-27 00:17:32 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE TDGETR (PNAM,RVAL)
C
      CHARACTER*(*) PNAM
C
C The subroutine TDGETR may be used to get TDPACK parameters which have
C values of type REAL.
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
C The variables in the following common block together determine how
C big characters written by TDLBLA will be.
C
        COMMON /TDCOM4/ CSM1,CSM2
        SAVE   /TDCOM4/
C
C The variables in the following common block define the shading type
C and the position of the light source.
C
        COMMON /TDCOM6/ ISHD,XPLS,YPLS,ZPLS
        SAVE   /TDCOM6/
C
C Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
        CALL TDBLDA
C
C Check for an uncleared prior error.
C
        IF (ICFELL('TDGETR - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Get the selected parameter.
C
        IF      (PNAM(1:3).EQ.'CS1'.OR.PNAM(1:3).EQ.'cs1') THEN
          RVAL=CSM1
        ELSE IF (PNAM(1:3).EQ.'CS2'.OR.PNAM(1:3).EQ.'cs2') THEN
          RVAL=CSM2
        ELSE IF (PNAM(1:3).EQ.'FOV'.OR.PNAM(1:3).EQ.'fov') THEN
          RVAL=FV
        ELSE IF (PNAM(1:3).EQ.'HND'.OR.PNAM(1:3).EQ.'hnd') THEN
          RVAL=REAL(IH)
        ELSE IF (PNAM(1:3).EQ.'LSU'.OR.PNAM(1:3).EQ.'lsu') THEN
          RVAL=XPLS
        ELSE IF (PNAM(1:3).EQ.'LSV'.OR.PNAM(1:3).EQ.'lsv') THEN
          RVAL=YPLS
        ELSE IF (PNAM(1:3).EQ.'LSW'.OR.PNAM(1:3).EQ.'lsw') THEN
          RVAL=ZPLS
        ELSE IF (PNAM(1:3).EQ.'SET'.OR.PNAM(1:3).EQ.'set') THEN
          RVAL=REAL(IS)
        ELSE IF (PNAM(1:3).EQ.'SHD'.OR.PNAM(1:3).EQ.'shd') THEN
          RVAL=REAL(ISHD)
        ELSE IF (PNAM(1:3).EQ.'STE'.OR.PNAM(1:3).EQ.'ste') THEN
          RVAL=REAL(IT)
        ELSE IF (PNAM(1:3).EQ.'VPB'.OR.PNAM(1:3).EQ.'vpb') THEN
          RVAL=VB
        ELSE IF (PNAM(1:3).EQ.'VPL'.OR.PNAM(1:3).EQ.'vpl') THEN
          RVAL=VL
        ELSE IF (PNAM(1:3).EQ.'VPR'.OR.PNAM(1:3).EQ.'vpr') THEN
          RVAL=VR
        ELSE IF (PNAM(1:3).EQ.'VPT'.OR.PNAM(1:3).EQ.'vpt') THEN
          RVAL=VT
        ELSE
          CALL SETER ('TDGETR - UNRECOGNIZED PARAMETER NAME',2,1)
          RETURN
        END IF
C
C Done.
C
        RETURN
C
      END

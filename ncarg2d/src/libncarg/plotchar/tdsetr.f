C
C $Id: tdsetr.f,v 1.3 1994-03-17 21:37:49 kennison Exp $
C
      SUBROUTINE TDSETR (PNAM,RVAL)
C
      CHARACTER*(*) PNAM
C
C The subroutine TDSETR may be used to set TDPACK parameters which have
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
C Declare the BLOCK DATA routine external to force it to load.
C
        EXTERNAL TDBLDA
C
C Check for an uncleared prior error.
C
        IF (ICFELL('TDSETR - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Set the selected parameter.
C
      IF      (PNAM(1:3).EQ.'FOV'.OR.PNAM(1:3).EQ.'fov') THEN
        FV=RVAL
      ELSE IF (PNAM(1:3).EQ.'HND'.OR.PNAM(1:3).EQ.'hnd') THEN
        IH=MAX(0,MIN(1,INT(RVAL)))
      ELSE IF (PNAM(1:3).EQ.'SET'.OR.PNAM(1:3).EQ.'set') THEN
        IS=MAX(0,MIN(1,INT(RVAL)))
      ELSE IF (PNAM(1:3).EQ.'STY'.OR.PNAM(1:3).EQ.'sty') THEN
        IT=MAX(0,MIN(1,INT(RVAL)))
      ELSE
        CALL SETER ('TDSETR - UNRECOGNIZED PARAMETER NAME',2,1)
        RETURN
      END IF
C
C Done.
C
      RETURN
C
      END

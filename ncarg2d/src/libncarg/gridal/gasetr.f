C
C $Id: gasetr.f,v 1.8 2008-07-27 00:17:14 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GASETR (PNAM,RVAL)
C
        CHARACTER*(*) PNAM
C
C The subroutine GASETR may be used to set GRIDAL parameters which have
C values of type REAL.
C
C Declare the common block containing real and integer parameters.
C
        COMMON /GAREIN/ ICAX,ICLB,ICMJ,ICMN,ILTY,IORX,NCFX,NCFY,RCWX,
     +                  RCWY,RDCX,RDCY,RMJX,RMJY,RMNX,RMNY,RWAX,RWLB,
     +                  RWMJ,RWMN
        SAVE   /GAREIN/
C
C Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
        CALL GABLDT
C
C Check for an uncleared prior error.
C
        IF (ICFELL('GASETR - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Set the selected parameter.
C
        IF      (PNAM(1:3).EQ.'CAX'.OR.PNAM(1:3).EQ.'cax') THEN
          ICAX=INT(RVAL)
        ELSE IF (PNAM(1:3).EQ.'CLB'.OR.PNAM(1:3).EQ.'clb') THEN
          ICLB=INT(RVAL)
        ELSE IF (PNAM(1:3).EQ.'CMJ'.OR.PNAM(1:3).EQ.'cmj') THEN
          ICMJ=INT(RVAL)
        ELSE IF (PNAM(1:3).EQ.'CMN'.OR.PNAM(1:3).EQ.'cmn') THEN
          ICMN=INT(RVAL)
        ELSE IF (PNAM(1:3).EQ.'LTY'.OR.PNAM(1:3).EQ.'lty') THEN
          ILTY=MAX(0,MIN(2,INT(RVAL)))
        ELSE IF (PNAM(1:3).EQ.'WAX'.OR.PNAM(1:3).EQ.'wax') THEN
          RWAX=RVAL
        ELSE IF (PNAM(1:3).EQ.'WLB'.OR.PNAM(1:3).EQ.'wlb') THEN
          RWLB=RVAL
        ELSE IF (PNAM(1:3).EQ.'WMJ'.OR.PNAM(1:3).EQ.'wmj') THEN
          RWMJ=RVAL
        ELSE IF (PNAM(1:3).EQ.'WMN'.OR.PNAM(1:3).EQ.'wmn') THEN
          RWMN=RVAL
        ELSE IF (PNAM(1:3).EQ.'XLL'.OR.PNAM(1:3).EQ.'xll') THEN
          NCFX=INT(RVAL)
        ELSE IF (PNAM(1:3).EQ.'XLO'.OR.PNAM(1:3).EQ.'xlo') THEN
          RDCY=RVAL
        ELSE IF (PNAM(1:3).EQ.'XLS'.OR.PNAM(1:3).EQ.'xls') THEN
          RCWX=RVAL
        ELSE IF (PNAM(1:3).EQ.'XMJ'.OR.PNAM(1:3).EQ.'xmj') THEN
          RMJX=RVAL
        ELSE IF (PNAM(1:3).EQ.'XMN'.OR.PNAM(1:3).EQ.'xmn') THEN
          RMNX=RVAL
        ELSE IF (PNAM(1:3).EQ.'XOR'.OR.PNAM(1:3).EQ.'xor') THEN
          IORX=INT(RVAL)
        ELSE IF (PNAM(1:3).EQ.'YLL'.OR.PNAM(1:3).EQ.'yll') THEN
          NCFY=INT(RVAL)
        ELSE IF (PNAM(1:3).EQ.'YLO'.OR.PNAM(1:3).EQ.'ylo') THEN
          RDCX=RVAL
        ELSE IF (PNAM(1:3).EQ.'YLS'.OR.PNAM(1:3).EQ.'yls') THEN
          RCWY=RVAL
        ELSE IF (PNAM(1:3).EQ.'YMJ'.OR.PNAM(1:3).EQ.'ymj') THEN
          RMJY=RVAL
        ELSE IF (PNAM(1:3).EQ.'YMN'.OR.PNAM(1:3).EQ.'ymn') THEN
          RMNY=RVAL
        ELSE
          CALL SETER ('GASETR - UNRECOGNIZED PARAMETER NAME',2,1)
        END IF
C
C Done.
C
        RETURN
C
      END

C
C $Id: gagetr.f,v 1.7 2008-07-27 00:17:13 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GAGETR (PNAM,RVAL)
C
      CHARACTER*(*) PNAM
C
C The subroutine GAGETR may be used to get GRIDAL parameters which have
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
        IF (ICFELL('GAGETR - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Get the selected parameter.
C
        IF      (PNAM(1:3).EQ.'CAX'.OR.PNAM(1:3).EQ.'cax') THEN
          RVAL=REAL(ICAX)
        ELSE IF (PNAM(1:3).EQ.'CLB'.OR.PNAM(1:3).EQ.'clb') THEN
          RVAL=REAL(ICLB)
        ELSE IF (PNAM(1:3).EQ.'CMJ'.OR.PNAM(1:3).EQ.'cmj') THEN
          RVAL=REAL(ICMJ)
        ELSE IF (PNAM(1:3).EQ.'CMN'.OR.PNAM(1:3).EQ.'cmn') THEN
          RVAL=REAL(ICMN)
        ELSE IF (PNAM(1:3).EQ.'LTY'.OR.PNAM(1:3).EQ.'lty') THEN
          RVAL=REAL(ILTY)
        ELSE IF (PNAM(1:3).EQ.'WAX'.OR.PNAM(1:3).EQ.'wax') THEN
          RVAL=RWAX
        ELSE IF (PNAM(1:3).EQ.'WLB'.OR.PNAM(1:3).EQ.'wlb') THEN
          RVAL=RWLB
        ELSE IF (PNAM(1:3).EQ.'WMJ'.OR.PNAM(1:3).EQ.'wmj') THEN
          RVAL=RWMJ
        ELSE IF (PNAM(1:3).EQ.'WMN'.OR.PNAM(1:3).EQ.'wmn') THEN
          RVAL=RWMN
        ELSE IF (PNAM(1:3).EQ.'XLL'.OR.PNAM(1:3).EQ.'xll') THEN
          RVAL=REAL(NCFX)
        ELSE IF (PNAM(1:3).EQ.'XLO'.OR.PNAM(1:3).EQ.'xlo') THEN
          RVAL=RDCY
        ELSE IF (PNAM(1:3).EQ.'XLS'.OR.PNAM(1:3).EQ.'xls') THEN
          RVAL=RCWX
        ELSE IF (PNAM(1:3).EQ.'XMJ'.OR.PNAM(1:3).EQ.'xmj') THEN
          RVAL=RMJX
        ELSE IF (PNAM(1:3).EQ.'XMN'.OR.PNAM(1:3).EQ.'xmn') THEN
          RVAL=RMNX
        ELSE IF (PNAM(1:3).EQ.'XOR'.OR.PNAM(1:3).EQ.'xor') THEN
          RVAL=REAL(IORX)
        ELSE IF (PNAM(1:3).EQ.'YLL'.OR.PNAM(1:3).EQ.'yll') THEN
          RVAL=REAL(NCFY)
        ELSE IF (PNAM(1:3).EQ.'YLO'.OR.PNAM(1:3).EQ.'ylo') THEN
          RVAL=RDCX
        ELSE IF (PNAM(1:3).EQ.'YLS'.OR.PNAM(1:3).EQ.'yls') THEN
          RVAL=RCWY
        ELSE IF (PNAM(1:3).EQ.'YMJ'.OR.PNAM(1:3).EQ.'ymj') THEN
          RVAL=RMJY
        ELSE IF (PNAM(1:3).EQ.'YMN'.OR.PNAM(1:3).EQ.'ymn') THEN
          RVAL=RMNY
        ELSE
          CALL SETER ('GAGETR - UNRECOGNIZED PARAMETER NAME',2,1)
        END IF
C
C Done.
C
        RETURN
C
      END

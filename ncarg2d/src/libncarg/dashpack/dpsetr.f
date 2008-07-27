C
C $Id: dpsetr.f,v 1.7 2008-07-27 00:16:59 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE DPSETR (PNAM,RVAL)
C
C This routine, given a real value, sets the value of an internal
C parameter of type INTEGER or REAL to that value.
C
        CHARACTER*(*) PNAM
C
C Declare the real/integer common block.
C
        COMMON /DPCMRI/ ANGF,DBPI,EPSI,IDPI,IDPS,ILTL,INDP,IPCF,ISBF,
     +                  ISCF,LCDP,RLS1,RLS2,RMFS,TENS,WCHR,WGAP,WSLD
        SAVE   /DPCMRI/
C
C Declare a temporary variable in which to form error messages.
C
        CHARACTER*39 CTMP
C
C Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
        CALL DPBLDA
C
C Check for an uncleared prior error.
C
        IF (ICFELL('DPSETR - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Because most changes in internal parameters of DASHPACK result in
C changing the way in which the current dash pattern is interpreted,
C force a buffer dump before changing anything and ensure that, the
C next time DPDRAW is called, the dash pattern element descriptors
C will be regenerated.
C
        CALL DPSMTH (0.,0.,2)
        IF (ICFELL('DPSETR',2).NE.0) RETURN
C
        IDPI=0
C
C Change the specified parameter or log an error.
C
        IF (LEN(PNAM).LT.3) THEN
          CTMP='DPSETR - PARAMETER NAME TOO SHORT - '//PNAM
          CALL SETER (CTMP(1:38),3,1)
          RETURN
        ELSE IF (PNAM(1:3).EQ.'DPL'.OR.PNAM(1:3).EQ.'dpl') THEN
          LCDP=MAX(1,MIN(256,INT(RVAL)))
        ELSE IF (PNAM(1:3).EQ.'DPS'.OR.PNAM(1:3).EQ.'dps') THEN
          IDPS=MAX(-32,MIN(256,INT(RVAL)))
        ELSE IF (PNAM(1:3).EQ.'DPT'.OR.PNAM(1:3).EQ.'dpt') THEN
          INDP=INT(RVAL)
        ELSE IF (PNAM(1:3).EQ.'EPS'.OR.PNAM(1:3).EQ.'eps') THEN
          EPSI=MAX(0.,RVAL)
        ELSE IF (PNAM(1:3).EQ.'LS1'.OR.PNAM(1:3).EQ.'ls1') THEN
          RLS1=MAX(0.,MIN(10.,RVAL))
        ELSE IF (PNAM(1:3).EQ.'LS2'.OR.PNAM(1:3).EQ.'ls2') THEN
          RLS2=MAX(0.,MIN(10.,RVAL))
        ELSE IF (PNAM(1:3).EQ.'LTL'.OR.PNAM(1:3).EQ.'ltl') THEN
          ILTL=MAX(0,MIN(1,INT(RVAL)))
        ELSE IF (PNAM(1:3).EQ.'MFS'.OR.PNAM(1:3).EQ.'mfs') THEN
          RMFS=MAX(0.,RVAL)
        ELSE IF (PNAM(1:3).EQ.'PCF'.OR.PNAM(1:3).EQ.'pcf') THEN
          IPCF=MAX(0,MIN(2,INT(RVAL)))
        ELSE IF (PNAM(1:3).EQ.'SAF'.OR.PNAM(1:3).EQ.'saf') THEN
          ANGF=MAX(-360.,MIN(360.,RVAL))
        ELSE IF (PNAM(1:3).EQ.'SBF'.OR.PNAM(1:3).EQ.'sbf') THEN
          ISBF=MAX(0,MIN(1,INT(RVAL)))
        ELSE IF (PNAM(1:3).EQ.'SCF'.OR.PNAM(1:3).EQ.'scf') THEN
          ISCF=MAX(0,MIN(1,INT(RVAL)))
        ELSE IF (PNAM(1:3).EQ.'SSL'.OR.PNAM(1:3).EQ.'ssl') THEN
          DBPI=MAX(.000001,MIN(1.,RVAL))
        ELSE IF (PNAM(1:3).EQ.'TCS'.OR.PNAM(1:3).EQ.'tcs') THEN
          TENS=RVAL
        ELSE IF (PNAM(1:3).EQ.'WOC'.OR.PNAM(1:3).EQ.'woc') THEN
          WCHR=MAX(.000001,MIN(1.,RVAL))
        ELSE IF (PNAM(1:3).EQ.'WOG'.OR.PNAM(1:3).EQ.'wog') THEN
          WGAP=MAX(.000001,MIN(1.,RVAL))
        ELSE IF (PNAM(1:3).EQ.'WOS'.OR.PNAM(1:3).EQ.'wos') THEN
          WSLD=MAX(.000001,MIN(1.,RVAL))
        ELSE
          CTMP='DPSETR - PARAMETER NAME NOT KNOWN - '//PNAM
          CALL SETER (CTMP(1:39),4,1)
          RETURN
        END IF
C
C Done.
C
        RETURN
C
      END

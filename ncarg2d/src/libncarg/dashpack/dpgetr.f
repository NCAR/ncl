C
C $Id: dpgetr.f,v 1.7 2008-07-27 00:16:59 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE DPGETR (PNAM,RVAL)
C
C This routine is used to get the real value of an internal parameter
C of type INTEGER or REAL.
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
        IF (ICFELL('DPGETR - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Get the specified parameter or log an error.
C
        IF (LEN(PNAM).LT.3) THEN
          CTMP='DPGETR - PARAMETER NAME TOO SHORT - '//PNAM
          CALL SETER (CTMP(1:38),2,1)
          RETURN
        ELSE IF (PNAM(1:3).EQ.'DPL'.OR.PNAM(1:3).EQ.'dpl') THEN
          RVAL=REAL(LCDP)
        ELSE IF (PNAM(1:3).EQ.'DPS'.OR.PNAM(1:3).EQ.'dps') THEN
          RVAL=REAL(IDPS)
        ELSE IF (PNAM(1:3).EQ.'DPT'.OR.PNAM(1:3).EQ.'dpt') THEN
          RVAL=REAL(INDP)
        ELSE IF (PNAM(1:3).EQ.'EPS'.OR.PNAM(1:3).EQ.'eps') THEN
          RVAL=EPSI
        ELSE IF (PNAM(1:3).EQ.'LS1'.OR.PNAM(1:3).EQ.'ls1') THEN
          RVAL=RLS1
        ELSE IF (PNAM(1:3).EQ.'LS2'.OR.PNAM(1:3).EQ.'ls2') THEN
          RVAL=RLS2
        ELSE IF (PNAM(1:3).EQ.'LTL'.OR.PNAM(1:3).EQ.'ltl') THEN
          RVAL=REAL(ILTL)
        ELSE IF (PNAM(1:3).EQ.'MFS'.OR.PNAM(1:3).EQ.'mfs') THEN
          RVAL=RMFS
        ELSE IF (PNAM(1:3).EQ.'PCF'.OR.PNAM(1:3).EQ.'pcf') THEN
          RVAL=REAL(IPCF)
        ELSE IF (PNAM(1:3).EQ.'SAF'.OR.PNAM(1:3).EQ.'saf') THEN
          RVAL=ANGF
        ELSE IF (PNAM(1:3).EQ.'SBF'.OR.PNAM(1:3).EQ.'sbf') THEN
          RVAL=REAL(ISBF)
        ELSE IF (PNAM(1:3).EQ.'SCF'.OR.PNAM(1:3).EQ.'scf') THEN
          RVAL=REAL(ISCF)
        ELSE IF (PNAM(1:3).EQ.'SSL'.OR.PNAM(1:3).EQ.'ssl') THEN
          RVAL=DBPI
        ELSE IF (PNAM(1:3).EQ.'TCS'.OR.PNAM(1:3).EQ.'tcs') THEN
          RVAL=TENS
        ELSE IF (PNAM(1:3).EQ.'WOC'.OR.PNAM(1:3).EQ.'woc') THEN
          RVAL=WCHR
        ELSE IF (PNAM(1:3).EQ.'WOG'.OR.PNAM(1:3).EQ.'wog') THEN
          RVAL=WGAP
        ELSE IF (PNAM(1:3).EQ.'WOS'.OR.PNAM(1:3).EQ.'wos') THEN
          RVAL=WSLD
        ELSE
          CTMP='DPGETR - PARAMETER NAME NOT KNOWN - '//PNAM
          CALL SETER (CTMP(1:39),3,1)
          RETURN
        END IF
C
C Done.
C
        RETURN
C
      END

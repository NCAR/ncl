C
C $Id: slseti.f,v 1.6.8.1 2010-03-17 20:51:57 brownrig Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE SLSETI (PNAM,IVAL)
C
C Set the integer value of the STITLE parameter named PNAM from IVAL.
C
        CHARACTER*(*) PNAM
        INTEGER IVAL
C
C The common block SLCOMN holds all of the internal parameters of
C the package STITLE except for color-table parameters.
C
        COMMON /SLCOMN/ GPSZ,IBGC,IBGF,ICOP,IDOT,IFGC,IFGF,IJMP,IMAP,
     +                  INCU,IWLU,IWRK,IWWI,IXND,IXST,OORV,PCSZ,RNFS,
     +                  RVPB,RVPL,RVPR,RVPT,TFIN,TFOU,TGP1,TGP2,TGP3
        SAVE   /SLCOMN/
C
C Define a temporary variable in which to put the first three characters
C of PNAM.
C
        CHARACTER*3 CTMP
C
C Define a character variable in which messages may be formed.
C
        CHARACTER*29 CMSG
C
C RLB 3/2010: Previously the integer parameter was converted to a float
C   and the work was delegated off to SLSETR. This provided a sort
C   of "automatic type conversion", allowing the user to set a real
C   parameter using either slseti() or slsetr(), as in:
C        CALL SLSETI ('xxx',-9999)
C     or
C        CALL SLSETR ('xxx',-9999.0)
C
C   Color-indices are now either encoded RGBa values, or indices as
C   before. RGBa values are typically large integer values, beyond the
C   precision of floats, and thus this delegation scheme no longer
C   works correctly. The code has been refactored such that the integer
C   cases are now handled directly herein. If no action is found for
C   the PNAM, then we delegate over to SLSETR.
C --------------------------------------------------------------------
C
C Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
        CALL SLBLDA
C
C Check for an uncleared prior error.
C
        IF (ICFELL('SLSETI - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Extract the first three characters of the parameter name.
C
        CTMP=PNAM
C
C If the parameter name has less than three characters, log an error.
C
        IF (LEN(PNAM).LT.3) GO TO 901
C
C See what the parameter name is ...
C
C ... the flag controlling the output of the alignment frames, ...
C
        IF      (CTMP.EQ.'ALN'.OR.CTMP.EQ.'aln') THEN
C
          IDOT=MAX(0,MIN(2,IVAL))
C
C ... the default background color index ...
C
        ELSE IF (CTMP.EQ.'BGC'.OR.CTMP.EQ.'bgc') THEN
C
          IBGC=MAX(0,IVAL)
C
C ... the background color fade flag ...
C
        ELSE IF (CTMP.EQ.'BGF'.OR.CTMP.EQ.'bgf') THEN
C
          IBGF=MAX(-2,MIN(999999,IVAL))
C
C ... the default foreground color index ...
C
        ELSE IF (CTMP.EQ.'FGC'.OR.CTMP.EQ.'fgc') THEN
C
          IFGC=MAX(1,IVAL)
C
C ... the foreground color fade flag ...
C
        ELSE IF (CTMP.EQ.'FGF'.OR.CTMP.EQ.'fgf') THEN
C
          IFGF=MAX(-2,MIN(999999,IVAL))
C
C ... the centering parameter, ...
C
        ELSE IF (CTMP.EQ.'ICO'.OR.CTMP.EQ.'ico') THEN
C
          ICOP=MAX(0,MIN(2,IVAL))
C
C ... the FORTRAN logical unit number for "card" input, ...
C
        ELSE IF (CTMP.EQ.'ICU'.OR.CTMP.EQ.'icu') THEN
C
          INCU=MAX(0,IVAL)
C
C ... the interline spacing for practice runs, ...
C
        ELSE IF (CTMP.EQ.'INC'.OR.CTMP.EQ.'inc') THEN
C
          IJMP=MAX(1,IVAL)
C
C ... the FORTRAN logical unit number for WISS, ...
C
        ELSE IF (CTMP.EQ.'LOG'.OR.CTMP.EQ.'log') THEN
C
          IWLU=MAX(0,IVAL)
C
C ... the PLOTCHAR mapping flag, ...
C
        ELSE IF (CTMP.EQ.'MAP'.OR.CTMP.EQ.'map') THEN
C
          IMAP=MAX(0,IVAL)
C
C ... the horizontal scroll end coordinate, ...
C
        ELSE IF (CTMP.EQ.'NXE'.OR.CTMP.EQ.'nxe') THEN
C
          IXND=MAX(0,MIN(1023,IVAL))
C
C ... the horizontal scroll start coordinate, ...
C
        ELSE IF (CTMP.EQ.'NXS'.OR.CTMP.EQ.'nxs') THEN
C
          IXST=MAX(0,MIN(1023,IVAL))
C
C ... or the workstation identifier for WISS.
C
        ELSE IF (CTMP.EQ.'WID'.OR.CTMP.EQ.'wid') THEN
C
          IWWI=MAX(0,IVAL)
C
        ELSE
C         Pass SLSETR the real equivalent of the integral value and
C         let it do the work.
          CALL SLSETR (PNAM,REAL(IVAL))
          IF (ICFELL('SLSETI',2).NE.0) RETURN
        END IF
C
C Normal exit.
C
        RETURN
C
C Error exit.
C
  901   CMSG(1:29)='SLSETI - INVALID KEYWORD: '//CTMP
        CALL SETER (CMSG(1:29),8,1)
        RETURN
      END

C
C $Id: gaseti.f,v 1.6.8.1 2010-03-17 20:51:57 brownrig Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GASETI (PNAM,IVAL)
C
        CHARACTER*(*) PNAM
        INTEGER IVAL
C
C The subroutine GASETI may be used to set GRIDAL parameters which have
C values of type INTEGER.
C
C Declare the common block containing real and integer parameters.
C
        COMMON /GAREIN/ ICAX,ICLB,ICMJ,ICMN,ILTY,IORX,NCFX,NCFY,RCWX,
     +                  RCWY,RDCX,RDCY,RMJX,RMJY,RMNX,RMNY,RWAX,RWLB,
     +                  RWMJ,RWMN
        SAVE   /GAREIN/
C --------------------------------------------------------
C RLB 3/2010: Previously the integer parameter was converted to a float
C   and the work was delegated off to GASETR. This provided a sort
C   of "automatic type conversion", allowing the user to set a real
C   parameter using either gaseti() or gasetr(), as in:
C        CALL GASETI ('xxx',-9999)
C     or
C        CALL GASETR ('xxx',-9999.0)
C
C   Color-indices are now either encoded RGBa values, or indices as
C   before. RGBa values are typically large integer values, beyond the
C   precision of floats, and thus this delegation scheme no longer
C   works correctly. The code has been refactored such that the integer
C   cases are now handled directly herein. If no action is found for
C   the PNAM, then we delegate over to GASETR.
C -------------------------------------------------------
C
C Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
        CALL GABLDT
C
C Check for an uncleared prior error.
C
        IF (ICFELL('GASETI - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Set the selected parameter.
C
        IF      (PNAM(1:3).EQ.'CAX'.OR.PNAM(1:3).EQ.'cax') THEN
          ICAX=IVAL
        ELSE IF (PNAM(1:3).EQ.'CLB'.OR.PNAM(1:3).EQ.'clb') THEN
          ICLB=IVAL
        ELSE IF (PNAM(1:3).EQ.'CMJ'.OR.PNAM(1:3).EQ.'cmj') THEN
          ICMJ=IVAL
        ELSE IF (PNAM(1:3).EQ.'CMN'.OR.PNAM(1:3).EQ.'cmn') THEN
          ICMN=IVAL
        ELSE IF (PNAM(1:3).EQ.'LTY'.OR.PNAM(1:3).EQ.'lty') THEN
          ILTY=MAX(0,MIN(2,IVAL))
        ELSE IF (PNAM(1:3).EQ.'XLL'.OR.PNAM(1:3).EQ.'xll') THEN
          NCFX=IVAL
        ELSE IF (PNAM(1:3).EQ.'XOR'.OR.PNAM(1:3).EQ.'xor') THEN
          IORX=IVAL
        ELSE IF (PNAM(1:3).EQ.'YLL'.OR.PNAM(1:3).EQ.'yll') THEN
          NCFY=IVAL
        ELSE
C         Delegate the call to GASETR.
          CALL GASETR (PNAM,REAL(IVAL))
          IF (ICFELL('GASETI',2).NE.0) RETURN
        END IF
C
C Done.
C
        RETURN
C
      END

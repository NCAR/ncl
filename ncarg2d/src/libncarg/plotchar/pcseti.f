C
C $Id: pcseti.f,v 1.11.8.1 2010-03-17 20:51:57 brownrig Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE PCSETI (WHCH,IVAL)
C
      CHARACTER*(*) WHCH
C
C The subroutine PCSETI may be used to set PLCHHQ parameters which have
C values of type INTEGER.
C
C COMMON block declarations.
C
      COMMON /PCPRMS/ ADDS,CONS,DSTB,DSTL,DSTR,DSTT,HPIC(3),IBNU,
     +                IBXC(3),IBXF,ICEN,IORD,IOUC,IOUF,IPCC,IQUF,
     +                ISHC,ISHF,ITEF,JCOD,LSCI(16),NFCC,NODF,RBXL,
     +                RBXM,RBXX,RBXY,ROLW,RPLW,RSLW,SHDX,SHDY,SIZA,
     +                SSIC,SSPR,SUBS,VPIC(3),WPIC(3),XBEG,XCEN,XEND,
     +                XMUL(3),YBEG,YCEN,YEND,YMUL(3),ZINX,ZINY,ZINZ
      SAVE   /PCPRMS/
C
      COMMON /PCPFLQ/ IMAP,OORV,RHTW
      SAVE   /PCPFLQ/
C ------------------------------------------------------------------
C RLB 3/2010: Previously the integer parameter was converted to a float
C   and the work was delegated off to PCSETR. This provided a sort
C   of "automatic type conversion", allowing the user to set a real
C   parameter using either pcseti() or pcsetr(), as in:
C        CALL PCSETI ('xxx',-9999)
C     or
C        CALL PCSETR ('xxx',-9999.0)
C
C   Color-indices are now either encoded RGBa values, or indices as
C   before. RGBa values are typically large integer values, beyond the
C   precision of floats, and thus this delegation scheme no longer
C   works correctly. The code has been refactored such that the integer
C   cases are now handled directly herein. If no action is found for
C   the WHCH, then we delegate over to PCSETR.
C -------------------------------------------------------
C
C Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
      CALL PCBLDA
C
C Check for an uncleared prior error.
C
      IF (ICFELL('PCSETI - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Set the selected parameter.
C
      IF      (WHCH(1:2).EQ.'BC'.OR.WHCH(1:2).EQ.'bc') THEN
        CALL PCGPAI (WHCH,3,IPAI)
        IF (IPAI.EQ.0) THEN
          IBXC(1)=IVAL
        ELSE IF (IPAI.GE.1.AND.IPAI.LE.3) THEN
          IBXC(IPAI)=IVAL
        ELSE
          CALL SETER ('PCSETI - BOX COLOR ARRAY INDEX IS OUT OF RANGE',
     +                                                             2,1)
          RETURN
        END IF
      ELSE IF (WHCH(1:2).EQ.'BF'.OR.WHCH(1:2).EQ.'bf') THEN
        IBXF=IVAL
      ELSE IF (WHCH(1:2).EQ.'CC'.OR.WHCH(1:2).EQ.'cc') THEN
        CALL PCGPAI (WHCH,3,IPAI)
        IF (IPAI.EQ.0) THEN
          IPCC=IVAL
        ELSE IF (IPAI.GE.1.AND.IPAI.LE.16) THEN
          LSCI(IPAI)=IVAL
        ELSE
          CALL SETER ('PCSETI - COLOR ARRAY INDEX IS OUT OF RANGE',3,1)
          RETURN
        END IF
      ELSE IF (WHCH(1:2).EQ.'CD'.OR.WHCH(1:2).EQ.'cd') THEN
        JCOD=MAX(0,MIN(1,IVAL))
      ELSE IF (WHCH(1:2).EQ.'CE'.OR.WHCH(1:2).EQ.'ce') THEN
        ICEN=MAX(0,MIN(1,IVAL))
      ELSE IF (WHCH(1:2).EQ.'DO'.OR.WHCH(1:2).EQ.'do') THEN
        IORD=MAX(-2,MIN(+2,IVAL))
        IF (IORD.EQ.0) IORD=1
      ELSE IF (WHCH(1:2).EQ.'FN'.OR.WHCH(1:2).EQ.'fn') THEN
        NODF=ABS(IVAL)
        IF ((NODF.GE. 23.AND.NODF.LE. 24).OR.
     +      (NODF.GE. 27.AND.NODF.LE. 28).OR.
     +      (NODF.GE. 31.AND.NODF.LE. 32).OR.
     +      (NODF.GE. 38.AND.NODF.LE.120).OR.
     +      (NODF.GE.123.AND.NODF.LE.124).OR.
     +      (NODF.GE.127.AND.NODF.LE.128).OR.
     +      (NODF.GE.131.AND.NODF.LE.132).OR.NODF.GE.138) NODF=1
      ELSE IF (WHCH(1:2).EQ.'MA'.OR.WHCH(1:2).EQ.'ma') THEN
        IMAP=MAX(0,IVAL)
      ELSE IF (WHCH(1:2).EQ.'OC'.OR.WHCH(1:2).EQ.'oc') THEN
        IOUC=IVAL
      ELSE IF (WHCH(1:2).EQ.'OF'.OR.WHCH(1:2).EQ.'of') THEN
        IOUF=MAX(0,MIN(1,IVAL))
      ELSE IF (WHCH(1:2).EQ.'QU'.OR.WHCH(1:2).EQ.'qu') THEN
        IQUF=MAX(0,MIN(2,IVAL))
      ELSE IF (WHCH(1:2).EQ.'SF'.OR.WHCH(1:2).EQ.'sf') THEN
        ISHF=MAX(0,MIN(1,IVAL))
      ELSE IF (WHCH(1:2).EQ.'TE'.OR.WHCH(1:2).EQ.'te') THEN
        ITEF=MAX(0,MIN(1,IVAL))
      ELSE IF (WHCH(1:2).EQ.'UN'.OR.WHCH(1:2).EQ.'un') THEN
        IBNU=MAX(1,IVAL)
      ELSE
C       Just convert it into a call to the routine PCSETR.
        CALL PCSETR (WHCH,REAL(IVAL))
        IF (ICFELL('PCSETI',2).NE.0) RETURN
      END IF
C
C Done.
C
      RETURN
C
      END

C
C $Id: pcsetc.f,v 1.15 2008-07-27 00:17:20 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE PCSETC (WHCH,CVAL)
C
      CHARACTER*(*) WHCH,CVAL
C
C The subroutine PCSETC may be used to set PLCHHQ parameters which have
C values of type CHARACTER.
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
      COMMON /PCFNNO/ LFNO(43),LFNL
      SAVE   /PCFNNO/
C
      COMMON /PCFNNM/ LFNM(43,2)
      CHARACTER*18 LFNM
      SAVE   /PCFNNM/
C
C Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
      CALL PCBLDA
C
C Check for an uncleared prior error.
C
      IF (ICFELL('PCSETC - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C There are two possibilities:  In the case of the function-code
C character, what is really stored is the index of the character
C in the local collating sequence.  If the default font name is
C being specified, what is really set is the font number.
C
      IF (WHCH(1:2).EQ.'FC'.OR.WHCH(1:2).EQ.'fc') THEN
        NFCC=ICHAR(CVAL(1:1))
      ELSE IF (WHCH(1:2).EQ.'FN'.OR.WHCH(1:2).EQ.'fn') THEN
        DO 101 I=1,LFNL
          IF (CVAL.EQ.LFNM(I,1).OR.CVAL.EQ.LFNM(I,2)) THEN
            NODF=LFNO(I)
            GO TO 102
          END IF
  101   CONTINUE
        CALL SETER ('PCSETC - UNRECOGNIZED FONT NAME',2,1)
        RETURN
      ELSE
        CALL SETER ('PCSETC - UNRECOGNIZED PARAMETER NAME',3,1)
        RETURN
      END IF
C
C Done.
C
  102 RETURN
C
      END

C
C	$Id: wmchbg.f,v 1.6 2008-07-27 00:17:36 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE WMCHBG(X,Y,STR,SIZ)
C
C  Draw the label with an optional surrounding box on
C  a character-by-character basis for characters in STR.
C
      include 'wmcomn.h'
C
      CHARACTER*(*) STR
      CHARACTER*1 CTMP
      DIMENSION XBB(5),YBB(5)
C
      CALL PCGETI('TE - save text extent computation flag',ITEOLD)
      CALL PCGETI('SF - shadow flag',ISF)
      CALL PCGETI('SC - shadow color',ISC)
      CALL PCGETR('SX - shadow X offset',OSX)
      CALL PCGETR('SY - shadow Y offset',OSY)
      CALL PCGETI('CC - character color',ICC)
      CALL PCGETI('OF - outline flag',IOF)
      CALL PCGETI('OC - outline color',IOC)
      CALL PCGETI('FN - font number',IFNO)
C
C  Get starting coordinate for input string.
C
      CALL PCSETI('FN - font number',22)
      CALL PCSETI('TE - compute text extent information',1)
      CALL PLCHHQ(X,Y,STR,SIZ,360.,0.)
      CALL PCGETR('XB - beginning X point for plotting string',XBEG)
      CALL PCGETR('YC - beginning Y point for plotting string',YCEN)
C
      LLIM = LEN(STR)
      IF (IRLBKC .GE. 0) THEN
C
C  Plot background boxes for non-blank characters.
C
        DO 10 I=1,LLIM
          CTMP = STR(I:I)
          CALL PLCHHQ (XBEG,YCEN,CTMP,SIZ,360.,-1.)
          IF (CTMP .NE. ' ') THEN
            CALL PCGETR ('DL',XL)
            CALL PCGETR ('DR',XR)
            CALL PCGETR ('DB',YB)
            CALL PCGETR ('DT',YT)
            
            XBB(1) = (XBEG-XL)-CTYMRG
            YBB(1) = (YCEN+YT)+CTYMRG
            XBB(2) = (XBEG+XR)+CTYMRG
            YBB(2) = (YCEN+YT)+CTYMRG
            XBB(3) = (XBEG+XR)+CTYMRG
            YBB(3) = (YCEN-YB)-CTYMRG
            XBB(4) = (XBEG-XL)-CTYMRG
            YBB(4) = (YCEN-YB)-CTYMRG
            XBB(5) = XBB(1)
            YBB(5) = YBB(1)
            CALL GQFACI(IER,IFACO)
            CALL GSFACI(IRLBKC)
            CALL GFA(5,XBB,YBB)
            CALL GSFACI(IFACO)
          ENDIF
          CALL PCGETR ('XE',XEND)
          XBEG = XEND
   10   CONTINUE
      ENDIF
C
      CALL PCSETI('SF - shadow flag',0)
      IF (IRLLSC .GE. 0) THEN
        CALL PCSETI('SF - shadow flag',1)
        CALL PCSETI('SC - shadow color',IRLLSC)
        CALL PCSETR('SX - shadow X offset',-.14)
        CALL PCSETR('SY - shadow Y offset',-.13)
      ENDIF
      CALL PCSETI('OF - outline flag',0)
      IF (IRLOUC .GE. 0) THEN
        CALL PCSETI('OF - outline flag',1)
        CALL PCSETI('OC - outline color',IRLOUC)
      ENDIF
      CALL PCSETI('CC - character color',IFGTRG)
      CALL PLCHHQ (X,Y,STR,SIZ,0.,0.)
C
C  Restore original attributes.
C
      CALL PCSETI('TE',ITEOLD)
      CALL PCSETI('FN',IFNO)
      CALL PCSETI('SF - shadow flag',ISF)
      CALL PCSETI('SC - shadow color',ISC)
      CALL PCSETR('SX - shadow X offset',OSX)
      CALL PCSETR('SY - shadow Y offset',OSY)
      CALL PCSETI('OF - outline flag',IOF)
      CALL PCSETI('OC - outline color',IOC)
      CALL PCSETI('CC - character color',ICC)
C
      RETURN
      END

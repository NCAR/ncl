C
C	$Id: wmchbg.f,v 1.1 1994-09-09 23:54:45 fred Exp $
C
      SUBROUTINE WMCHBG(X,Y,STR,SIZ,OFFMRG,ICLR)
C
C  Draw the label with surrounding box (using color index ICLR) on
C  a character-by-character basis for characters in STR.  OFFMRG
C  is the margin to be drawn around the character in addition to
C  the Plotchar extent box.
C
C  If ICLR is positive, use it as a color index to draw a background 
C  box in that color and draw the characters using PLCHHQ as usual;
C  if ICLR is negative, use it to draw a background box in that color
C  and draw the characters in the background color with an outline
C  drawn in the foreground color.
C
      include 'wmcomn.h'
C
      CHARACTER*(*) STR
      CHARACTER*1 CTMP
      DIMENSION XBB(5),YBB(5)
C
      LCLR = IABS(ICLR)
      CALL PCGETI ('TE - save text extent computation flag',ITEOLD)
C
C  Turn on text extent computation for the remainder of the subroutine.
C
      CALL PCSETI ('TE - compute text extent information',1)
C
C  Get starting coordinate for input string.
C
      CALL PLCHHQ (X,Y,STR,SIZ,360.,0.)
      CALL PCGETR ('XB - beginning X point for plotting string',XBEG)
      CALL PCGETR ('YC - beginning Y point for plotting string',YCEN)
C
      LLIM = LEN(STR)
      IF (LCLR .NE. 1000) THEN
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
            XBB(1) = (XBEG-XL)-OFFMRG
            YBB(1) = (YCEN+YT)+OFFMRG
            XBB(2) = (XBEG+XR)+OFFMRG
            YBB(2) = (YCEN+YT)+OFFMRG
            XBB(3) = (XBEG+XR)+OFFMRG
            YBB(3) = (YCEN-YB)-OFFMRG
            XBB(4) = (XBEG-XL)-OFFMRG
            YBB(4) = (YCEN-YB)-OFFMRG
            XBB(5) = XBB(1)
            YBB(5) = YBB(1)
            CALL GQFACI(IER,IFACO)
            CALL GSFACI(LCLR)
            CALL GFA(5,XBB,YBB)
            CALL GSFACI(IFACO)
          ENDIF
          CALL PCGETR ('XE',XEND)
          XBEG = XEND
   10   CONTINUE
      ENDIF
C
      IF (ICLR .LT. 0) THEN
C
C  Plot shadows as well as the characters.
C
        CALL PCGETI('SF - shadow flag',ISFOLD)
        CALL PCGETI('SC - shadow color',ISCOLD)
        CALL PCGETR('SX - shadow X offset',SXOLD)
        CALL PCGETR('SY - shadow Y offset',SYOLD)
        CALL PCGETI('CC - character color',ICOLD)
C
        CALL PCSETI('SF - shadow flag',1)
        CALL PCSETI('SC - shadow color',1)
        CALL PCSETR('SX - shadow X offset',-.14)
        CALL PCSETR('SY - shadow Y offset',-.13)
        CALL PCSETI('CC - character color',IFGTRG)
C
        CALL PLCHHQ (X,Y,STR,SIZ,0.,0.)
C
        CALL PCSETI('SF - shadow flag',ISFOLD)
        CALL PCSETI('SC - shadow color',ISCOLD)
        CALL PCSETR('SX - shadow X offset',SXOLD)
        CALL PCSETR('SY - shadow Y offset',SYOLD)
        CALL PCSETI('CC - character color',ICOLD)
C
      ELSE
C
C  Just plot the original character string.
C
        CALL PLCHHQ (X,Y,STR,SIZ,0.,0.)
      ENDIF
C
C  Restore original attributes.
C
      CALL PCSETI ('TE',ITEOLD)
C
      RETURN
      END

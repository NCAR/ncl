C
C	$Id: wmlabt.f,v 1.2 1994-09-12 19:07:35 fred Exp $
C
      SUBROUTINE WMLABT(X,Y,LABEL,IFLG,IBGCLR)
C
C  Plots regional temperature labels with the option of
C  plotting arrows, depending on the value of IFLG.
C
C  If IFLG =
C
C       0 - Plots the label centered at (X,Y).
C       1-12 plots the label with an arrow positioned as per:
C
C
C
C                      6   5   4   3   2
C                       \   \  |  /   /
C                        \   \ | /   /
C                         -----------
C                         |         |
C                    7 -- |  LABEL  | -- 1
C                         |         |
C                         -----------
C                        /   / | \  \
C                       /   /  |  \  \
C                      8   9  10  11  12
C
C
C
C  For IFLG=1 through 12 the tip of the arrow is positioned at (X,Y).
C
C     0 <= IBGCLR <  1000  --  IBGCLR will be used as the color index 
C                              for a background box around the label.
C -1000 <  IBGCLR <     0  --  IBGCLR will be used as the color index 
C                              for a background box around the label,
C                              and the label will be drawn with color
C                              index IFGTRG with a shadow in the
C                              foreground color.
C          IBGCLR = -1000  --  Draw the string and arrow and return, no
C                              background box.
C          IBGCLR =  1000  --  Draw the string in the background color
C                              and an outline and shadow in the foreground
C                              color, no background box.
C
      CHARACTER*(*) LABEL
      PARAMETER (D2RAD=.017453293)
C
      include 'wmcomn.h'
C
      DIMENSION ANGS(12)
      DATA ANGS/0.,45.,60.,90.,120.,135.,180.,225.,250.,270.,300.,315./       
C
C  Save the current line and fill colors and set them to ICOLOR.
C
      CALL GQFAIS(IER,IFAISO)
      CALL GQFACI(IER,IFCLRO)
      CALL GQPLCI(IER,ILCLRO)
      CALL GSFAIS(1)
      CALL GSFACI(ICOLOR)
      CALL GSPLCI(ICOLOR)
C
C  Convert X and Y to NDC and work in NDC space.
C
      CALL WMW2NX(1,X,XNDC)
      CALL WMW2NY(1,Y,YNDC)
      CALL GQCNTN(IER,NTRO)
      CALL GSELNT(0)
C
      SIZEL = WSIZET
C
C  Use Plotchar font Helvetica-bold.
C
      CALL PCGETI ('FN',IFNO)
      CALL PCSETI ('FN',22)
C
C  If IFLG is out out range, plot the string and return.
C
      IF (IFLG.GT.12 .OR. IFLG.LT.0) THEN
        CALL PLCHHQ (XNDC,YNDC,LABEL,SIZEL,0.,0.)
        GO TO 20
      ENDIF
C
C  Scale the length of an arrow to leave space between the tail of
C  the arrow and the label.
C
      ELEN = 1.25*ARWSIZ
C
C  Compute new coordinate positions to reposition the label so that
C  the arrow will have its tip at (X,Y).
C
      D1 = 0.707*ELEN
      D2 = ELEN*COS(60.*D2RAD)
      D3 = ELEN*SIN(60.*D2RAD)
      CALL PCSETI ('TE',1)
      CALL PLCHHQ (XNDC,YNDC,LABEL,SIZEL,360.,0.)
      CALL PCGETR ('DL',XL)
      CALL PCGETR ('DR',XR)
      CALL PCGETR ('DB',YB)
      CALL PCGETR ('DT',YT)
      CALL PCSETI ('TE',0)
C     
      IF (IFLG .EQ. 0) THEN
        XP = XNDC
        YP = YNDC
      ELSE IF (IFLG .EQ. 1) THEN
        XP = XNDC - ELEN - XR
        YP = YNDC
      ELSE IF (IFLG .EQ. 2) THEN
        XP = XNDC - D1 - XR
        YP = YNDC - D1 - YT
      ELSE IF (IFLG .EQ. 3) THEN
        XP = XNDC - D2 - 0.5*XR
        YP = YNDC - D3 - YT
      ELSE IF (IFLG .EQ. 4) THEN
        XP = XNDC
        YP = YNDC - ELEN - YT
      ELSE IF (IFLG .EQ. 5) THEN
        XP = XNDC + D2 + 0.5*XL
        YP = YNDC - D3 - YT
      ELSE IF (IFLG .EQ. 6) THEN
        XP = XNDC + D1 + XL
        YP = YNDC - D1 - YT
      ELSE IF (IFLG .EQ. 7) THEN
        XP = XNDC + ELEN + XL
        YP = YNDC
      ELSE IF (IFLG .EQ. 8) THEN
        XP = XNDC + D1 + XL
        YP = YNDC + D1 + YB
      ELSE IF (IFLG .EQ. 9) THEN
        XP = XNDC + D2 + 0.5*XL
        YP = YNDC + D3 + YB
      ELSE IF (IFLG .EQ. 10) THEN
        XP = XNDC
        YP = YNDC + ELEN + YB
      ELSE IF (IFLG .EQ. 11) THEN
        XP = XNDC - D2 - 0.5*XR
        YP = YNDC + D3 + YB
      ELSE IF (IFLG .EQ. 12) THEN
        XP = XNDC - D1 - XR
        YP = YNDC + D1 + YB
      ENDIF
C
C  Draw the label.
C
      IF (IBGCLR .EQ.  1000) THEN
C
C  Characters drawn in background color with an outline in the 
C  foreground color and a shadow in the foreground color.
C
        CALL PCGETI('CC - character color',ICOLD)
        CALL PCGETI('FN - font name',IFNOLD)
        CALL PCGETI('SF - shadow flag',ISFOLD)
        CALL PCGETR('OL - outline width',WIDOLD)
        CALL PCGETI('OF - outline flag',IOFOLD)
        CALL PCGETI('SC - shadow color',ISHOLD)
C
        CALL PCSETI('CC - character color',0)
        CALL PCSETI('FN - font name',22)
        CALL PCSETI('SF - shadow flag',1)
        CALL PCSETR('OL - outline width',0.5)
        CALL PCSETI('OF - outline flag',1)
        CALL PCSETI('SC - shadow color',1)
        CALL PCSETR('SX - shadow X offset',-.08)
        CALL PCSETR('SY - shadow Y offset',-.08)
C
        CALL PLCHHQ (XP,YP,LABEL,SIZEL,0.,0.)
C
        CALL PCSETI('CC',ICOLD)
        CALL PCSETI('FN',IFNOLD)
        CALL PCSETI('SF - shadow flag',ISFOLD)
        CALL PCSETR('OL - outline width',WIDOLD)
        CALL PCSETI('OF - outline flag',IOFOLD)
        CALL PCSETI('SC - shadow color',ISHOLD)
      ELSE IF (IBGCLR .EQ. -1000) THEN
C
C  Just plot the string.
C  
        CALL PLCHHQ (XP,YP,LABEL,SIZEL,0.,0.)
      ELSE
C
C  Draw string with background boxes.
C
        CALL WMCHBG(XP,YP,LABEL,SIZEL,TMPMRG,IBGCLR)
      ENDIF
      IF (IFLG .EQ. 0) GO TO 20
C
C  Draw the arrow.
C
      CALL WMGETR ('ARD',ARDO)
      CALL WMSETR ('ARD',ANGS(IFLG))
      CALL WMLABS (XNDC,YNDC,'ARROW')
C
C  Restore the original environment.
C
   20 CONTINUE
      CALL GSFAIS(IFAISO)
      CALL GSFACI(IFCLRO)
      CALL GSPLCI(ILCLRO)
      CALL GSELNT(NTRO)
      CALL PCSETI ('FN',IFNO)
      CALL WMSETR ('ARD',ARDO)
C
      RETURN
      END

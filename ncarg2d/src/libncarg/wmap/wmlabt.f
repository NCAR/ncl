C
C	$Id: wmlabt.f,v 1.7 2008-07-27 00:17:37 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE WMLABT(X,Y,LABEL,IFLG)
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
C  For IFLG=0, only the label is drawn and not the arrow.
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
      CALL PCGETI('FN',IFNO)
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
      CALL PCSETI ('FN',22)
      JFLG = IFLG
C
C  If IFLG is out out range, just plot the string.
C
      IF (IFLG.GT.12 .OR. IFLG.LT.0) THEN
        JFLG = 0
      ENDIF
C
C  Scale the length of an arrow to leave space between the tail of
C  the arrow and the label.
C
      ELEN = 1.25*ARWSIZ*ARWLEN
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
      IF (JFLG .EQ. 0) THEN
        XP = XNDC
        YP = YNDC
      ELSE IF (JFLG .EQ. 1) THEN
        XP = XNDC - ELEN - XR
        YP = YNDC
      ELSE IF (JFLG .EQ. 2) THEN
        XP = XNDC - D1 - XR
        YP = YNDC - D1 - YT
      ELSE IF (JFLG .EQ. 3) THEN
        XP = XNDC - D2 - 0.5*XR
        YP = YNDC - D3 - YT
      ELSE IF (JFLG .EQ. 4) THEN
        XP = XNDC
        YP = YNDC - ELEN - YT
      ELSE IF (JFLG .EQ. 5) THEN
        XP = XNDC + D2 + 0.5*XL
        YP = YNDC - D3 - YT
      ELSE IF (JFLG .EQ. 6) THEN
        XP = XNDC + D1 + XL
        YP = YNDC - D1 - YT
      ELSE IF (JFLG .EQ. 7) THEN
        XP = XNDC + ELEN + XL
        YP = YNDC
      ELSE IF (JFLG .EQ. 8) THEN
        XP = XNDC + D1 + XL
        YP = YNDC + D1 + YB
      ELSE IF (JFLG .EQ. 9) THEN
        XP = XNDC + D2 + 0.5*XL
        YP = YNDC + D3 + YB
      ELSE IF (JFLG .EQ. 10) THEN
        XP = XNDC
        YP = YNDC + ELEN + YB
      ELSE IF (JFLG .EQ. 11) THEN
        XP = XNDC - D2 - 0.5*XR
        YP = YNDC + D3 + YB
      ELSE IF (JFLG .EQ. 12) THEN
        XP = XNDC - D1 - XR
        YP = YNDC + D1 + YB
      ENDIF
C
C  Draw the label.
C
      CALL WMGETR('CMG',CMGO)
      CALL WMSETR('CMG',TMPMRG)
      CALL WMCHBG(XP,YP,LABEL,SIZEL)
      CALL WMSETR('CMG',CMGO)
C
      IF (JFLG .NE. 0) THEN
C
C  Draw the arrow.
C
        CALL WMGETR ('ARD',ARDO)
        CALL WMSETR ('ARD',ANGS(JFLG))
        CALL WMLABS (XNDC,YNDC,'ARROW')
      ENDIF
C
C  Restore the original environment.
C
   20 CONTINUE
      CALL GSFAIS(IFAISO)
      CALL GSFACI(IFCLRO)
      CALL GSPLCI(ILCLRO)
      CALL GSELNT(NTRO)
      CALL WMSETR('ARD',ARDO)
      CALL PCSETI('FN',IFNO)
C
      RETURN
      END

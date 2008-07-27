C
C $Id: ngezlogo.f,v 1.3 2008-07-27 00:17:17 haley Exp $
C                                                                      
C                Copyright (C)  2002
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE NGEZLOGO()
C
C  Draws a logo at the lower right of the current plot on all 
C  active workstations as per the current user settings.
C 
      SAVE
C
      include 'ngcomn.h'
C
C  Check for an uncleared prior error.
C
      IF (ICFELL('NGEZLOGO - Uncleared prior error',1) .NE. 0) RETURN
C
      CALL GQCNTN(IER,NTNR)
      CALL GETSET(VX1,VX2,VY1,VY2,WX1,WX2,WY1,WY2,ISC)
      CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)
C
C  Get the current logo settings.
C
      CALL NGGETI('LT',LTYPE)
      CALL NGGETI('LC',LCOL1)
      CALL NGGETI('LB',LCOL2)
      CALL NGGETR('OX',OLX)
      CALL NGGETR('OY',OLY)
      CALL NGGETR('OS',OLS)
C
C  Adjust the X position.
C
      CALL GQACWK(1,IER,NUMACT,IWK)
      IF (LTYPE .EQ. 1) THEN
        OLYT = OLY
        OLXT = OLX
      ELSE If (LTYPE .EQ. 2) THEN
        OLYT = OLY+0.01
        OLXT = OLX+0.01
      ELSE If (LTYPE .EQ. 3) THEN
        OLYT = OLY+0.015
        OLXT = OLX-0.08
      ELSE If (LTYPE .EQ. 4) THEN
        OLYT = OLY+0.015
        OLXT = OLX-0.08
      ELSE If (LTYPE .EQ. 5) THEN
        OLYT = OLY+0.015
        OLXT = OLX-0.12
      ELSE
        OLYT = OLY
        OLXT = OLX
      ENDIF
C
C  Loop through the active workstations and put out the logo.
C
      DO 10 I=1,NUMACT
        CALL GQACWK(I,IER,NUMACT,IWK)
        CALL NGLOGO(IWK,OLXT,OLYT,OLS,LTYPE,LCOL1,LCOL2)
   10 CONTINUE 
C
C  Restore the SET call and the current normalization transformation.
C
      CALL SET(VX1,VX2,VY1,VY2,WX1,WX2,WY1,WY2,ISC)
      CALL GSELNT(NTNR)
C
      RETURN
      END

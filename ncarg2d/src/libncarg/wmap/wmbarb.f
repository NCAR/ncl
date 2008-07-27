C
C	$Id: wmbarb.f,v 1.7 2008-07-27 00:17:36 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE WMBARB(X,Y,U,V)
C
C  This subroutine plots wind barbs with the origin of the barb at
C  (X,Y).  U and V are the components of the wind vector.
C
      include 'wmcomn.h'
C
      PARAMETER (D2RAD=.017453293)
      DIMENSION XX(4),YY(4)
C
C  TRIINC - Increment for which triangles are drawn.
C  TICINC - Increment for which a full tic is drawn.
C
      DATA TRIINC,TICINC/50.,10./
C
C  Save the current line and fill colors and set them to ICOLOR.
C
      CALL GQFAIS(IER,IFAISO)
      CALL GQFACI(IER,IFCLRO)
      CALL GQPLCI(IER,ILCLRO)
      CALL GQLWSC(IER,RLNWTH)
      CALL GSFAIS(1)
      CALL GSFACI(ICOLOR)
      CALL GSPLCI(ICOLOR)
      CALL WMGETR('BLW',BRBLWD)
      CALL GSLWSC(BRBLWD)
      CALL PCGETI ('FN',IFNO)
C
C  Convert X and Y to NDC and work in NDC space.
C
      CALL WMW2NX(1,X,XNDC)
      CALL WMW2NY(1,Y,YNDC)
      CALL GQCNTN(IER,NTRO)
      CALL GSELNT(0)
C
      SPEED  = SQRT(U*U+V*V)
C
C  Handle some initial special cases.
C
      WBXL = 1.
      WBXR = 0.
      WBYB = 1.
      WBYT = 0.
      IF (SPEED .LT. 0.5) THEN
C
C  Calm.
C
        CALL NGGETI('CT',ICTYPO)
        CALL NGSETI('CT',1)
        CALL NGDOTS(XNDC,YNDC,1,WBCLMR*WBSHFT,ICOLOR)
        CALL NGDOTS(XNDC,YNDC,1,1.50*WBCLMR*WBSHFT,ICOLOR)
        CALL NGSETI('CT',ICTYPO)
        WBXL = XNDC
        WBXR = XNDC
        WBYB = YNDC
        WBYT = YNDC
        GO TO 100
      ENDIF
C
C  Shift the wind direction by 180 degrees if the user has
C  set the direction flag.
C
      IF (IWBDIR .NE. 0) THEN
        UP = -U
        VP = -V
      ELSE
        UP =  U
        VP =  V
      ENDIF
      COSANG = UP/SPEED
      SINANG = VP/SPEED
C
C  Draw shaft (adjust if the flag is set to allow for the sky cover
C  circle to be drawn at the base).
C
      IF (IWBBAS .EQ. 0) THEN
        XX(1) = XNDC
        YY(1) = YNDC
      ELSE
        XX(1) = XNDC+0.5*WBBASE*WBSHFT*COSANG
        YY(1) = YNDC+0.5*WBBASE*WBSHFT*SINANG
      ENDIF
      XEND  = XNDC+WBSHFT*COSANG
      YEND  = YNDC+WBSHFT*SINANG
      XX(2) = XEND
      YY(2) = YEND
      CALL GPL(2,XX,YY)
      IF (SPEED .LT. 2.5) THEN
C
C  Shaft already drawn.
C
        WBXL = XEND
        WBXR = XEND
        WBYB = YEND
        WBYT = YEND
        GO TO 100
      ENDIF
C
C  Compute some local variables.
C
      RANG = WBBANG*D2RAD
      XNOM =  COS(RANG)*WBFTIC*WBSHFT
      YNOM = -SIN(RANG)*WBFTIC*WBSHFT
      XTIC = XNOM*COSANG-YNOM*SINANG
      YTIC = XNOM*SINANG+YNOM*COSANG
      XINC = WBDIST*WBSHFT*COSANG
      YINC = WBDIST*WBSHFT*SINANG
      XSID = XNOM*COSANG
      YSID = XNOM*SINANG
      XPOS = XEND
      YPOS = YEND
      IF (SPEED .LT. 7.5) THEN
C
C  Draw a half tic spaced WBDIST down the shaft.
C
        XX(1) = XEND-XINC
        YY(1) = YEND-YINC
        XX(2) = XX(1)+0.5*XTIC
        YY(2) = YY(1)+0.5*YTIC
        CALL GPL(2,XX,YY)
        WBXL = MIN(XX(1),XX(2))
        WBXR = MAX(XX(1),XX(2))
        WBYB = MIN(YY(1),YY(2))
        WBYT = MAX(YY(1),YY(2))
        GO TO 100
      ENDIF
C
C  Draw the appropriate number of triangles.
C
      NUMTRI = INT((SPEED+2.5)/TRIINC)
      IF (NUMTRI .GT. 0) THEN
        DO 10 NT=1,NUMTRI
          IF (NT .EQ. 1) THEN
C
C  Extend the shaft and redraw it (it would not be correct otherwise
C  if it were being drawn in a non-default linewidth, for example).
C
            IF (IWBBAS .EQ. 0) THEN
              XX(1) = XNDC
              YY(1) = YNDC
            ELSE
              XX(1) = XNDC+0.5*WBBASE*WBSHFT*COSANG
              YY(1) = YNDC+0.5*WBBASE*WBSHFT*SINANG
            ENDIF
            XX(2) = XEND+XSID
            YY(2) = YEND+YSID
            CALL GPL(2,XX,YY)
            XPOS = XPOS+XINC
            YPOS = YPOS+YINC
          ENDIF
C
C  Draw triangles.
C
          XPOS  = XPOS-XINC
          YPOS  = YPOS-YINC
          XX(1) = XPOS
          YY(1) = YPOS
          XX(2) = XPOS+XTIC
          YY(2) = YPOS+YTIC
          XX(3) = XPOS+XSID
          YY(3) = YPOS+YSID
          XX(4) = XX(1)
          YY(4) = YY(1)
          CALL GFA(4,XX,YY)
          DO 30 NE=1,4
            WBXL = MIN(WBXL,XX(NE))
            WBXR = MAX(WBXR,XX(NE))
            WBYB = MIN(WBYB,YY(NE))
            WBYT = MAX(WBYT,YY(NE))
   30     CONTINUE
          XPOS  = XPOS-XINC
          YPOS  = YPOS-YINC
          SPEED = SPEED-TRIINC
   10   CONTINUE
      ENDIF
C
C  Draw the full ticks.
C
      NUMTIC = INT((SPEED+2.5)/TICINC)
      IF (NUMTIC .GT. 0) THEN
        DO 20 I=1,NUMTIC
          XX(1) = XPOS
          YY(1) = YPOS
          XX(2) = XPOS+XTIC
          YY(2) = YPOS+YTIC
          CALL GPL(2,XX,YY)
          DO 40 NE=1,2
            WBXL = MIN(WBXL,XX(NE))
            WBXR = MAX(WBXR,XX(NE))
            WBYB = MIN(WBYB,YY(NE))
            WBYT = MAX(WBYT,YY(NE))
   40     CONTINUE
          XPOS  = XPOS-XINC
          YPOS  = YPOS-YINC
          SPEED = SPEED-TICINC
   20   CONTINUE
      ENDIF
C
C  Draw a half tick if needed.
C
      IF (SPEED .GT. 2.5) THEN
        XX(1) = XPOS
        YY(1) = YPOS
        XX(2) = XPOS+0.5*XTIC
        YY(2) = YPOS+0.5*YTIC
        CALL GPL(2,XX,YY)
        DO 50 NE=1,2
          WBXL = MIN(WBXL,XX(NE))
          WBXR = MAX(WBXR,XX(NE))
          WBYB = MIN(WBYB,YY(NE))
          WBYT = MAX(WBYT,YY(NE))
   50   CONTINUE
      ENDIF
  100 CONTINUE
C
C  Restore the original environment.
C
      CALL GSFAIS(IFAISO)
      CALL GSFACI(IFCLRO)
      CALL GSPLCI(ILCLRO)
      CALL GSELNT(NTRO)
      CALL GSLWSC(RLNWTH)
      CALL PCSETI ('FN',IFNO)
C
      RETURN
      END

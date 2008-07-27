
C	$Id: wmlabs.f,v 1.13 2008-07-27 00:17:36 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE WMLABS(X,Y,SYMTYP)
C
C  Plot the symbol indicated by the string SYMTYP.  The symbol
C  is centered at (X,Y), where X and Y are world coordinates.
C  Legal values for SYMTYP are:
C
C         'HI'
C         'LOW'
C         'ARROW'
C         'DOT'
C         'CLOUD'
C         'ICE'
C         'IS' (Intermittent showers)
C         'IT' (Sunny, chance of T-storms)
C         'MC' (Mostly cloudy)
C         'MS' (Mostly sunny)
C         'RAIN'
C         'RS' (Rain and snow)
C         'SNOWFLAKES'
C         'SUN'
C         'THUNDERSTORM'
C         'VECTOR'
C         'WIND'
C
      CHARACTER*(*) SYMTYP
      CHARACTER*1 FCODE
      CHARACTER*5 CHRDIST
C
      include 'wmcomn.h'
C
C  Reference arrow.
C
      PARAMETER (IADIM=8,D2RAD=.017453293)
      DIMENSION XX(IADIM),YY(IADIM),ARROWX(IADIM),ARROWY(IADIM)
      REAL OLDWN(4),OLDVP(4),OCLIP(4)
      DIMENSION TMPX(IADIM),TMPY(IADIM)
      DATA XX/-1.000, -0.330, -0.360,  0.000,
     +        -0.360, -0.330, -1.000, -1.000  /
      DATA YY/-0.030, -0.030, -0.120,  0.000,
     +         0.120,  0.030,  0.030, -0.030  /
C
C  Array for vector draws.
C
      PARAMETER (IVSDM=5)
      DIMENSION VECTSX(IVSDM),VECTSY(IVSDM)
C
C  Save the current text function code and set it to a colon for
C  this subroutine.
C
      CALL PCGETC('FC',FCODE)
      CALL PCSETC('FC',':')
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
C
C  Save normalization transformation number 1 along with its
C  window and viewport and define a new transformation 1
C  that has as its window and viewport the current clip
C  rectangle.
C
      CALL GQCNTN(IER,NTRO)
      CALL GQCLIP(IER,ICLIP,OCLIP)
      CALL GQNT(1,IER,OLDWN,OLDVP)
C
      CALL GSWN(1,OCLIP(1),OCLIP(2),OCLIP(3),OCLIP(4))
      CALL GSVP(1,OCLIP(1),OCLIP(2),OCLIP(3),OCLIP(4))
      CALL GSELNT(1)
C
C  Turn on text extent computation for the remainder of the subroutine.
C
      CALL PCSETI('TE',1)
C
      SIZEL = WSIZES
C
      IF (SYMTYP(1:1).EQ.'H' .OR. SYMTYP(1:1).EQ.'h') THEN
C
C  Draw a high symbol.
C
        RADIUS = 1.10*SIZEL
        OFFSET = 0.15*RADIUS
        CALL NGGETI('CT',ICTYPO)
        CALL NGSETI('CT',0)
C
C  Shadow.
C
        IF (IHIGC1 .GE. 0) THEN
          CALL NGDOTS(XNDC-OFFSET,YNDC-OFFSET,1,2.*RADIUS,IHIGC1)
        ENDIF
C
C  Character background.
C
        IF (IHIGC2 .GE. 0) THEN
          CALL NGDOTS(XNDC,YNDC,1,2.*RADIUS,IHIGC2)
        ENDIF
        CALL NGSETI('CT',1)
C
C  Circumscribed circle.
C
        IF (IHIGC4 .GE. 0) THEN
          CALL NGDOTS(XNDC,YNDC,1,2.*RADIUS,IHIGC4)
        ENDIF
        CALL NGSETI('CT',ICTYPO)
        CALL PCGETI('CC - character color',ICLRO)
        CALL PCGETI('FN - font name',IFNTO)
        CALL PCSETI('CC - character color',IHIGC3)
        CALL PCSETI('FN - font name',22)
C
C  "H" character.
C
        IF (IHIGC3 .GE. 0) THEN
          CALL PLCHHQ(XNDC,YNDC,'H',SIZEL,0.,0.)
        ENDIF
        CALL PCSETI('CC - character color',ICLRO)
        CALL PCSETI('FN - font name',IFNTO)
      ELSE IF (SYMTYP(1:1).EQ.'L' .OR. SYMTYP(1:1).EQ.'l') THEN
C
C  Draw a low symbol.
C
        RADIUS = 1.0*SIZEL
        OFFSET = 0.20*RADIUS
        CALL NGGETI('CT',ICTYPO)
        CALL NGSETI('CT',0)
C
C  Shadow.
C
        IF (ILOWC1 .GE. 0) THEN
          CALL NGDOTS(XNDC,YNDC,1,2.*RADIUS+OFFSET,ILOWC1)
        ENDIF
C
C  Character background.
C
        IF (ILOWC2 .GE. 0) THEN
          CALL NGDOTS(XNDC,YNDC,1,2.*RADIUS,ILOWC2)
        ENDIF
        CALL NGSETI('CT',1)
C
C  Circumscribed circle.
C
        IF (ILOWC4 .GE. 0) THEN
          CALL NGDOTS(XNDC,YNDC,1,2.*RADIUS,ILOWC4)
        ENDIF
        CALL NGSETI('CT',ICTYPO)
        CALL PCGETI('CC - character color',ICLRO)
        CALL PCGETI('FN - font name',IFNTO)
        CALL PCSETI('CC - character color',ILOWC3)
        CALL PCSETI('FN - font name',22)
C
C  "L" character.
C
        IF (ILOWC3 .GE. 0) THEN
          CALL PLCHHQ (XNDC,YNDC,':F22:L',SIZEL,0.,0.)
        ENDIF
        CALL PCSETI('CC - CHARACTER COLOR',ICLRO)
        CALL PCSETI('FN - font name',IFNTO)
      ELSE IF (SYMTYP(1:1).EQ.'A' .OR. SYMTYP(1:1).EQ.'a') THEN
C
C  Draw an arrow of length ARWSIZ in direction ARWDIR with the
C  point of the arrow at (X,Y).
C
C  Scale the arrow.
C
        SS = ARWSIZ
        DO 10 I=1,IADIM
          IF (I.EQ.1 .OR. I.EQ.7 .OR. I.EQ.8) THEN
            ARROWX(I) = SS*ARWLEN*XX(I)
          ELSE
            ARROWX(I) = SS*XX(I)
          ENDIF
          ARROWY(I) = SS*YY(I)
   10   CONTINUE
C
C  Rotate the arrow.
C
        COSANG = COS(D2RAD*ARWDIR)
        SINANG = SIN(D2RAD*ARWDIR)
        DO 20 I=1,IADIM
          TMPX(I) = ARROWX(I)*COSANG-ARROWY(I)*SINANG
          TMPY(I) = ARROWX(I)*SINANG+ARROWY(I)*COSANG
   20   CONTINUE
        DO 30 I=1,IADIM
          ARROWX(I) = TMPX(I)
          ARROWY(I) = TMPY(I)
   30   CONTINUE
C
C  Translate the arrow.
C
        DO 40 I=1,IADIM
          ARROWX(I) = XNDC+ARROWX(I)
          ARROWY(I) = YNDC+ARROWY(I)
   40   CONTINUE
C
C  Draw a shadow if the arrow shadow color is non-negative.
C
        IF (IARSHC .GE. 0) THEN
          DO 50 I=1,IADIM 
            TMPX(I) = ARROWX(I)-.0019*ARWSIZ/0.035
            TMPY(I) = ARROWY(I)-.0019*ARWSIZ/0.035
   50     CONTINUE
          CALL GSFACI(IARSHC)
          CALL GFA(IADIM,TMPX,TMPY)
        ENDIF
C
C  Draw the arrow.
C
        CALL GSFACI(IAROWC)
        CALL GFA(IADIM,ARROWX,ARROWY)
C
C  Draw an outline around the arrow if the outline color is non-negative.
C
        IF (IAROUC .GE. 0) THEN
          CALL GSPLCI(IAROUC)
          CALL GPL(IADIM,ARROWX,ARROWY)
        ENDIF
      ELSE IF (SYMTYP(1:1).EQ.'V' .OR. SYMTYP(1:1).EQ.'v') THEN
C
C  Draw a vector scaled by a size based on the ratio between
C  VCNREF and VCUREF, in direction VCWDIR with the
C  tail of the vector at (X,Y).  Scale the vector arrow head 
C  by VCHSIZ.
C
C
C  Create a base vector of the correct size that will be 
C  rotated and translated appropirately.
C
        SCALE = VCNREF/VCUREF
        HEADANG = VHDANG*D2RAD
        VECTSX(1) = 0.
        VECTSX(2) = SCALE*VCSIZE
        VECTSX(3) = VECTSX(2)-COS(HEADANG)*VCHSIZ
        VECTSX(4) = VECTSX(2)
        VECTSX(5) = VECTSX(3)
C
        VECTSY(1) = 0.
        VECTSY(2) = 0.
        VECTSY(3) = SIN(HEADANG)*VCHSIZ
        VECTSY(4) = 0.
        VECTSY(5) = -VECTSY(3)
C
C  Rotate.
C
        COSANG = COS(D2RAD*VCWDIR)
        SINANG = SIN(D2RAD*VCWDIR)
        DO 110 I=1,IVSDM
          TMPX(I) = VECTSX(I)*COSANG-VECTSY(I)*SINANG
          TMPY(I) = VECTSX(I)*SINANG+VECTSY(I)*COSANG
  110   CONTINUE
        DO 111 I=1,IVSDM
          VECTSX(I) = TMPX(I)
          VECTSY(I) = TMPY(I)
  111   CONTINUE
C
C  Translate the vector.
C
        DO 120 I=1,IVSDM
          VECTSX(I) = VECTSX(I)+XNDC
          VECTSY(I) = VECTSY(I)+YNDC
  120   CONTINUE
C
C  Draw the vector.
C
        CALL GSPLCI(VCCOLR)
        CALL GSLWSC(VCLWID)
        CALL GPL(IVSDM,VECTSX,VECTSY)

      ELSE IF (SYMTYP(1:1).EQ.'D' .OR. SYMTYP(1:1).EQ.'d') THEN
C
C  Draw a dot to mark a city location.
C
        RADIUS = 0.5*CDOTSZ
        CALL NGGETI('CT',ICTYPO)
        CALL NGSETI('CT',0)
        CALL NGDOTS(XNDC,YNDC,1,3.5*RADIUS,IDOTBG)
        CALL NGDOTS(XNDC,YNDC,1,2.0*RADIUS,IDOTCO)
        CALL NGSETI('CT',ICTYPO)
      ELSE IF (SYMTYP(1:2).EQ.'SU' .OR. SYMTYP(1:2).EQ.'su' .OR.
     +         SYMTYP(1:2).EQ.'MC' .OR. SYMTYP(1:2).EQ.'mc' .OR.
     +         SYMTYP(1:2).EQ.'IS' .OR. SYMTYP(1:2).EQ.'is' .OR.
     +         SYMTYP(1:2).EQ.'IT' .OR. SYMTYP(1:2).EQ.'it' .OR.
     +         SYMTYP(1:2).EQ.'MS' .OR. SYMTYP(1:2).EQ.'ms') THEN
C
C  Draw a sun as a daily weather icon.
C
        CALL PCGETI('CC - character color',ICC)
C
C  Cloud first if mostly sunny or intermittant showers.
C
        IF(SYMTYP(1:2).EQ.'MS' .OR. SYMTYP(1:2).EQ.'ms') THEN
          CXOFF =  0.02
          CYOFF = -0.02
          IF (ICLDC3 .GE. 0) THEN
C
            CALL PCGETI('SF - shadow flag',ISF)
            CALL PCGETI('SC - shadow color',ISC)
            CALL PCGETR('SX - shadow X offset',OSX)
            CALL PCGETR('SY - shadow Y offset',OSY)
C
            CALL PCSETI('SF - shadow flag',1)
            CALL PCSETI('SC - shadow color',ICLDC3)
            CALL PCSETI('CC - character color',ICLDC3)
            CALL PCSETR('SX - shadow X offset', -.016)
            CALL PCSETR('SY - shadow Y offset', -.02)
C
            CALL PLCHHQ(XNDC+CXOFF,YNDC+CYOFF,':F37:k',10.*SIZEL,0.,0.)
C
            CALL PCSETI('SF - shadow flag',ISF)
            CALL PCSETI('SC - shadow color',ISC)
            CALL PCSETR('SX - shadow X offset',OSX)
            CALL PCSETR('SY - shadow Y offset',OSY)
          ENDIF
          IF (ICLDC1 .GE. 0) THEN
            CALL PCSETI('CC',ICLDC1)
            CALL PLCHHQ(XNDC+CXOFF,YNDC+CYOFF,':F37:k',10.*SIZEL,0.,0.)       
          ENDIF
          IF (ICLDC2 .GE. 0) THEN
            CALL PCSETI('CC',ICLDC2)
            CALL PLCHHQ(XNDC+CXOFF,YNDC+CYOFF,':F137:k',10.*SIZEL,0.,0.)       
          ENDIF
        ENDIF
C
C  Draw sun.
C
        SUNSIZ = 7.5*SIZEL
        SXOFF = 0.0
        SYOFF = 0.0
        IF (SYMTYP(1:2).EQ.'MS' .OR. SYMTYP(1:2).EQ.'ms' .OR.
     +      SYMTYP(1:2).EQ.'IT' .OR. SYMTYP(1:2).EQ.'it' .OR.
     +      SYMTYP(1:2).EQ.'IS' .OR. SYMTYP(1:2).EQ.'is' .OR.
     +      SYMTYP(1:2).EQ.'MC' .OR. SYMTYP(1:2).EQ.'mc') THEN
          SXOFF = -0.02
          SYOFF =  0.02
        ENDIF
        IF (ISUNC4 .GE. 0) THEN
C
          CALL PCGETI('SF - shadow flag',ISF)
          CALL PCGETI('SC - shadow color',ISC)
          CALL PCGETR('SX - shadow X offset',OSX)
          CALL PCGETR('SY - shadow Y offset',OSY)
C
          CALL PCSETI('SF - shadow flag',1)
          CALL PCSETI('SC - shadow color',ISUNC4)
          CALL PCSETI('CC - character color',ISUNC4)
          CALL PCSETR('SX - shadow X offset', -.016)
          CALL PCSETR('SY - shadow Y offset', -.02)
C
          CALL PLCHHQ(XNDC+SXOFF,YNDC+SYOFF,':F37:l',SUNSIZ,0.,0.)
C
          CALL PCSETI('SF - shadow flag',ISF)
          CALL PCSETI('SC - shadow color',ISC)
          CALL PCSETR('SX - shadow X offset',OSX)
          CALL PCSETR('SY - shadow Y offset',OSY)
        ENDIF
        IF (ISUNC2 .GE. 0) THEN
          CALL PCSETI('CC',ISUNC2)
          CALL PLCHHQ(XNDC+SXOFF,YNDC+SYOFF,':F37:l',SUNSIZ,0.,0.)
          IF (ISUNC3 .GE. 0) THEN
            CALL PCSETI('CC',ISUNC3)
            CALL PLCHHQ(XNDC+SXOFF,YNDC+SYOFF,':F137:l',SUNSIZ,0.,0.)
          ENDIF
        ENDIF
        IF (ISUNC1 .GE. 0) THEN
          CALL PCSETI('CC',ISUNC1)
          CALL PLCHHQ(XNDC+SXOFF,YNDC+SYOFF,':F37:m',SUNSIZ,0.,0.)
          IF (ISUNC3 .GE. 0) THEN
            CALL PCSETI('CC',ISUNC3)
            CALL PLCHHQ(XNDC+SXOFF,YNDC+SYOFF,':F137:m',SUNSIZ,0.,0.)
          ENDIF
        ENDIF
C
C  Add showers.
C
        IF (SYMTYP(1:2).EQ.'IS' .OR. SYMTYP(1:2).EQ.'is' .OR.
     +      SYMTYP(1:2).EQ.'IT' .OR. SYMTYP(1:2).EQ.'it') THEN
          CALL PCSETI('CC - character color',ICOLOR)
          CALL PLCHHQ(XNDC+CXOFF,YNDC+CYOFF,':F37:o',10.*SIZEL,0.,0.)
        ENDIF
C
C  Add thunder bolt.
C
        IF (SYMTYP(1:2).EQ.'IT' .OR. SYMTYP(1:2).EQ.'it') THEN
          OXNDC = XNDC+0.019*50.*SIZEL+CXOFF
          OYNDC = YNDC-0.06*50.*SIZEL+CYOFF
          TSIZ  = 6.*SIZEL
          CALL PCSETR('ZX',.8)
          CALL PCSETR('ZY',1.3)
          IF (ILTNC3 .GE. 0) THEN
            CALL PCGETI('SF - shadow flag',ISF)
            CALL PCGETI('SC - shadow color',ISC)
            CALL PCGETR('SX - shadow X offset',OSX)
            CALL PCGETR('SY - shadow Y offset',OSY)
C
            CALL PCSETI('SF - shadow flag',1)
            CALL PCSETI('SC - shadow color',ILTNC3)
            CALL PCSETI('CC - character color',ILTNC3)
            CALL PCSETR('SX - shadow X offset', -.015)
            CALL PCSETR('SY - shadow Y offset', -.04)
C
            CALL PLCHHQ(OXNDC,OYNDC,':F37:f',TSIZ,0.,0.)
C
            CALL PCSETI('SF - shadow flag',ISF)
            CALL PCSETI('SC - shadow color',ISC)
            CALL PCSETR('SX - shadow X offset',OSX)
            CALL PCSETR('SY - shadow Y offset',OSY)
          ENDIF
          IF (ILTNC1 .GE. 0) THEN
            CALL PCSETI('CC',ILTNC1)
            CALL PLCHHQ(OXNDC,OYNDC,':F37:f',TSIZ,0.,0.)
          ENDIF
          IF (ILTNC2 .GE. 0) THEN
            CALL PCSETI('CC',ILTNC2)
            CALL PLCHHQ(OXNDC,OYNDC,':F137:f',TSIZ,0.,0.)
          ENDIF
          CALL PCSETR('ZX',1.)
          CALL PCSETR('ZY',1.)
        ENDIF
C
C  Cloud last if mostly cloudy.
C
        IF(SYMTYP(1:2).EQ.'MC' .OR. SYMTYP(1:2).EQ.'mc' .OR.
     +     SYMTYP(1:2).EQ.'IT' .OR. SYMTYP(1:2).EQ.'it' .OR.
     +     SYMTYP(1:2).EQ.'IS' .OR. SYMTYP(1:2).EQ.'is') THEN
          CXOFF =  0.02
          CYOFF = -0.02
          IF (ICLDC3 .GE. 0) THEN
C
            CALL PCGETI('SF - shadow flag',ISF)
            CALL PCGETI('SC - shadow color',ISC)
            CALL PCGETR('SX - shadow X offset',OSX)
            CALL PCGETR('SY - shadow Y offset',OSY)
C
            CALL PCSETI('SF - shadow flag',1)
            CALL PCSETI('SC - shadow color',ICLDC3)
            CALL PCSETI('CC - character color',ICLDC3)
            CALL PCSETR('SX - shadow X offset', -.016)
            CALL PCSETR('SY - shadow Y offset', -.02)
C
            CALL PLCHHQ(XNDC+CXOFF,YNDC+CYOFF,':F37:k',10.*SIZEL,0.,0.)
C
            CALL PCSETI('SF - shadow flag',ISF)
            CALL PCSETI('SC - shadow color',ISC)
            CALL PCSETR('SX - shadow X offset',OSX)
            CALL PCSETR('SY - shadow Y offset',OSY)
          ENDIF
          IF (ICLDC1 .GE. 0) THEN
            CALL PCSETI('CC',ICLDC1)
            CALL PLCHHQ(XNDC+CXOFF,YNDC+CYOFF,':F37:k',10.*SIZEL,0.,0.)
          ENDIF
          IF (ICLDC2 .GE. 0) THEN
            CALL PCSETI('CC',ICLDC2)
            CALL PLCHHQ(XNDC+CXOFF,YNDC+CYOFF,':F137:k',10.*SIZEL,0.,0.)
          ENDIF
        ENDIF
        CALL PCSETI('CC - character color',ICC)
      ELSE IF (SYMTYP(1:1).EQ.'C' .OR. SYMTYP(1:1).EQ.'c'   .OR.
     +         SYMTYP(1:1).EQ.'R' .OR. SYMTYP(1:1).EQ.'r'   .OR.
     +         SYMTYP(1:2).EQ.'RS' .OR. SYMTYP(1:2).EQ.'rs' .OR.
     +         SYMTYP(1:2).EQ.'MC' .OR. SYMTYP(1:2).EQ.'mc' .OR.
     +         SYMTYP(1:2).EQ.'SN' .OR. SYMTYP(1:2).EQ.'sn') THEN
C
C  Draw a cloud as a daily weather icon.
C
        CALL PCGETI('CC - character color',ICC)
        IF (ICLDC3 .GE. 0) THEN
C
          CALL PCGETI('SF - shadow flag',ISF)
          CALL PCGETI('SC - shadow color',ISC)
          CALL PCGETR('SX - shadow X offset',OSX)
          CALL PCGETR('SY - shadow Y offset',OSY)
C
          CALL PCSETI('SF - shadow flag',1)
          CALL PCSETI('SC - shadow color',ICLDC3)
          CALL PCSETI('CC - character color',ICLDC3)
          CALL PCSETR('SX - shadow X offset', -.016)
          CALL PCSETR('SY - shadow Y offset', -.02)
C
          CALL PLCHHQ(XNDC,YNDC,':F37:k',10.*SIZEL,0.,0.)
C
          CALL PCSETI('SF - shadow flag',ISF)
          CALL PCSETI('SC - shadow color',ISC)
          CALL PCSETR('SX - shadow X offset',OSX)
          CALL PCSETR('SY - shadow Y offset',OSY)
        ENDIF
        IF (ICLDC1 .GE. 0) THEN
          CALL PCSETI('CC',ICLDC1)
          CALL PLCHHQ(XNDC,YNDC,':F37:k',10.*SIZEL,0.,0.)
        ENDIF
        IF (ICLDC2 .GE. 0) THEN
          CALL PCSETI('CC',ICLDC2)
          CALL PLCHHQ(XNDC,YNDC,':F137:k',10.*SIZEL,0.,0.)
        ENDIF
        IF (SYMTYP(1:1).EQ.'R'  .OR. SYMTYP(1:1).EQ.'r' .OR.
     +      SYMTYP(1:2).EQ.'RS' .OR. SYMTYP(1:2).EQ.'rs') THEN
          CALL PCSETI('CC - character color',ICOLOR)
          CALL PLCHHQ(XNDC,YNDC,':F37:o',10.*SIZEL,0.,0.)
        ENDIF
        IF (SYMTYP(1:2).EQ.'SN' .OR. SYMTYP(1:2).EQ.'sn' .OR.
     +      SYMTYP(1:2).EQ.'RS' .OR. SYMTYP(1:2).EQ.'rs') THEN
C
C  Draw snowflakes as part of daily weather icon.
C
          DSIZ = 1.3*SIZEL
          CALL PCGETI('CC',ISNFC1)
          CALL PCSETI('CC',ICOLOR)
          DO 60 I=1,3
            XP = XNDC+0.8*DSIZ+2.7*DSIZ*REAL(I-2)
            YP = YNDC-2.40*DSIZ
            CALL PLCHHQ(XP,YP,':F37:q',DSIZ,0.,0.)
   60     CONTINUE
          DO 70 I=1,2
            XP = XNDC+0.8*DSIZ+2.7*DSIZ*(REAL(I)-1.5)
            YP = YNDC-3.50*DSIZ
            CALL PLCHHQ(XP,YP,':F37:q',DSIZ,0.,0.)
   70     CONTINUE
          CALL PCSETI('CC',ISNFC1)
        ENDIF
        CALL PCSETI('CC - character color',ICC)
      ELSE IF (SYMTYP(1:1).EQ.'W' .OR. SYMTYP(1:1).EQ.'w') THEN
          CALL PCGETI('CC',ISNFC1)
          CALL PCSETI('CC',ICOLOR)
          CALL PLCHHQ(XNDC,YNDC,':F37:r',10.*SIZEL,0.,0.)
          CALL PCSETI('CC',ISNFC1)
      ELSE IF (SYMTYP(1:2).EQ.'IC' .OR. SYMTYP(1:2).EQ.'ic') THEN
C
C  Ice. Use the ARROW arrays as scratch space to store a region.
C
        DELX = 5.2*SIZEL
        DELY = 3.0*SIZEL
        ARROWX(1) = XNDC-DELX
        ARROWY(1) = YNDC+DELY
        ARROWX(2) = XNDC
        ARROWY(2) = YNDC+DELY
        ARROWX(3) = XNDC+DELX
        ARROWY(3) = YNDC+DELY
        ARROWX(4) = XNDC+DELX
        ARROWY(4) = YNDC-DELY
        ARROWX(5) = XNDC
        ARROWY(5) = YNDC-DELY
        ARROWX(6) = XNDC-DELX
        ARROWY(6) = YNDC-DELY
        ARROWX(7) = ARROWX(1)
        ARROWY(7) = ARROWY(1)
        CALL WMRGWT(7,ARROWX,ARROWY,37,105)
      ELSE IF (SYMTYP(1:1).EQ.'T' .OR. SYMTYP(1:1).EQ.'t') THEN
C
C  Draw a thunderstorm symbol as a daily weather icon.
C
        CALL PCGETI('CC - character color',ICC)
        CALL PCSETI('CC - character color',ICOLOR)
        CALL PLCHHQ(XNDC,YNDC,':F37:o',10.*SIZEL,0.,0.)
C
        OXNDC = XNDC+0.019*50.*SIZEL
        OYNDC = YNDC-0.06*50.*SIZEL
        TSIZ  = 6.*SIZEL
        CALL PCSETR('ZX',.8)
        CALL PCSETR('ZY',1.3)
        IF (ILTNC3 .GE. 0) THEN
          CALL PCGETI('SF - shadow flag',ISF)
          CALL PCGETI('SC - shadow color',ISC)
          CALL PCGETR('SX - shadow X offset',OSX)
          CALL PCGETR('SY - shadow Y offset',OSY)
C
          CALL PCSETI('SF - shadow flag',1)
          CALL PCSETI('SC - shadow color',ILTNC3)
          CALL PCSETI('CC - character color',ILTNC3)
          CALL PCSETR('SX - shadow X offset', -.015)
          CALL PCSETR('SY - shadow Y offset', -.04)
C
          CALL PLCHHQ(OXNDC,OYNDC,':F37:f',TSIZ,0.,0.)
C
          CALL PCSETI('SF - shadow flag',ISF)
          CALL PCSETI('SC - shadow color',ISC)
          CALL PCSETR('SX - shadow X offset',OSX)
          CALL PCSETR('SY - shadow Y offset',OSY)
        ENDIF
        IF (ILTNC1 .GE. 0) THEN
          CALL PCSETI('CC',ILTNC1)
          CALL PLCHHQ(OXNDC,OYNDC,':F37:f',TSIZ,0.,0.)
        ENDIF
        IF (ILTNC2 .GE. 0) THEN
          CALL PCSETI('CC',ILTNC2)
          CALL PLCHHQ(OXNDC,OYNDC,':F137:f',TSIZ,0.,0.)
        ENDIF
        CALL PCSETR('ZX',1.)
        CALL PCSETR('ZY',1.)
C
        IF (ICLDC3 .GE. 0) THEN
          CALL PCGETI('SF - shadow flag',ISF)
          CALL PCGETI('SC - shadow color',ISC)
          CALL PCGETR('SX - shadow X offset',OSX)
          CALL PCGETR('SY - shadow Y offset',OSY)
C
          CALL PCSETI('SF - shadow flag',1)
          CALL PCSETI('SC - shadow color',ICLDC3)
          CALL PCSETI('CC - character color',ICLDC3)
          CALL PCSETR('SX - shadow X offset', -.016)
          CALL PCSETR('SY - shadow Y offset', -.02)
C
          CALL PLCHHQ(XNDC,YNDC,':F37:k',10.*SIZEL,0.,0.)
C
          CALL PCSETI('SF - shadow flag',ISF)
          CALL PCSETI('SC - shadow color',ISC)
          CALL PCSETR('SX - shadow X offset',OSX)
          CALL PCSETR('SY - shadow Y offset',OSY)
        ENDIF
        IF (ICLDC1 .GE. 0) THEN
          CALL PCSETI('CC',ICLDC1)
          CALL PLCHHQ(XNDC,YNDC,':F37:k',10.*SIZEL,0.,0.)
        ENDIF
        IF (ICLDC2 .GE. 0) THEN
          CALL PCSETI('CC',ICLDC2)
          CALL PLCHHQ(XNDC,YNDC,':F137:k',10.*SIZEL,0.,0.)
        ENDIF
        CALL PCSETI('CC - character color',ICC)
      ENDIF
C
C  Restore original attrributes.
C
      CALL GSFAIS(IFAISO)
      CALL GSFACI(IFCLRO)
      CALL GSPLCI(ILCLRO)
      CALL GSELNT(NTRO)
      CALL GSWN(1,OLDWN(1),OLDWN(2),OLDWN(3),OLDWN(4))
      CALL GSVP(1,OLDVP(1),OLDVP(2),OLDVP(3),OLDVP(4))
      CALL PCSETI('TE',0)
      CALL PCSETC('FC',FCODE)
C
      RETURN
      END

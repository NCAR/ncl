C
C	$Id: wmlabs.f,v 1.1 1994-09-09 23:55:11 fred Exp $
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
C
      CHARACTER*(*) SYMTYP
C
      include 'wmcomn.h'
C
C  Reference arrow.
C
      PARAMETER (IADIM=8,D2RAD=.017453293)
      DIMENSION XX(IADIM),YY(IADIM),ARROWX(IADIM),ARROWY(IADIM)
      DIMENSION TMPX(IADIM),TMPY(IADIM)
      DATA XX/-1.000, -0.330, -0.360,  0.000,
     +        -0.360, -0.330, -1.000, -1.000  /
      DATA YY/-0.030, -0.030, -0.120,  0.000,
     +         0.120,  0.030,  0.030, -0.030  /
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
C  Turn on text extent computation for the remainder of the subroutine.
C
      CALL PCSETI('TE',1)
C
      SIZEL = WSIZES
C
      IF (SYMTYP(1:1) .EQ. 'H') THEN
C
C  Draw a high symbol.
C
        RADIUS = 1.10*SIZEL
        OFFSET = 0.15*RADIUS
        CALL NGGETI('CT',ICTYPO)
        CALL NGSETI('CT',0)
        CALL NGDOTS(XNDC-OFFSET,YNDC-OFFSET,1,2.*RADIUS,ICOLOR)
        CALL NGDOTS(XNDC,YNDC,1,2.*RADIUS,0)
        CALL NGSETI('CT',1)
        CALL NGDOTS(XNDC,YNDC,1,2.*RADIUS,ICOLOR)
        CALL NGSETI('CT',ICTYPO)
        CALL PCGETI('CC - character color',ICLRO)
        CALL PCGETI('FN - font name',IFNTO)
        CALL PCSETI('CC - character color',ICOLOR)
        CALL PCSETI('FN - font name',22)
        CALL PLCHHQ(XNDC,YNDC,'H',SIZEL,0.,0.)
        CALL PCSETI('CC - character color',ICLRO)
        CALL PCSETI('FN - font name',IFNTO)
      ELSE IF (SYMTYP(1:1) .EQ. 'L') THEN
C
C  Draw a low symbol.
C
        RADIUS = 1.0*SIZEL
        OFFSET = 0.20*RADIUS
        CALL NGGETI('CT',ICTYPO)
        CALL NGSETI('CT',0)
        CALL NGDOTS(XNDC,YNDC,1,2.*RADIUS+OFFSET,0)
        CALL NGDOTS(XNDC,YNDC,1,2.*RADIUS,ICOLOR)
        CALL NGSETI('CT',ICTYPO)
        CALL PCGETI('CC - character color',ICLRO)
        CALL PCGETI('FN - font name',IFNTO)
        CALL PCSETI('CC - character color',0)
        CALL PCSETI('FN - font name',22)
        CALL PLCHHQ (XNDC,YNDC,':F22:L',SIZEL,0.,0.)
        CALL PCSETI('CC - CHARACTER COLOR',ICLRO)
        CALL PCSETI('FN - font name',IFNTO)
      ELSE IF (SYMTYP(1:1) .EQ. 'A') THEN
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
C  Draw the arrow.
C
        CALL GFA(IADIM,ARROWX,ARROWY)
      ELSE IF (SYMTYP(1:1) .EQ. 'D') THEN
C
C  Draw a dot to mark a city location.
C
        RADIUS = 0.5*CDOTSZ
        CALL NGGETI('CT',ICTYPO)
        CALL NGSETI('CT',0)
        CALL NGDOTS(XNDC,YNDC,1,3.5*RADIUS,0)
        CALL NGDOTS(XNDC,YNDC,1,2.0*RADIUS,IDOTCO)
        CALL NGSETI('CT',ICTYPO)
      ENDIF
C
C  Restore original attrributes.
C
      CALL GSFAIS(IFAISO)
      CALL GSFACI(IFCLRO)
      CALL GSPLCI(ILCLRO)
      CALL GSELNT(NTRO)
      CALL PCSETI('TE',0)
C
      RETURN
      END

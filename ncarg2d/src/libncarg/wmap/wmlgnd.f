C
C	$Id: wmlgnd.f,v 1.7 2008-07-27 00:17:37 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE WMLGND(X,Y,NTYPE,IROWS,ICOLS)
C
C  Subroutine for producing legends. NTYPE specifies what legends 
C  are to be drawn:
C
C    NTYPE = 1  Plot the legend for the weather types.  There are 
C               six weather types: showers; T-storms; rain; flurries; 
C               snow; ice.  The coordinate (X,Y) for this type
C               specifies the lower left corner of the legend.
C          = 2  Plot the legend for the front types.  There are 
C               three front types: cold; warm; stationary.  The
C               (X,Y) coordinate for this legend specifies the
C               lower right corner of the legend.
C          = 3  Plot the explanatory legend.  The (X,Y) coordinate
C               for this legend specifies the bottom center of the
C               legend.
C
C  IROWS and ICOLS specify how many rows and columns there should
C  be in displaying the weather types, these values are significant
C  only when NTYPE=1.  Choices for IROWSxICOLS are: 1x6, 2x3, 3x2,
C  and 6x1.
C
      include 'wmcomn.h'
C
      DIMENSION XB(5),YB(5),XF(5),YF(5),IFNT(6),ICHR(6)
      CHARACTER*8  LBSW(6)
      CHARACTER*10 LBSF(3)
      INTEGER WMGTLN
      DATA XB/0.0, 3.0, 3.0, 0.0, 0.0/
      DATA YB/0.0, 0.0, 1.3, 1.3, 0.0/
      DATA IFNT/ 37, 37, 37, 37, 37, 37/
      DATA ICHR/102,101,103,106,104,105/
      DATA LBSW/'T-storms','Showers','Rain','Flurries','Snow','Ice'/
      DATA LBSF/'Stationary','Warm','Cold'/
C
C  Convert the input coordinate to NDC.
C
      IF (IROWS .EQ. 0) THEN
        NROWS = 6
        NCOLS = 1
      ELSE
        NROWS = IROWS
        NCOLS = ICOLS
      ENDIF
C
      CALL WMW2NX(1,X,XN)
      CALL WMW2NY(1,Y,YN)
C
C  Save the current normalization transformation number, and select
C  transformation 0.
C
      CALL GQCNTN(IER,NTRO)
      CALL GSELNT(0)
      CALL GQPLCI(IER,IPLCO)
      CALL GQFACI(IER,IFACO)
      CALL GQFAIS(IER,ISTOLD)
      CALL GSFAIS(1)
      CALL GSFACI(ICOLOR)
      CALL GSPLCI(ICOLOR)
      CALL PCGETI('FN',IFNO)
      CALL PCSETI('FN',21)
      CALL PCGETI('CC',ICOLD)
      CALL PCSETI('CC',ICOLOR)
C
C  Weather types.
C
      IF (NTYPE.EQ.1) THEN
        DO 30 K=1,NCOLS
          DO 20 J=1,NROWS
            NDX = J+(K-1)*NROWS
            DO 10 I=1,5
              XF(I) = XN+(K-1)*3.2*XB(3)*WSIZEW+WSIZEW*XB(I)
              YF(I) = YN+(J-1)*1.40*YB(3)*WSIZEW+WSIZEW*YB(I)
   10       CONTINUE
            CALL WMRGWT(5,XF,YF,IFNT(NDX),ICHR(NDX))
            CALL GPL(5,XF,YF)
            CALL PLCHHQ(XF(2)+0.01,0.5*(YF(2)+YF(3)),LBSW(NDX),
     +                  0.75*WSIZEW,0.,-1.)
   20     CONTINUE
   30   CONTINUE
      ENDIF
C
C  Fronts.
C
      IF (NTYPE .EQ. 2) THEN
C
C    Top label.
        OFFLB = 6.0*WSIZEW
        YPOS = YN+WSIZEW+4.3*YB(3)*WSIZEW
        CALL PCGETI ('BF - BOX FLAG',IBFO)
        CALL PCGETR ('BM - BOX MARGIN',BMO)
        CALL PCGETI ('BC(2) - BOX COLOR - BOX FILL       ',IBC2)
        CALL PCSETI ('BF - BOX FLAG',2)
        CALL PCSETR ('BM - BOX MARGIN',1.5)
        CALL PCSETI ('BC(2) - BOX COLOR - BOX FILL       ',0)
        CALL PLCHHQ(XN-0.5*OFFLB,YPOS,'Fronts',0.75*WSIZEW,0.,0.)       
        CALL PCSETI ('BF - BOX FLAG',IBFO)
        CALL PCSETR ('BM - BOX MARGIN',BMO)
        CALL PCSETI ('BC(2) - BOX COLOR - BOX FILL       ',IBC2)
C
        CALL WMGETR('SWI',SWIOLD)
        CALL WMGETR('LIN',WLOLD)
        CALL WMGETR('END',ENDO)
        CALL WMGETR('BEG',BEGO)
        CALL WMGETR('BET',BETO)
        CALL WMSETR('SWI',1.2*WSIZEW)
        CALL WMSETR('LIN',0.75*WLOLD)
        CALL WMSETR('END',.006)
        CALL WMSETR('BEG',.006)
        CALL WMSETR('BET',.009)
C
C    Example fronts
        XF(1) = XN-OFFLB
        XF(2) = XN 
        DO 40 I=1,3
          YPOS = YN+WSIZEW+(I-1)*1.5*YB(3)*WSIZEW
          YF(1) = YPOS
          YF(2) = YPOS
          LL = WMGTLN(LBSF(I),LEN(LBSF(1)),0)
          CALL WMSETC('FRO',LBSF(I))
          IF (I.EQ.1 .OR. I.EQ.2) CALL WMSETI('REV',1)
          CALL WMDRFT(2,XF,YF)
          CALL PLCHHQ(XF(1)-0.01,YPOS,LBSF(I)(1:LL),0.75*WSIZEW,0.,1.)       
   40   CONTINUE
        CALL WMSETR('SWI',SWIOLD)
        CALL WMSETR('LIN',WLOLD)
        CALL WMSETR('END',ENDO)
        CALL WMSETR('BEG',BEGO)
        CALL WMSETR('BET',BETO)
      ENDIF
C
C  Explanatory legend.
C
      IF (NTYPE .EQ. 3) THEN
        CALL PLCHHQ(XN,YN+.95*WSIZEW,'Forecast high/low temperatures are       
     + given for selected cities.',0.70*WSIZEW,0.,0.)
        CALL PLCHHQ(XN,YN+2.3*WSIZEW,'precipitation.  Temperature bands 
     +are highs for the day.',0.70*WSIZEW,0.,0.)
        CALL PLCHHQ(XN,YN+3.6*WSIZEW,'Shown are noon positions of weathe
     +r systems and',0.70*WSIZEW,0.,0.)
      ENDIF
C
C  Restore original settings.
C
      CALL GSELNT(NTRO)
      CALL PCSETI('FN',IFNO)
      CALL PCSETI('CC',ICOLD)
      CALL GSFAIS(ISTOLD)
      CALL GSPLCI(IPLCO)
      CALL GSFACI(IFACO)
C
      RETURN
      END

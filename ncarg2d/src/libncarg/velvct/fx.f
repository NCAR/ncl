C
C	$Id: fx.f,v 1.4 1993-01-20 22:02:43 dbrown Exp $
C
      FUNCTION FX(X,Y)
C
C
C The mapping common block: made available to user mapping routines
C
      COMMON /VVMAP/
     +                IMAP       ,
     +                XVPL       ,XVPR       ,YVPB       ,YVPT       ,
     +                WXMN       ,WXMX       ,WYMN       ,WYMX       ,
     +                XLOV       ,XHIV       ,YLOV       ,YHIV       ,
     +                SXDC       ,SYDC       ,NXCT       ,NYCT       ,
     +                RLEN       ,LNLG       ,INVX       ,INVY       ,
     +                ITRT       ,IWCT       ,FW2W       ,FH2H       ,
     +                DVMN       ,DVMX       ,RBIG       ,IBIG
C
      SAVE /VVMAP/
C
C Math constants
C
      PARAMETER (PDTOR  = 0.017453292519943,
     +           PRTOD  = 57.2957795130823,
     +           P1XPI  = 3.14159265358979,
     +           P2XPI  = 6.28318530717959,
     +           P1D2PI = 1.57079632679489,
     +           P5D2PI = 7.85398163397448) 
C
C --------------------------------------------------------------------
C
      PARAMETER (PRCFAC=1E5,
     +           PVFRAC=0.001,
     +           PFOVFL=1E12,
     +           IPMXCT=10,
     +           PDUVML=2.0)
C
      GOTO (10,20,30)  IMAP+1
C
C     Identity transformation.
C
10    CONTINUE
      FX = X
      RETURN
C
C
C     EZMAP overlaying.
C
20    CONTINUE
C
      XLON = XLOV + (X-1.)*(XHIV-XLOV)/(FLOAT(NXCT)-1.)
      YLAT = YLOV + (Y-1.)*(YHIV-YLOV)/(FLOAT(NYCT)-1.)
C
      IF (IFIX(ABS(YLAT)*PRCFAC) .EQ. IFIX(90.*PRCFAC)) THEN
         FX=RBIG
         RETURN
      END IF
C
      CALL MAPTRN (YLAT, XLON, FXLON, YDUM)
C
      FX = FXLON
      RETURN
C     Polar coordinate transformation.
C
30    CONTINUE
      THETA=YLOV+YHIV*(Y-1)/(FLOAT(NYCT)-1.0)
      R=XLOV+(XHIV-XLOV)*(X-1.)/(FLOAT(NXCT)-1.)
      FX=R*COS(PDTOR*THETA)
      RETURN
C
      END
C

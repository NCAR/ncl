C
C	$Id: fy.f,v 1.3 1993-01-15 22:46:26 dbrown Exp $
C
      FUNCTION FY(X,Y)
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
C
C     The function FY behaves in an analagous manner to FX as
C     described above.
C
      GOTO (10,20,30)  IMAP+1
C
C     The identity transformation.
C
10    CONTINUE
      FY = Y
      RETURN
C
C     EZMAP overlaying.
C
20    CONTINUE
      XLON = XLOV + (X-1.)*(XHIV-XLOV)/(FLOAT(NXCT)-1.)
      YLAT = YLOV + (Y-1.)*(YHIV-YLOV)/(FLOAT(NYCT)-1.)

      IF (IFIX(ABS(YLAT)*PRCFAC) .EQ. IFIX(90.*PRCFAC)) THEN
         FY=RBIG
         RETURN
      END IF

      CALL MAPTRN(YLAT,XLON,XDUM,FYLAT)
C
      FY = FYLAT
      RETURN
C
C     Polar coordinate transformation.
C
30    CONTINUE
      THETA=YLOV+YHIV*(Y-1)/(FLOAT(NYCT)-1.0)
      R=XLOV+(XHIV-XLOV)*(X-1.)/(FLOAT(NXCT)-1.)
      FY=R*SIN(PDTOR*THETA)
C
      RETURN
      END

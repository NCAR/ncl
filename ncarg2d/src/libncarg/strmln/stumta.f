C
C ---------------------------------------------------------------------
C
      SUBROUTINE STUMTA(XDA,YDA,XUS,YUS,XND,YND,DU,DV,TA,IST)
C
C User modifiable routine for mapping a tangent angle in data space to 
C normalized device coordinate space.
C
C Input parameters:
C
C XDA,YDA - Point in data coordinate space
C XUS,YUS - Point in user coordinate space
C XND,YND - Point in NDC space
C DU,DV   - Differential vector components in data space
C
C Output parameters:
C
C TA      - Streamline tangent angle in NDC space
C IST     - Status code indicating success or failure
C
C --------------------------------------------------------------------
C
C The mapping common block: made available to user mapping routines
C
      COMMON /STMAP/
     +                IMAP       ,LNLG       ,INVX       ,INVY       ,
     +                XLOV       ,XHIV       ,YLOV       ,YHIV       ,
     +                WXMN       ,WXMX       ,WYMN       ,WYMX       ,
     +                XVPL       ,XVPR       ,YVPB       ,YVPT       ,
     +                XGDS       ,YGDS       ,NXCT       ,NYCT       ,
     +                ITRT       ,FW2W       ,FH2H       ,
     +                DFMG       ,VNML       ,RBIG       ,IBIG
C
      SAVE /STMAP/
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
C ---------------------------------------------------------------------
C
      IST=0
      TA=ATAN2(DV,DU)
C
C Done.
C
      RETURN
C
      END

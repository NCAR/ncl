C
C	$Id: stumxy.f,v 1.1 1993-01-15 23:53:52 dbrown Exp $
C
C-----------------------------------------------------------------------
C
      SUBROUTINE STUMXY(XWO,YWO,XUS,YUS,IST)
C
C User modifiable routine for mapping world coordinate space to
C user space
C
C
C Input parameters:
C
C XWO,YWO - Point in world coordinate space
C
C Output parameters:
C
C XUS,YUS - Point in user coordinate space
C IST     - Status code indicating success or failure
C
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
C --------------------------------------------------------------------
C
C -------------------------------------------------------------
C
C Identity transformation
C
      IST=0
      XUS=XWO
      YUS=YWO
C
C Done.
C
      RETURN
C
      END
C
C ---------------------------------------------------------------------
C
      SUBROUTINE STUIXY(XUS,YUS,XWO,YWO,IST)
C
C User modifiable routine for inversely transforming
C a point in user coordinate space to world space
C
C Input parameters:
C
C XUS,YUS - Point in user coordinate space
C
C Output parameters:
C
C XWO,YWO - Point in world coordinate space
C IST     - Status code indicating success or failure
C
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
C --------------------------------------------------------------------
C
C ---------------------------------------------------------------------
C
      IST=0
      XWO=XUS
      YWO=YUS
C
C Done
C
      RETURN
      END
C
C ---------------------------------------------------------------------
C
      SUBROUTINE STUMTA(XWO,YWO,XUS,YUS,XND,YND,DU,DV,TA,IST)
C
C User modifiable routine for mapping a tangent angle in world space to 
C normalized device coordinate space.
C
C Input parameters:
C
C XWO,YWO - Point in world coordinate space
C XUS,YUS - Point in user coordinate space
C XND,YND - Point in normalized device coordinate space
C DU,DV   - Differential vector components in world space
C
C Output parameters:
C
C TA      - Streamline tangent angle in NDC space
C IST     - Status code indicating success or failure
C
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
C --------------------------------------------------------------------
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





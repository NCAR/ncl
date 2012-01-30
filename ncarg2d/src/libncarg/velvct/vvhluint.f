C
C       $Id: vvhluint.f,v 1.12 2008-07-27 00:17:35 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
C This module contains interface routines for VectorPlot and VecAnno
C
      SUBROUTINE VVGETARROWBOUND (ARS,XB,YB,XE,YE,VLN,XMN,YMN,XMX,YMX)
C
C Given the beginning and end points of a vector arrow and the vector
C length, returns the bounding box as the minimum and maximum x and y 
C values. ARS is an array of values necessary to put Vectors into the
C proper state to perform the calculations correctly.
C 
      REAL ARS(26)
C
C ---------------------------------------------------------------------
C
C NOTE:
C Since implicit typing is used for all real and integer variables
C a consistent length convention has been adopted to help clarify the
C significance of the variables encountered in the code for this 
C utility. All local variable and subroutine parameter identifiers 
C are limited to 1,2,or 3 characters. Four character names identify  
C members of common blocks. Five and 6 character variable names 
C denote PARAMETER constants or subroutine or function names.
C
C Declare the VV common blocks.
C
C IPLVLS - Maximum number of color threshold level values
C IPAGMX - Maximum number of area groups allowed in the area map
C
      PARAMETER (IPLVLS = 256, IPAGMX = 64)
C
C Integer and real common block variables
C
C
      COMMON /VVCOM/
     +                IUD1       ,IVD1       ,IPD1       ,IXDM       ,
     +                IYDN       ,VLOM       ,VHIM       ,ISET       ,
     +                VRMG       ,VRLN       ,VFRC       ,IXIN       ,
     +                IYIN       ,ISVF       ,UUSV       ,UVSV       ,
     +                UPSV       ,IMSK       ,ICPM       ,UVPS       ,
     +                UVPL       ,UVPR       ,UVPB       ,UVPT       ,
     +                UWDL       ,UWDR       ,UWDB       ,UWDT       ,
     +                UXC1       ,UXCM       ,UYC1       ,UYCN       ,
     +                NLVL       ,IPAI       ,ICTV       ,WDLV       ,
     +                UVMN       ,UVMX       ,PMIN       ,PMAX       ,
     +                RVMN       ,RVMX       ,RDMN       ,RDMX       ,
     +                ISPC       ,RVMD       ,IPLR       ,IVST       ,
     +                IVPO       ,ILBL       ,IDPF       ,IMSG       ,
     +                ICLR(IPLVLS)           ,TVLU(IPLVLS)
C
C Arrow size/shape parameters
C
        COMMON / VVARO /
     +                HDSZ       ,HINF       ,HANG       ,IAST       ,
     +                HSIN       ,HCOS       ,FAMN       ,FAMX       ,
     +                UVMG       ,FAIR       ,FAWR       ,FAWF       ,
     +                FAXR       ,FAXF       ,FAYR       ,FAYF       ,
     +                AROX(8)    ,AROY(8)    ,FXSZ       ,FYSZ       ,
     +                FXRF       ,FXMN       ,FYRF       ,FYMN       ,
     +                FWRF       ,FWMN       ,FIRF       ,FIMN       ,
     +                AXMN       ,AXMX       ,AYMN       ,AYMX       ,
     +                IACM       ,IAFO       ,WBAD       ,WBTF       ,
     +                WBCF       ,WBDF       ,WBSC
C
C
C Text related parameters
C
        COMMON /VVTXP /
     +                FCWM    ,ICSZ    ,
     +                FMNS    ,FMNX    ,FMNY    ,IMNP    ,IMNC  ,
     +                FMXS    ,FMXX    ,FMXY    ,IMXP    ,IMXC  ,
     +                FZFS    ,FZFX    ,FZFY    ,IZFP    ,IZFC  ,
     +                FILS    ,FILX    ,FILY    ,IILP    ,IILC  ,
     +                FLBS    ,ILBC

C
C Character variable declartions
C
      CHARACTER*160 CSTR
      PARAMETER (IPCHSZ=36)
      CHARACTER*(IPCHSZ)  CMNT,CMXT,CZFT,CLBT,CILT
C
C Text string parameters
C
      COMMON /VVCHAR/ CSTR,CMNT,CMXT,CZFT,CLBT,CILT
C
      SAVE /VVCOM/, /VVARO/, /VVTXP/, /VVCHAR/
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
      CHARACTER*1  CDM
      EXTERNAL VVDUMB
C
      IAST = INT(ARS(1))
      UVMX = ARS(2)
      UVMN = ARS(3)
      DVMX = ARS(4)
      DVMN = 0.0
      FW2W = ARS(5)
      UVMG = ARS(6)
      VLOM = ARS(7)
      VHIM = ARS(8)
      VFRC = ARS(9)
      VRLN = ARS(10)
      VRMG = ARS(11)
      FAMN = MIN(1.0,MAX(0.0,ARS(12)))
      FAMX = MIN(1.0,MAX(0.0,ARS(13)))
      FAIR = MIN(1.0,MAX(0.0,ARS(14)))
      FAWR = MIN(1.0,MAX(0.0,ARS(15)))
      FAWF = MIN(1.0,MAX(0.0,ARS(16)))
      FAXR = MIN(2.0,MAX(0.0,ARS(17)))
      FAXF = MIN(1.0,MAX(0.0,ARS(18)))
      FAYR = MIN(1.0,MAX(0.0,ARS(19)))
      FAYF = MIN(1.0,MAX(0.0,ARS(20)))
      IAFO = INT(ARS(21))
      WBAD=MIN(80.0,MAX(-80.0,ARS(22)))
      WBTF=MIN(1.0,MAX(0.001,ARS(23)))
      WBCF=MIN(1.0,MAX(0.001,ARS(24)))
      WBDF=MIN(1.0,MAX(0.001,ARS(25)))
      WBSC=MAX(0.0,ARS(26))
C
      CALL VVILNS(DRL,VFR,IAV)
      VL = -VLN
C
C Call the appropriate drawing routine with the vector length set
C negative in order to simply calculate the height and width of the
C vector arrow.
C
      ISP = IVPO
      IVPO = 1
      IF (IAST.EQ.0) THEN
         CALL VVDRAW(XB,YB,XE,YE,VL,CDM,0,IDM,VVDUMB,0)
      ELSE IF (IAST.EQ.1) THEN
         CALL VVINFA()
         CALL VVDRFL(XB,YB,XE,YE,VL,CDM,0,IDM,VVDUMB,0)
      ELSE
         CALL VVINWB(1)
         CALL VVDRWB(XB,YB,XE,YE,-DVMX,CDM,0,IDM,VVDUMB,0)
      END IF
      IVPO = ISP
C
      XMN = AXMN
      XMX = AXMX
      YMN = AYMN
      YMX = AYMX
C
      RETURN
      END
C
C ---------------------------------------------------------------------
C
      SUBROUTINE VVARROWDRAW (DRS,ARS,XB,YB,XE,YE,VLN)
C
      REAL ARS(26),DRS(11)
C
C ---------------------------------------------------------------------
C
C NOTE:
C Since implicit typing is used for all real and integer variables
C a consistent length convention has been adopted to help clarify the
C significance of the variables encountered in the code for this 
C utility. All local variable and subroutine parameter identifiers 
C are limited to 1,2,or 3 characters. Four character names identify  
C members of common blocks. Five and 6 character variable names 
C denote PARAMETER constants or subroutine or function names.
C
C Declare the VV common blocks.
C
C IPLVLS - Maximum number of color threshold level values
C IPAGMX - Maximum number of area groups allowed in the area map
C
      PARAMETER (IPLVLS = 256, IPAGMX = 64)
C
C Integer and real common block variables
C
C
      COMMON /VVCOM/
     +                IUD1       ,IVD1       ,IPD1       ,IXDM       ,
     +                IYDN       ,VLOM       ,VHIM       ,ISET       ,
     +                VRMG       ,VRLN       ,VFRC       ,IXIN       ,
     +                IYIN       ,ISVF       ,UUSV       ,UVSV       ,
     +                UPSV       ,IMSK       ,ICPM       ,UVPS       ,
     +                UVPL       ,UVPR       ,UVPB       ,UVPT       ,
     +                UWDL       ,UWDR       ,UWDB       ,UWDT       ,
     +                UXC1       ,UXCM       ,UYC1       ,UYCN       ,
     +                NLVL       ,IPAI       ,ICTV       ,WDLV       ,
     +                UVMN       ,UVMX       ,PMIN       ,PMAX       ,
     +                RVMN       ,RVMX       ,RDMN       ,RDMX       ,
     +                ISPC       ,RVMD       ,IPLR       ,IVST       ,
     +                IVPO       ,ILBL       ,IDPF       ,IMSG       ,
     +                ICLR(IPLVLS)           ,TVLU(IPLVLS)
C
C Arrow size/shape parameters
C
        COMMON / VVARO /
     +                HDSZ       ,HINF       ,HANG       ,IAST       ,
     +                HSIN       ,HCOS       ,FAMN       ,FAMX       ,
     +                UVMG       ,FAIR       ,FAWR       ,FAWF       ,
     +                FAXR       ,FAXF       ,FAYR       ,FAYF       ,
     +                AROX(8)    ,AROY(8)    ,FXSZ       ,FYSZ       ,
     +                FXRF       ,FXMN       ,FYRF       ,FYMN       ,
     +                FWRF       ,FWMN       ,FIRF       ,FIMN       ,
     +                AXMN       ,AXMX       ,AYMN       ,AYMX       ,
     +                IACM       ,IAFO       ,WBAD       ,WBTF       ,
     +                WBCF       ,WBDF       ,WBSC
C
C
C Text related parameters
C
        COMMON /VVTXP /
     +                FCWM    ,ICSZ    ,
     +                FMNS    ,FMNX    ,FMNY    ,IMNP    ,IMNC  ,
     +                FMXS    ,FMXX    ,FMXY    ,IMXP    ,IMXC  ,
     +                FZFS    ,FZFX    ,FZFY    ,IZFP    ,IZFC  ,
     +                FILS    ,FILX    ,FILY    ,IILP    ,IILC  ,
     +                FLBS    ,ILBC

C
C Character variable declartions
C
      CHARACTER*160 CSTR
      PARAMETER (IPCHSZ=36)
      CHARACTER*(IPCHSZ)  CMNT,CMXT,CZFT,CLBT,CILT
C
C Text string parameters
C
      COMMON /VVCHAR/ CSTR,CMNT,CMXT,CZFT,CLBT,CILT
C
      SAVE /VVCOM/, /VVARO/, /VVTXP/, /VVCHAR/
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
      CHARACTER*1  CDM
      EXTERNAL VVDUMB
C
      IAST = INT(ARS(1))
      UVMX = ARS(2)
      UVMN = ARS(3)
      DVMX = ARS(4)
      DVMN = 0.0
      FW2W = ARS(5)
      UVMG = ARS(6)
      VLOM = ARS(7)
      VHIM = ARS(8)
      VFRC = ARS(9)
      VRLN = ARS(10)
      VRMG = ARS(11)
      FAMN = MIN(1.0,MAX(0.0,ARS(12)))
      FAMX = MIN(1.0,MAX(0.0,ARS(13)))
      FAIR = MIN(1.0,MAX(0.0,ARS(14)))
      FAWR = MIN(1.0,MAX(0.0,ARS(15)))
      FAWF = MIN(1.0,MAX(0.0,ARS(16)))
      FAXR = MIN(2.0,MAX(0.0,ARS(17)))
      FAXF = MIN(1.0,MAX(0.0,ARS(18)))
      FAYR = MIN(1.0,MAX(0.0,ARS(19)))
      FAYF = MIN(1.0,MAX(0.0,ARS(20)))
      IAFO = INT(ARS(21))
      WBAD=MIN(80.0,MAX(-80.0,ARS(22)))
      WBTF=MIN(1.0,MAX(0.001,ARS(23)))
      WBCF=MIN(1.0,MAX(0.001,ARS(24)))
      WBDF=MIN(1.0,MAX(0.001,ARS(25)))
      WBSC=MAX(0.0,ARS(26))
C
      XVPL = DRS(1)
      XVPR = DRS(2)
      XVPB = DRS(3)
      XVPT = DRS(4)
      WXMN = DRS(5)
      WXMX = DRS(6)
      WYMN = DRS(7)
      WYMX = DRS(8)
      INVX = DRS(9)
      INVY = DRS(10)
      LNLG = DRS(11)
C
      CALL VVILNS(DRL,VFR,IAV)
      VL = VLN
C
C
C Call the appropriate drawing routine with the vector length set
C negative in order to simply calculate the height and width of the
C vector arrow.
C
      ISP = IVPO
      IVPO = 1
      IF (IAST.EQ.0) THEN
         CALL VVDRAW(XB,YB,XE,YE,VL,CDM,0,IDM,VVDUMB,0)
      ELSE IF (IAST.EQ.1) THEN
         CALL VVINFA()
         CALL VVDRFL(XB,YB,XE,YE,VL,CDM,0,IDM,VVDUMB,0)
      ELSE
         CALL VVINWB(1)
         CALL VVDRWB(XB,YB,XE,YE,DVMX,CDM,0,IDM,VVDUMB,0)
      END IF
      IVPO = ISP
C
      RETURN
      END
C
C ---------------------------------------------------------------------
C
      SUBROUTINE VVGETMAPINFO (IMP,ITR,DMX,SXD,SYD,XMN,XMX,YMN,YMX)
C
C The sole purpose of this routine is to make information from the
C mapping common block available to hluvvmpxy
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
      IMP = IMAP
      DMX = DVMX
      SXD = SXDC
      SYD = SYDC
      ITR = ITRT
      XMN = WXMN
      XMX = WXMX
      YMN = WYMN
      YMX = WYMX
C
C Done.
C
      RETURN
C
      END



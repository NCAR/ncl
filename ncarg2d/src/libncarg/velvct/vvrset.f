C
C       $Id: vvrset.f,v 1.14 2008-07-27 00:17:35 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE VVRSET
C
C This subroutine may be called to reset all variables which have
C default values to those values.
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
C Reset individual parameters.
C
C Common block VVCOM
C
      IUD1 = -1
      IVD1 = -1
      IPD1 = -1
      IXDM = -1
      IYDN = -1
      VLOM = 0.0
      VHIM = 0.0
      ISET = 1
      VRMG = 0.0
      VRLN = 0.0
      VFRC = 0.0
      IXIN = 1
      IYIN = 1
      ISVF = 0
      UUSV = 0.0
      UVSV = 0.0
      UPSV = 0.0
      IMSK = 0
      ICPM = 0
      UVPS = 0.25
      UVPL = 0.05
      UVPR = 0.95
      UVPB = 0.05
      UVPT = 0.95
      UWDL = 0.0
      UWDR = 0.0
      UWDB = 0.0
      UWDT = 0.0
      UXC1 = 0.0
      UXCM = 0.0
      UYC1 = 0.0
      UYCN = 0.0
      NLVL = 0
      IPAI = 1
      ICTV = 0
      WDLV = 1.0
      UVMN = 0.0
      UVMX = 0.0
      PMIN = 0.0
      PMAX = 0.0
      RVMN = 0.0
      RVMX = 0.0
      RDMN = 0.0
      RDMX = 0.0
      ISPC = -1
      RVMD = 0.0
      IMAP = 0
      IPLR = 0
      IVST = 0
      IVPO = 0
      ILBL = 0
      IDPF = 1
C
C Parameter arrays
C
      DO 101 I=1,IPLVLS,1
         ICLR(I) = 1
         TVLU(I) = 0.0
 101  CONTINUE
C
C -------------------------------------------------------------------
C
C VVARO values
C
      HDSZ = 0.25
      HINF = 0.5
      HANG = 22.5
      FAMN = 0.005
      FAMX = 0.05
      IAST = 0
      UVMG = 0.0
      FAIR = 0.33
      FAWR = 0.1
      FAWF = 0.25
      FAXR = 0.36
      FAXF = 0.25
      FAYR = 0.12
      FAYF = 0.25
      IACM = 0
      IAFO = 1
      WBAD = 62.0
      WBTF = 0.33
      WBCF = 0.25
      WBDF = 0.1
      WBSC = 1.0
C
C ---------------------------------------------------------------------
C
C VVTXP values
C
      FCWM = 1.0
      ICSZ = IPCHSZ
      FMNS = 0.0075
      FMNX = 0.475
      FMNY = -0.01
      IMNP = 4
      IMNC = -1
      FMXS = 0.0075
      FMXX = 0.525
      FMXY = -0.01
      IMXP = 2
      IMXC = -1
      FZFS = 0.033
      FZFX = 0.5
      FZFY = 0.5
      IZFP = 0
      IZFC = -1
      FILS = 0.05
      FILX = 0.0
      FILY = 0.0
      IILP = -1
      IILC = -1
      FLBS = 0.007
      ILBC = -1
C
C ---------------------------------------------------------------------
C
C VVCHAR values
C
      CMNT = 'Minimum Vector'
      CMXT = 'Maximum Vector'
      CZFT = 'ZERO FIELD'
      CILT = 'Vector Field Map' 
C
C ---------------------------------------------------------------------
C
C VVMAP values
C
      IMAP = 0
      RLEN = 0.0
      ITRT = 1
      SXDC = 1.0
      SYDC = 1.0
      DVMN = 0.0
      DVMX = 0.0
      IBIG = I1MACH(9)
      RBIG = R1MACH(2)
C
C ---------------------------------------------------------------------
C
C Done
C
      RETURN
C
      END

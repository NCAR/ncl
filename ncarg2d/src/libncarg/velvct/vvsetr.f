C
C       $Id: vvsetr.f,v 1.18 2008-07-27 00:17:35 haley Exp $
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
      SUBROUTINE VVSETR (CNM,RVL)
C
      CHARACTER*(*) CNM
C
C This subroutine is called to set the real value of a specified
C parameter.
C
C CNM is the name of the parameter whose value is to be set.
C
C RVL is a real variable containing the new value of the parameter.
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
C Check for a parameter name that is too short.
C
      IF (LEN(CNM).LT.3) THEN
        CSTR(1:46)='VVSETI OR VVSETR - PARAMETER NAME TOO SHORT - '
        CSTR(47:46+LEN(CNM))=CNM
        CALL SETER (CSTR(1:46+LEN(CNM)),1,1)
        RETURN
      END IF
C
C Check for incorrect use of the index parameter.
C
      IF (CNM(1:3).EQ.'CLR'.OR.CNM(1:3).EQ.'clr'
     +    .OR.CNM(1:3).EQ.'TVL'.OR.CNM(1:3).EQ.'tvl') THEN
         IF (IPAI.LT.1.OR.IPAI.GT.IPLVLS) THEN
            CSTR(1:46)='VVSETI OR VVSETR - SETTING XXX - PAI INCORRECT'
            CSTR(28:30)=CNM(1:3)
            CALL SETER (CSTR(1:46),2,1)
            RETURN
         END IF
      END IF
C
C Set the appropriate parameter value.
C
C Values in VVCOM
C 
      IF (CNM(1:3).EQ.'UD1'.OR. CNM(1:3).EQ.'ud1') THEN
         IUD1=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'VD1'.OR. CNM(1:3).EQ.'vd1') THEN
         IVD1=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'PD1'.OR. CNM(1:3).EQ.'pd1') THEN
         IPD1=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'XDM'.OR. CNM(1:3).EQ.'xdm') THEN
         IXDM=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'YDN'.OR. CNM(1:3).EQ.'ydn') THEN
         IYDN=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'VLC'.OR.CNM(1:3).EQ.'vlc') THEN
         VLOM=RVL 
      ELSE IF (CNM(1:3).EQ.'VHC'.OR.CNM(1:3).EQ.'vhc') THEN
         VHIM=RVL 
      ELSE IF (CNM(1:3).EQ.'SET'.OR. CNM(1:3).EQ.'set') THEN
         ISET=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'VRM'.OR. CNM(1:3).EQ.'vrm') THEN
         VRMG=RVL
      ELSE IF (CNM(1:3).EQ.'VRL'.OR. CNM(1:3).EQ.'vrl') THEN
         VRLN=RVL
      ELSE IF (CNM(1:3).EQ.'VFR'.OR. CNM(1:3).EQ.'vfr') THEN
         VFRC=RVL
      ELSE IF (CNM(1:3).EQ.'XIN'.OR. CNM(1:3).EQ.'xin') THEN
         IXIN=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'YIN'.OR. CNM(1:3).EQ.'yin') THEN
         IYIN=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'SVF'.OR. CNM(1:3).EQ.'svf') THEN
         ISVF=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'USV'.OR.CNM(1:3).EQ.'usv') THEN
         UUSV=RVL
      ELSE IF (CNM(1:3).EQ.'VSV'.OR.CNM(1:3).EQ.'vsv') THEN
         UVSV=RVL
      ELSE IF (CNM(1:3).EQ.'PSV'.OR.CNM(1:3).EQ.'psv') THEN
         UPSV=RVL
      ELSE IF (CNM(1:3).EQ.'MSK'.OR. CNM(1:3).EQ.'msk') THEN
         IMSK=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'CPM'.OR. CNM(1:3).EQ.'cpm') THEN
         ICPM=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'VPS'.OR. CNM(1:3).EQ.'vps') THEN
         UVPS=RVL
      ELSE IF (CNM(1:3).EQ.'VPL'.OR.CNM(1:3).EQ.'vpl') THEN
         UVPL=MIN(1.0,MAX(0.0,RVL))
      ELSE IF (CNM(1:3).EQ.'VPR'.OR.CNM(1:3).EQ.'vpr') THEN
         UVPR=MIN(1.0,MAX(0.0,RVL))
      ELSE IF (CNM(1:3).EQ.'VPB'.OR.CNM(1:3).EQ.'vpb') THEN
         UVPB=MIN(1.0,MAX(0.0,RVL))
      ELSE IF (CNM(1:3).EQ.'VPT'.OR.CNM(1:3).EQ.'vpt') THEN
         UVPT=MIN(1.0,MAX(0.0,RVL))
      ELSE IF (CNM(1:3).EQ.'WDL'.OR.CNM(1:3).EQ.'wdl') THEN
         UWDL=RVL
      ELSE IF (CNM(1:3).EQ.'WDR'.OR.CNM(1:3).EQ.'wdr') THEN
         UWDR=RVL
      ELSE IF (CNM(1:3).EQ.'WDB'.OR.CNM(1:3).EQ.'wdb') THEN
         UWDB=RVL
      ELSE IF (CNM(1:3).EQ.'WDT'.OR.CNM(1:3).EQ.'wdt') THEN
         UWDT=RVL
      ELSE IF (CNM(1:3).EQ.'XC1'.OR.CNM(1:3).EQ.'xc1') THEN
         UXC1=RVL
      ELSE IF (CNM(1:3).EQ.'XCM'.OR.CNM(1:3).EQ.'xcm') THEN
         UXCM=RVL
      ELSE IF (CNM(1:3).EQ.'YC1'.OR.CNM(1:3).EQ.'yc1') THEN
         UYC1=RVL
      ELSE IF (CNM(1:3).EQ.'YCN'.OR.CNM(1:3).EQ.'ycn') THEN
         UYCN=RVL
      ELSE IF (CNM(1:3).EQ.'NLV'.OR.CNM(1:3).EQ.'nlv') THEN
         NLVL=MIN(IPLVLS,INT(RVL))
      ELSE IF (CNM(1:3).EQ.'PAI'.OR.CNM(1:3).EQ.'pai') THEN
         IF (RVL .LT. 1.0 .OR. RVL .GT. IPLVLS) GO TO 9800
         IPAI=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'CTV'.OR.CNM(1:3).EQ.'ctv') THEN
         ICTV=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'LWD'.OR.CNM(1:3).EQ.'lwd') THEN
         IF (RVL .LE. 0.0) GO TO 9800
         WDLV=RVL
C
C UVMN,UVMX, PMIN, PMAX are read-only
C
      ELSE IF (CNM(1:3).EQ.'SPC'.OR. CNM(1:3).EQ.'spc') THEN
         ISPC=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'VMD'.OR. CNM(1:3).EQ.'vmd') THEN
         RVMD=MAX(0.0,RVL)
      ELSE IF (CNM(1:3).EQ.'MAP'.OR. CNM(1:3).EQ.'map') THEN
         IMAP=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'PLR'.OR. CNM(1:3).EQ.'plr') THEN
         IPLR=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'VST'.OR. CNM(1:3).EQ.'vst') THEN
         IVST=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'VPO'.OR. CNM(1:3).EQ.'vpo') THEN
         IVPO=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'LBL'.OR. CNM(1:3).EQ.'lbl') THEN
         ILBL=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'DPF'.OR. CNM(1:3).EQ.'dpf') THEN
         IDPF=INT(RVL)
C
C arrow parameters
C
      ELSE IF (CNM(1:3).EQ.'AMN'.OR.CNM(1:3).EQ.'amn') THEN
         FAMN=RVL
      ELSE IF (CNM(1:3).EQ.'AMX'.OR.CNM(1:3).EQ.'amx') THEN
         FAMX=RVL
      ELSE IF (CNM(1:3).EQ.'AST'.OR.CNM(1:3).EQ.'ast') THEN
         IAST=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'AIR'.OR.CNM(1:3).EQ.'air') THEN
         FAIR=MIN(1.0,MAX(0.0,RVL))
      ELSE IF (CNM(1:3).EQ.'AWR'.OR.CNM(1:3).EQ.'awr') THEN
         FAWR=MIN(1.0,MAX(0.0,RVL))
      ELSE IF (CNM(1:3).EQ.'AWF'.OR.CNM(1:3).EQ.'awf') THEN
         FAWF=MIN(1.0,MAX(0.0,RVL))
      ELSE IF (CNM(1:3).EQ.'AXR'.OR.CNM(1:3).EQ.'axr') THEN
         FAXR=MIN(2.0,MAX(0.0,RVL))
      ELSE IF (CNM(1:3).EQ.'AXF'.OR.CNM(1:3).EQ.'axf') THEN
         FAXF=MIN(1.0,MAX(0.0,RVL))
      ELSE IF (CNM(1:3).EQ.'AYR'.OR.CNM(1:3).EQ.'ayr') THEN
         FAYR=MIN(1.0,MAX(0.0,RVL))
      ELSE IF (CNM(1:3).EQ.'AYF'.OR.CNM(1:3).EQ.'ayf') THEN
         FAYF=MIN(1.0,MAX(0.0,RVL))
      ELSE IF (CNM(1:3).EQ.'ACM'.OR.CNM(1:3).EQ.'acm') THEN
         IACM=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'AFO'.OR.CNM(1:3).EQ.'afo') THEN
         IAFO=INT(RVL)
C
C wind barb parameters
C
      ELSE IF (CNM(1:3).EQ.'WBA'.OR.CNM(1:3).EQ.'wba') THEN
         WBAD=MIN(80.0,MAX(-80.0,RVL))
      ELSE IF (CNM(1:3).EQ.'WBT'.OR.CNM(1:3).EQ.'wbt') THEN
         WBTF=MIN(1.0,MAX(0.001,RVL))
      ELSE IF (CNM(1:3).EQ.'WBC'.OR.CNM(1:3).EQ.'wbc') THEN
         WBCF=MIN(1.0,MAX(0.001,RVL))
      ELSE IF (CNM(1:3).EQ.'WBD'.OR.CNM(1:3).EQ.'wbd') THEN
         WBDF=MIN(1.0,MAX(0.001,RVL))
      ELSE IF (CNM(1:3).EQ.'WBS'.OR.CNM(1:3).EQ.'wbs') THEN
         WBSC=MAX(0.0,RVL)
C
C character multiplier
C
      ELSE IF (CNM(1:3).EQ.'CWM'.OR.CNM(1:3).EQ.'cwm') THEN
        FCWM=RVL
C
C character attributes
C
      ELSE IF (CNM(1:3).EQ.'MNS'.OR.CNM(1:3).EQ.'mns') THEN
         FMNS=RVL
      ELSE IF (CNM(1:3).EQ.'MNX'.OR.CNM(1:3).EQ.'mnx') THEN
         FMNX=RVL
      ELSE IF (CNM(1:3).EQ.'MNY'.OR.CNM(1:3).EQ.'mny') THEN
         FMNY=RVL
      ELSE IF (CNM(1:3).EQ.'MNP'.OR. CNM(1:3).EQ.'mnp') THEN
         IMNP=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'MNC'.OR. CNM(1:3).EQ.'mnc') THEN
         IMNC=INT(RVL)
C
      ELSE IF (CNM(1:3).EQ.'MXS'.OR.CNM(1:3).EQ.'mxs') THEN
         FMXS=RVL
      ELSE IF (CNM(1:3).EQ.'MXX'.OR.CNM(1:3).EQ.'mxx') THEN
         FMXX=RVL
      ELSE IF (CNM(1:3).EQ.'MXY'.OR.CNM(1:3).EQ.'mxy') THEN
         FMXY=RVL
      ELSE IF (CNM(1:3).EQ.'MXP'.OR. CNM(1:3).EQ.'mxp') THEN
         IMXP=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'MXC'.OR. CNM(1:3).EQ.'mxc') THEN
         IMXC=INT(RVL)
C
      ELSE IF (CNM(1:3).EQ.'ZFS'.OR.CNM(1:3).EQ.'zfs') THEN
         FZFS=RVL
      ELSE IF (CNM(1:3).EQ.'ZFX'.OR.CNM(1:3).EQ.'zfx') THEN
         FZFX=RVL
      ELSE IF (CNM(1:3).EQ.'ZFY'.OR.CNM(1:3).EQ.'zfy') THEN
         FZFY=RVL
      ELSE IF (CNM(1:3).EQ.'ZFP'.OR. CNM(1:3).EQ.'zfp') THEN
         IZFP=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'ZFC'.OR. CNM(1:3).EQ.'zfc') THEN
         IZFC=INT(RVL)
C     
      ELSE IF (CNM(1:3).EQ.'LBS'.OR.CNM(1:3).EQ.'lbs') THEN
         FLBS=RVL
      ELSE IF (CNM(1:3).EQ.'LBC'.OR. CNM(1:3).EQ.'lbc') THEN
         ILBC=INT(RVL)
C
      ELSE IF (CNM(1:3).EQ.'ILS'.OR.CNM(1:3).EQ.'ils') THEN
         FILS=RVL
      ELSE IF (CNM(1:3).EQ.'ILX'.OR.CNM(1:3).EQ.'ilx') THEN
         FILX=RVL
      ELSE IF (CNM(1:3).EQ.'ILY'.OR.CNM(1:3).EQ.'ily') THEN
         FILY=RVL
      ELSE IF (CNM(1:3).EQ.'ILP'.OR. CNM(1:3).EQ.'ilp') THEN
         IILP=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'ILC'.OR. CNM(1:3).EQ.'ilc') THEN
         IILC=INT(RVL)
C     
C     color field parameters
C
      ELSE IF (CNM(1:3).EQ.'CLR'.OR.CNM(1:3).EQ.'clr') THEN
         ICLR(IPAI)=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'TVL'.OR.CNM(1:3).EQ.'tvl') THEN
         TVLU(IPAI)=RVL
      ELSE IF (CNM(1:3).EQ.'TRT'.OR. CNM(1:3).EQ.'trt') THEN
         ITRT=INT(RVL)
      ELSE
        CSTR(1:46)='VVSETI OR VVSETR - PARAMETER NAME NOT KNOWN - '
        CSTR(47:49)=CNM(1:3)
        CALL SETER (CSTR(1:49),3,1)
        RETURN
      END IF
C
      GOTO 9900
C
 9800 CONTINUE
C
      CSTR(1:50)='VVSETI OR VVSETR - PARAMETER VALUE OUT OF RANGE - '
      CSTR(51:53)=CNM(1:3)
      CALL SETER (CSTR(1:53),4,1)
      RETURN
C      
 9900 CONTINUE
C
C Done.
C
      RETURN
C
      END

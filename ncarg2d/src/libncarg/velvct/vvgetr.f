C
C	$Id: vvgetr.f,v 1.4 1993-01-15 22:46:52 dbrown Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE VVGETR (WHCH,RVAL)
C
      CHARACTER*(*) WHCH
C
C This subroutine is called to retrieve the real value of a specified
C parameter.
C
C WHCH is the name of the parameter whose value is to be retrieved.
C
C RVAL is a real variable in which the desired value is to be returned
C by VVGETR.
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
      PARAMETER (IPLVLS = 64)
C
C Integer and real common block variables
C
C
      COMMON /VVCOM/
     +                IUD1       ,IVD1       ,IPD1       ,IXDM       ,
     +                IYDN       ,VLOM       ,VHIM       ,ISET       ,
     +                VMXL       ,VFRC       ,IXIN       ,IYIN       ,
     +                ISVF       ,UUSV       ,UVSV       ,
     +                UPSV       ,IMSK       ,ICPM       ,UVPS       ,
     +                UVPL       ,UVPR       ,UVPB       ,UVPT       ,
     +                UWDL       ,UWDR       ,UWDB       ,UWDT       ,
     +                UXC1       ,UXCM       ,UYC1       ,UYCM       ,
     +                NLVL       ,IPAI       ,ICTV       ,WDLV       ,
     +                UVMN       ,UVMX       ,PMIN       ,PMAX       ,
     +                ISPC       ,ITHN       ,IPLR       ,IVST       ,
     +                IVPO       ,ILBL       ,IDPF       ,IMSG       ,
     +                ICLR(IPLVLS)           ,TVLU(IPLVLS)
C
C Arrow size/shape parameters
C
	COMMON / VVARO /
     +                HDSZ       ,HINF       ,HANG       ,
     +	              HSIN       ,HCOS       ,FAMN       ,FAMX

C
C Text related parameters
C
	COMMON /VVTXP /
     +                FCWM    ,ICSZ    ,
     +                FMNS    ,FMNX    ,FMNY    ,IMNP    ,IMNC  ,
     +                FMXS    ,FMXX    ,FMXY    ,IMXP    ,IMXC  ,
     +                FZFS    ,FZFX    ,FZFY    ,IZFP    ,IZFC  ,
     +                FILS    ,FILX    ,FILY    ,IILP     IILC  ,
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
      IF (LEN(WHCH).LT.3) THEN
        CSTR(1:46)='VVGETI OR VVGETR - PARAMETER NAME TOO SHORT - '
        CSTR(47:46+LEN(WHCH))=WHCH
        CALL SETER (CSTR(1:46+LEN(WHCH)),1,2)
        STOP
      END IF
C
C Check for incorrect use of the index parameter.
C
      IF (WHCH(1:3).EQ.'CLR'.OR.WHCH(1:3).EQ.'clr'
     +    .OR.WHCH(1:3).EQ.'TVL'.OR.WHCH(1:3).EQ.'tvl') THEN
         IF (IPAI.LT.1.OR.IPAI.GT.NLVL) THEN
            CSTR(1:46)='VVGETI OR VVGETR - GETTING XXX - PAI INCORRECT'
            CSTR(28:30)=WHCH(1:3)
            CALL SETER (CSTR(1:46),2,2)
            STOP
         END IF
      END IF
C
C Get the appropriate parameter value.
C
C Values in VVCOM
C
      IF (WHCH(1:3).EQ.'UD1'.OR. WHCH(1:3).EQ.'ud1') THEN
         RVAL=REAL(IUD1)
      ELSE IF (WHCH(1:3).EQ.'VD1'.OR. WHCH(1:3).EQ.'vd1') THEN
         RVAL=REAL(IVD1)
      ELSE IF (WHCH(1:3).EQ.'PD1'.OR. WHCH(1:3).EQ.'pd1') THEN
         RVAL=REAL(IPD1)
      ELSE IF (WHCH(1:3).EQ.'XDM'.OR. WHCH(1:3).EQ.'xdm') THEN
         RVAL=REAL(IXDM)
      ELSE IF (WHCH(1:3).EQ.'YDN'.OR. WHCH(1:3).EQ.'ydn') THEN
         RVAL=REAL(IYDN)
      ELSE IF (WHCH(1:3).EQ.'VLM'.OR.WHCH(1:3).EQ.'vlm') THEN
        RVAL=VLOM
      ELSE IF (WHCH(1:3).EQ.'VHM'.OR.WHCH(1:3).EQ.'vhm') THEN
        RVAL=VHIM
      ELSE IF (WHCH(1:3).EQ.'SET'.OR. WHCH(1:3).EQ.'set') THEN
         RVAL=REAL(ISET)
      ELSE IF (WHCH(1:3).EQ.'VML'.OR. WHCH(1:3).EQ.'vml') THEN
         RVAL=VMXL
      ELSE IF (WHCH(1:3).EQ.'VFR'.OR. WHCH(1:3).EQ.'vfr') THEN
         RVAL=VFRC
      ELSE IF (WHCH(1:3).EQ.'SVF'.OR. WHCH(1:3).EQ.'svf') THEN
         RVAL=REAL(ISVF)
      ELSE IF (WHCH(1:3).EQ.'USV'.OR.WHCH(1:3).EQ.'usv') THEN
         RVAL=UUSV
      ELSE IF (WHCH(1:3).EQ.'VSV'.OR.WHCH(1:3).EQ.'vsv') THEN
         RVAL=UVSV
      ELSE IF (WHCH(1:3).EQ.'PSV'.OR.WHCH(1:3).EQ.'psv') THEN
         RVAL=UPSV
      ELSE IF (WHCH(1:3).EQ.'CPM'.OR. WHCH(1:3).EQ.'cpm') THEN
         RVAL=REAL(ICPM)
      ELSE IF (WHCH(1:3).EQ.'WML'.OR. WHCH(1:3).EQ.'wml') THEN
         RVAL=RLEN
      ELSE IF (WHCH(1:3).EQ.'MSK'.OR. WHCH(1:3).EQ.'msk') THEN
         RVAL=REAL(IMSK)
      ELSE IF (WHCH(1:3).EQ.'VPS'.OR. WHCH(1:3).EQ.'vps') THEN
         RVAL=REAL(IVPS)
      ELSE IF (WHCH(1:3).EQ.'VPL'.OR.WHCH(1:3).EQ.'vpl') THEN
         RVAL=UVPL
      ELSE IF (WHCH(1:3).EQ.'VPR'.OR.WHCH(1:3).EQ.'vpr') THEN
         RVAL=UVPR
      ELSE IF (WHCH(1:3).EQ.'VPB'.OR.WHCH(1:3).EQ.'vpb') THEN
         RVAL=UVPB
      ELSE IF (WHCH(1:3).EQ.'VPT'.OR.WHCH(1:3).EQ.'vpt') THEN
         RVAL=UVPT
      ELSE IF (WHCH(1:3).EQ.'WDL'.OR.WHCH(1:3).EQ.'wdl') THEN
         RVAL=UWDL
      ELSE IF (WHCH(1:3).EQ.'WDR'.OR.WHCH(1:3).EQ.'wdr') THEN
         RVAL=UWDR
      ELSE IF (WHCH(1:3).EQ.'WDB'.OR.WHCH(1:3).EQ.'wdb') THEN
         RVAL=UWDB
      ELSE IF (WHCH(1:3).EQ.'WDT'.OR.WHCH(1:3).EQ.'wdt') THEN
         RVAL=UWDT
      ELSE IF (WHCH(1:3).EQ.'XC1'.OR.WHCH(1:3).EQ.'xc1') THEN
         RVAL=UXC1
      ELSE IF (WHCH(1:3).EQ.'XCM'.OR.WHCH(1:3).EQ.'xcm') THEN
         RVAL=UXCM
      ELSE IF (WHCH(1:3).EQ.'YC1'.OR.WHCH(1:3).EQ.'yc1') THEN
         RVAL=UYC1
      ELSE IF (WHCH(1:3).EQ.'YCM'.OR.WHCH(1:3).EQ.'ycm') THEN
         RVAL=UYCM
      ELSE IF (WHCH(1:3).EQ.'NLV'.OR.WHCH(1:3).EQ.'nlv') THEN
        RVAL=REAL(NLVL)
      ELSE IF (WHCH(1:3).EQ.'PAI'.OR.WHCH(1:3).EQ.'pai') THEN
        RVAL=REAL(IPAI)
      ELSE IF (WHCH(1:3).EQ.'CTV'.OR.WHCH(1:3).EQ.'ctv') THEN
        RVAL=REAL(ICTV)
      ELSE IF (WHCH(1:3).EQ.'LWD'.OR.WHCH(1:3).EQ.'lwd') THEN
        RVAL=WDLV
      ELSE IF (WHCH(1:3).EQ.'VMN'.OR.WHCH(1:3).EQ.'vmn') THEN
        RVAL=UVMN
      ELSE IF (WHCH(1:3).EQ.'VMX'.OR.WHCH(1:3).EQ.'vmx') THEN
        RVAL=UVMX
      ELSE IF (WHCH(1:3).EQ.'DMN'.OR.WHCH(1:3).EQ.'dmn') THEN
        RVAL=DVMN
      ELSE IF (WHCH(1:3).EQ.'DMX'.OR.WHCH(1:3).EQ.'dmx') THEN
        RVAL=DVMX
      ELSE IF (WHCH(1:3).EQ.'PMN'.OR.WHCH(1:3).EQ.'pmn') THEN
        RVAL=PMIN
      ELSE IF (WHCH(1:3).EQ.'PMX'.OR.WHCH(1:3).EQ.'pmx') THEN
        RVAL=PMAX
      ELSE IF (WHCH(1:3).EQ.'SPC'.OR. WHCH(1:3).EQ.'spc') THEN
         RVAL=REAL(ISPC)
      ELSE IF (WHCH(1:3).EQ.'THN'.OR. WHCH(1:3).EQ.'thn') THEN
         RVAL=REAL(ITHN)
      ELSE IF (WHCH(1:3).EQ.'MAP'.OR. WHCH(1:3).EQ.'map') THEN
         RVAL=REAL(IMAP)
      ELSE IF (WHCH(1:3).EQ.'PLR'.OR. WHCH(1:3).EQ.'plr') THEN
         RVAL=REAL(IPLR)
      ELSE IF (WHCH(1:3).EQ.'VST'.OR. WHCH(1:3).EQ.'vst') THEN
         RVAL=REAL(IVST)
      ELSE IF (WHCH(1:3).EQ.'VPO'.OR. WHCH(1:3).EQ.'vpo') THEN
         RVAL=REAL(IVPO)
      ELSE IF (WHCH(1:3).EQ.'LBL'.OR. WHCH(1:3).EQ.'lbl') THEN
         RVAL=REAL(ILBL)
      ELSE IF (WHCH(1:3).EQ.'DPF'.OR. WHCH(1:3).EQ.'dpf') THEN
         RVAL=REAL(IDPF)
      ELSE IF (WHCH(1:3).EQ.'MSG'.OR. WHCH(1:3).EQ.'msg') THEN
         IMSG=INT(RVAL)
C
C arrow min, max
C
      ELSE IF (WHCH(1:3).EQ.'AMN'.OR.WHCH(1:3).EQ.'amn') THEN
        RVAL=FAMN
      ELSE IF (WHCH(1:3).EQ.'AMX'.OR.WHCH(1:3).EQ.'amx') THEN
        RVAL=FAMX
C
C character multiplier
C
      ELSE IF (WHCH(1:3).EQ.'CWM'.OR.WHCH(1:3).EQ.'cwm') THEN
        RVAL=FCWM
C
C character attributes
C
      ELSE IF (WHCH(1:3).EQ.'MNS'.OR.WHCH(1:3).EQ.'mns') THEN
         RVAL=FMNS
      ELSE IF (WHCH(1:3).EQ.'MNX'.OR.WHCH(1:3).EQ.'mnx') THEN
         RVAL=FMNX
      ELSE IF (WHCH(1:3).EQ.'MNY'.OR.WHCH(1:3).EQ.'mny') THEN
         RVAL=FMNY
      ELSE IF (WHCH(1:3).EQ.'MNP'.OR. WHCH(1:3).EQ.'mnp') THEN
         RVAL=REAL(IMNP)
      ELSE IF (WHCH(1:3).EQ.'MNC'.OR. WHCH(1:3).EQ.'mnc') THEN
         RVAL=REAL(IMNC)
C
      ELSE IF (WHCH(1:3).EQ.'MXS'.OR.WHCH(1:3).EQ.'mxs') THEN
         RVAL=FMXS
      ELSE IF (WHCH(1:3).EQ.'MXX'.OR.WHCH(1:3).EQ.'mxx') THEN
         RVAL=FMXX
      ELSE IF (WHCH(1:3).EQ.'MXY'.OR.WHCH(1:3).EQ.'mxy') THEN
         RVAL=FMXY
      ELSE IF (WHCH(1:3).EQ.'MXP'.OR. WHCH(1:3).EQ.'mxp') THEN
         RVAL=REAL(IMXP)
      ELSE IF (WHCH(1:3).EQ.'MXC'.OR. WHCH(1:3).EQ.'mxc') THEN
         RVAL=REAL(IMXC)
C
      ELSE IF (WHCH(1:3).EQ.'ZFS'.OR.WHCH(1:3).EQ.'zfs') THEN
         RVAL=FZFS
      ELSE IF (WHCH(1:3).EQ.'ZFX'.OR.WHCH(1:3).EQ.'zfx') THEN
         RVAL=FZFX
      ELSE IF (WHCH(1:3).EQ.'ZFY'.OR.WHCH(1:3).EQ.'zfy') THEN
         RVAL=FZFY
      ELSE IF (WHCH(1:3).EQ.'ZFP'.OR. WHCH(1:3).EQ.'zfp') THEN
         RVAL=REAL(IZFP)
      ELSE IF (WHCH(1:3).EQ.'ZFC'.OR. WHCH(1:3).EQ.'zfc') THEN
         RVAL=REAL(IZFC)
C
      ELSE IF (WHCH(1:3).EQ.'LBS'.OR.WHCH(1:3).EQ.'lbs') THEN
         RVAL=FLBS
      ELSE IF (WHCH(1:3).EQ.'LBC'.OR. WHCH(1:3).EQ.'lbc') THEN
         RVAL=REAL(ILBC)
C
      ELSE IF (WHCH(1:3).EQ.'ILS'.OR.WHCH(1:3).EQ.'ils') THEN
         RVAL=FILS
      ELSE IF (WHCH(1:3).EQ.'ILX'.OR.WHCH(1:3).EQ.'ilx') THEN
         RVAL=FILX
      ELSE IF (WHCH(1:3).EQ.'ILY'.OR.WHCH(1:3).EQ.'ily') THEN
         RVAL=FILY
      ELSE IF (WHCH(1:3).EQ.'ILP'.OR. WHCH(1:3).EQ.'ilp') THEN
         RVAL=REAL(IILP)
      ELSE IF (WHCH(1:3).EQ.'ILC'.OR. WHCH(1:3).EQ.'ilc') THEN
         RVAL=REAL(IILC)
C
C color field parameters
C
      ELSE IF (WHCH(1:3).EQ.'CLR'.OR.WHCH(1:3).EQ.'clr') THEN
         RVAL=REAL(ICLR(IPAI))
      ELSE IF (WHCH(1:3).EQ.'TVL'.OR.WHCH(1:3).EQ.'tvl') THEN
         RVAL=TVLU(IPAI)
      ELSE IF (WHCH(1:3).EQ.'TRT'.OR. WHCH(1:3).EQ.'trt') THEN
         RVAL=REAL(ITRT)
      ELSE
         CSTR(1:46)='VVGETI OR VVGETR - PARAMETER NAME NOT KNOWN - '
         CSTR(47:49)=WHCH(1:3)
         CALL SETER (CSTR(1:49),3,2)
         STOP
      END IF
C
 9900 CONTINUE
C
C Done.
C
      RETURN
C
      END

C
C	$Id: vvsetr.f,v 1.5 1993-01-20 19:58:54 dbrown Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE VVSETR (WHCH,RVAL)
C
      CHARACTER*(*) WHCH
C
C This subroutine is called to set the real value of a specified
C parameter.
C
C WHCH is the name of the parameter whose value is to be set.
C
C RVAL is a real variable containing the new value of the parameter.
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
     +                UXC1       ,UXCM       ,UYC1       ,UYCN       ,
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
      IF (LEN(WHCH).LT.3) THEN
        CSTR(1:46)='VVSETI OR VVSETR - PARAMETER NAME TOO SHORT - '
        CSTR(47:46+LEN(WHCH))=WHCH
        CALL SETER (CSTR(1:46+LEN(WHCH)),1,2)
        STOP
      END IF
C
C Check for incorrect use of the index parameter.
C
      IF (WHCH(1:3).EQ.'CLR'.OR.WHCH(1:3).EQ.'clr'
     +    .OR.WHCH(1:3).EQ.'TVL'.OR.WHCH(1:3).EQ.'tvl') THEN
         IF (IPAI.LT.1.OR.IPAI.GT.IPLVLS) THEN
            CSTR(1:46)='VVSETI OR VVSETR - SETTING XXX - PAI INCORRECT'
            CSTR(28:30)=WHCH(1:3)
            CALL SETER (CSTR(1:46),2,2)
            STOP
         END IF
      END IF
C
C Set the appropriate parameter value.
C
C Values in VVCOM
C 
      IF (WHCH(1:3).EQ.'UD1'.OR. WHCH(1:3).EQ.'ud1') THEN
         IUD1=INT(RVAL)
      ELSE IF (WHCH(1:3).EQ.'VD1'.OR. WHCH(1:3).EQ.'vd1') THEN
         IVD1=INT(RVAL)
      ELSE IF (WHCH(1:3).EQ.'PD1'.OR. WHCH(1:3).EQ.'pd1') THEN
         IPD1=INT(RVAL)
      ELSE IF (WHCH(1:3).EQ.'XDM'.OR. WHCH(1:3).EQ.'xdm') THEN
         IXDM=INT(RVAL)
      ELSE IF (WHCH(1:3).EQ.'YDN'.OR. WHCH(1:3).EQ.'ydn') THEN
         IYDN=INT(RVAL)
      ELSE IF (WHCH(1:3).EQ.'VLM'.OR.WHCH(1:3).EQ.'vlm') THEN
         VLOM=ABS(RVAL) 
      ELSE IF (WHCH(1:3).EQ.'VHM'.OR.WHCH(1:3).EQ.'vhm') THEN
         VHIM=ABS(RVAL) 
      ELSE IF (WHCH(1:3).EQ.'SET'.OR. WHCH(1:3).EQ.'set') THEN
         ISET=INT(RVAL)
      ELSE IF (WHCH(1:3).EQ.'VML'.OR. WHCH(1:3).EQ.'vml') THEN
         VMXL=RVAL
      ELSE IF (WHCH(1:3).EQ.'VFR'.OR. WHCH(1:3).EQ.'vfr') THEN
         VFRC=RVAL
      ELSE IF (WHCH(1:3).EQ.'XIN'.OR. WHCH(1:3).EQ.'xin') THEN
         IXIN=INT(RVAL)
      ELSE IF (WHCH(1:3).EQ.'YIN'.OR. WHCH(1:3).EQ.'yin') THEN
         IYIN=INT(RVAL)
      ELSE IF (WHCH(1:3).EQ.'SVF'.OR. WHCH(1:3).EQ.'svf') THEN
         ISVF=INT(RVAL)
      ELSE IF (WHCH(1:3).EQ.'USV'.OR.WHCH(1:3).EQ.'usv') THEN
         UUSV=RVAL
      ELSE IF (WHCH(1:3).EQ.'VSV'.OR.WHCH(1:3).EQ.'vsv') THEN
         UVSV=RVAL
      ELSE IF (WHCH(1:3).EQ.'PSV'.OR.WHCH(1:3).EQ.'psv') THEN
         UPSV=RVAL
      ELSE IF (WHCH(1:3).EQ.'MSK'.OR. WHCH(1:3).EQ.'msk') THEN
         IMSK=INT(RVAL)
      ELSE IF (WHCH(1:3).EQ.'CPM'.OR. WHCH(1:3).EQ.'cpm') THEN
         ICPM=INT(RVAL)
      ELSE IF (WHCH(1:3).EQ.'VPS'.OR. WHCH(1:3).EQ.'vps') THEN
         UVPS=RVAL
      ELSE IF (WHCH(1:3).EQ.'VPL'.OR.WHCH(1:3).EQ.'vpl') THEN
         UVPL=MIN(1.0,MAX(0.0,RVAL))
      ELSE IF (WHCH(1:3).EQ.'VPR'.OR.WHCH(1:3).EQ.'vpr') THEN
         UVPR=MIN(1.0,MAX(0.0,RVAL))
      ELSE IF (WHCH(1:3).EQ.'VPB'.OR.WHCH(1:3).EQ.'vpb') THEN
         UVPB=MIN(1.0,MAX(0.0,RVAL))
      ELSE IF (WHCH(1:3).EQ.'VPT'.OR.WHCH(1:3).EQ.'vpt') THEN
         UVPT=MIN(1.0,MAX(0.0,RVAL))
      ELSE IF (WHCH(1:3).EQ.'WDL'.OR.WHCH(1:3).EQ.'wdl') THEN
         UWDL=RVAL
      ELSE IF (WHCH(1:3).EQ.'WDR'.OR.WHCH(1:3).EQ.'wdr') THEN
         UWDR=RVAL
      ELSE IF (WHCH(1:3).EQ.'WDB'.OR.WHCH(1:3).EQ.'wdb') THEN
         UWDB=RVAL
      ELSE IF (WHCH(1:3).EQ.'WDT'.OR.WHCH(1:3).EQ.'wdt') THEN
         UWDT=RVAL
      ELSE IF (WHCH(1:3).EQ.'XC1'.OR.WHCH(1:3).EQ.'xc1') THEN
         UXC1=RVAL
      ELSE IF (WHCH(1:3).EQ.'XCM'.OR.WHCH(1:3).EQ.'xcm') THEN
         UXCM=RVAL
      ELSE IF (WHCH(1:3).EQ.'YC1'.OR.WHCH(1:3).EQ.'yc1') THEN
         UYC1=RVAL
      ELSE IF (WHCH(1:3).EQ.'YCN'.OR.WHCH(1:3).EQ.'ycn') THEN
         UYCN=RVAL
      ELSE IF (WHCH(1:3).EQ.'NLV'.OR.WHCH(1:3).EQ.'nlv') THEN
         NLVL=INT(RVAL)
      ELSE IF (WHCH(1:3).EQ.'PAI'.OR.WHCH(1:3).EQ.'pai') THEN
         IF (RVAL .LT. 1.0 .OR. RVAL .GT. IPLVLS) GO TO 9800
         IPAI=INT(RVAL)
      ELSE IF (WHCH(1:3).EQ.'CTV'.OR.WHCH(1:3).EQ.'ctv') THEN
         ICTV=INT(RVAL)
      ELSE IF (WHCH(1:3).EQ.'LWD'.OR.WHCH(1:3).EQ.'lwd') THEN
         IF (RVAL .LE. 0.0) GO TO 9800
         WDLV=RVAL
C
C UVMN,UVMX, PMIN, PMAX are read-only
C
      ELSE IF (WHCH(1:3).EQ.'SPC'.OR. WHCH(1:3).EQ.'spc') THEN
         ISPC=INT(RVAL)
      ELSE IF (WHCH(1:3).EQ.'THN'.OR. WHCH(1:3).EQ.'thn') THEN
         ITHN=INT(RVAL)
      ELSE IF (WHCH(1:3).EQ.'MAP'.OR. WHCH(1:3).EQ.'map') THEN
         IMAP=INT(RVAL)
      ELSE IF (WHCH(1:3).EQ.'PLR'.OR. WHCH(1:3).EQ.'plr') THEN
         IPLR=INT(RVAL)
      ELSE IF (WHCH(1:3).EQ.'VST'.OR. WHCH(1:3).EQ.'vst') THEN
         IVST=INT(RVAL)
      ELSE IF (WHCH(1:3).EQ.'VPO'.OR. WHCH(1:3).EQ.'vpo') THEN
         IVPO=INT(RVAL)
      ELSE IF (WHCH(1:3).EQ.'LBL'.OR. WHCH(1:3).EQ.'lbl') THEN
         ILBL=INT(RVAL)
      ELSE IF (WHCH(1:3).EQ.'DPF'.OR. WHCH(1:3).EQ.'dpf') THEN
         IDPF=INT(RVAL)
      ELSE IF (WHCH(1:3).EQ.'MSG'.OR. WHCH(1:3).EQ.'msg') THEN
         IMSG=INT(RVAL)
C
C arrow min, max
C
      ELSE IF (WHCH(1:3).EQ.'AMN'.OR.WHCH(1:3).EQ.'amn') THEN
         FAMN=RVAL
      ELSE IF (WHCH(1:3).EQ.'AMX'.OR.WHCH(1:3).EQ.'amx') THEN
         FAMX=RVAL
C
C character multiplier
C
      ELSE IF (WHCH(1:3).EQ.'CWM'.OR.WHCH(1:3).EQ.'cwm') THEN
        FCWM=RVAL
C
C character attributes
C
      ELSE IF (WHCH(1:3).EQ.'MNS'.OR.WHCH(1:3).EQ.'mns') THEN
         FMNS=RVAL
      ELSE IF (WHCH(1:3).EQ.'MNX'.OR.WHCH(1:3).EQ.'mnx') THEN
         FMNX=RVAL
      ELSE IF (WHCH(1:3).EQ.'MNY'.OR.WHCH(1:3).EQ.'mny') THEN
         FMNY=RVAL
      ELSE IF (WHCH(1:3).EQ.'MNP'.OR. WHCH(1:3).EQ.'mnp') THEN
         IMNP=INT(RVAL)
      ELSE IF (WHCH(1:3).EQ.'MNC'.OR. WHCH(1:3).EQ.'mnc') THEN
         IMNC=INT(RVAL)
C
      ELSE IF (WHCH(1:3).EQ.'MXS'.OR.WHCH(1:3).EQ.'mxs') THEN
         FMXS=RVAL
      ELSE IF (WHCH(1:3).EQ.'MXX'.OR.WHCH(1:3).EQ.'mxx') THEN
         FMXX=RVAL
      ELSE IF (WHCH(1:3).EQ.'MXY'.OR.WHCH(1:3).EQ.'mxy') THEN
         FMXY=RVAL
      ELSE IF (WHCH(1:3).EQ.'MXP'.OR. WHCH(1:3).EQ.'mxp') THEN
         IMXP=INT(RVAL)
      ELSE IF (WHCH(1:3).EQ.'MXC'.OR. WHCH(1:3).EQ.'mxc') THEN
         IMXC=INT(RVAL)
C
      ELSE IF (WHCH(1:3).EQ.'ZFS'.OR.WHCH(1:3).EQ.'zfs') THEN
         FZFS=RVAL
      ELSE IF (WHCH(1:3).EQ.'ZFX'.OR.WHCH(1:3).EQ.'zfx') THEN
         FZFX=RVAL
      ELSE IF (WHCH(1:3).EQ.'ZFY'.OR.WHCH(1:3).EQ.'zfy') THEN
         FZFY=RVAL
      ELSE IF (WHCH(1:3).EQ.'ZFP'.OR. WHCH(1:3).EQ.'zfp') THEN
         IZFP=INT(RVAL)
      ELSE IF (WHCH(1:3).EQ.'ZFP'.OR. WHCH(1:3).EQ.'zfp') THEN
         IZFP=INT(RVAL)
C     
      ELSE IF (WHCH(1:3).EQ.'LBS'.OR.WHCH(1:3).EQ.'lbs') THEN
         FLBS=RVAL
      ELSE IF (WHCH(1:3).EQ.'LBC'.OR. WHCH(1:3).EQ.'lbc') THEN
         ILBC=INT(RVAL)
C
      ELSE IF (WHCH(1:3).EQ.'ILS'.OR.WHCH(1:3).EQ.'ils') THEN
         FILS=RVAL
      ELSE IF (WHCH(1:3).EQ.'ILX'.OR.WHCH(1:3).EQ.'ilx') THEN
         FILX=RVAL
      ELSE IF (WHCH(1:3).EQ.'ILY'.OR.WHCH(1:3).EQ.'ily') THEN
         FILY=RVAL
      ELSE IF (WHCH(1:3).EQ.'ILP'.OR. WHCH(1:3).EQ.'ilp') THEN
         IILP=INT(RVAL)
      ELSE IF (WHCH(1:3).EQ.'ILC'.OR. WHCH(1:3).EQ.'ilc') THEN
         IILC=INT(RVAL)
C     
C     color field parameters
C
      ELSE IF (WHCH(1:3).EQ.'CLR'.OR.WHCH(1:3).EQ.'clr') THEN
         ICLR(IPAI)=INT(RVAL)
      ELSE IF (WHCH(1:3).EQ.'TVL'.OR.WHCH(1:3).EQ.'tvl') THEN
         TVLU(IPAI)=RVAL
      ELSE IF (WHCH(1:3).EQ.'TRT'.OR. WHCH(1:3).EQ.'trt') THEN
         ITRT=INT(RVAL)
      ELSE
        CSTR(1:46)='VVSETI OR VVSETR - PARAMETER NAME NOT KNOWN - '
        CSTR(47:49)=WHCH(1:3)
        CALL SETER (CSTR(1:49),3,2)
        STOP
      END IF
C
      GOTO 9900
C
 9800 CONTINUE
C
      CSTR(1:50)='VVSETI OR VVSETR - PARAMETER VALUE OUT OF RANGE - '
      CSTR(51:53)=WHCH(1:3)
      CALL SETER (CSTR(1:53),3,2)
      STOP
C      
 9900 CONTINUE
C
C Done.
C
      RETURN
C
      END

C
C       $Id: stsetr.f,v 1.6 1993-12-03 21:18:57 kennison Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE STSETR (CNM,RVL)
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
C Declare the ST common blocks.
C
      PARAMETER (IPLVLS = 64)
C
C Integer and real common block variables
C
C
      COMMON / STPAR /
     +                IUD1       ,IVD1       ,IPD1       ,
     +                IXD1       ,IXDM       ,IYD1       ,IYDN       ,
     +                IXM1       ,IYM1       ,IXM2       ,IYM2       ,
     +                IWKD       ,IWKU       ,ISET       ,IERR       ,
     +                IXIN       ,IYIN       ,IMSK       ,ICPM       ,
     +                NLVL       ,IPAI       ,ICTV       ,WDLV       ,
     +                UVMN       ,UVMX       ,PMIN       ,PMAX       ,
     +                ITHN       ,IPLR       ,ISST       ,
     +                ICLR(IPLVLS)           ,TVLU(IPLVLS)
C
      COMMON / STTRAN /
     +                UVPS       ,
     +                UVPL       ,UVPR       ,UVPB       ,UVPT       ,
     +                UWDL       ,UWDR       ,UWDB       ,UWDT       ,
     +                UXC1       ,UXCM       ,UYC1       ,UYCN 
C
C Stream algorithm parameters
C
      COMMON / STSTRM /
     +                ISGD       ,IAGD       ,RARL       ,ICKP       ,
     +                ICKX       ,ITRP       ,ICYK       ,RVNL       ,
     +                ISVF       ,RUSV       ,RVSV       ,RNDA       ,
     +                ISPC       ,RPSV       ,RCDS       ,RSSP       ,
     +                RDFM
C
C Text related parameters
C Note: graphical text output is not yet implemented for the
C       Streamline utility.
C
      COMMON / STTXP /
     +                FCWM    ,ICSZ    ,
     +                FMNS    ,FMNX    ,FMNY    ,IMNP    ,IMNC  ,
     +                FMXS    ,FMXX    ,FMXY    ,IMXP    ,IMXC  ,
     +                FZFS    ,FZFX    ,FZFY    ,IZFP    ,IZFC  ,
     +                FILS    ,FILX    ,FILY    ,IILP    ,IILC 
C
C Character variable declartions
C
      CHARACTER*160 CSTR
      PARAMETER (IPCHSZ=80)
      CHARACTER*(IPCHSZ)  CMNT,CMXT,CZFT,CILT
C
C Text string parameters
C
      COMMON / STCHAR / CSTR,CMNT,CMXT,CZFT,CILT
C
      SAVE /STPAR/, /STTRAN/, /STSTRM/, /STTXP/, /STCHAR/
C
C Internal buffer lengths
C
C IPNPTS - Number of points in the point buffer -- not less than 3
C IPLSTL - Streamline-crossover-check circular list length
C IPGRCT - Number of groups supported for area masking
C
      PARAMETER (IPNPTS = 10, IPLSTL = 750, IPGRCT = 64)
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
C Check for a parameter name that is too short.
C
      IF (LEN(CNM).LT.3) THEN
        CSTR(1:46)='STSETI OR STSETR - PARAMETER NAME TOO SHORT - '
        CSTR(47:46+LEN(CNM))=CNM
        CALL SETER (CSTR(1:46+LEN(CNM)),1,2)
        STOP
      END IF
C
C Check for incorrect use of the index parameter.
C
      IF (CNM(1:3).EQ.'CLR'.OR.CNM(1:3).EQ.'clr'
     +    .OR.CNM(1:3).EQ.'TVL'.OR.CNM(1:3).EQ.'tvl') THEN
         IF (IPAI.LT.1.OR.IPAI.GT.IPLVLS) THEN
            CSTR(1:46)='STSETI OR STSETR - SETTING XXX - PAI INCORRECT'
            CSTR(28:30)=CNM(1:3)
            CALL SETER (CSTR(1:46),2,2)
            STOP
         END IF
      END IF
C
C Set the appropriate parameter value.
C
C ---------------------------------------------------------------------
C
C Values in STPAR
C
      IF (CNM(1:3).EQ.'UD1'.OR. CNM(1:3).EQ.'ud1') THEN
         IUD1=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'VD1'.OR. CNM(1:3).EQ.'vd1') THEN
         IVD1=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'PD1'.OR. CNM(1:3).EQ.'pd1') THEN
         IPD1=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'XD1'.OR. CNM(1:3).EQ.'xd1') THEN
         IXD1=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'XDM'.OR. CNM(1:3).EQ.'xdm') THEN
         IXDM=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'YD1'.OR. CNM(1:3).EQ.'yd1') THEN
         IYD1=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'YDN'.OR. CNM(1:3).EQ.'ydn') THEN
         IYDN=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'WKD'.OR.CNM(1:3).EQ.'wkd') THEN
         IWKD=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'WKU'.OR.CNM(1:3).EQ.'wku') THEN
         IWKU=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'SET'.OR. CNM(1:3).EQ.'set') THEN
         ISET=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'ERR'.OR. CNM(1:3).EQ.'err') THEN
         IERR=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'XIN'.OR. CNM(1:3).EQ.'xin') THEN
         IXIN=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'YIN'.OR. CNM(1:3).EQ.'yin') THEN
         IYIN=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'MSK'.OR. CNM(1:3).EQ.'msk') THEN
         IMSK=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'CPM'.OR. CNM(1:3).EQ.'cpm') THEN
         ICPM=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'NLV'.OR.CNM(1:3).EQ.'nlv') THEN
         NLVL=INT(RVL)
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
      ELSE IF (CNM(1:3).EQ.'THN'.OR. CNM(1:3).EQ.'thn') THEN
         ITHN=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'PLR'.OR. CNM(1:3).EQ.'plr') THEN
         IPLR=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'SST'.OR. CNM(1:3).EQ.'sst') THEN
         ISST=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'CLR'.OR.CNM(1:3).EQ.'clr') THEN
         ICLR(IPAI)=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'TVL'.OR.CNM(1:3).EQ.'tvl') THEN
         TVLU(IPAI)=RVL
C
C ---------------------------------------------------------------------
C
C Values in STTRAN
C
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
C
C ---------------------------------------------------------------------
C
C Values in STSTRM
C
      ELSE IF (CNM(1:3).EQ.'SGD'.OR. CNM(1:3).EQ.'sgd') THEN
         ISGD=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'AGD'.OR. CNM(1:3).EQ.'agd') THEN
         IAGD=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'ARL'.OR. CNM(1:3).EQ.'arl') THEN
         RARL=RVL
      ELSE IF (CNM(1:3).EQ.'CKP'.OR. CNM(1:3).EQ.'ckp') THEN
         ICKP=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'CKX'.OR. CNM(1:3).EQ.'ckx') THEN
         ICKX=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'TRP'.OR. CNM(1:3).EQ.'trp') THEN
         ITRP=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'CYK'.OR. CNM(1:3).EQ.'cyk') THEN
         ICYK=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'VNL'.OR. CNM(1:3).EQ.'vnl') THEN
         RVNL=RVL
      ELSE IF (CNM(1:3).EQ.'SVF'.OR. CNM(1:3).EQ.'svf') THEN
         ISVF=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'USV'.OR. CNM(1:3).EQ.'usv') THEN
         RUSV=RVL
      ELSE IF (CNM(1:3).EQ.'VSV'.OR. CNM(1:3).EQ.'vsv') THEN
         RVSV=RVL
      ELSE IF (CNM(1:3).EQ.'PSV'.OR. CNM(1:3).EQ.'psv') THEN
         RPSV=RVL
      ELSE IF (CNM(1:3).EQ.'SPC'.OR. CNM(1:3).EQ.'spc') THEN
         ISPC=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'CDS'.OR. CNM(1:3).EQ.'cds') THEN
         RCDS=RVL
      ELSE IF (CNM(1:3).EQ.'SSP'.OR. CNM(1:3).EQ.'ssp') THEN
         RSSP=RVL
      ELSE IF (CNM(1:3).EQ.'DFM'.OR. CNM(1:3).EQ.'dfm') THEN
         RDFM=RVL
C
C ---------------------------------------------------------------------
C
C Values in STTXP
C
C Character multiplier
C
      ELSE IF (CNM(1:3).EQ.'CWM'.OR.CNM(1:3).EQ.'cwm') THEN
        FCWM=RVL
C
C Character attributes
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
C ---------------------------------------------------------------------
C
C Values in STMAP
C
      ELSE IF (CNM(1:3).EQ.'MAP'.OR. CNM(1:3).EQ.'map') THEN
         IMAP=INT(RVL)
      ELSE IF (CNM(1:3).EQ.'TRT'.OR. CNM(1:3).EQ.'trt') THEN
         ITRT=INT(RVL)
C
C ---------------------------------------------------------------------
C
      ELSE
        CSTR(1:46)='STSETI OR STSETR - PARAMETER NAME NOT KNOWN - '
        CSTR(47:49)=CNM(1:3)
        CALL SETER (CSTR(1:49),3,2)
        STOP
      END IF
C
      GOTO 9900
C
 9800 CONTINUE
C
      CSTR(1:50)='STSETI OR STSETR - PARAMETER VALUE OUT OF RANGE - '
      CSTR(51:53)=CNM(1:3)
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

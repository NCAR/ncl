C
C	$Id: stgetr.f,v 1.2 1993-01-21 01:14:44 dbrown Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE STGETR (WHCH,RVAL)
C
      CHARACTER*(*) WHCH
C
C This subroutine is called to retrieve the real value of a specified
C parameter.
C
C WHCH is the name of the parameter whose value is to be retrieved.
C
C RVAL is a real variable in which the desired value is to be returned
C by STGETR.
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
     +	              IXIN       ,IYIN       ,IMSK       ,ICPM       ,
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
      SAVE /STPAR/, /STCOM/, /STTRAN/, /STSTRM/, /STTXP/, /STCHAR/
C
C Internal buffer lengths
C
C IPNPTS - Number of points in the point buffer -- not less than 3
C IPLSTL - Streamline-crossover-check circular list length
C IPGRCT - Number of groups supported for area masking
C
      PARAMETER (IPNPTS = 10, IPLSTL = 750, IPGRCT = 64)
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
C Check for a parameter name that is too short.
C
      IF (LEN(WHCH).LT.3) THEN
        CSTR(1:46)='STGETI OR STGETR - PARAMETER NAME TOO SHORT - '
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
            CSTR(1:46)='STGETI OR STGETR - GETTING XXX - PAI INCORRECT'
            CSTR(28:30)=WHCH(1:3)
            CALL SETER (CSTR(1:46),2,2)
            STOP
         END IF
      END IF
C
C Get the appropriate parameter value.
C
C ---------------------------------------------------------------------
C
C Values in STPAR
C
      IF (WHCH(1:3).EQ.'UD1'.OR. WHCH(1:3).EQ.'ud1') THEN
         RVAL=REAL(IUD1)
      ELSE IF (WHCH(1:3).EQ.'VD1'.OR. WHCH(1:3).EQ.'vd1') THEN
         RVAL=REAL(IVD1)
      ELSE IF (WHCH(1:3).EQ.'PD1'.OR. WHCH(1:3).EQ.'pd1') THEN
         RVAL=REAL(IPD1)
      ELSE IF (WHCH(1:3).EQ.'XD1'.OR. WHCH(1:3).EQ.'xd1') THEN
         RVAL=REAL(IXD1)
      ELSE IF (WHCH(1:3).EQ.'XDM'.OR. WHCH(1:3).EQ.'xdm') THEN
         RVAL=REAL(IXDM)
      ELSE IF (WHCH(1:3).EQ.'YD1'.OR. WHCH(1:3).EQ.'yd1') THEN
         RVAL=REAL(IYD1)
      ELSE IF (WHCH(1:3).EQ.'YDN'.OR. WHCH(1:3).EQ.'ydn') THEN
         RVAL=REAL(IYDN)
      ELSE IF (WHCH(1:3).EQ.'WKD'.OR.WHCH(1:3).EQ.'wkd') THEN
        RVAL=REAL(IWKD)
      ELSE IF (WHCH(1:3).EQ.'WKU'.OR.WHCH(1:3).EQ.'wku') THEN
        RVAL=REAL(IWKU)
      ELSE IF (WHCH(1:3).EQ.'SET'.OR. WHCH(1:3).EQ.'set') THEN
         RVAL=REAL(ISET)
      ELSE IF (WHCH(1:3).EQ.'ERR'.OR. WHCH(1:3).EQ.'err') THEN
         RVAL=REAL(IERR)
      ELSE IF (WHCH(1:3).EQ.'XIN'.OR.WHCH(1:3).EQ.'xin') THEN
        RVAL=IXIN
      ELSE IF (WHCH(1:3).EQ.'YIN'.OR.WHCH(1:3).EQ.'yin') THEN
        RVAL=IYIN
      ELSE IF (WHCH(1:3).EQ.'MSK'.OR. WHCH(1:3).EQ.'msk') THEN
         RVAL=REAL(IMSK)
      ELSE IF (WHCH(1:3).EQ.'CPM'.OR. WHCH(1:3).EQ.'cpm') THEN
         RVAL=REAL(ICPM)
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
      ELSE IF (WHCH(1:3).EQ.'PMN'.OR.WHCH(1:3).EQ.'pmn') THEN
        RVAL=PMIN
      ELSE IF (WHCH(1:3).EQ.'PMX'.OR.WHCH(1:3).EQ.'pmx') THEN
        RVAL=PMAX
      ELSE IF (WHCH(1:3).EQ.'THN'.OR. WHCH(1:3).EQ.'thn') THEN
         RVAL=REAL(ITHN)
      ELSE IF (WHCH(1:3).EQ.'PLR'.OR. WHCH(1:3).EQ.'plr') THEN
         RVAL=REAL(IPLR)
      ELSE IF (WHCH(1:3).EQ.'SST'.OR. WHCH(1:3).EQ.'sst') THEN
         RVAL=REAL(ISST)
      ELSE IF (WHCH(1:3).EQ.'CLR'.OR.WHCH(1:3).EQ.'clr') THEN
         RVAL=REAL(ICLR(IPAI))
      ELSE IF (WHCH(1:3).EQ.'TVL'.OR.WHCH(1:3).EQ.'tvl') THEN
         RVAL=TVLU(IPAI)
C
C ---------------------------------------------------------------------
C
C Values in STTRAN
C
      ELSE IF (WHCH(1:3).EQ.'VPS'.OR. WHCH(1:3).EQ.'vps') THEN
         RVAL=REAL(UVPS)
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
      ELSE IF (WHCH(1:3).EQ.'YCN'.OR.WHCH(1:3).EQ.'ycn') THEN
         RVAL=UYCN
C
C ---------------------------------------------------------------------
C
C Values in STSTRM
C
      ELSE IF (WHCH(1:3).EQ.'SGD'.OR. WHCH(1:3).EQ.'sgd') THEN
         RVAL=REAL(ISGD)
      ELSE IF (WHCH(1:3).EQ.'AGD'.OR. WHCH(1:3).EQ.'agd') THEN
         RVAL=REAL(IAGD)
      ELSE IF (WHCH(1:3).EQ.'ARL'.OR. WHCH(1:3).EQ.'arl') THEN
         RVAL=RARL
      ELSE IF (WHCH(1:3).EQ.'CKP'.OR. WHCH(1:3).EQ.'ckp') THEN
         RVAL=REAL(ICKP)
      ELSE IF (WHCH(1:3).EQ.'CKX'.OR. WHCH(1:3).EQ.'ckx') THEN
         RVAL=REAL(ICKX)
      ELSE IF (WHCH(1:3).EQ.'TRP'.OR. WHCH(1:3).EQ.'trp') THEN
         RVAL=REAL(ITRP)
      ELSE IF (WHCH(1:3).EQ.'CYK'.OR. WHCH(1:3).EQ.'cyk') THEN
         RVAL=REAL(ICYK)
      ELSE IF (WHCH(1:3).EQ.'VNL'.OR. WHCH(1:3).EQ.'vnl') THEN
         RVAL=RVNL
      ELSE IF (WHCH(1:3).EQ.'SVF'.OR. WHCH(1:3).EQ.'svf') THEN
         RVAL=REAL(ISVF)
      ELSE IF (WHCH(1:3).EQ.'USV'.OR. WHCH(1:3).EQ.'usv') THEN
         RVAL=RUSV
      ELSE IF (WHCH(1:3).EQ.'VSV'.OR. WHCH(1:3).EQ.'vsv') THEN
         RVAL=RVSV
      ELSE IF (WHCH(1:3).EQ.'PSV'.OR. WHCH(1:3).EQ.'psv') THEN
         RVAL=RPSV
      ELSE IF (WHCH(1:3).EQ.'SPC'.OR. WHCH(1:3).EQ.'spc') THEN
         RVAL=REAL(ISPC)
      ELSE IF (WHCH(1:3).EQ.'CDS'.OR. WHCH(1:3).EQ.'cds') THEN
         RVAL=RCDS
      ELSE IF (WHCH(1:3).EQ.'SSP'.OR. WHCH(1:3).EQ.'ssp') THEN
         RVAL=RSSP
      ELSE IF (WHCH(1:3).EQ.'DFM'.OR. WHCH(1:3).EQ.'dfm') THEN
         RVAL=RDFM
C
C ---------------------------------------------------------------------
C
C Values in STTXP
C
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
C ---------------------------------------------------------------------
C
C Values in STMAP
C
      ELSE IF (WHCH(1:3).EQ.'MAP'.OR. WHCH(1:3).EQ.'map') THEN
         RVAL=REAL(IMAP)
      ELSE IF (WHCH(1:3).EQ.'TRT'.OR. WHCH(1:3).EQ.'trt') THEN
         RVAL=REAL(ITRT)
      ELSE IF (WHCH(1:3).EQ.'VPL'.OR.WHCH(1:3).EQ.'vpl') THEN
         RVAL=XVPL
      ELSE IF (WHCH(1:3).EQ.'VPR'.OR.WHCH(1:3).EQ.'vpr') THEN
         RVAL=XVPR
      ELSE IF (WHCH(1:3).EQ.'VPB'.OR.WHCH(1:3).EQ.'vpb') THEN
         RVAL=YVPB
      ELSE IF (WHCH(1:3).EQ.'VPT'.OR.WHCH(1:3).EQ.'vpt') THEN
         RVAL=YVPT
      ELSE IF (WHCH(1:3).EQ.'XMN'.OR.WHCH(1:3).EQ.'xmn') THEN
         RVAL=WXMN
      ELSE IF (WHCH(1:3).EQ.'XMX'.OR.WHCH(1:3).EQ.'xmx') THEN
         RVAL=WXMX
      ELSE IF (WHCH(1:3).EQ.'YMN'.OR.WHCH(1:3).EQ.'ymn') THEN
         RVAL=WYMN
      ELSE IF (WHCH(1:3).EQ.'YMX'.OR.WHCH(1:3).EQ.'ymx') THEN
         RVAL=WYMX
      ELSE IF (WHCH(1:3).EQ.'XLV'.OR.WHCH(1:3).EQ.'xlv') THEN
         RVAL=XLOV
      ELSE IF (WHCH(1:3).EQ.'XHV'.OR.WHCH(1:3).EQ.'xhv') THEN
         RVAL=XHIV
      ELSE IF (WHCH(1:3).EQ.'YLV'.OR.WHCH(1:3).EQ.'ylv') THEN
         RVAL=YLOV
      ELSE IF (WHCH(1:3).EQ.'YHV'.OR.WHCH(1:3).EQ.'yhv') THEN
         RVAL=YHIV
      ELSE IF (WHCH(1:3).EQ.'NXC'.OR. WHCH(1:3).EQ.'nxc') THEN
         RVAL=REAL(NXCT)
      ELSE IF (WHCH(1:3).EQ.'NYC'.OR. WHCH(1:3).EQ.'nyc') THEN
         RVAL=REAL(NYCT)
      ELSE IF (WHCH(1:3).EQ.'LLG'.OR. WHCH(1:3).EQ.'llg') THEN
         RVAL=REAL(LNLG)
      ELSE IF (WHCH(1:3).EQ.'IVX'.OR. WHCH(1:3).EQ.'ivx') THEN
         RVAL=REAL(INVX)
      ELSE IF (WHCH(1:3).EQ.'IVY'.OR. WHCH(1:3).EQ.'ivy') THEN
         RVAL=REAL(INVY)
      ELSE IF (WHCH(1:3).EQ.'RBG'.OR. WHCH(1:3).EQ.'rbg') THEN
         RVAL=REAL(RBIG)
      ELSE IF (WHCH(1:3).EQ.'IBG'.OR. WHCH(1:3).EQ.'ibg') THEN
         RVAL=REAL(IBIG)
C
C ---------------------------------------------------------------------
C
      ELSE
         CSTR(1:46)='STGETI OR STGETR - PARAMETER NAME NOT KNOWN - '
         CSTR(47:49)=WHCH(1:3)
         CALL SETER (CSTR(1:49),3,2)
         STOP
      END IF
C
C Done.
C
      RETURN
C
      END

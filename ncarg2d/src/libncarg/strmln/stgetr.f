C
C       $Id: stgetr.f,v 1.11 2008-07-27 00:17:28 haley Exp $
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
      SUBROUTINE STGETR (CNM,RVL)
C
      CHARACTER*(*) CNM
C
C This subroutine is called to retrieve the real value of a specified
C parameter.
C
C CNM is the name of the parameter whose value is to be retrieved.
C
C RVL is a real variable in which the desired value is to be returned
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
      PARAMETER (IPLVLS = 256)
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
     +                IPLR       ,ISST       ,
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
     +                RDFM       ,RSMD       ,RAMD       ,IGBS       ,
     +                ISTM       ,RVRL       ,RVFR       ,RVRM       ,
     +                IVPO       ,RAFR       ,RDMX       ,RDMN
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
      PARAMETER (IPNPTS = 256, IPLSTL = 750, IPGRCT = 64)
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
        CSTR(1:46)='STGETI OR STGETR - PARAMETER NAME TOO SHORT - '
        CSTR(47:46+LEN(CNM))=CNM
        CALL SETER (CSTR(1:46+LEN(CNM)),1,1)
        RETURN
      END IF
C
C Check for incorrect use of the index parameter.
C
      IF (CNM(1:3).EQ.'CLR'.OR.CNM(1:3).EQ.'clr'
     +    .OR.CNM(1:3).EQ.'TVL'.OR.CNM(1:3).EQ.'tvl') THEN
         IF (IPAI.LT.1.OR.IPAI.GT.NLVL) THEN
            CSTR(1:46)='STGETI OR STGETR - GETTING XXX - PAI INCORRECT'
            CSTR(28:30)=CNM(1:3)
            CALL SETER (CSTR(1:46),2,1)
            RETURN
         END IF
      END IF
C
C Get the appropriate parameter value.
C
C ---------------------------------------------------------------------
C
C Values in STPAR
C
      IF (CNM(1:3).EQ.'UD1'.OR. CNM(1:3).EQ.'ud1') THEN
         RVL=REAL(IUD1)
      ELSE IF (CNM(1:3).EQ.'VD1'.OR. CNM(1:3).EQ.'vd1') THEN
         RVL=REAL(IVD1)
      ELSE IF (CNM(1:3).EQ.'PD1'.OR. CNM(1:3).EQ.'pd1') THEN
         RVL=REAL(IPD1)
      ELSE IF (CNM(1:3).EQ.'XD1'.OR. CNM(1:3).EQ.'xd1') THEN
         RVL=REAL(IXD1)
      ELSE IF (CNM(1:3).EQ.'XDM'.OR. CNM(1:3).EQ.'xdm') THEN
         RVL=REAL(IXDM)
      ELSE IF (CNM(1:3).EQ.'YD1'.OR. CNM(1:3).EQ.'yd1') THEN
         RVL=REAL(IYD1)
      ELSE IF (CNM(1:3).EQ.'YDN'.OR. CNM(1:3).EQ.'ydn') THEN
         RVL=REAL(IYDN)
      ELSE IF (CNM(1:3).EQ.'WKD'.OR.CNM(1:3).EQ.'wkd') THEN
        RVL=REAL(IWKD)
      ELSE IF (CNM(1:3).EQ.'WKU'.OR.CNM(1:3).EQ.'wku') THEN
        RVL=REAL(IWKU)
      ELSE IF (CNM(1:3).EQ.'SET'.OR. CNM(1:3).EQ.'set') THEN
         RVL=REAL(ISET)
      ELSE IF (CNM(1:3).EQ.'ERR'.OR. CNM(1:3).EQ.'err') THEN
         RVL=REAL(IERR)
      ELSE IF (CNM(1:3).EQ.'XIN'.OR.CNM(1:3).EQ.'xin') THEN
        RVL=IXIN
      ELSE IF (CNM(1:3).EQ.'YIN'.OR.CNM(1:3).EQ.'yin') THEN
        RVL=IYIN
      ELSE IF (CNM(1:3).EQ.'MSK'.OR. CNM(1:3).EQ.'msk') THEN
         RVL=REAL(IMSK)
      ELSE IF (CNM(1:3).EQ.'CPM'.OR. CNM(1:3).EQ.'cpm') THEN
         RVL=REAL(ICPM)
      ELSE IF (CNM(1:3).EQ.'NLV'.OR.CNM(1:3).EQ.'nlv') THEN
        RVL=REAL(NLVL)
      ELSE IF (CNM(1:3).EQ.'PAI'.OR.CNM(1:3).EQ.'pai') THEN
        RVL=REAL(IPAI)
      ELSE IF (CNM(1:3).EQ.'CTV'.OR.CNM(1:3).EQ.'ctv') THEN
        RVL=REAL(ICTV)
      ELSE IF (CNM(1:3).EQ.'LWD'.OR.CNM(1:3).EQ.'lwd') THEN
        RVL=WDLV
      ELSE IF (CNM(1:3).EQ.'VMN'.OR.CNM(1:3).EQ.'vmn') THEN
        RVL=UVMN
      ELSE IF (CNM(1:3).EQ.'VMX'.OR.CNM(1:3).EQ.'vmx') THEN
        RVL=UVMX
      ELSE IF (CNM(1:3).EQ.'PMN'.OR.CNM(1:3).EQ.'pmn') THEN
        RVL=PMIN
      ELSE IF (CNM(1:3).EQ.'PMX'.OR.CNM(1:3).EQ.'pmx') THEN
        RVL=PMAX
      ELSE IF (CNM(1:3).EQ.'THN'.OR. CNM(1:3).EQ.'thn') THEN
         RVL=REAL(ITHN)
      ELSE IF (CNM(1:3).EQ.'PLR'.OR. CNM(1:3).EQ.'plr') THEN
         RVL=REAL(IPLR)
      ELSE IF (CNM(1:3).EQ.'SST'.OR. CNM(1:3).EQ.'sst') THEN
         RVL=REAL(ISST)
      ELSE IF (CNM(1:3).EQ.'CLR'.OR.CNM(1:3).EQ.'clr') THEN
         RVL=REAL(ICLR(IPAI))
      ELSE IF (CNM(1:3).EQ.'TVL'.OR.CNM(1:3).EQ.'tvl') THEN
         RVL=TVLU(IPAI)
C
C ---------------------------------------------------------------------
C
C Values in STTRAN
C
      ELSE IF (CNM(1:3).EQ.'VPS'.OR. CNM(1:3).EQ.'vps') THEN
         RVL=REAL(UVPS)
      ELSE IF (CNM(1:3).EQ.'VPL'.OR.CNM(1:3).EQ.'vpl') THEN
         RVL=UVPL
      ELSE IF (CNM(1:3).EQ.'VPR'.OR.CNM(1:3).EQ.'vpr') THEN
         RVL=UVPR
      ELSE IF (CNM(1:3).EQ.'VPB'.OR.CNM(1:3).EQ.'vpb') THEN
         RVL=UVPB
      ELSE IF (CNM(1:3).EQ.'VPT'.OR.CNM(1:3).EQ.'vpt') THEN
         RVL=UVPT
      ELSE IF (CNM(1:3).EQ.'WDL'.OR.CNM(1:3).EQ.'wdl') THEN
         RVL=UWDL
      ELSE IF (CNM(1:3).EQ.'WDR'.OR.CNM(1:3).EQ.'wdr') THEN
         RVL=UWDR
      ELSE IF (CNM(1:3).EQ.'WDB'.OR.CNM(1:3).EQ.'wdb') THEN
         RVL=UWDB
      ELSE IF (CNM(1:3).EQ.'WDT'.OR.CNM(1:3).EQ.'wdt') THEN
         RVL=UWDT
      ELSE IF (CNM(1:3).EQ.'XC1'.OR.CNM(1:3).EQ.'xc1') THEN
         RVL=UXC1
      ELSE IF (CNM(1:3).EQ.'XCM'.OR.CNM(1:3).EQ.'xcm') THEN
         RVL=UXCM
      ELSE IF (CNM(1:3).EQ.'YC1'.OR.CNM(1:3).EQ.'yc1') THEN
         RVL=UYC1
      ELSE IF (CNM(1:3).EQ.'YCN'.OR.CNM(1:3).EQ.'ycn') THEN
         RVL=UYCN
C
C ---------------------------------------------------------------------
C
C Values in STSTRM
C
      ELSE IF (CNM(1:3).EQ.'SGD'.OR. CNM(1:3).EQ.'sgd') THEN
         RVL=REAL(ISGD)
      ELSE IF (CNM(1:3).EQ.'AGD'.OR. CNM(1:3).EQ.'agd') THEN
         RVL=REAL(IAGD)
      ELSE IF (CNM(1:3).EQ.'ARL'.OR. CNM(1:3).EQ.'arl') THEN
         RVL=RARL
      ELSE IF (CNM(1:3).EQ.'CKP'.OR. CNM(1:3).EQ.'ckp') THEN
         RVL=REAL(ICKP)
      ELSE IF (CNM(1:3).EQ.'CKX'.OR. CNM(1:3).EQ.'ckx') THEN
         RVL=REAL(ICKX)
      ELSE IF (CNM(1:3).EQ.'TRP'.OR. CNM(1:3).EQ.'trp') THEN
         RVL=REAL(ITRP)
      ELSE IF (CNM(1:3).EQ.'CYK'.OR. CNM(1:3).EQ.'cyk') THEN
         RVL=REAL(ICYK)
      ELSE IF (CNM(1:3).EQ.'VNL'.OR. CNM(1:3).EQ.'vnl') THEN
         RVL=RVNL
      ELSE IF (CNM(1:3).EQ.'SVF'.OR. CNM(1:3).EQ.'svf') THEN
         RVL=REAL(ISVF)
      ELSE IF (CNM(1:3).EQ.'USV'.OR. CNM(1:3).EQ.'usv') THEN
         RVL=RUSV
      ELSE IF (CNM(1:3).EQ.'VSV'.OR. CNM(1:3).EQ.'vsv') THEN
         RVL=RVSV
      ELSE IF (CNM(1:3).EQ.'PSV'.OR. CNM(1:3).EQ.'psv') THEN
         RVL=RPSV
      ELSE IF (CNM(1:3).EQ.'SPC'.OR. CNM(1:3).EQ.'spc') THEN
         RVL=REAL(ISPC)
      ELSE IF (CNM(1:3).EQ.'CDS'.OR. CNM(1:3).EQ.'cds') THEN
         RVL=RCDS
      ELSE IF (CNM(1:3).EQ.'SSP'.OR. CNM(1:3).EQ.'ssp') THEN
         RVL=RSSP
      ELSE IF (CNM(1:3).EQ.'DFM'.OR. CNM(1:3).EQ.'dfm') THEN
         RVL=RDFM
      ELSE IF (CNM(1:3).EQ.'SMD'.OR. CNM(1:3).EQ.'smd') THEN
         RVL=RSMD
      ELSE IF (CNM(1:3).EQ.'AMD'.OR. CNM(1:3).EQ.'amd') THEN
         RVL=RAMD
      ELSE IF (CNM(1:3).EQ.'GBS'.OR. CNM(1:3).EQ.'gbs') THEN
         RVL=REAL(IGBS)
      ELSE IF (CNM(1:3).EQ.'STM'.OR. CNM(1:3).EQ.'stm') THEN
         RVL=REAL(ISTM)
      ELSE IF (CNM(1:3).EQ.'VRL'.OR. CNM(1:3).EQ.'vrl') THEN
         RVL=REAL(RVRL)
      ELSE IF (CNM(1:3).EQ.'VFR'.OR. CNM(1:3).EQ.'vfr') THEN
         RVL=REAL(RVFR)
      ELSE IF (CNM(1:3).EQ.'VRM'.OR. CNM(1:3).EQ.'vrm') THEN
         RVL=REAL(RVRM)
      ELSE IF (CNM(1:3).EQ.'VPO'.OR. CNM(1:3).EQ.'vpo') THEN
         RVL=REAL(IVPO)
      ELSE IF (CNM(1:3).EQ.'AFR'.OR. CNM(1:3).EQ.'afr') THEN
         RVL=REAL(RAFR)
      ELSE IF (CNM(1:3).EQ.'DMX'.OR. CNM(1:3).EQ.'dmx') THEN
         RVL=REAL(RDMX)
      ELSE IF (CNM(1:3).EQ.'DMN'.OR. CNM(1:3).EQ.'dmn') THEN
         RVL=REAL(RDMN)
C
C ---------------------------------------------------------------------
C
C Values in STTXP
C
C character attributes
C
C
      ELSE IF (CNM(1:3).EQ.'ZFS'.OR.CNM(1:3).EQ.'zfs') THEN
         RVL=FZFS
      ELSE IF (CNM(1:3).EQ.'ZFX'.OR.CNM(1:3).EQ.'zfx') THEN
         RVL=FZFX
      ELSE IF (CNM(1:3).EQ.'ZFY'.OR.CNM(1:3).EQ.'zfy') THEN
         RVL=FZFY
      ELSE IF (CNM(1:3).EQ.'ZFP'.OR. CNM(1:3).EQ.'zfp') THEN
         RVL=REAL(IZFP)
      ELSE IF (CNM(1:3).EQ.'ZFC'.OR. CNM(1:3).EQ.'zfc') THEN
         RVL=REAL(IZFC)
C
C ---------------------------------------------------------------------
C
C Values in STMAP
C
      ELSE IF (CNM(1:3).EQ.'MAP'.OR. CNM(1:3).EQ.'map') THEN
         RVL=REAL(IMAP)
      ELSE IF (CNM(1:3).EQ.'TRT'.OR. CNM(1:3).EQ.'trt') THEN
         RVL=REAL(ITRT)
      ELSE IF (CNM(1:3).EQ.'VPL'.OR.CNM(1:3).EQ.'vpl') THEN
         RVL=XVPL
      ELSE IF (CNM(1:3).EQ.'VPR'.OR.CNM(1:3).EQ.'vpr') THEN
         RVL=XVPR
      ELSE IF (CNM(1:3).EQ.'VPB'.OR.CNM(1:3).EQ.'vpb') THEN
         RVL=YVPB
      ELSE IF (CNM(1:3).EQ.'VPT'.OR.CNM(1:3).EQ.'vpt') THEN
         RVL=YVPT
      ELSE IF (CNM(1:3).EQ.'XMN'.OR.CNM(1:3).EQ.'xmn') THEN
         RVL=WXMN
      ELSE IF (CNM(1:3).EQ.'XMX'.OR.CNM(1:3).EQ.'xmx') THEN
         RVL=WXMX
      ELSE IF (CNM(1:3).EQ.'YMN'.OR.CNM(1:3).EQ.'ymn') THEN
         RVL=WYMN
      ELSE IF (CNM(1:3).EQ.'YMX'.OR.CNM(1:3).EQ.'ymx') THEN
         RVL=WYMX
      ELSE IF (CNM(1:3).EQ.'XLV'.OR.CNM(1:3).EQ.'xlv') THEN
         RVL=XLOV
      ELSE IF (CNM(1:3).EQ.'XHV'.OR.CNM(1:3).EQ.'xhv') THEN
         RVL=XHIV
      ELSE IF (CNM(1:3).EQ.'YLV'.OR.CNM(1:3).EQ.'ylv') THEN
         RVL=YLOV
      ELSE IF (CNM(1:3).EQ.'YHV'.OR.CNM(1:3).EQ.'yhv') THEN
         RVL=YHIV
      ELSE IF (CNM(1:3).EQ.'NXC'.OR. CNM(1:3).EQ.'nxc') THEN
         RVL=REAL(NXCT)
      ELSE IF (CNM(1:3).EQ.'NYC'.OR. CNM(1:3).EQ.'nyc') THEN
         RVL=REAL(NYCT)
      ELSE IF (CNM(1:3).EQ.'LLG'.OR. CNM(1:3).EQ.'llg') THEN
         RVL=REAL(LNLG)
      ELSE IF (CNM(1:3).EQ.'IVX'.OR. CNM(1:3).EQ.'ivx') THEN
         RVL=REAL(INVX)
      ELSE IF (CNM(1:3).EQ.'IVY'.OR. CNM(1:3).EQ.'ivy') THEN
         RVL=REAL(INVY)
      ELSE IF (CNM(1:3).EQ.'RBG'.OR. CNM(1:3).EQ.'rbg') THEN
         RVL=REAL(RBIG)
      ELSE IF (CNM(1:3).EQ.'IBG'.OR. CNM(1:3).EQ.'ibg') THEN
         RVL=REAL(IBIG)
C
C ---------------------------------------------------------------------
C
      ELSE
         CSTR(1:46)='STGETI OR STGETR - PARAMETER NAME NOT KNOWN - '
         CSTR(47:49)=CNM(1:3)
         CALL SETER (CSTR(1:49),3,1)
         RETURN
      END IF
C
C Done.
C
      RETURN
C
      END

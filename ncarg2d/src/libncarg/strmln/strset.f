C
C	$Id: strset.f,v 1.1 1993-01-15 23:53:42 dbrown Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE STRSET
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
     +                IXM1       ,IYM1       ,IXM2       ,IYM2
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
     +                UXC1       ,UXCM       ,UYC1       ,UYCM 
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
C
      COMMON / STTXP /
     +                FCWM    ,ICSZ    ,
     +                FMNS    ,FMNX    ,FMNY    ,IMNP    ,IMNC  ,
     +                FMXS    ,FMXX    ,FMXY    ,IMXP    ,IMXC  ,
     +                FZFS    ,FZFX    ,FZFY    ,IZFP    ,IZFC  ,
     +                FILS    ,FILX    ,FILY    ,IILP     IILC 
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
C Reset individual parameters.
C
C Common block STPAR
C
      IUD1 = -1
      IVD1 = -1
      IPD1 = -1
      IXD1 = 1
      IXDM = -1
      IYD1 = 1
      IYDN = -1
      IWKD = -1
      IWKU = 0
      ISET = 0
      IERR = 0
      IXIN = 1
      IYIN = 1
      IMSK = 0
      ICPM = 0
      NLVL = 0
      IPAI = 1
      ICTV = 0
      WDLV = 1.0
      UVMN = 0.0
      UVMX = 0.0
      PMIN = 0.0
      PMAX = 0.0
      ITHN = 0
      IMAP = 0
      IPLR = 0
      ISST = 0
C
C Parameter arrays
C
      DO 101 I=1,IPLVLS,1
         ICLR(I) = 1
         TVLU(I) = 0.0
 101  CONTINUE
C
C
C ---------------------------------------------------------------------
C
C STTRAN
C
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
      UYCM = 0.0
C
C ---------------------------------------------------------------------
C
C STSTRM
C
      ISGD = 2
      IAGD = 2
      RARL = 0.015
      ICKP = 35
      ICKX = -99
      ITRP = 0
      ICYK = 0
      RVNL = 0.33
      ISVF = 0
      RUSV = 1.0E12
      RVSV = 1.0E12
      RPSV = 1.0E12
      ISPC = -1
      RCDS = 2.0
      RSSP = 0.02
      RDFM = 0.02
C
C ---------------------------------------------------------------------
C
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
C STCHAR values
C
      CMNT = 'Minimum Speed'
      CMXT = 'Maximum Speed'
      CZFT = 'ZERO FIELD'
      CILT = 'Streamlines' 
C
C ---------------------------------------------------------------------
C
C STMAP values
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

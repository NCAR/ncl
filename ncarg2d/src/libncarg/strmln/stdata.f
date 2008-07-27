C
C       $Id: stdata.f,v 1.15 2008-07-27 00:17:27 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE STDATA
C
C Calling this do-nothing subroutine forces "ld" to load the following
C block data routine (but only if they are in the same ".f" file).
C
        RETURN
C
      END
CNOSPLIT
      BLOCKDATA STDATAX
C
C This routine defines the default values of the Streamline parameters.
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
C Old STRMLN interface common blocks
C
      COMMON /STR02/  EXT , SIDE , XLT , YBT
C
      COMMON /STR03/  INITA , INITB , AROWL , ITERP , ITERC , IGFLG
     +             ,  IMSG , UVMSG , ICYC , DISPL , DISPC , CSTOP
C
C ---------------------------------------------------------------------
C
C Initialization of STPAR
C
C IUD1 -- 'UD1' -- First dimension of U
C
      DATA     IUD1 / -1 /
C
C IVD1 -- 'VD1' -- First dimension of V
C
      DATA     IVD1 / -1 /
C
C IPD1 -- 'PD1' -- First dimension of P
C
      DATA     IPD1 / -1 /
C
C IXD1 -- 'XD1' -- Array index for start of data, first dimension
C
      DATA     IXD1 / 1 /
C
C IXDM -- 'XDM' -- Array index for end of data, first dimension
C
      DATA     IXDM / -1 /
C
C IYD1 -- 'YD1' -- Array index for start of data, second dimension
C
      DATA     IYD1 / 1 /
C   
C IYDN -- 'YDN' -- Array index for end of data, second dimension
C
      DATA     IYDN / -1 /
C
C IWKD -- 'WKD' -- Dimension of work array
C
      DATA     IWKD / -1 /
C
C IWKU -- 'WKU' -- Amount of work array actually used (read-only)
C
      DATA     IWKU / 0 /
C
C ISET -- 'SET' -- The Set call flag - Old NSET parameter
C
      DATA     ISET / 1 /
C
C IERR -- 'ERR' -- Error code set by STRMLN (read-only)
C                  -101 - Cyclic flag set for non-cyclic data
C
      DATA     IERR / 0 /
C
C
C IXIN -- 'XIN' -- The X Axis grid increment, must be > 0
C IYIN -- 'YIN' -- The Y Axis grid increment, must be > 0
C
      DATA IXIN / 1 /
      DATA IYIN / 1 /
C
C IXM1 -- (IXDM - 1) (not user accessible)
C IXM2 -- (IXDM - 2) (not user accessible)
C IYM1 -- (IYDN - 1) (not user accessible)
C IYM2 -- (IYDN - 2) (not user accessible)
C
C IMSK -- 'MSK' -- Mask streamlines to an area map: <1 -- no mapping,
C                  >=1 - mapping;
C
      DATA IMSK / 0 /
C
C ICPM -- 'CPM' -- the compatibility mode. If >0 the FX,FY,
C                  functions are used. Additionally, when
C                  used in conjunction with the STRMLN routine, 
C                  has a meaningful range from -4 to +4 inclusive,
C                  where various combinations are allowed to use or
C                  ignore 1) the optional input parameters to
C                  VELVCT, 2) the data in STR01,STR02,STR03,STR04
C                  common, 3) FX, etc routines, as follows:
C
C                  -4: no FX, ignore params, ignore old common data
C                  -3: no FX, ignore params, use old common data
C                  -2: no FX, use params, ignore old common data
C                  -1: no FX, use params, use old common data
C                   0: default, same as -4 if STINIT,STREAM called,
C                      same as +1 if STRMLN or EZSTRM called
C                  +1: FX, use params, use old common data
C                  +2: FX, use params, ignore old common data
C                  +3: FX, ignore params, use old common data
C                  +4: FX, ignore params, ignore old common data
C
C                  FX means using FX,FY
C                  When parameters and common block values are
C                  used they override any values set using the
C                  STSETx routines
C
      DATA ICPM / 0 /
C
C NLVL -- 'NLV' -- number of distinct colors to use for the
C                    independent variable mapping -- cannot exceed
C                    IPLVLS -- default: 16
C                    
      DATA  NLVL /  0 /
C
C IPAI -- 'PAI' -- the current level -- must be set before 
C                   modifying an internal level array value
C
      DATA   IPAI /   1     /
C
C ICTV -- 'CTV' -- compute thresholds flag:
C                  -2: color by scalar value, user sets TVL
C                  -1: color by vector magnitude, user sets TVL
C                   0: no coloring
C                  +1: color by magnitude, STINIT sets TVL
C                  +2: color by scalar array, STINIT sets TVL
C
      DATA  ICTV /   0     /
C
C WDLV -- 'LWD' -- the width of a streamline
C 
      DATA  WDLV /   1.0   /
C
C UVMN -- 'VMN' -- the minimum displayed vector magnitude, read-only
C UVMX -- 'VMX' -- the maximum displayed vector magnitude, read-only
C PMIN -- 'PMN' -- the minimum scalar array value, read-only
C PMAX -- 'PMX' -- the maximum scalar array value, read-only
C
      DATA UVMN / 0.0 /
      DATA UVMX / 0.0 /
      DATA PMIN / 0.0 /
      DATA PMAX / 0.0 /
C
C IPLR -- 'PLR' -- Polar coordinates for UV array flag
C
      DATA IPLR / 0 /
C
C ISST -- 'SST' -- Streamline statistics flag
C
      DATA ISST / 0 /
C
C ICLR -- 'CLR' -- the GKS color index value
C
      DATA  ICLR / IPLVLS * 1 /
C
C TVLU -- 'TVL' -- the list of threshold values
C
      DATA  TVLU / IPLVLS * 0.0 /
C
C End of STPAR intialization
C
C --------------------------------------------------------------------
C
C STTRAN initialization 
C
C User coordinate system to viewport, UV array to user coordinates
C
C UVPS -- 'VPS' -- The viewport mode
C
      DATA UVPS / 0.25 /
C
C UVPL -- 'VPL' -- Viewport left
C
      DATA UVPL / 0.05 /
C
C UVPR -- 'VPR' -- Viewport right
C
      DATA UVPR / 0.95 /
C
C UVPB -- 'VPB' -- Viewport bottom
C
      DATA UVPB / 0.05 /
C
C UVPT -- 'VPT' -- Viewport top
C
      DATA UVPT / 0.95 /
C
C UWDL -- 'WDL' -- Window left
C
      DATA UWDL / 0.0 /
C
C UWDR -- 'WDR' -- Window right
C
      DATA UWDR / 0.0 /
C
C UWDB -- 'WDB' -- Window bottom
C
      DATA UWDB / 0.0 /
C
C UWDT -- 'WDT' -- Window top
C
      DATA UWDT / 0.0 /
C
C UXC1 -- 'XC1' -- minimum X coord
C
      DATA UXC1 / 0.0 /
C
C UXCM -- 'XCM' -- maximum Y coord
C
      DATA UXCM / 0.0 /
C
C UYC1 -- 'YC1' -- minimum Y coord
C
      DATA UYC1 / 0.0 /
C
C UYCN -- 'YCN' -- maximum Y coord
C
      DATA UYCN / 0.0 /
C
C End of STTRAN
C ----------------------------------------------------------------------
C
C STSTRM - Parameters affecting the stream processing algorithm
C
C ISGD -- 'SGD' - Stream starting grid increment (INITA)
C
      DATA ISGD / 2 /
C
C IAGD -- 'AGD' - Arrow placement grid increment (INITB)
C
      DATA IAGD / 2 /
C
C RARL -- 'ARL' - Length of one side of arrow as fraction 
C                 of the viewport width (replaces AROWL)
C
      DATA RARL / 0.012 /
C
C ICKP -- 'CKP' - Check progress after this many iterations (ITERP)
C
      DATA ICKP / 35 /
C
C ICKX -- 'CKX' - Check streamline crossover after this many 
C                 iterations (ITERC). (If negative crossover is 
C                 checked at each entrance to a new grid cell)
C
      DATA ICKX / -99 /
C
C ITRP -- 'TRP' - Interpolaton method (IGFLG)
C                 0 - Use 16 point bessel where possible
C                 non 0 - use bi-linear interpolation everywhere
C
      DATA ITRP / 0 /
C
C ICYK -- 'CYK' - Cyclical data flag (ICYC) If non-zero, instructs
C                 the utility to use cyclic interpolation formulas.
C                 If set and data is non-cyclic the error flag is set.
C
      DATA ICYK / 0 /
C
C RVNL -- 'VNL' - Normalization factor for the differential magnitude.
C                 This controls number of steps in compatibility mode
C                 only when the FX,FY mapping routines are used. See 
C                 parameter 'DFM' for step control when STMPXY and
C                 associated routines are used
C
      DATA RVNL / 0.33 /
C
C ISVF -- 'SVF' - Special value flag  (IMSG)
C                 0 - no special values
C                 non 0 - there may be special values, use only
C                         bi-linear interpolation
      DATA ISVF / 0 /
C
C RUSV -- 'USV' -- The U array special value (UVMSG)
C
      DATA RUSV / 1.0E12 /
C
C RVSV -- 'VSV' -- The V array special value (UVMSG)
C
      DATA RVSV / 1.0E12 /
C
C RNDA -- assigned the NDC value of the arrow size.
C
C ISPC -- 'SPC' -- Special color -- 
C                      < 0: no P special value
C                      >= 0: draw P special values using color SPC
C
      DATA ISPC / -1 /
C
C RPSV -- 'PSV' -- The P array special value
C 
      DATA RPSV / 1.0E12 /
C
C RCDS -- 'CDS' - The critical displacement as a multiple of 'DFM'.
C                 Replaces DISPC. If the streamline has not moved
C                 CDS*DFM units in NDC space after ICKP iterations,
C                 the streamline is terminated
C
      DATA RCDS / 2.0 /
C
C RSSP -- 'SSP' - Stream spacing value as a fraction of the viewport
C                 width; replaces CSTOP. Checked when a new grid box is
C                 entered.
C
      DATA RSSP / 0.015 /
C
C RDFM -- 'DFM' - Differential magnitude as a fraction of the viewport
C                 width. Smaller values result in more steps and a more
C                 accurate approximation of the streamline.
C
      DATA RDFM / 0.02 /
C
C RSMD -- 'SMD' - Streamline minimum starting distance as fraction
C                 of viewport width.
C
      DATA RSMD / 0.0 /
C
C RAMD -- 'AMD' - Arrow minimum distance as a fraction of the 
C                 viewport width.
C
      DATA RAMD / 0.0 /
C
C IGBS -- 'GBS' - Grid based spacing flag
C
      DATA IGBS / 0 /
C
C ISTM -- 'STM' - Streamline mode (0 - normal, 1 - curly vectors)
C
      DATA ISTM / 0 /
C
C RVRL -- 'VRL' - Curly vector mode only: Vector reference length
C                 (if 0.0 streamlines selects the value) -- as
C                 fraction of viewport width.
C
      DATA RVRL / 0.0 /
C
C RVFR -- 'VFR' - Curly vector mode only: Fraction of the maximum 
C                 length used to represent the minimum length vector. 
C                 1.0 causes all vectors to become maximum length. 
C                 0.0 means the minimum length is linearly scaled 
C                 from the maximum length.
C
      DATA RVFR / 0.0 /
C
C RVRM -- 'VRM' - Curly vector mode only: Reference magnitude - 
C                 the magnitude represented by the reference length.
C                 If 0.0, the maximum magnitude is used as the 
C                 reference magnitude.
C
      DATA RVRM / 0.0 /
C
C IVPO -- 'VPO' -- Curly vector mode only: 
C                  vector position flag: < 0 - head at position,
C                                          0 - center at position, 
C                                        > 0 - tail at position
C
      DATA IVPO / 0 /
C
C RAFR -- 'AFR' - Curly vector mode only: fraction of full size
C                 arrow length used for minimum arrow; others are
C                 proportionally scaled. If 0.0 scaling is strictly
C                 proportional
C
      DATA RAFR / 1.0 /
C
C RDMX -- 'DMX' - Curly vector mode only: (read only)
C                 NDC length of maximum vector
C
      DATA RDMX / 0.0 /
C
C RDMN -- 'DMN' - Curly vector mode only: (read only)
C                 NDC length of minimum vector
C
      DATA RDMN / 0.0 /
C
C End of STSTRM
C --------------------------------------------------------------------
C
C STTXP - Text parameters 
C
C ICCM -- internal - maximum length of character strings
C
      DATA ICSZ / IPCHSZ /
C
C FZFS -- 'ZFS' -- size of text for zero field string as FVPW
C FZFX -- 'ZFX' -- X position of zero field string as FVPW
C FZFY -- 'ZFY' -- Y position of zero field string as FVPW
C IZFP -- 'ZFP' -- zero field string position flag
C IZFC -- 'ZFC' -- color of text for zero field label
C 
      DATA FZFS / 0.033 /
      DATA FZFX / 0.5 /
      DATA FZFY / 0.5 /
      DATA IZFP / 0 /
      DATA IZFC / -1 /
C
C ---------------------------------------------------------------------
C
C Beginning of STCHAR initialization
C
      DATA CZFT / 'ZERO FIELD' /
C
C End of STCHAR initialization
C
C
C ---------------------------------------------------------------------
C
C STMAP initialization
C
C IMAP -- 'MAP' -- the mapping transformation to use
C
      DATA  IMAP / 0 /
C
C ITRT -- 'TRT' -- Transform type flag: 
C                      0  - transform position only
C                      1  - transform position and angle
C                     -1  - transform position, angle, and magnitude
C
      DATA ITRT / 1 /
C
C XVPL,XVPT,YVPB,YVPT -- the viewport values (NDC boundaries)
C
C WXMN,WXMX,WYMN,WYMX -- the window minimum and maximum values
C                        (User coordinate space)
C
C XLOV,XHIV,YLOV,YHIV -- the mapped array endpoint values
C                        (Data coordinate space)
C
C XGDS,YGDS -- size in data coordinates of a grid box
C
C NXCT,NYCT -- number of points in X and Y used for the plot
C
C DFMG -- The magnitude of the diffential increment in NDC space
C
C LNLG -- the log scale mapping flag from SET call
C
C INVX,INVY -- inverse flags for the window boundaries
C
C IWCT - unused
C
C FW2W,FH2H -- fraction of viewport to fraction of viewspace
C
C RBIG,IBIG -- maximum expressible real and integer values
C
C ---------------------------------------------------------------------
C
C STRMLN compatibility common blocks
C
C Beginning of STR02 initialization
C
      DATA EXT  / 0.25 /
      DATA SIDE / 0.90  /
      DATA XLT  / 0.05 /
      DATA YBT  / 0.05 /
C
C End of STR02 initialization
C
C Beginning of STR03 initialization
C
      DATA INITA  / 2 /
      DATA INITB  / 2  /
      DATA AROWL  / 0.33 /
      DATA ITERP  / 35 /
      DATA ITERC  / -99 /
      DATA IGFLG  / 0 /
      DATA ICYC   / 0 /
      DATA IMSG   / 0 /
      DATA UVMSG  / 1.E+36 /
      DATA DISPL  / 0.33 /
      DATA DISPC  / 0.67 /
      DATA CSTOP  / 0.50 /
C
C End of STR03 initialization
C
      END

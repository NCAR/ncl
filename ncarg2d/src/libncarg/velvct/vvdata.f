C
C       $Id: vvdata.f,v 1.15 2008-07-27 00:17:35 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE VVDATA
C
C Calling this do-nothing subroutine forces "ld" to load the following
C block data routine (but only if they are in the same ".f" file).
C
        RETURN
C
      END
CNOSPLIT
      BLOCKDATA VVDATAX
C
C This 'routine' sets the default values of the VVECTR parameters.
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
C *********************************************************************
C 
C Initialization of VVCOM
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
C IXDM -- 'XDM' -- First dimension of data for U,V,P
C
      DATA     IXDM / -1 /
C   
C IYDN -- 'YDN' -- Second dimension of data for U,V,P
C
      DATA     IYDN / -1 /
C
C VLOM -- 'VLC' -- Vector low cutoff  -- minimum size vector to plot
C                  Old FLO parameter
C
      DATA     VLOM / 0.0 /
C
C VHIM -- 'VHC' -- Vector high cutoff -- maximum size vector to plot
C                  Old HI parameter
C
      DATA     VHIM / 0.0 /
C
C ISET -- 'SET' -- The Set call flag - Old NSET parameter
C
      DATA     ISET / 1 /
C
C VRMG -- 'VRM' -- Reference magnitude - the magnitude represented by
C                  the reference length. If 0.0, the maximum magnitude
C                  is used as the reference magnitude
C
      DATA     VRMG / 0.0 /
C
C
C VRLN -- 'VRL' -- Old LENGTH parameter (but stored as a fraction of
C                  the viewport width). 
C                  0.0 causes VVINIT to select a value.
C
      DATA     VRLN / 0.0 /
C
C VFRC -- 'VFR' -- Fraction of the maximum length used to represent
C                  the minimum length vector. 1.0 causes all vectors to
C                  become maximum length. 0.0 means the minimum length 
C                  is linearly scaled from the maximum length (as now),
C
      DATA     VFRC / 0.0 /
C
C IXIN -- 'XIN' -- The X Axis grid increment, must be > 0
C IYIN -- 'YIN' -- The Y Axis grid increment, must be > 0
C
      DATA IXIN / 1 /
      DATA IYIN / 1 /
C
C
C ISVF -- 'SVF' -- Special value flag
C                  Old ISPV parameter
C
      DATA ISVF / 0 /
C
C UUSV -- 'USV' -- The U array special value 
C                  Old SPV(1)
C
      DATA UUSV / 1.0E12 /
C
C UVSV -- 'VSV' -- The V array special value
C                  Old SPV(2)
C
      DATA UVSV / 1.0E12 /
C
C UPSV -- 'PSV' -- The P array special value
C 
      DATA UPSV / 1.0E12 /
C
C IMSK -- 'MSK' -- Mask the vectors to an area map: <1 -- no mapping,
C                  1 - high precision mapping;>1 low precision mapping
C
      DATA IMSK / 0 /
C
C ICPM -- 'CPM' -- the compatibility mode. If 0, the default, the
C                  the behavior depends on whether the old routines
C                  or the new routines are called. If >0 the FX,FY,
C                  MXF,MYF functions are used. Additionally, when
C                  used in conjunction with the VELVCT routine, 
C                  has a meaningful range from -4 to +4 inclusive,
C                  where various combinations are allowed to use or
C                  ignore 1) the optional input parameters to
C                  VELVCT, 2) the data in VEC1 and VEC2 common
C                  3) FX, etc routines, as follows:
C
C                  -4: no FX, ignore params, ignore VEC1,VEC2
C                  -3: no FX, ignore params, use VEC1,VEC2
C                  -2: no FX, use params, ignore VEC1,VEC2
C                  -1: no FX, use params, use VEC1,VEC2
C                   0: default: same as -4 if VVINIT,VVECTR called,
C                      same as +1 if VELVCT,EZVEC, or VELVEC called.
C                  +1: FX, use params, use VEC1,VEC2
C                  +2: FX, use params, ignore VEC1,VEC2
C                  +3: FX, ignore params, use VEC1,VEC2
C                  +4: FX, ignore params, ignore VEC1,VEC2
C
C                  FX means using FX,FY,MXF,and MYF
C                  Params include FLO,HI,NSET,LENGTH,ISPV,SPV.
C                  VEC1,VEC2 data includes all settable values
C                  from these two common blocks.
C                  When parameters and common block values are
C                  used they override anything done using the
C                  vvset routines
C
      DATA ICPM / 0 /
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
C NLVL -- 'NLV' -- number of distinct colors to use for the
C                    independent variable mapping -- cannot exceed
C                    IPLVLS, number of elements used in CLR and TVL
C                    
      DATA  NLVL /  0 /
C
C IPAI -- 'PAI' -- the current level -- must be set before 
C                   modifying an internal level array value
C
      DATA   IPAI /   1     /
C
C ICTV -- 'CTV' -- compute thresholds flag:
C                  -2: color vectors by scalar value, user sets TVL
C                  -1: color vectors by vector magnitude, user sets TVL
C                   0: no vector coloring
C                  +1: color vectors by magnitude, VVINIT sets TVL
C                  +2: color vectors by scalar array, VVINIT sets TVL
C
      DATA  ICTV /   0     /
C
C WDLV -- 'LWD' -- the width of a vector line
C 
      DATA  WDLV /   1.0   /
C
C UVMN -- the minimum vector magnitude
C UVMX -- the maximum vector magnitude
C PMIN -- 'PMN' -- the minimum scalar array value, read-only
C PMAX -- 'PMX' -- the maximum scalar array value, read-only
C
      DATA UVMN / 0.0 /
      DATA UVMX / 0.0 /
      DATA PMIN / 0.0 /
      DATA PMAX / 0.0 /
C
C RVMN -- 'VMN' -- the minimum vector magnitude, read-only
C RVMX -- 'VMX' -- the maximum vector magnitude, read-only
C RDMN -- 'DMN' -- the minimum vector in NDC, read-only
C RDMX -- 'DMX' -- the maximum vector in NDC, read-only
C
      DATA RVMN / 0.0 /
      DATA RVMX / 0.0 /
      DATA RDMN / 0.0 /
      DATA RDMX / 0.0 /
C
C ISPC -- 'SPC' -- Special color -- 
C                      < 0: no P special value
C                      = 0: don't draw vector that has a P spec val
C                      > 0: draw P special values using color SPC
C
C     DATA ISPC / -1 /
C
C RVMD -- 'VMD' -- vector minimum distance
C
      DATA RVMD / 0.0 /
C
C IPLR -- 'PLR' -- Polar coordinates for UV array flag
C
      DATA IPLR / 0 /
C
C IVST -- 'VST' -- Vector statistics flag
C
      DATA IVST / 0 /
C
C
C IVPO -- 'VPO' -- vector position flag: < 0 - head at position,
C               0 - center at position,  > 0 - tail at position
C
      DATA IVPO / 0 /
C
C
C ILBL -- 'LBL' -- Vector labelling flag: 0 - don't, 1 - do
C
      DATA ILBL / 0 /
C
C IDPF -- 'DPF' -- decimal point flag for vector label strings
C               1 - include decimal point, non-zero don't
C
      DATA IDPF / 1 /
C
C ICLR -- 'CLR' -- the GKS color index value
C
      DATA  ICLR / IPLVLS * 1 /
C
C TVLU -- 'TVL' -- the list of threshold values
C
      DATA  TVLU / IPLVLS * 0.0 /
C
C End of VVCOM intialization
C
C --------------------------------------------------------------------
C
C VVARO -Arrow size/shape parameters
C
C HDSZ - head size as fraction of arrow length
C
      DATA HDSZ / 0.25 /
C
C HINF - interior point of head as fraction of head size
C
      DATA HINF / 0.5 /
C
C HANG - half angle of head
C
      DATA HANG / 22.5 /
C
C FAMN -- 'AMN' -- arrow head min size as FVPW 
C                                        (fraction of viewport width)
C
      DATA FAMN / 0.005 /
C
C FAMX -- 'AMX' -- arrow head max size as FVPW
C
      DATA FAMX / 0.05 /
C
C IAST -- 'AST' -- arrow style (currently line or filled)
C
      DATA IAST / 0 /
C
C FAIR -- 'AIR' -- arrow interior position (reference)
C
      DATA FAIR / 0.33 /
C
C FAWR -- 'AWR' -- arrow width (reference)
C
      DATA FAWR / 0.1 /
C
C FAWF -- 'AWF' -- arrow width (minimum)
C
      DATA FAWF / 0.25 /
C
C FAXR -- 'AXR' -- arrowhead X-coord length (reference)
C
      DATA FAXR / 0.36 /
C
C FAXF -- 'AXF' -- arrowhead X-coord length (minimum)
C
      DATA FAXF / 0.25 /
C
C FAYR -- 'AYR' -- arrowhead Y-coord length (reference)
C
      DATA FAYR / 0.12 /
C
C FAYF -- 'AYF' -- arrowhead Y-coord length (minimum)
C
      DATA FAYF / 0.25 /
C
C IACM -- 'ACM' -- arrow color mode 
C                  (ignored if AST = 0)
C                 (if CTV is 0, all lines and fill become mono;
C                  only -2, -1, and 0 will be distinguishable states)
C                 -2 multi-fill, no-line
C                 -1 no-fill, multi-line
C                  0 multi-fill, mono-line
C                  1 mono-fill, multi-line
C                  2 multi-fill, multi-line
C
C
      DATA IACM / 0 /
C
C IAFO -- 'AFO' -- arrow fill over (arrow lines)
C
      DATA IAFO / 1 /
C
C WBAD -- 'WBA' -- wind barb angle
C
      DATA WBAD / 62.0 /
C
C WBTF -- 'WBT' -- wind barb tic size (fraction of wb length)
C
      DATA WBTF / 0.33 /
C
C WBCF -- 'WBC' -- wind barb zero circle size (fraction of wb length)
C
      DATA WBCF / 0.25 /
C
C WBDF -- 'WBD' -- wind barb distance (spacing beteen ticks as
C                                      fraction of wb length)
C
      DATA WBDF / 0.1 /
C
C WBSC -- 'WBS' -- wind barb scale factor
C
      DATA WBSC / 1.0 /
C
C
C --------------------------------------------------------------------
C
C VVTXP - Text parameters 
C
C FCWM -- 'CWM' -- character width multiple, scale factor for all 
C                  text put out by the VELVCT routine
C
      DATA FCWM / 1.0 /
C
C ICCM -- internal - maximum length of character strings
C
      DATA ICSZ / IPCHSZ /
C
C FMNS -- 'MNS' -- size of text for minimum vector string as FVPW
C FMNX -- 'MNX' -- X position of minimum vector string as FVPW
C FMNY -- 'MNY' -- Y position of minimum vector string as FVPW
C IMNP -- 'MNP' -- minimum vector string position flag
C IMNC -- 'MNC' -- color of text for minimum vector label
C 
      DATA FMNS / 0.0075 /
      DATA FMNX / 0.475 /
      DATA FMNY / -0.01 /
      DATA IMNP / 4 /
      DATA IMNC / -1 /
C
C FMXS -- 'MXS' -- size of text for maximum vector string as FVPW
C FMXX -- 'MXX' -- X position of maximum vector string as FVPW
C FMXY -- 'MXY' -- Y position of maximum vector string as FVPW
C IMXP -- 'MXP' -- maximum vector string position flag
C IMNC -- 'MXC' -- color of text for maximum vector label
C 
      DATA FMXS / 0.0075 /
      DATA FMXX / 0.525 /
      DATA FMXY / -0.01 /
      DATA IMXP / 2 /
      DATA IMXC / -1 /
C
C FZFS -- 'ZFS' -- size of text for zero field string as FVPW
C FZFX -- 'ZFX' -- X position of zero field string as FVPW
C FZFY -- 'ZFY' -- Y position of zero field string as FVPW
C IZFP -- 'ZFP' -- zero field string position flag
C IMNC -- 'ZFC' -- color of text for zero field label
C 
      DATA FZFS / 0.033 /
      DATA FZFX / 0.5 /
      DATA FZFY / 0.5 /
      DATA IZFP / 0 /
      DATA IZFC / -1 /
C
C FLBS -- 'LBS' -- vector label text size as FVPW
C IMNC -- 'LBC' -- color of text for vector labels
C 
      DATA FLBS / 0.007 /
      DATA ILBC / -1 /
C
C The informational label has not yet been implemented
C FILS -- 'ILS' -- size of text for informational label string as FVPW
C FILX -- 'ILX' -- X position of informational label string as FVPW
C FILY -- 'ILY' -- Y position of informational label string as FVPW
C IILP -- 'ILP' -- informational label string position flag
C IILC -- 'ILC' -- color of text for informational label
C 
      DATA FILS / 0.05 /
      DATA FILX / 0.0 /
      DATA FILY / 0.0 /
      DATA IILP / 0 /
      DATA IILC / -1 /
C
C ---------------------------------------------------------------------
C
C Beginning of VVCHAR initialization
C
      DATA CMNT / 'Minimum Vector' /
      DATA CMXT / 'Maximum Vector' /
      DATA CZFT / 'ZERO FIELD' /
      DATA CILT / 'Vector Field Map' /
C
C End of VVCHAR initialization
C
C *********************************************************************
C 
C VVMAP initialization
C
C RLEN -- 'RML' -- Vector max length in WC
C
      DATA RLEN / 0.0 /
C
C ITRT -- 'TRT' -- Transform type flag: 
C                      0  - transform position only
C                      1  - transform position and angle
C                      -1 - transform position, angle, and magnitude
C
      DATA ITRT / 1 /
C
C DVMN -- the minimum vector size in NDC
C DVMX -- the maximum vector size in NDC
C
      DATA DVMN / 0.0 /
      DATA DVMX / 0.0 /
C
C IMAP -- 'MAP' -- the mapping transformation to use
C
      DATA  IMAP / 0 /
C
C XVPL,XVPT,YVPB,YVPT -- the viewport values
C
C WXMN,WXMX,WYMN,WYMX -- the window minimum and maximum values
C
C XLOV,XHIV,YLOV,YHIV -- the mapped array endpoint values
C
C SXDC,SYDC -- scale values for the calculated vector endpoint
C
C NXCT,NYCT -- number of points in X and Y used for the plot
C
C RLEN -- maximum vector length in window coordinates
C
C LNLG -- the log scale mapping flag from SET call
C
C INVX,INVY -- inverse flags for the window boundaries
C
C *********************************************************************
C
C REVISION HISTORY ----------------------------------------------------
C
C FEBRUARY, 1979   ADDED REVISION HISTORY
C                  MODIFIED CODE TO CONFORM TO FORTRAN 66 STANDARD
C
C JULY, 1979       FIXED HI VECTOR TRAP AND MESSAGE INDICATING
C                  MAXIMUM VECTOR PLOTTED.
C
C DECEMBER, 1979   CHANGED THE STATISTICS CALL FROM CRAYLIB TO NSSL
C
C MARCH, 1981      FIXED SOME FRINGE-CASE ERRORS, CHANGED THE CODE TO
C                  USE FL2INTT AND PLOTIT INSTEAD OF MXMY, FRSTPT, AND
C                  VECTOR, AND MADE THE ARROWHEADS NARROWER (45 DEGREES
C                  APART, RATHER THAN 60 DEGREES APART)
C
C FEBRUARY, 1984   PROVIDED A DIMENSION STATEMENT FOR A VARIABLE INTO
C                  WHICH A TEN-CHARACTER STRING WAS BEING ENCODED.  ON
C                  THE CRAY, WHEN THE ENCODE WAS DONE, A WORD FOLLOWING
C                  THE VARIABLE WAS CLOBBERED, BUT THIS APPARENTLY MADE
C                  NO DIFFERENCE.  ON AT LEAST ONE OTHER MACHINE, THE
C                  CODE BLEW UP.  (ERROR REPORTED BY GREG WOODS)
C
C JULY, 1984       CONVERTED TO FORTRAN77 AND GKS.
C
C MARCH, 1990      CORRECTED THE USE OF SET CALLS.
C
C ---------------------------------------------------------------------
      END

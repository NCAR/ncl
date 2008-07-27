C
C       $Id: stream.f,v 1.15 2008-07-27 00:17:28 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE STREAM (U,V,P,IAM,STUMSL,WRK)
C
      DIMENSION  U(IUD1,*), V(IVD1,*), P(IPD1,*), IAM(*), WRK(*)
C
      EXTERNAL STUMSL
C
C Input parameters:
C
C U,V    - arrays containing vector field data
C P      - 2-d scalar data array. (dummy - not implemented yet)
C IAM    - An area map array, may be dummied if 'MSK' is zero
C STUMSL - User modifiable masked drawing function; also may
C          be dummied if 'MSK is zero
C WRK    - workspace 
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
C -----------------------------------------------------------------
C
C Check for valid area map and area group overflow if masking is enabled
C
      IF (IMSK.GT.0) THEN
         IF (IAM(7).GT.IPGRCT) THEN
            CSTR(1:29)='STREAM - TOO MANY AREA GROUPS'
            CALL SETER (CSTR(1:29),1,1)
            RETURN
         END IF
         IF (IAM(7).LE.0) THEN
            CSTR(1:25)='STREAM - INVALID AREA MAP'
            CALL SETER (CSTR(1:29),2,1)
            RETURN
         END IF
      END IF
C
C Save the line color, text color and linewidth.
C Then set up the new linewidth values
C 
      CALL GQPLCI(IER,IOC)
      CALL GQTXCI(IER,IOT)
      CALL GQLWSC(IER,ROW)
      CALL GSLWSC(WDLV)
C
C Calculation of NDC sizing values varies based on whether grid 
C relative sizing is in effect.
C
      IF (IGBS .EQ. 0) THEN
         RNDA=RARL*FW2W
         DFMG=RDFM*FW2W
      ELSE
         RNDA=RARL*FW2W/REAL(IXDM)
         DFMG=RDFM*FW2W/REAL(IXDM)
      END IF
C
C If not using the FX,FY routines, then the vector normalization
C value is fixed. 
C
      IF (ICPM.LT.1) THEN
         VNML=0.3333333
      ELSE
         VNML=RVNL
      END IF
C
C Set up the indexes into the work array. The first 2 sections hold
C the normalized vector values. If coloring by magnitude the next
C section holds the magnitudes, and if thinning the final two sections
C hold the thinning data.
C
      IU = 1
      IV = IXDM*IYDN+1
      IF (ABS(ICTV) .EQ. 1) THEN
         IM = 2*IXDM*IYDN+1
         IF (RSMD .GT. 0.0) THEN
            IT = 3*IXDM*IYDN+1
         ELSE
            IT = 1
         END IF
      ELSE IF (RSMD .GT. 0.0) THEN
         IT = 2*IXDM*IYDN+1
      ELSE
         IT = 1
      END IF
C
C
C Draw the streamlines.
C Break the work array into parts.
C If capatability mode is enabled use STDRCM (ST DRaw CoMpatible).
C If mode is curly vector use STCRCV (ST Draw Curly Vector)
C Otherwise use STDRAW
C If coloring by magnitude, the magnitudes are stored in the
C third part of the work array.
C
      IF (ICPM .GT. 0) THEN
         CALL STDRCM(U,V,WRK(IU),WRK(IV),IAM,STUMSL)
      ELSE IF (ISTM .EQ. 1) THEN
         IF (ABS(ICTV) .EQ. 1) THEN
            CALL STDRCV(U,V,WRK(IU),WRK(IV),WRK(IM),WRK(IT),IAM,STUMSL)
         ELSE
            CALL STDRCV(U,V,WRK(IU),WRK(IV),P,WRK(IT),IAM,STUMSL)
         END IF
      ELSE IF (ABS(ICTV) .EQ. 1) THEN
         CALL STDRAW(U,V,WRK(IU),WRK(IV),WRK(IM),WRK(IT),IAM,STUMSL)
      ELSE
         CALL STDRAW (U,V,WRK(IU),WRK(IV),P,WRK(IT),IAM,STUMSL)
      END IF
C
C Reset the polyline color, text color, and the linewidth
C
      CALL GSPLCI(IOC)
      CALL GSLWSC(ROW)
      CALL GSTXCI(IOT)
C
      RETURN
      END

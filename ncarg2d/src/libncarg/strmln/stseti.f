C
C       $Id: stseti.f,v 1.7.8.1 2010-03-17 20:51:58 brownrig Exp $
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
      SUBROUTINE STSETI (CNM,IVL)
C
      CHARACTER*(*) CNM
      INTEGER IVL
C
C This subroutine is called to give a specified integer value to a
C specified parameter.
C
C CNM is the name of the parameter whose value is to be set.
C
C IVL is an integer variable containing the new value of the parameter.
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
C ------------------------------------------------------------------
C RLB 3/2010: Previously the integer parameter was converted to a float
C   and the work was delegated off to STSETR. This provided a sort
C   of "automatic type conversion", allowing the user to set a real
C   parameter using either stseti() or stsetr(), as in:
C        CALL STSETI ('xxx',-9999)
C     or
C        CALL STSETR ('xxx',-9999.0)
C
C   Color-indices are now either encoded RGBa values, or indices as
C   before. RGBa values are typically large integer values, beyond the
C   precision of floats, and thus this delegation scheme no longer
C   works correctly. The code has been refactored such that the integer
C   cases are now handled directly herein. If no action is found for
C   the CNM, then we delegate over to STSETR.
C -------------------------------------------------------
C
C Check for a parameter name that is too short.
C
      IF (LEN(CNM).LT.3) THEN
        CSTR(1:46)='STSETI - PARAMETER NAME TOO SHORT - '
        CSTR(47:46+LEN(CNM))=CNM
        CALL SETER (CSTR(1:46+LEN(CNM)),1,1)
        RETURN
      END IF
C
C Check for incorrect use of the index parameter.
C
      IF (CNM(1:3).EQ.'CLR'.OR.CNM(1:3).EQ.'clr') THEN
         IF (IPAI.LT.1.OR.IPAI.GT.IPLVLS) THEN
            CSTR(1:46)='STSETI - SETTING XXX - PAI INCORRECT'
            CSTR(28:30)=CNM(1:3)
            CALL SETER (CSTR(1:46),2,1)
            RETURN
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
         IUD1=IVL
      ELSE IF (CNM(1:3).EQ.'VD1'.OR. CNM(1:3).EQ.'vd1') THEN
         IVD1=IVL
      ELSE IF (CNM(1:3).EQ.'PD1'.OR. CNM(1:3).EQ.'pd1') THEN
         IPD1=IVL
      ELSE IF (CNM(1:3).EQ.'XD1'.OR. CNM(1:3).EQ.'xd1') THEN
         IXD1=IVL
      ELSE IF (CNM(1:3).EQ.'XDM'.OR. CNM(1:3).EQ.'xdm') THEN
         IXDM=IVL
      ELSE IF (CNM(1:3).EQ.'YD1'.OR. CNM(1:3).EQ.'yd1') THEN
         IYD1=IVL
      ELSE IF (CNM(1:3).EQ.'YDN'.OR. CNM(1:3).EQ.'ydn') THEN
         IYDN=IVL
      ELSE IF (CNM(1:3).EQ.'WKD'.OR.CNM(1:3).EQ.'wkd') THEN
         IWKD=IVL
      ELSE IF (CNM(1:3).EQ.'WKU'.OR.CNM(1:3).EQ.'wku') THEN
         IWKU=IVL
      ELSE IF (CNM(1:3).EQ.'SET'.OR. CNM(1:3).EQ.'set') THEN
         ISET=IVL
      ELSE IF (CNM(1:3).EQ.'ERR'.OR. CNM(1:3).EQ.'err') THEN
         IERR=IVL
      ELSE IF (CNM(1:3).EQ.'XIN'.OR. CNM(1:3).EQ.'xin') THEN
         IXIN=IVL
      ELSE IF (CNM(1:3).EQ.'YIN'.OR. CNM(1:3).EQ.'yin') THEN
         IYIN=IVL
      ELSE IF (CNM(1:3).EQ.'MSK'.OR. CNM(1:3).EQ.'msk') THEN
         IMSK=IVL
      ELSE IF (CNM(1:3).EQ.'CPM'.OR. CNM(1:3).EQ.'cpm') THEN
         ICPM=IVL
      ELSE IF (CNM(1:3).EQ.'NLV'.OR.CNM(1:3).EQ.'nlv') THEN
         NLVL=IVL
      ELSE IF (CNM(1:3).EQ.'PAI'.OR.CNM(1:3).EQ.'pai') THEN
         IF (IVL .LT. 1 .OR. IVL .GT. IPLVLS) GO TO 9800
         IPAI=IVL
      ELSE IF (CNM(1:3).EQ.'CTV'.OR.CNM(1:3).EQ.'ctv') THEN
         ICTV=IVL
C
C UVMN,UVMX, PMIN, PMAX are read-only
C
      ELSE IF (CNM(1:3).EQ.'THN'.OR. CNM(1:3).EQ.'thn') THEN
         ITHN=IVL
      ELSE IF (CNM(1:3).EQ.'PLR'.OR. CNM(1:3).EQ.'plr') THEN
         IPLR=IVL
      ELSE IF (CNM(1:3).EQ.'SST'.OR. CNM(1:3).EQ.'sst') THEN
         ISST=IVL
      ELSE IF (CNM(1:3).EQ.'CLR'.OR.CNM(1:3).EQ.'clr') THEN
         ICLR(IPAI)=IVL
C
C ---------------------------------------------------------------------
C
C Values in STSTRM
C
      ELSE IF (CNM(1:3).EQ.'SGD'.OR. CNM(1:3).EQ.'sgd') THEN
         ISGD=IVL
      ELSE IF (CNM(1:3).EQ.'AGD'.OR. CNM(1:3).EQ.'agd') THEN
         IAGD=IVL
      ELSE IF (CNM(1:3).EQ.'CKP'.OR. CNM(1:3).EQ.'ckp') THEN
         ICKP=IVL
      ELSE IF (CNM(1:3).EQ.'CKX'.OR. CNM(1:3).EQ.'ckx') THEN
         ICKX=IVL
      ELSE IF (CNM(1:3).EQ.'TRP'.OR. CNM(1:3).EQ.'trp') THEN
         ITRP=IVL
      ELSE IF (CNM(1:3).EQ.'CYK'.OR. CNM(1:3).EQ.'cyk') THEN
         ICYK=IVL
      ELSE IF (CNM(1:3).EQ.'SVF'.OR. CNM(1:3).EQ.'svf') THEN
         ISVF=IVL
      ELSE IF (CNM(1:3).EQ.'SPC'.OR. CNM(1:3).EQ.'spc') THEN
         ISPC=IVL
      ELSE IF (CNM(1:3).EQ.'GBS'.OR. CNM(1:3).EQ.'gbs') THEN
         IGBS=IVL
C
C This parameter is special in that it causes RSSP,RDFM, and RARL
C to be reset.
C
        IF (IGBS .EQ. 0) THEN
           RARL = 0.012
           RDFM = 0.02
           RSSP = 0.015
        ELSE
           RARL = 0.33
           RDFM = 0.33
           RSSP = 0.5
        END IF
C
      ELSE IF (CNM(1:3).EQ.'STM'.OR. CNM(1:3).EQ.'stm') THEN
         ISTM=IVL
      ELSE IF (CNM(1:3).EQ.'VPO'.OR. CNM(1:3).EQ.'vpo') THEN
         IVPO=IVL
C
C ---------------------------------------------------------------------
C
C Values in STTXP
C
C Character attributes
C
C
      ELSE IF (CNM(1:3).EQ.'ZFP'.OR. CNM(1:3).EQ.'zfp') THEN
         IZFP=IVL
      ELSE IF (CNM(1:3).EQ.'ZFC'.OR. CNM(1:3).EQ.'zfc') THEN
         IZFC=IVL
C
C ---------------------------------------------------------------------
C
C Values in STMAP
C
      ELSE IF (CNM(1:3).EQ.'MAP'.OR. CNM(1:3).EQ.'map') THEN
         IMAP=IVL
      ELSE IF (CNM(1:3).EQ.'TRT'.OR. CNM(1:3).EQ.'trt') THEN
         ITRT=IVL
C
C ---------------------------------------------------------------------
C
      ELSE
C       Float the integer value and pass it on to STSETR.
        RVL=REAL(IVL)
        CALL STSETR (CNM,RVL)
      END IF
C
      GOTO 9900
C
 9800 CONTINUE
C
      CSTR(1:50)='STSETI - PARAMETER VALUE OUT OF RANGE - '
      CSTR(51:53)=CNM(1:3)
      CALL SETER (CSTR(1:53),3,1)
      RETURN
C
 9900 CONTINUE
C
C Done.
C
      RETURN
      END

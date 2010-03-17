C
C       $Id: vvseti.f,v 1.6.8.1 2010-03-17 20:51:58 brownrig Exp $
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
      SUBROUTINE VVSETI (CNM,IVL)
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
C
C RLB 3/2010: Previously the integer parameter was converted to a float
C   and the work was delegated off to VVSETR. This provided a sort
C   of "automatic type conversion", allowing the user to set a real
C   parameter using either vvseti() or vvsetr(), as in:
C        CALL VVSETI ('USV - U Array Special Value',-9999)
C     or
C        CALL VVSETR ('USV - U Array Special Value',-9999.0)
C
C   Color-indices are now either encoded RGBa values, or indices as
C   before. RGBa values are typically large integer values, beyond the
C   precision of floats, and thus this delegation scheme no longer
C   works correctly. The code has been refactored such that the integer
C   cases are now handled directly herein. If no action is found for
C   the CNM, then we delegate over to VVSETR.
C --------------------------------------------------------------------
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
C --------------------------------------------------------------------
C
C Check for a parameter name that is too short.
C
      IF (LEN(CNM).LT.3) THEN
        CSTR(1:46)='VVSETI - PARAMETER NAME TOO SHORT - '
        CSTR(47:46+LEN(CNM))=CNM
        CALL SETER (CSTR(1:46+LEN(CNM)),1,1)
        RETURN
      END IF
C
C Check for incorrect use of the index parameter.
C
      IF (CNM(1:3).EQ.'CLR'.OR.CNM(1:3).EQ.'clr') THEN
         IF (IPAI.LT.1.OR.IPAI.GT.IPLVLS) THEN
            CSTR(1:46)='VVSETI - SETTING XXX - PAI INCORRECT'
            CSTR(28:30)=CNM(1:3)
            CALL SETER (CSTR(1:46),2,1)
            RETURN
         END IF
      END IF

C
C Set the appropriate parameter value.
C
C Values in VVCOM
C
      IF (CNM(1:3).EQ.'UD1'.OR. CNM(1:3).EQ.'ud1') THEN
         IUD1=IVL
      ELSE IF (CNM(1:3).EQ.'VD1'.OR. CNM(1:3).EQ.'vd1') THEN
         IVD1=IVL
      ELSE IF (CNM(1:3).EQ.'PD1'.OR. CNM(1:3).EQ.'pd1') THEN
         IPD1=IVL
      ELSE IF (CNM(1:3).EQ.'XDM'.OR. CNM(1:3).EQ.'xdm') THEN
         IXDM=IVL
      ELSE IF (CNM(1:3).EQ.'YDN'.OR. CNM(1:3).EQ.'ydn') THEN
         IYDN=IVL
      ELSE IF (CNM(1:3).EQ.'SET'.OR. CNM(1:3).EQ.'set') THEN
         ISET=IVL
      ELSE IF (CNM(1:3).EQ.'XIN'.OR. CNM(1:3).EQ.'xin') THEN
         IXIN=IVL
      ELSE IF (CNM(1:3).EQ.'YIN'.OR. CNM(1:3).EQ.'yin') THEN
         IYIN=IVL
      ELSE IF (CNM(1:3).EQ.'SVF'.OR. CNM(1:3).EQ.'svf') THEN
         ISVF=IVL
      ELSE IF (CNM(1:3).EQ.'MSK'.OR. CNM(1:3).EQ.'msk') THEN
         IMSK=IVL
      ELSE IF (CNM(1:3).EQ.'CPM'.OR. CNM(1:3).EQ.'cpm') THEN
         ICPM=IVL
      ELSE IF (CNM(1:3).EQ.'NLV'.OR.CNM(1:3).EQ.'nlv') THEN
         NLVL=MIN(IPLVLS,IVL)
      ELSE IF (CNM(1:3).EQ.'PAI'.OR.CNM(1:3).EQ.'pai') THEN
         IF (IVL .LT. 1 .OR. IVL .GT. IPLVLS) GO TO 9800
         IPAI=IVL
      ELSE IF (CNM(1:3).EQ.'CTV'.OR.CNM(1:3).EQ.'ctv') THEN
         ICTV=IVL
C
C UVMN,UVMX, PMIN, PMAX are read-only
C
      ELSE IF (CNM(1:3).EQ.'SPC'.OR. CNM(1:3).EQ.'spc') THEN
         ISPC=IVL
      ELSE IF (CNM(1:3).EQ.'MAP'.OR. CNM(1:3).EQ.'map') THEN
         IMAP=IVL
      ELSE IF (CNM(1:3).EQ.'PLR'.OR. CNM(1:3).EQ.'plr') THEN
         IPLR=IVL
      ELSE IF (CNM(1:3).EQ.'VST'.OR. CNM(1:3).EQ.'vst') THEN
         IVST=IVL
      ELSE IF (CNM(1:3).EQ.'VPO'.OR. CNM(1:3).EQ.'vpo') THEN
         IVPO=IVL
      ELSE IF (CNM(1:3).EQ.'LBL'.OR. CNM(1:3).EQ.'lbl') THEN
         ILBL=IVL
      ELSE IF (CNM(1:3).EQ.'DPF'.OR. CNM(1:3).EQ.'dpf') THEN
         IDPF=IVL
C
C arrow parameters
C
      ELSE IF (CNM(1:3).EQ.'AST'.OR.CNM(1:3).EQ.'ast') THEN
         IAST=IVL
      ELSE IF (CNM(1:3).EQ.'ACM'.OR.CNM(1:3).EQ.'acm') THEN
         IACM=IVL
      ELSE IF (CNM(1:3).EQ.'AFO'.OR.CNM(1:3).EQ.'afo') THEN
         IAFO=IVL
C
C character attributes
C
      ELSE IF (CNM(1:3).EQ.'MNP'.OR. CNM(1:3).EQ.'mnp') THEN
         IMNP=IVL
      ELSE IF (CNM(1:3).EQ.'MNC'.OR. CNM(1:3).EQ.'mnc') THEN
         IMNC=IVL
C
      ELSE IF (CNM(1:3).EQ.'MXP'.OR. CNM(1:3).EQ.'mxp') THEN
         IMXP=IVL
      ELSE IF (CNM(1:3).EQ.'MXC'.OR. CNM(1:3).EQ.'mxc') THEN
         IMXC=IVL
C
      ELSE IF (CNM(1:3).EQ.'ZFP'.OR. CNM(1:3).EQ.'zfp') THEN
         IZFP=IVL
      ELSE IF (CNM(1:3).EQ.'ZFC'.OR. CNM(1:3).EQ.'zfc') THEN
         IZFC=IVL
C
      ELSE IF (CNM(1:3).EQ.'LBC'.OR. CNM(1:3).EQ.'lbc') THEN
         ILBC=IVL
C
      ELSE IF (CNM(1:3).EQ.'ILP'.OR. CNM(1:3).EQ.'ilp') THEN
         IILP=IVL
      ELSE IF (CNM(1:3).EQ.'ILC'.OR. CNM(1:3).EQ.'ilc') THEN
         IILC=IVL
C
C     color field parameters
C
      ELSE IF (CNM(1:3).EQ.'CLR'.OR.CNM(1:3).EQ.'clr') THEN
         ICLR(IPAI)=IVL
      ELSE IF (CNM(1:3).EQ.'TRT'.OR. CNM(1:3).EQ.'trt') THEN
         ITRT=IVL
      ELSE
C        convert to real and delegate
         RVL = REAL(IVL)
         CALL VVSETR(CNM, RVL)
      END IF
C
      GOTO 9900
C
 9800 CONTINUE
C
      CSTR(1:50)='VVSETI - PARAMETER VALUE OUT OF RANGE - '
      CSTR(51:53)=CNM(1:3)
      CALL SETER (CSTR(1:53),4,1)
      RETURN
C
 9900 CONTINUE
C
C Done.
C
      RETURN
      END

C
C	$Id: vvgetc.f,v 1.4 1993-01-15 22:46:49 dbrown Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE VVGETC (WHCH,CVAL)
C
      CHARACTER*(*) WHCH,CVAL
C
C This subroutine is called to retrieve the character value of a
C specified parameter.
C
C WHCH is the name of the parameter whose value is to be retrieved.
C
C CVAL is a character variable in which the desired value is to be
C returned by VVGETC.
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
     +                UXC1       ,UXCM       ,UYC1       ,UYCM       ,
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
     +                FILS    ,FILX    ,FILY    ,IILP     IILC  ,
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
        CSTR(1:36)='VVGETC - PARAMETER NAME TOO SHORT - '
        CSTR(37:36+LEN(WHCH))=WHCH
        CALL SETER (CSTR(1:36+LEN(WHCH)),1,2)
      STOP
      END IF
C
C Get the proper parameter.
C
      IF (WHCH(1:3).EQ.'MNT'.OR.WHCH(1:3).EQ.'mnt') THEN
         CALL VVTXLN(CMNT,IPCHSZ,IB,IE)
         CVAL=CMNT(IB:IE)
      ELSE IF (WHCH(1:3).EQ.'MXT'.OR.WHCH(1:3).EQ.'mxt') THEN
         CALL VVTXLN(CMXT,IPCHSZ,IB,IE)
         CVAL=CMXT(IB:IE)
      ELSE IF (WHCH(1:3).EQ.'ZFT'.OR.WHCH(1:3).EQ.'zft') THEN
         CALL VVTXLN(CZFT,IPCHSZ,IB,IE)
         CVAL=CZFT(IB:IE)
      ELSE IF (WHCH(1:3).EQ.'ILT'.OR.WHCH(1:3).EQ.'ilt') THEN
         CALL VVTXLN(CILT,IPCHSZ,IB,IE)
         CVAL=CILT(IB:IE)
      ELSE
C
         CSTR(1:36)='VVGETC - PARAMETER NAME NOT KNOWN - '
         CSTR(37:39)=WHCH(1:3)
         CALL SETER (CSTR(1:39),3,2)
         STOP
C
      END IF
C
C
C Done.
C
      RETURN
C
      END

C
C       $Id: vvdraw.f,v 1.5 1993-12-03 21:27:26 kennison Exp $
C
      SUBROUTINE VVDRAW (XB,YB,XE,YE,VLN,LBL,NC,IAM,VVUDMV,IDA)
C
      DIMENSION IAM(*)
C
      EXTERNAL VVUDMV
C
C This routine is called to draw a single arrow.  It has arguments as
C follows -
C
C XB,YB    -  Coordinate of arrow base, fractional coordinate
C XE,YE    -  Coordinate of arrow head, fractional coordinate
C VLN      -  Length of the vector (arrow)
C LBL      -  Character label to be put above arrow.
C NC       -  Number of characters in label.
C IAM      -  Area map (optional) for masking vectors
C VVUDMV   -  User-supplied function for drawing masked vectors
C IDA      -  Do area masking flag
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
      PARAMETER (IPLVLS = 64, IPAGMX = 64)
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
     +                UXC1       ,UXCM       ,UYC1       ,UYCN       ,
     +                NLVL       ,IPAI       ,ICTV       ,WDLV       ,
     +                UVMN       ,UVMX       ,PMIN       ,PMAX       ,
     +                RVMN       ,RVMX       ,RDMN       ,RDMX       ,
     +                ISPC       ,ITHN       ,IPLR       ,IVST       ,
     +                IVPO       ,ILBL       ,IDPF       ,IMSG       ,
     +                ICLR(IPLVLS)           ,TVLU(IPLVLS)
C
C Arrow size/shape parameters
C
        COMMON / VVARO /
     +                HDSZ       ,HINF       ,HANG       ,
     +                HSIN       ,HCOS       ,FAMN       ,FAMX

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
C
C Local parameters
C
C Number of points in an arrow
C
      PARAMETER (IPAPCT=6)
C
C Local arrays
C
      DIMENSION IAI(IPAGMX), IAG(IPAGMX)
      DIMENSION XF(IPAPCT), YF(IPAPCT), XW(IPAPCT), YW(IPAPCT)
      DIMENSION XO(IPAPCT), YO(IPAPCT)
C
      CHARACTER*10 LBL
C
C Transfer arguments to local variables and compute the vector length.
C
      DX=XE-XB
      DY=YE-YB
C
C Adjust the coordinates of the vector endpoints as implied by the
C vector positioning option.
C
      IF (IVPO .LT. 0) THEN
         XF(4) = XB
         YF(4) = YB
         XF(1) = XB - DX
         YF(1) = YB - DY
      ELSE IF (IVPO .EQ. 0) THEN
         XF(1) = XB - 0.5*DX
         YF(1) = YB - 0.5*DY
         XF(4) = XE - 0.5*DX
         YF(4) = YE - 0.5*DY
      ELSE
         XF(1) = XB
         YF(1) = YB
         XF(4) = XE
         YF(4) = YE
      END IF
C
C Determine the coordinates of the points used to draw the arrowhead.
C If the size is outside the range of the minimum and maximum sizes
C use fixed sizes
C
      C1 = HDSZ
      IF (VLN*C1 .LT. FAMN*FW2W) C1 = FAMN*FW2W/VLN
      IF (VLN*C1 .GT. FAMX*FW2W) C1 = FAMX*FW2W/VLN
C
C Calculate the remaining points in fractional coordinates
C
      XF(3) = XF(4)-C1*(HCOS*DX-HSIN*DY)
      YF(3) = YF(4)-C1*(HCOS*DY+HSIN*DX)
      XF(2) = XF(4)-C1*HINF*DX
      YF(2) = YF(4)-C1*HINF*DY
      XF(5) = XF(4)-C1*(HCOS*DX+HSIN*DY)
      YF(5) = YF(4)-C1*(HCOS*DY-HSIN*DX)
C
C Convert to user coodinates
C
      XW(1) = CFUX(XF(1))
      YW(1) = CFUY(YF(1))
      XW(2) = CFUX(XF(2))
      YW(2) = CFUY(YF(2))
      XW(3) = CFUX(XF(3))
      YW(3) = CFUY(YF(3))
      XW(4) = CFUX(XF(4))
      YW(4) = CFUY(YF(4))
      XW(5) = CFUX(XF(5))
      YW(5) = CFUY(YF(5))
      XW(6) = XW(2)
      YW(6) = YW(2)
C
C Plot the arrow using areas or not, as required.
C
      IF (IDA .GT. 1) THEN
        CALL ARGTAI(IAM,XW(1),YW(1),IAI,IAG,IPAGMX,NAI,0)
        CALL VVUDMV(X,Y,IPAPCT,IAI,IAG,NAI)
      ELSE IF (IDA .EQ. 1) THEN
         CALL ARDRLN(IAM, XW, YW, IPAPCT, 
     +        XO, YO, IPAPCT, IAI, IAG, IPAGMX, VVUDMV)
      ELSE
         CALL CURVE(XW,YW,IPAPCT)
      END IF
C
C If requested, put the vector magnitude above the arrow.
C
      IF (NC .GT. 0) THEN
C
         PHI = ATAN2(DY,DX)
         IF (AMOD(PHI+P5D2PI,P2XPI) .GT. P1XPI) PHI = PHI+P1XPI
         XC = 0.5*(XF(1)+XF(4))+1.25*FLBS*FW2W*COS(PHI+P1D2PI)
         YC = 0.5*(YF(1)+YF(4))+1.25*FLBS*FW2W*SIN(PHI+P1D2PI)
C
         XC = CFUX(XC)
         YC = CFUY(YC)
         CALL VVTXLN(LBL,10,IB,IE)
         CALL PLCHLQ(XC,YC,LBL(IB:IE),FLBS*FW2W,PRTOD*PHI,0.0)
C
      END IF
C
      RETURN
      END

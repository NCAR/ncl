C
C       $Id: vvdraw.f,v 1.13 2008-07-27 00:17:35 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
C          -  (if negative does not draw the vector, but just 
C          -  calculates the vector's height and width)
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
      DIMENSION XF(IPAPCT), YF(IPAPCT)
      DIMENSION XO(IPAPCT), YO(IPAPCT)
      DIMENSION WIN(4),VPT(4)
C
      CHARACTER*10 LBL
C
      IF (VLN.LT.0.0) THEN
         ISZ=1
         VLL=-VLN
      ELSE
         ISZ=0
         VLL=VLN
      END IF
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
      IF (VLL*C1 .LT. FAMN*FW2W) C1 = FAMN*FW2W/VLL
      IF (VLL*C1 .GT. FAMX*FW2W) C1 = FAMX*FW2W/VLL
C
C Calculate the remaining points in fractional coordinates
C
      XF(3) = XF(4)-C1*(HCOS*DX-HSIN*DY)
      YF(3) = YF(4)-C1*(HCOS*DY+HSIN*DX)
      XF(2) = XF(4)-C1*HINF*DX
      YF(2) = YF(4)-C1*HINF*DY
      XF(5) = XF(4)-C1*(HCOS*DX+HSIN*DY)
      YF(5) = YF(4)-C1*(HCOS*DY-HSIN*DX)
      XF(6) = XF(2)
      YF(6) = YF(2)
C
C
C Early return if just calculating the size
C
      IF (ISZ.EQ.1) THEN
         AYMN = 100.0
         AYMX = -100.
         AXMN = 100.0
         AXMX = -100.0
         DO 60 I=1,IPAPCT
            IF (XF(I) .LT. AXMN) AXMN = XF(I)
            IF (XF(I) .GT. AXMX) AXMX = XF(I)
            IF (YF(I) .LT. AYMN) AYMN = YF(I)
            IF (YF(I) .GT. AYMX) AYMX = YF(I)
 60      CONTINUE
         FXSZ = AXMX-AXMN
         FYSZ = AYMX-AYMN 
C
         RETURN
      END IF
C
C Set the the normalization transformation to an identity. Can't use
C the default transformation because we need to clip to the viewport
C
      CALL GQCNTN(IER,NTR)
      CALL GQNT(NTR,IER,WIN,VPT)
      CALL GSWN(NTR,VPT(1),VPT(2),VPT(3),VPT(4))
C
C Plot the arrow using areas or not, as required.
C
      IF (IDA .GT. 1) THEN
        CALL ARGTAI(IAM,XB,YB,IAI,IAG,IPAGMX,NAI,0)
        CALL VVUDMV(XF,YF,IPAPCT,IAI,IAG,NAI)
      ELSE IF (IDA .EQ. 1) THEN
         CALL ARDRLN(IAM,XF,YF,IPAPCT, 
     +        XO,YO,IPAPCT,IAI,IAG,IPAGMX,VVUDMV)
      ELSE
         CALL GPL(IPAPCT,XF,YF)
      END IF
C
C If requested, put the vector magnitude above the arrow.
C
      IF (NC .GT. 0) THEN
C
         PHI = ATAN2(DY,DX)
         IF (MOD(PHI+P5D2PI,P2XPI) .GT. P1XPI) PHI = PHI+P1XPI
         XC = 0.5*(XF(1)+XF(4))+1.25*FLBS*FW2W*COS(PHI+P1D2PI)
         YC = 0.5*(YF(1)+YF(4))+1.25*FLBS*FW2W*SIN(PHI+P1D2PI)
C
         CALL PLCHLQ(XC,YC,LBL(1:NC),FLBS*FW2W,PRTOD*PHI,0.0)
C
      END IF
C
C Restore the original transformation
C
      CALL GSWN(NTR,WIN(1),WIN(2),WIN(3),WIN(4))
C
C
      RETURN
      END

C
C       $Id: vvdrfl.f,v 1.11 2008-07-27 00:17:35 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE VVDRFL (XB,YB,XE,YE,VLN,LBL,NC,IAM,VVUDMV,IDA)
C
      DIMENSION IAM(*)
C
      EXTERNAL VVUDMV
C
C This routine is called to draw a single filled arrow. Its 
C arguments are the same as the line-based arrow drawing routine 
C VVDRAW. However it recognizes some parameters that the line arrow
C routine does not. It has arguments as follows -
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
      PARAMETER (IPAPCT=8)
C
C Local arrays
C
      DIMENSION IAI(IPAGMX),IAG(IPAGMX)
      DIMENSION XO(IPAPCT),YO(IPAPCT)
C
      PARAMETER (IADIM=8,D2RAD=.017453293)
      DIMENSION ARROWX(IADIM),ARROWY(IADIM),AOUTX(IADIM),AOUTY(IADIM)
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
         AXE = XB
         AYE = YB
         AXB = XB - DX
         AYB = YB - DY
      ELSE IF (IVPO .EQ. 0) THEN
         AXB = XB - 0.5*DX
         AYB = YB - 0.5*DY
         AXE = XE - 0.5*DX
         AYE = YE - 0.5*DY
      ELSE
         AXB = XB
         AYB = YB
         AXE = XE
         AYE = YE
      END IF
C
      IF (FAWF .LE. 0.0 .AND. FAYF .LE. 0.0 .AND. FAXF .LE. 0.0) THEN
         DO 10 I=1,IADIM
            ARROWX(I) = VLL * AROX(I)
            ARROWY(I) = VLL * AROY(I)
 10      CONTINUE
      ELSE
         RFM = UVMX
         IF (VRMG.GT.0.0) RFM = VRMG
         VRL = DVMX
         IF (VRLN.GT.0.0) VRL = VRLN*FW2W
         FMG = UVMG/RFM
         IF (FAWF.GT.0) THEN
            WSZ = FWMN + MAX(0.0,FWRF-FWMN)*FMG
         ELSE
            WSZ = VLL*FAWR*0.5
         END IF
         IF (FAXF.GT.0) THEN
            XSZ = FXMN + MAX(0.0,FXRF-FXMN)*FMG
            ESZ = MIN(VLL,FIMN + MAX(0.0,FIRF-FIMN)*FMG)
         ELSE
            XSZ = VLL*FAXR
            ESZ = VLL*FAIR
         END IF
         IF (FAYF.GT.0) THEN
            YSZ = WSZ+FYMN+MAX(0.0,FYRF-FYMN)*FMG
         ELSE
            YSZ = WSZ+VLL*FAYR
         END IF
C
         ARROWX(1) = VLL*AROX(1)
         ARROWX(3) = -XSZ
         ARROWX(4) = 0.0
         ARROWX(5) = -XSZ
         ARROWX(7) = VLL*AROX(7)
         ARROWX(8) = VLL*AROX(8)
         ARROWY(1) = -WSZ
         ARROWY(2) = -WSZ
         ARROWY(3) = -YSZ
         ARROWY(4) = 0.0
         ARROWY(5) = YSZ
         ARROWY(6) = WSZ
         ARROWY(7) = WSZ
         ARROWY(8) = -WSZ
C
         IF (WSZ.GT.0.0) THEN
C
C Find point of intersection of line between rear arrowhead tip and 
C the X interior point and the line forming the edge of the arrow body
C using ratio of similar triangles
C
            ARROWX(2) = -(ESZ + ABS(XSZ-ESZ)*WSZ/(WSZ+YSZ))
         ELSE
            ARROWX(2) = -ESZ
         END IF
         ARROWX(6) = ARROWX(2)
C
      END IF
      IF (FAXR .LE. 0.4) THEN
         RMG = ARROWY(6)
      ELSE
         RMG = MAX(ARROWY(5),ARROWY(6))
      END IF
C
C  Rotate and translate the arrow.
C
      COSANG = DX / VLL
      SINANG = DY / VLL
      DO 20 I=1,IADIM
         AOUTX(I) = AXE+ARROWX(I)*COSANG-ARROWY(I)*SINANG
         AOUTY(I) = AYE+ARROWX(I)*SINANG+ARROWY(I)*COSANG
 20   CONTINUE
C
C Early return if just calculating the size
C
      IF (ISZ.EQ.1) THEN
C
         AYMN = 100.0
         AYMX = -100.
         AXMN = 100.0
         AXMX = -100.0
         DO 50 I=1,IADIM
            IF (AOUTX(I) .LT. AXMN) AXMN = AOUTX(I)
            IF (AOUTX(I) .GT. AXMX) AXMX = AOUTX(I)
            IF (AOUTY(I) .LT. AYMN) AYMN = AOUTY(I)
            IF (AOUTY(I) .GT. AYMX) AYMX = AOUTY(I)
 50      CONTINUE
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
C Output the arrow, using the masking routines if specified.
C
      IF (IDA .GT. 1) THEN
         CALL ARGTAI(IAM,XB,YB,IAI,IAG,IPAGMX,NAI,0)
         CALL VVUDMV(AOUTX,AOUTY,IPAPCT,IAI,IAG,NAI)
      ELSE IF (IDA .EQ. 1) THEN
         CALL ARDRLN(IAM,AOUTX,AOUTY,IPAPCT, 
     +        XO,YO,IPAPCT,IAI,IAG,IPAGMX,VVUDMV)
      ELSE
         IF (IAFO.GT.0 .AND. IACM.GT.-2) THEN
            CALL GPL(IPAPCT,AOUTX,AOUTY)
         END IF
         IF (IACM.NE.-1) THEN
            CALL GFA(IPAPCT,AOUTX,AOUTY)
         END IF
         IF (IAFO.LE.0 .AND. IACM.GT.-2) THEN
            CALL GPL(IPAPCT,AOUTX,AOUTY)
         END IF
      END IF
C
C If requested, put the vector magnitude above the arrow.
C
      IF (NC .GT. 0) THEN
C
         PHI = ATAN2(DY,DX)
         IF (MOD(PHI+P5D2PI,P2XPI) .GT. P1XPI) PHI = PHI+P1XPI
         XC = AXB+0.33*(AXE-AXB) +
     +        (1.25*FLBS*FW2W+RMG)*COS(PHI+P1D2PI)
         YC = AYB+0.33*(AYE-AYB) +
     +        (1.25*FLBS*FW2W+RMG)*SIN(PHI+P1D2PI)
C
         CALL PLCHLQ(XC,YC,LBL(1:NC),FLBS*FW2W,PRTOD*PHI,0.0)
C
      END IF
C
C Restore the original transformation
C
      CALL GSWN(NTR,WIN(1),WIN(2),WIN(3),WIN(4))
C
      RETURN
      END
C
      SUBROUTINE VVINFA ()
C
C Initializes filled arrow common block values
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
C
C Fill in the reference arrow data
C
      WID = 0.5 * FAWR
      AROX(1) = -1.0
      AROX(2) = -FAIR
      AROX(3) = -FAXR
      AROX(4) = 0.0
      AROX(5) = -FAXR
      AROX(6) = -FAIR
      AROX(7) = -1.0
      AROX(8) = -1.0
      AROY(1) = -WID
      AROY(2) = -WID
      AROY(3) = -WID-FAYR
      AROY(4) = 0.0
      AROY(5) = WID+FAYR
      AROY(6) = WID
      AROY(7) = WID
      AROY(8) = -WID
C
      IF (WID.GT.0.0) THEN
C
C Find point of intersection of line between rear arrowhead tip and 
C the X interior point and the line forming the edge of the arrow body
C using ratio of similar triangles
C
         AROX(2) = - (FAIR + ABS(FAXR-FAIR)*WID/(WID+FAYR))
      ELSE
         AROX(2) = -FAIR
      END IF
      AROX(6) = AROX(2)
C
      VRL = DVMX
      IF (VRLN.GT.0.0) VRL = VRLN*FW2W
      FWRF = VRL*WID
      FWMN = FWRF*FAWF
      FXRF = VRL*FAXR
      FXMN = FXRF*FAXF
      FIRF = VRL*FAIR
      FIMN = FIRF*FAXF
      FYRF = VRL*FAYR
      FYMN = FYRF*FAYF
C
      RETURN
      END

C
C       $Id: vvdrwb.f,v 1.8 2008-07-27 00:17:35 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE VVDRWB (XB,YB,XE,YE,VLN,LBL,NC,IAM,VVUDMV,IDA)
C
      DIMENSION IAM(*)
C
      EXTERNAL VVUDMV
C
C This routine is called to draw a single wind barb. Its 
C arguments are the same as the line-based arrow drawing routine 
C VVDRAW. However it uses other parameters from the VVARO common block
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
C  PENINC - Increment for which triangles are drawn.
C  BRBINC - Increment for which a full tic is drawn.
C
      PARAMETER (IADIM=6,PENINC=50.0,BRBINC=10.0)
      DIMENSION XO(IADIM),YO(IADIM)
      DIMENSION WIN(4),VPT(4)
C
      CHARACTER*10 LBL
C
      CALL GQCNTN(IER,NTR)
      CALL GQNT(NTR,IER,WIN,VPT)
      CALL GSWN(NTR,VPT(1),VPT(2),VPT(3),VPT(4))
C
C ABS(VLN) is constant and equal to DVMX for wind barbs.
C
      IF (VLN.LT.0.0) THEN
         ISZ=1
         VLL=-VLN
      ELSE
         ISZ=0
         VLL=VLN
      END IF
C
      SPD = UVMG * WBSC
C
C If low speed just draw a circle
C
      ICR = 0
      IF (SPD .LT. 0.5) THEN
         CALL GQPLCI(IER,IC)
         IF (ISZ .EQ. 0) THEN
            CALL NGDOTS(XB,YB,1,WBCF*VLL,IC)
         ELSE
            AXMN = XB - 0.5 * WBCF*VLL
            AXMX = XB + 0.5 * WBCF*VLL
            AYMN = YB - 0.5 * WBCF*VLL
            AYMX = YB + 0.5 * WBCF*VLL
         END IF
         ICR = 1
         GO TO 999
      END IF
C
C X and Y components of the vector.
C Note that direction is XB to XE. The barbs go at the XB end.
C
      DX=XE-XB
      DY=YE-YB
C
C Determinc sine and cosine of the shaft angle, and scaled speed
C
      CSA = DX / VLL
      SNA = DY / VLL
C
C Figure out how many pennants are required.
C If speed great enough to require a pennant then determine pennant 
C extent along shaft and extend shaft length to accommodate top
C pennant.
C
      NPN = INT((SPD + 2.5) / PENINC)
      XPI=-AROX(1)*CSA
      YPI=-AROX(1)*SNA
      IF (NPN .EQ. 0) THEN
         AXE = XE - XPI 
         AYE = YE - YPI
         DX=AXE-XB
         DY=AYE-YB
      END IF
C
C Adjust the coordinates of the vector endpoints as implied by the
C vector positioning option.
C
      IF (IVPO .LT. 0) THEN
         AXB = XB - DX
         AYB = YB - DY
         AXE = XB
         AYE = YB
      ELSE IF (IVPO .EQ. 0) THEN
         AXB = XB - 0.5*DX
         AYB = YB - 0.5*DY
         AXE = XB + 0.5*DX
         AYE = YB + 0.5*DY
      ELSE
         AXB = XB
         AYB = YB
         AXE = XB + DX
         AYE = YB + DY
      END IF
C
C Initial position and distance between barbs 
C
      XPS = AXB
      YPS = AYB
      XIN = WBDF*DVMX*CSA
      YIN = WBDF*DVMX*SNA
C
C Draw the shaft
C
      XO(1) = AXB
      XO(2) = AXE
      YO(1) = AYB
      YO(2) = AYE
C
      IF (ISZ .EQ. 0) THEN
         CALL GPL(2,XO,YO)
      ELSE
         AXMN = MIN(XO(1),XO(2))
         AXMX = MAX(XO(1),XO(2))
         AYMN = MIN(YO(1),YO(2))
         AYMX = MAX(YO(1),YO(2))
      END IF
C 
C For speed between 0.5 and 2.5 the shaft is unadorned
C
      IF (SPD .LT. 2.5) THEN
         GO TO 999
      END IF
C
C If speed is less than 7.5 then draw a half barb one increment down
C
      IF (SPD .LT. 7.5) THEN
         XO(1) = XPS+XIN
         YO(1) = YPS+YIN
         XO(2) = XO(1) + AROX(5)*CSA-AROY(5)*SNA
         YO(2) = YO(1) + AROX(5)*SNA+AROY(5)*CSA
         IF (ISZ .EQ. 0) THEN
            CALL GPL(2,XO,YO)
         ELSE
            AXMN = MIN(AXMN,XO(2))
            AXMX = MAX(AXMX,XO(2))
            AYMN = MIN(AYMN,YO(2))
            AYMX = MAX(AYMX,YO(2))
         END IF
         GO TO 999
      END IF
C
C Now draw the pennants if any
C     
      DO 10 I = 1,NPN
         XO(1) = XPS + XPI
         YO(1) = YPS + YPI
         XO(2) = XO(1) + AROX(2)*CSA-AROY(2)*SNA
         YO(2) = YO(1) + AROX(2)*SNA+AROY(2)*CSA
         XO(3) = XPS
         YO(3) = YPS
         XO(4) = XO(1)
         YO(4) = YO(1)
         IF (ISZ .EQ. 0) THEN
            CALL GFA(4,XO,YO)
         ELSE
            AXMN = MIN(AXMN,XO(2))
            AXMX = MAX(AXMX,XO(2))
            AYMN = MIN(AYMN,YO(2))
            AYMX = MAX(AYMX,YO(2))
         END IF
         IF (I.EQ.NPN) THEN
            XPS = XO(1) + XIN
            YPS = YO(1) + YIN
         ELSE
            XPS = XO(1)+0.5*XIN
            YPS = YO(1)+0.5*YIN
         END IF
 10   CONTINUE
C
      SPD = SPD - NPN * PENINC
      NBR = INT((SPD + 2.5) / BRBINC)
C
C Draw the barbs
C
      DO 20 I = 1, NBR
         XO(1) = XPS
         YO(1) = YPS
         XO(2) = XO(1) + AROX(2)*CSA-AROY(2)*SNA
         YO(2) = YO(1) + AROX(2)*SNA+AROY(2)*CSA
         IF (ISZ .EQ. 0) THEN
            CALL GPL(2,XO,YO)
         ELSE
            AXMN = MIN(AXMN,XO(2))
            AXMX = MAX(AXMX,XO(2))
            AYMN = MIN(AYMN,YO(2))
            AYMX = MAX(AYMX,YO(2))
         END IF
         XPS = XPS + XIN
         YPS = YPS + YIN
 20   CONTINUE
C
C Draw a half barb if required
C
      IF (SPD - NBR * BRBINC .GT. 2.5) THEN
         XO(1) = XPS
         YO(1) = YPS
         XO(2) = XO(1) + AROX(5)*CSA-AROY(5)*SNA
         YO(2) = YO(1) + AROX(5)*SNA+AROY(5)*CSA
         IF (ISZ .EQ. 0) THEN
            CALL GPL(2,XO,YO)
         ELSE
            AXMN = MIN(AXMN,XO(2))
            AXMX = MAX(AXMX,XO(2))
            AYMN = MIN(AYMN,YO(2))
            AYMX = MAX(AYMX,YO(2))
         END IF
      END IF
      
 999  CONTINUE

      FXSZ = AXMX-AXMN
      FYSZ = AYMX-AYMN
C
C If requested, put the vector magnitude above the arrow.
C
      IF (NC .GT. 0 .AND. ISZ .EQ. 0) THEN
C
C Figure circle location differently
C
         IF (ICR .EQ. 1) THEN
            RMG = 0.5 * WBCF*VLL
            PHI = 0.0
            IF (NC .EQ. 1) THEN
               XC = XB
            ELSE
               XC = XB - FLBS*FW2W
            END IF
            YC = YB - RMG - 1.25*FLBS*FW2W
         ELSE
            RMG = 0.001
            PHI = ATAN2(DY,DX)
            ISN = -1
            IF (MOD(PHI+P5D2PI,P2XPI) .GT. P1XPI) THEN
               PHI = PHI+P1XPI
               ISN = 1
            END IF
            IF (WBAD .LT. 0.0) THEN
               ISN = -ISN
            END IF
            XC = AXB+0.33*(AXE-AXB) + ISN *
     +           (1.25*FLBS*FW2W+RMG)*COS(PHI+P1D2PI)
            YC = AYB+0.33*(AYE-AYB) + ISN *
     +           (1.25*FLBS*FW2W+RMG)*SIN(PHI+P1D2PI)
         END IF
C
         CALL PLCHLQ(XC,YC,LBL(1:NC),FLBS*FW2W,PRTOD*PHI,0.0)
C
      END IF
C
C Restore original transformation
C
      CALL GSWN(NTR,WIN(1),WIN(2),WIN(3),WIN(4))
C
      RETURN
      END
C
      SUBROUTINE VVINWB (IHM)
C
C IHM      -  Hemisphere (1 - North, -1 - South)
C
C Initializes wind barb specific common block values.
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
      PARAMETER (D2RAD=.017453293)
C
C
C Fill in the reference arrow data
C
c$$$C     parameter WBA wind barb angle
c$$$      WBAD = 62.0
c$$$C     parameter WBS wind barb tic size (fraction of barb length)
c$$$      WBTF = 0.33 
c$$$C     parameter WBR wind barb radius (fraction of bl)
c$$$      WBCF = 0.25
c$$$C     parameter WBD wind barb distance between ticks (fraction of bl)
c$$$      WBDF = 0.1
c$$$C     parameter WBC wind barb scale factor 
c$$$      WBSC = 1.0
C
C Make sure this does nothing if the style is not wind barb.
C
      IF (IAST .LT. 2) THEN
         RETURN
      END IF
         
      RAG = IHM * WBAD * D2RAD
C
C Use the first four elements of the fill arrow array to store the 
C pennant triangle. Elements 2 and 3 work for the barb line.
C Four and five are used for the half barb line.
C
      AROX(1) = -COS(RAG) * WBTF * DVMX
      AROY(1) = 0.0
      AROX(2) = AROX(1)
      AROY(2) = SIN(RAG) * WBTF * DVMX
      AROX(3) = 0.0
      AROY(3) = 0.0
      AROX(4) = 0.0
      AROY(4) = AROX(1)
      AROX(5) = -0.5 * COS(RAG) * WBTF * DVMX
      AROY(5) = 0.5 * SIN(RAG) * WBTF * DVMX
      AROX(6) = 0.0
      AROY(6) = 0.0
C
      RETURN
      END

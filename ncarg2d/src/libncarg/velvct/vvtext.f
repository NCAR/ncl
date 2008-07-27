C
C       $Id: vvtext.f,v 1.15 2008-07-27 00:17:35 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
C This module contains four subroutines for text support of the
C VELVCT utility. 
C The first, VVARTX, is a relatively high level routine
C that outputs a horizontally aligned vector arrow, with a numerical
C string presumably specifying the vector magnitude above and an
C informational label below. It is designed to allow a message
C showing the maximum and/or minimum vector contained the plot
C to be displayed. If the vectors are colored by vector magnitude
C then the vector is drawn using its designated color.
C
C The other modules are lowel level support routines:
C
C VVTXLN determines the first and last non-blank characters of an
C        arbitrary text string
C
C VVTXIQ returns the height and width of piece of text intended for
C        output using the PLOTCHAR routine PLCHHQ. 
C
C VVTXPO allows for 9 way justification of a text string using a
C        position mode parameter compatible with the CONPACK text 
C        positioning method. Given the position mode, position,
C        width and height of the text in the fractional system,
C        a position in user coordinates is returned for use with
C        the PLCHHQ routine assuming the CNTR parameter is set to
C        0.0.
C 
C Note that these routines as yet have no support for non-horizontal
C text. 
C
C --------------------------------------------------------------------
C
      SUBROUTINE VVARTX(CTX,IPO,FWX,FWY,TSZ,ITC,VMG,ASZ)
C
      CHARACTER*(*) CTX
C
C Writes an informational label consisting of a horizontal vector
C with the size over it, and a text string describing the
C vector underneath
C
C Input parameters:
C
C CTX - the character string
C IPO - the positioning mode for the informational box as a whole
C FWX,FWY - the reference position as a fraction of the viewport
C TSZ - the text size as a fraction of the viewport
C ITC - the text color
C VMG - the vector magnitude 
C ASZ - the arrow size in the fractional system
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
      CHARACTER*10 LBL
      EXTERNAL VVDUMB
C
C Space height factors -- if the vector is large in comparison with
C the text the space is calculated as a fraction (PSPFC1) of the 
C text size.
C Otherwise, (and preferably, because the minimum and maximum labels
C will align properly), the space height is calculated as half the
C remainder when the vector height is subtracted from a multiple
C (PSPFC2) of the text height.
C
      PARAMETER (PSPFC1=0.5,PSPFC2=2.0)
      REAL IAR(4)
      DATA IDM / 0 /
C
C --------------------------------------------------------------------
C
C Turn off clipping because this label is likely to be outside
C the current viewport
C
      CALL GQCLIP(IER,ICL,IAR)
      CALL GSCLIP(0)
C
C Save and reset the SET transformation to an identity (for safety)
C     
      CALL GETSET(VPL,VPR,VPB,VPT,WDL,WDR,WDB,WDT,ILG)
      CALL SET(0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,1)
C
C Convert fraction of viewport values into fractional system
C coordinates
C      
      XF = XVPL + FWX * FW2W
      YF = YVPB + FWY * FH2H
C
C Write out the label and compute its height and width
C
      WRITE(LBL,'(E10.3)')VMG
      CALL VVTXLN(LBL,10,LB,LE)
      CALL VVTXIQ(LBL(LB:LE),TSZ*FW2W,WL,HL)
C
C We know the width of the arrow, compute its height
C If less than or equal to 0.0 it will not be drawn unless using
C wind barbs
C
C Determine the height of the arrow by calling the drawing routine
C using a negative value of the vector len
C
      WA = ASZ
      UVMG = VMG
      IF (WA .GT. 0.0) THEN
         IF (IAST .EQ. 0) THEN
            CALL VVDRAW(0.5,0.5,0.5+WA,0.5,-WA,LBL,0,IDM,VVDUMB,0)
         ELSE IF (IAST .EQ. 1) THEN
            CALL VVDRFL(0.5,0.5,0.5+WA,0.5,-WA,LBL,0,IDM,VVDUMB,0)
         ELSE
            CALL VVDRWB(0.5,0.5,0.5+WA,0.5,-WA,LBL,0,IDM,VVDUMB,0)
         END IF
      END IF
      IF (WA .LE. 0.0) THEN
         HA = 0.0
         YAJ = 0.0
      ELSE
         HA = FYSZ
         YAJ = ((AYMX - 0.5) - (0.5 - AYMN)) / 2.0
      END IF
C
C Now get the size of the vector text string
C
      CALL VVTXLN(CTX,IPCHSZ,IB,IE)
      CALL VVTXIQ(CTX(IB:IE),TSZ*FW2W,WT,HT)
C
C Compute a space height based on the text height and arrow height
C
      HS = MAX(PSPFC1 * HT, (PSPFC2*HT - HA) / 2.0) 
C
C Compute the total size of the info label
C Two space heights are added
C
      WTT = MAX(WT,WA,WL)
      HTT = HL + HA + 2*HS + HT
C
C Adjust the input position so that we can effectively
C treat it as lower left.
C
      IF (IPO.LE.-4) THEN
         XFA=XF
         YFA=YF
      ELSE IF (IPO.EQ.-3) THEN
         XFA=XF-0.5*WTT
         YFA=YF
      ELSE IF (IPO.EQ.-2) THEN
         XFA=XF-WTT
         YFA=YF
      ELSE IF (IPO.EQ.-1) THEN
         XFA=XF
         YFA=YF-0.5*HTT
      ELSE IF (IPO.EQ.0) THEN
         XFA=XF-0.5*WTT
         YFA=YF-0.5*HTT
      ELSE IF (IPO.EQ.1) THEN
         XFA=XF-WTT
         YFA=YF-0.5*HTT
      ELSE IF (IPO.EQ.2) THEN
         XFA=XF
         YFA=YF-HTT
      ELSE IF (IPO.EQ.3) THEN
         XFA=XF-0.5*WTT
         YFA=YF-HTT
      ELSE IF (IPO.GE.4) THEN
         XFA=XF-WTT
         YFA=YF-HTT
      END IF
C
C Position each piece of output and draw it
C
      CXT = XFA + 0.5*WTT
      CYT = YFA + 0.5*HT
      CXA = CXT
      CYA = YFA + HT + HS + 0.5*HA - YAJ
      CXL = CXT
      CYL = YFA + HT + 2.0*HS + HA + 0.5*HL
C
C Save the current polyline and text colors
C
      CALL GQTXCI(IER,IOT)
      CALL GQPLCI(IER,IOC)
      CALL GQFAIS(IER,IOF)
      CALL GQFACI(IER,IOK)
      IF (IAST .EQ. 2) THEN
         CALL GSFACI(IOC)
      END IF
C
C If the vectors are colored by magnitude then find the correct color
C Set the text color too if required.
C
      IF (ABS(ICTV).EQ.1) THEN
         DO 100 K=1,NLVL,1
            IF (VMG .LE. TVLU(K) .OR. K.EQ.NLVL) THEN
               IF (IAST .EQ. 0) THEN
                  CALL GSPLCI(ICLR(K))
               ELSE IF (IAST .EQ. 1) THEN
                  IF (IACM .EQ. -1 .OR. IACM .GE. 1) THEN
                     CALL GSPLCI(ICLR(K))
                  END IF
                  IF (IACM .EQ. 0 .OR. ABS(IACM) .GE. 2) THEN
                     CALL GSFACI(ICLR(K))
                  END IF
               ELSE
                     CALL GSPLCI(ICLR(K))
                     CALL GSFACI(ICLR(K))
               END IF
C
               IF (ITC .EQ. -2) CALL GSTXCI(ICLR(K))
C
               GO TO 101
            END IF
 100     CONTINUE
C     
 101     CONTINUE
      END IF
C
C Adjust the colors depending on the text block setting
C
      IF (ITC .GE. 0) THEN
         CALL GSTXCI(ITC)
         CALL GSPLCI(ITC)
         IF (IAST.NE.0) CALL GSFACI(ITC)
      ELSE IF (ITC .LT. -2) THEN
         CALL  GSPLCI(IOT)
         IF (IAST.NE.0) CALL GSFACI(IOT)
      END IF
C
C Temporarily reset the vector positioning flag to ensure a 
C centered arrow; also set linewidth
C
      ISP=IVPO
      IVPO=0
      CALL GQLWSC(IER,ROW)
      CALL GSLWSC(WDLV)
      IF (WA .GT. 0.0) THEN
         IF (IAST .EQ. 0) THEN
            CALL VVDRAW(CXA,CYA,CXA+WA,CYA,WA,LBL,0,IDM,VVDUMB,0)
         ELSE IF (IAST .EQ. 1) THEN
            CALL VVDRFL(CXA,CYA,CXA+WA,CYA,WA,LBL,0,IDM,VVDUMB,0)
         ELSE
            CALL VVDRWB(CXA,CYA,CXA+WA,CYA,WA,LBL,0,IDM,VVDUMB,0)
         END IF
      END IF
C
C Restore linewidth and centering flag
C
      CALL GSLWSC(ROW)
      IVPO=ISP
      IF (ITC.EQ.-1) CALL GSPLCI(IOT)
C
      XW=CFUX(CXT)
      YW=CFUY(CYT)
      CALL PLCHHQ(XW,YW,CTX(IB:IE),TSZ*FW2W,0.0,0.0)
      CALL PLCHHQ(CFUX(CXL),CFUY(CYL),LBL(LB:LE),TSZ*FW2W,0.0,0.0)
C
      CALL GSTXCI(IOT)
      CALL GSPLCI(IOC)
      CALL GSFACI(IOK)
      CALL GSFAIS(IOF)
C
C Restore clipping and the set transformation.
C
      CALL GSCLIP(ICL)
      CALL SET(VPL,VPR,VPB,VPT,WDL,WDR,WDB,WDT,ILG)
C
C Done
C
      RETURN
      END
C
C --------------------------------------------------------------------
C
      SUBROUTINE VVTXLN(CTX,MXL,IB,IE)
C
      CHARACTER*(*) CTX
C
C Input parameters:
C
C CTX - the text string
C MXL - the maximum length of the text string
C
C Output parameters:
C
C IB,IE - integer position of the first and last non-blank
C         characters in the text string
C
C ------------------------------------------------------------------
C
C Given a text string, finds the first and last non-blank characters
C
      DO 10 I=1,MXL,1
         IF (CTX(I:I) .NE. ' ') GO TO 11
 10   CONTINUE
 11   CONTINUE
      IB=I
C
      DO 20 I=MXL,1,-1
         IF (CTX(I:I) .NE. ' ') GO TO 21
 20   CONTINUE
 21   CONTINUE
      IE=I
C
C Done
C
      RETURN
      END
C
C --------------------------------------------------------------------
C
      SUBROUTINE VVTXIQ(CTX,SIZ,W,H)
C
      CHARACTER*(*) CTX
C
C Find the height and width of a piece of text in fractional system.
C
C Input parameters:
C
C CTX - Text string
C SIZ - Text size in fractional coordinates
C
C Output parameters:
C
C H,W - height and width of text
C
C Local variables:
C
C VPL,VPR,VPB,VPT,WDL,WDR,WDB,WDT,ILG  - saved coordinate system state
C ITE                                  - saved text extent
C DL,DR,DB,DT                          - returned text metrics
C
C --------------------------------------------------------------------
C
C Temporarily reset the window so the window coordinates
C are guaranteed to be uniform
C
      CALL GETSET(VPL,VPR,VPB,VPT,WDL,WDR,WDB,WDT,ILG)
      CALL SET(0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,1)
C
C Save then set the text extent inquiry PLOTCHAR parameter
C
      CALL PCGETI('TE - Text extent computation', ITE)
      CALL PCSETI('TE - Text extent computation', 1)
C
C Call PLCHHQ in inquiry mode
C
      CALL PLCHHQ(0.5,0.5,CTX,SIZ,360.0,0.0)
C
C Get the results
C
      CALL PCGETR('DL - left distance', DL)
      CALL PCGETR('DR - right distance', DR)
      CALL PCGETR('DB - bottom distance', DB)
      CALL PCGETR('DT - top distance', DT)
C
      W = DR+DL
      H = DT+DB
C
C Restore state
C
      CALL SET(VPL,VPR,VPB,VPT,WDL,WDR,WDB,WDT,ILG)
      CALL PCSETI('TE - Text extent computation', ITE)
C
C Done
C
      RETURN
      END
C
C --------------------------------------------------------------------
C
      SUBROUTINE VVTXPO(IPO,FXI,FYI,W,H,XW,YW)
C
C Given the a position mode, location, and width and height of a
C horizontally aligned text box in the fractional coordinate system 
C returns the center of the text box in window coordinates.
C Angles are not yet supported.
C
C Input parameters:
C
C IPO      - text positioning method a la CONPACK
C FXI, FYI - input position in the fractional system
C H,W      - height and width of text in the fractional system
C
C Output parameters:
C
C XW,YW  - position in window coordinates to use for a call 
C          to PLCHHQ, assuming CNTR is set to 0.0
C
C --------------------------------------------------------------------
C
C Calculate an adjusted position based on the positioning mode
C
      IF (IPO .LE. -4) THEN
         FXA=FXI+0.5*W
         FYA=FYI+0.5*H
      ELSE IF (IPO .EQ. -3) THEN
         FXA=FXI
         FYA=FYI+0.5*H
      ELSE IF (IPO .EQ. -2) THEN
         FXA=FXI-0.5*W
         FYA=FYI+0.5*H
      ELSE IF (IPO .EQ. -1) THEN
         FXA=FXI+0.5*W
         FYA=FYI
      ELSE IF (IPO .EQ. 0) THEN
         FXA=FXI
         FYA=FYI
      ELSE IF (IPO .EQ. 1) THEN
         FXA=FXI-0.5*W
         FYA=FYI
      ELSE IF (IPO .EQ. 2) THEN
         FXA=FXI+0.5*W
         FYA=FYI-0.5*H
      ELSE IF (IPO .EQ. 3) THEN
         FXA=FXI
         FYA=FYI-0.5*H
      ELSE IF (IPO .GE. 4) THEN
         FXA=FXI-0.5*W
         FYA=FYI-0.5*H
      END IF
C
      XW=CFUX(FXA)
      YW=CFUY(FYA)
C
C Done
C
      RETURN
      END
C
      SUBROUTINE VVDUMB()
      RETURN
      END

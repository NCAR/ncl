C
C       $Id: vvectr.f,v 1.23 2008-07-27 00:17:35 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE VVECTR (U,V,P,IAM,VVUDMV,WRK)
C
C Argument dimensions
C
      DIMENSION U(IUD1,*), V(IVD1,*), P(IPD1,*)
C
      DIMENSION WRK(*),IAM(*)
C
      EXTERNAL VVUDMV
C
C Input parameters
C
C U,V    - 2-d arrays holding the component values of a vector field
C P      - A 2-d array containing a scalar data field. The contents
C          of this array may be used to color the vectors 
C IAM    - Area mask array
C VVUDMV - User modifiable masked vector drawing function
C WRK    - work array (currently unused)
C
C Output parameters:
C
C None
C
C PURPOSE                VVECTR draws a representation of a two-
C                        dimensional velocity field by drawing arrows
C                        from each data location.  The length of the
C                        arrow is proportional to the strength of the
C                        field at that location and the direction of
C                        the arrow indicates the direction of the flow
C                        at that location.
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
C Local variable dimensions
C
      PARAMETER (IPLBSZ=10)
      CHARACTER*(IPLBSZ)LBL
      REAL IAR(4)
C
C Local variables
C
C
C The following status and count variables are used to gather
C statistics that are not currently available to the user
C
C IST - Status flag returned from the mapping routine
C ISC - Count of vectors rejected by the mapping routine
C ICT - Count of vector actually plotted
C MXO - Count of vectors rejected because magnitude > maximum
C MNO - Count of vectors rejected because magnitude < minimum
C
C Variables relating to the vector magnitude label
C
C LBL - Character string to hold the vector magnitude label
C NC - Number of characters in the vector magnitude label
C IDP - Local decimal flag for the ENCD routine
C ASH - Scale factor for the vector magnitude label
C
C Zero-field processing and label
C
C IZF - Zero field flag, set TRUE if no vectors are plotted
C XF,YF - fractional length of Zero field string
C IB,IE - beginning and end characters of the string
C W,H   - width and height of the string in fractional coordinates
C XW,YW - position of the string in window coordinates
C
C Vector length adjustment
C
C RAT - Temporary ratio variable
C VA  - adjusted length of current vector
C RA  - ratio of adjusted length to current length
C SMN,SMX - saved value of DVMN and DVMX so they can be restored
C
C Other variables
C
C IOC - the old (saved) color
C IOW - the old (saved) linewidth
C IDA - Do area masking flag
C VMN - The minimum vector size actually plotted (in frac coords)
C VMX - The maximum vector size actually plotted (in frac coords)
C I,J - loop indices for traversing the vector arrays
C K   - loop index for traversing the threshold values
C UI,VI - local copies of the current vector values
C XB,XE,YB,XE - the beginning/ending points of the vector in 
C               the fractional system
C X,Y - mapping of the array indices to a coordinate system
C VLN - length of the current vector in fractional coordinates
C XGV,YGV - X and Y grid value, the scaled distance between each
C           array grid point
C VPL,VPR,VPB,VPT,WDL,WDR,WDB,WDT,ILG - Saved SET call values
C ICI - NGDOT state 'CT' 1 or 0
C IER,ICL,IAR - Clip query values
C 
C ---------------------------------------------------------------------
C
C Check for valid area map and area group overflow if masking is enabled
C
      IF (IMSK.GT.0) THEN
         IF (IAM(7).GT.IPAGMX) THEN
            CSTR(1:29)='VVECTR - TOO MANY AREA GROUPS'
            CALL SETER (CSTR(1:29),1,1)
            RETURN
         END IF
         IF (IAM(7).LE.0) THEN
            CSTR(1:25)='VVECTR - INVALID AREA MAP'
            CALL SETER (CSTR(1:29),2,1)
            RETURN
         END IF
      END IF
C
C Initialize local variables
C
      NC  = 0
      ICT = 0
      IVC = 0
      ISC = 0
      IZC = 0
      ITH = 0
      MXO = 0
      MNO = 0
      IDA = IMSK
      VMN = RBIG
      VMX = 0.0
      IZF = 1
      SMN=DVMN
      SMX=DVMX
C 
C Save the current color and linewidth, then set the vector
C linewidth. Color must be set on a per vector basis within the 
C main loop. Label text color is set here if a single color is
C specified for all labels. 
C
      CALL NGGETI('CT',ICI)
      CALL GQPLCI(IER,IOC)
      CALL GQTXCI(IER,IOT)
      CALL GQFAIS(IER,IOF)
      CALL GQFACI(IER,IOK)
      CALL GQLWSC(IER,ROW)
      CALL GSLWSC(WDLV)
      IF (ILBC .GE. 0) THEN
         CALL GSTXCI(ILBC)
      END IF
      IF (IAST.NE.0) THEN
         CALL GSFAIS(1)
      END IF
C
C If there are no drawable vectors skip the main loop
C
      IF (UVMX .LE. 0.0) THEN
         IZC=NXCT*NYCT
         DVMX=0.0
         DVMN=0.0
         VMN=0.0
         VMX=0.0
         VFR=0.0
         DRL=0.0
         IAV=0
         GOTO 9800
      END IF
C
C Initialize variables (both local and common block values) that 
C control the mapping between vector magnitude and the realized 
C vector length. 
C
      CALL VVILNS(DRL,VFR,IAV)
C
      IF (DVMX .GT. 2.0*(XVPR - XVPL)) THEN
         CSTR(1:36)='VVECTR - VECTOR NDC LENGTH TOO GREAT'
         CALL SETER (CSTR(1:36),3,1)
         RETURN
      END IF
C
C If using filled arrows initialize the fill arrow data
C For wind barbs initialize data, set up for calling NGDOTS, and
C set the fill color the same as the line color
C
      IF (IAST.EQ.1) THEN
         CALL VVINFA
      ELSE IF (IAST.GE.2) THEN
         CALL NGSETI('CT',1)
         CALL GSFACI(IOC)
         CALL VVINWB(1)
      END IF
C
C Set the scaling for the optional vector labels
C
      IDP = IDPF
      IF (UVMN.NE.0.0 .AND. (ABS(UVMN).LT.0.1 .OR. ABS(UVMN).GE.1.E5))
     +    IDP = 1
      IF (UVMX.NE.0.0 .AND. (ABS(UVMX).LT.0.1 .OR. ABS(UVMX).GE.1.E5))
     +    IDP = 1
      ASH = 1.0
      IF (IDP .NE. 0) ASH =
     +     10.**(3-INT(ALOG10(MAX(ABS(UVMN),ABS(UVMX)))-500.)-500)
C
C If thinning is in effect, set up the thinning arrays
C
      IV=IXDM*IYDN+1
      IF (RVMD.GT.0.0) THEN
         CALL VVTHIN(U,V,P,WRK(1),WRK(IV))
      END IF
C
C Calculate the grid interval represented by adjacent array
C elements along each axis
C
      XGV=(XHIV-XLOV)/REAL(MAX(1,IXDM-1))
      YGV=(YHIV-YLOV)/REAL(MAX(1,IYDN-1))
C
C Draw the vectors. Note the extra processing if there are special 
C values to consider or the independent scalar array is processed.
C
      DO 201 J=1,IYDN,IYIN
         DO 200 I=1,IXDM,IXIN
C
            UI = U(I,J)
            VI = V(I,J)
C
C If thinning remove thinned out vectors
C
            IF (RVMD.GT.0.0) THEN
               CALL VVTHND(I,J,WRK(1),IS)
               IF (IS.EQ.1) GO TO 194
            END IF
C
C Cull out special values
C
            IF (ISVF .GT. 0) THEN
               IF (UI .EQ. UUSV) THEN
                  IF (ISVF .EQ. 1 .OR. ISVF .EQ. 3) GO TO 199
                  IF (VI .EQ. UVSV .AND. ISVF .EQ. 4) GO TO 199
               ELSE IF (VI .EQ. UVSV) THEN
                  IF (ISVF .EQ. 2 .OR. ISVF .EQ. 3) GO TO 199
               END IF
            END IF
C
C Calculate the vector magnitude or if the polar flag is set
C compute the cartesian component values
C
            IF (IPLR .LE. 0) THEN
               UVMG = SQRT(UI*UI+VI*VI)
            ELSE
               UVMG = ABS(UI)
               IF (IPLR .EQ. 1) VI = PDTOR * VI
               UI = UVMG * COS(VI)
               VI = UVMG * SIN(VI)
            END IF
C
C Bypass vectors that fall outside the user-specified range.
C
            IF (VLOM .NE. 0.0 .AND. UVMG .LT. UVMN) GO TO 196
C
            IF (VHIM .NE. 0.0 .AND. UVMG .GT. UVMX) GO TO 197
C
C Eliminate zero vectors unless using wind barbs
C
            IF (UVMG .EQ. 0.0 .AND. IAST .LT. 2) GO TO 198
C
C If using a scalar array, check for special values in the array, 
C then determine the color to use for the vector
C
            IF (ABS(ICTV) .GE. 2) THEN
C
               IF (ISPC .EQ. 0 .AND. P(I,J) .EQ. UPSV) THEN
                  GO TO 199
               ELSE IF (ISPC .GT. 0 .AND. P(I,J) .EQ. UPSV) THEN
                  IF (IAST .EQ. 0) THEN
                     CALL GSPLCI(ISPC)
                  ELSE IF (IAST .EQ. 1) THEN
                     IF (IACM .EQ. -1 .OR. IACM .GE. 1) THEN
                        CALL GSPLCI(ISPC)
                     END IF
                     IF (IACM .EQ. 0 .OR. ABS(IACM) .GE. 2) THEN
                        CALL GSFACI(ISPC)
                     END IF
                  ELSE
                     CALL GSPLCI(ISPC)
                     CALL GSFACI(ISPC)
                  END IF
                  GO TO 129
               END IF
C
               DO 128 K=1,NLVL,1
                  IF (P(I,J).LE.TVLU(K) .OR. K.EQ.NLVL) THEN
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
                     IF (ILBC .EQ. -1) THEN
                        CALL GSTXCI(ICLR(K))
                     END IF
                     GO TO 129
                  END IF
 128           CONTINUE
C
 129           CONTINUE
C               
            ELSE IF (ICTV .NE. 0) THEN
C
C If coloring based on vector magnitude, figure out the color
C
               DO 130 K=1,NLVL,1
                  IF (UVMG.LE.TVLU(K) .OR. K.EQ.NLVL) THEN
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
                     IF (ILBC .EQ. -1) THEN
                        CALL GSTXCI(ICLR(K))
                     END IF
                     GO TO 131
                  END IF
 130           CONTINUE
C
 131           CONTINUE
C
            END IF
C
C Map the vector. If the compatiblity flag is set use the 
C compatibility subroutine.
C
            IF (ICPM .GT. 0) THEN
C
               CALL VVFCPM(I,J,UI,VI,UVMG,XB,YB,XE,YE,IST)
               IF (IST .NE. 0 .AND. IST .NE. -999) GO TO 195
C
            ELSE
C
               X=XLOV+REAL(I-1)*XGV
               Y=YLOV+REAL(J-1)*YGV
               CALL HLUVVMPXY(X,Y,UI,VI,UVMG,XB,YB,XE,YE,IST)
               IF (IST .NE. 0 .AND. IST .NE. -999) GO TO 195
C
            END IF
C
            IF (IAST .GE. 2 .AND. IST .EQ. -999) THEN
               VLN = DVMX
            ELSE
               VLN = SQRT((XE-XB)*(XE-XB)+(YE-YB)*(YE-YB))
               IF (VLN .EQ. 0.0) GO TO 198
C
C Adjust the vector length in proportion to the difference between
C the minimum and maximum display vector magnitudes
C
               IF (IAV.NE.0) THEN
                  VA = VFR+(DVMX - VFR)*(UVMG - UVMN) /(UVMX - UVMN)
                  RA = VA / VLN
                  XE = XB + RA *(XE-XB)
                  YE = YB + RA *(YE-YB)
                  VLN = VA
               END IF
            END IF
C
C Track the minimum/maximum displayed values
C
            IF (UVMG .LT. VMN) VMN=UVMG
            IF (UVMG .GT. VMX) VMX=UVMG
C
C Turn zero field flag off; encode the number if a label is to
C be drawn
C
            IZF = 0
            IF (ILBL .NE. 0) CALL ENCD(UVMG,ASH,LBL,NC,IDP)
C
C Draw the vector
C
            IF (IAST .EQ. 0) THEN
               CALL VVDRAW (XB,YB,XE,YE,VLN,LBL,NC,IAM,VVUDMV,IDA)
            ELSE IF (IAST .EQ. 1) THEN
               CALL VVDRFL (XB,YB,XE,YE,VLN,LBL,NC,IAM,VVUDMV,IDA)
            ELSE
               CALL VVDRWB (XB,YB,XE,YE,VLN,LBL,NC,IAM,VVUDMV,IDA)
            END IF
C
C Statistical data:
C
C Vectors plotted
C
            ICT=ICT + 1
            GOTO 200
C
 194        CONTINUE
C
C Vectors culled out by thinning algorithm
C
            ITH=ITH+1
            GO TO 200
C
 195        CONTINUE
C
C Vectors rejected by mapping routine
C
            ISC=ISC+1
            GO TO 200
C
 196        CONTINUE
C
C Vectors under minimum magnitude
C
            MNO=MNO+1
            GO TO 200
C
 197        CONTINUE
C
C Vectors over maximum magnitude
C
            MXO=MXO + 1
            GO TO 200
C
C Zero length vectors cannot be drawn even if UVMN is 0.0, but
C need to be treated as if they were drawn.
C
 198        CONTINUE
C
            IF (UVMG .LT. VMN) VMN=UVMG
            IZC=IZC + 1
            GO TO 200
C
C Special values
C
 199        CONTINUE
            IVC = IVC+1
C
 200     CONTINUE
 201  CONTINUE
C
C End of main loop.
C
 9800 CONTINUE
C
C Plot statistics
C
      IF (IVST .EQ. 1) THEN
         LUN=I1MACH(2)
         WRITE(LUN,*) 'VVECTR Statistics'
         WRITE(LUN,*) '                    Vectors plotted:',ICT
         WRITE(LUN,*) 'Vectors rejected by mapping routine:',ISC
         WRITE(LUN,*) '    Vectors under minimum magnitude:',MNO
         WRITE(LUN,*) '     Vectors over maximum magnitude:',MXO
         WRITE(LUN,*) '          Other zero length vectors:',IZC
         WRITE(LUN,*) '            Rejected special values:',IVC
         IF (RVMD.GT.0) THEN
            WRITE(LUN,*) '     Vectors below minimum distance:',ITH
         END IF
         WRITE(LUN,*) '   Minimum plotted vector magnitude:',VMN
         WRITE(LUN,*) '   Maximum plotted vector magnitude:',VMX
         IF (ABS(ICTV).GE.2) THEN
            WRITE(LUN,*) '               Minimum scalar value:',PMIN
            WRITE(LUN,*) '               Maximum scalar value:',PMAX
         END IF
         WRITE(LUN,*) ' '
      END IF
C
C Reset attributes
C
      CALL GSPLCI(IOC)
      CALL GSLWSC(ROW)
      CALL GSTXCI(IOT)
      CALL GSFACI(IOK)
      CALL GSFAIS(IOF)
      CALL NGSETI('CT', ICI)
C
C Set the read-only min/max vector sizes to reflect the vectors
C actually drawn
C
      IF (IAV.EQ.0) THEN
         RDMN=VMN*SXDC
      ELSE
         RDMN = VFR+(DVMX - VFR)*(VMN - UVMN) /(UVMX - UVMN)
      END IF
      RDMX=VMX*SXDC
      RVMX=VMX
      RVMN=VMN
C
C If vectors were drawn, write out the vector informational text if 
C called for, else conditionally write the zero field text.
C The size printed out depends on whether absolute or relative
C size mode is in effect.
C 
      IF (IZF .EQ. 0) THEN
C
         IF (CMXT(1:1) .NE. ' ') THEN
            IF (VRMG .GT. 0.0) THEN
               CALL VVARTX(CMXT,IMXP,FMXX,FMXY,FMXS,IMXC,VRMG,DRL)
            ELSE IF (VHIM .LT. 0.0) THEN
               CALL VVARTX(CMXT,IMXP,FMXX,FMXY,FMXS,IMXC,UVMX,DVMX)
            ELSE
               CALL VVARTX(CMXT,IMXP,FMXX,FMXY,FMXS,IMXC,VMX,RDMX)
            ENDIF
         END IF
         IF (CMNT(1:1) .NE. ' ') THEN
            IF (VLOM .LT. 0.0) THEN
               CALL VVARTX(CMNT,IMNP,FMNX,FMNY,FMNS,IMNC,UVMN,DVMN)
            ELSE
               CALL VVARTX(CMNT,IMNP,FMNX,FMNY,FMNS,IMNC,VMN,RDMN)
            END IF
         END IF
C
      ELSE
C
         IF (CZFT(1:1) .NE. ' ') THEN
C
C Turn clipping off and SET to an identity transform
C
            CALL GQCLIP(IER,ICL,IAR)
            CALL GSCLIP(0)
            CALL GETSET(VPL,VPR,VPB,VPT,WDL,WDR,WDB,WDT,ILG)
            CALL SET(0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,1)
C
            XF = XVPL + FZFX * FW2W
            YF = YVPB + FZFY * FH2H
            CALL VVTXLN(CZFT,IPCHSZ,IB,IE)
            CALL VVTXIQ(CZFT(IB:IE),FZFS*FW2W,W,H)
            CALL VVTXPO(IZFP,XF,YF,W,H,XW,YW)
            IF (IZFC .GE. 0) THEN
               CALL GSTXCI(IZFC)
               CALL GSPLCI(IZFC)
            ELSE
               CALL  GSPLCI(IOT)
            END IF
C      
            CALL PLCHHQ(XW,YW,CZFT(IB:IE),FZFS*FW2W,0.0,0.0)
C
            CALL GSTXCI(IOT)
            CALL GSPLCI(IOC)
C
C Restore clipping and the set transformation.
C
            CALL NGSETI('CT',ICI)
            CALL GSCLIP(ICL)
            CALL SET(VPL,VPR,VPB,VPT,WDL,WDR,WDB,WDT,ILG)
C
         END IF
C
      END IF
C
C Restore DVMN and DVMX
C
      DVMN=SMN
      DVMX=SMX
C
C Done
C
      RETURN
      END
C
C -------------------------------------------------------------------
C
      SUBROUTINE VVFCPM(I,J,UI,VI,UVM,XB,YB,XE,YE,IST)
C
C This subroutine implements the compatibility mode for
C the FX,FY,MXY,MYF functions.
C
C Input parameters:
C  
C I,J   -- array indices specifying the current grid point
C UI,VI -- u,v vector component values at the current grid point
C UVM   -- magnitude of the vector
C
C Output parameters:
C
C XB,YB -- the begin position of the vector in fractional coords
C XE,YE -- the end position of the vector in fractional coords
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
      PARAMETER (PRCFAC=1E5)
C
C Internal functions which may be modified for data transformation -
C
C
C                        FX       Returns the X index as the
C                                 X-coordinate of the vector base.
C
C                        MXF      Returns the X-coordinate of the vector
C                                 head.
C
C                        FY       Returns the Y index as the
C                                 Y-coordinate of the vector base.
C
C                        MYF      REturns the Y-coordinate of the vector
C                                 head.
C
C                        SCALEX   Computes a scale factor used in the
C                                 determination of the length of the
C                                 vector to be drawn.
C
C                        SCALEY   Identical to SCALEX in the current
C                                 implementation
C
C     FX(XX,YY) = XX
C     FY(XX,YY) = YY
C     MXF(XX,YY,UU,VV,SFXX,SFYY,MXX,MYY) = MXX+INT(SFXX*UU)
C     MYF(XX,YY,UU,VV,SFXX,SFYY,MXX,MYY) = MYY+INT(SFYY*VV)
C
      SCALEX(MM,NN,IX,IY,UVM,XX1,XX2,YY1,YY2,XX3,XX4,YY3,YY4,
     1     LENN) = LENN/UVM
      SCALEY(MM,NN,IX,IY,UVM,XX1,XX2,YY1,YY2,XX3,XX4,YY3,YY4,
     1     LENN) = SCALEX(MM,NN,IX,IY,UVM,XX1,XX2,YY1,YY2,XX3,
     2     XX4,YY3,YY4,LENN)
C
C ---------------------------------------------------------------------
C
C Clear the status flag and reset the scale factors
C
      IST = 0
C
C Check for zero magnitude
C
      IF (INT(UVM*PRCFAC+0.5) .EQ. 0) THEN
         IZO = 1
      ELSE
         IZO = 0
      END IF
C
      ILN = KFMX(DVMX)
      SFX = SCALEX(IXDM,IYDN,IXIN,IYIN,UVMX,XVPL,XVPR,YVPB,YVPT,
     +     WXMN,WXMX,WYMN,WYMX,ILN)
      SFY = SCALEY(IXDM,IYDN,IXIN,IYIN,UVMX,XVPL,XVPR,YVPB,YVPT,
     +     WXMN,WXMX,WYMN,WYMX,ILN)
C
C Use the FX,FY functions to project the vector begin position and
C FL2INT to convert to the integer metacode system
C
      X=REAL(I)
      Y=REAL(J)
      TX=FX(X,Y)
      IF (TX .LT. WXMN .OR. TX .GT. WXMX) THEN
         IST = -1
         RETURN
      END IF
      TY=FY(X,Y)
      IF (TY .LT. WYMN .OR. TY .GT. WYMX) THEN
         IST = -1
         RETURN
      END IF
      CALL FL2INT(TX,TY,MX,MY)
C
C Return special status if zero magnitude
C
         IF (IZO .EQ. 1) THEN
            XB=CMFX(MX)
            YB=CMFY(MY)
            XE = XB
            YE = YB
            IST = -999
            RETURN
         END IF
C
C Use MYX,MYF to calculate the end of the vector in metacode
C coordinates.
C     
      LX = MXF(X,Y,UI,VI,SFX,SFY,MX,MY)
      LY = MYF(X,Y,UI,VI,SFX,SFY,MX,MY)
      IF(LX .GE. IBIG .OR. LY .GE. IBIG) THEN
         IST = -1
         RETURN
      END IF
C
C Convert to fractional coordinates
C
      XB=CMFX(MX)
      YB=CMFY(MY)
      XE=CMFX(LX)
      YE=CMFY(LY)
C
C Done
C
      RETURN
      END
C
C -------------------------------------------------------------------
C
      SUBROUTINE VVILNS(DRL,VFR,IAV)
C
C This subroutine initializes a number of variables that control the
C mapping between vector magnitude and the NDC length of the arrow.
C
C Output parameters:
C
C DRL - length of reference vector in NDC
C IAV - Adjust vector length flag (based on fractional length - VFR)
C VFR - Length in fractional coordinates of the adjusted minimum
C       vector
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
C Determine the maximum vector magnitude to use. If VHIM is less than
C 0.0 then it becomes the size to use, unconditionally. Otherwise,
C It will be the minimum of the parameter VHIM (if not equal to 0.0) 
C and the maximum vector size encountered in the field, stored in UVMX.
C Do likewise with the minimum vector.
C Note that it is possible for the user to set the max or min
C such that no vectors qualify for plotting. A 'ZERO FIELD' condition
C results in this case. 
C
      DRL = DVMX
      VFR = DVMN
      IAV = 0
C
      IF (VHIM .GT. 0.0) THEN
         UVMX = MIN(VHIM, UVMX)
      ELSE IF (VHIM.LT.0.0) THEN
         UVMX = ABS(VHIM)
      END IF
C
      IF (VLOM .GT. 0.0) THEN
         UVMN = MAX(VLOM, UVMN)
      ELSE IF (VLOM.LT.0.0) THEN
         UVMN = ABS(VLOM)
      END IF
C
C Determine the length of the maximum, minimum and reference
C vector magnitudes in NDC. Once these values are calculated
C the maximum vector length only is used for scaling purposes
C in the VVMPXY routine. Its value is also calculated in user
C coordinates. This value may not be useful to VVMPXY if the 
C user coordinate system is not uniform.
C
C There are a number of ways these lengths may be determined,
C depending on whether the user has specified (1) a 
C reference magnitude, (2) a reference length, or (3) a fractional
C size for the minimum magnitude. 
C
C Note that for wind barbs the fractional size is forced to 1.0,
C which will cause all the glyphs to have the same basic length.
C 
      IF (IAST.GE.2) THEN
         VFL = 1.0
         RMG = UVMX
      ELSE
         VFL = MIN(1.0, VFRC)
         RMG = VRMG
      END IF
C
C If the field is uniform, special conditions apply
C
      IF (UVMX - UVMN .LE. 0.0) THEN
         IF (VRLN .GT. 0.0 .AND. RMG .GT. 0.0) THEN
            DRL=VRLN*FW2W
            DVMX=DRL*UVMX/RMG
         ELSE IF (RMG .GT. 0.0) THEN
            DRL=DVMX*RMG/UVMX
         ELSE IF (VRLN .GT. 0.0) THEN
            DVMX=VRLN*FW2W
            DRL=DVMX
         END IF
         VFR=DRL
         DVMN=VFR
C
C If no reference magnitude specified, the maximum magnitude
C is used as the reference magnitude
C
      ELSE IF (RMG .LE. 0.0) THEN
         IF (VRLN .GT. 0.0) THEN
            DVMX=VRLN*FW2W
         END IF
         DRL=DVMX
         IF (VFL .GT. 0.0) THEN
            IAV=1
            VFR=VFL*DVMX
            DVMN=VFR
         ELSE
            DVMN=DVMX*(UVMN/UVMX)
            VFR=DVMN
         END IF
C
C If the reference magnitude is less than the minimum magnitude,
C the fractional size is ignored if a reference length is also
C specified. Otherwise, the fractional size determines not the
C minimum magnitude length, but the reference magnitude length.
C
      ELSE IF (RMG .LE. UVMN) THEN
         IAV=1
         IF (VRLN .GT. 0.0) THEN
            DRL=VRLN*FW2W
            DVMX=DRL*UVMX/RMG
         ELSE IF (VFL .GT. 0.0) THEN
            DRL=DVMX*VFL
         ELSE
            DRL=DVMX*RMG/UVMX
         END IF
         DVMN=DRL*UVMN/RMG
         VFR=DVMN
C
C A reference magnitude is specified, as well as a fractional
C magnitude. The min magnitude length is the fractional size 
C times the reference length. If a reference length is specified, 
C it becomes the length of the reference magnitude.  The maximum
C magnitude is determined proportionally. If no reference length
C is specified, the reference length is set proportionally to
C the default size assigned to the maximum magnitude.
C
      ELSE IF (VFL .GT. 0.0) THEN
         IAV=1
         IF (VRLN .GT. 0.0) THEN
            DRL=VRLN*FW2W
            VFR=VFL*DRL
            DVMN=VFR
            DVMX=DVMN+(DRL-DVMN)*(UVMX-UVMN)/(RMG-UVMN)
         ELSE
            RAT=(RMG-UVMN)/(UVMX-UVMN)
            DRL=DVMX*RAT/(1.0-VFL+VFL*RAT)
            VFR=VFL*DRL
            DVMN=VFR
         END IF
C
C A reference magnitude is specified. If a reference length is 
C specified, it becomes the length of the reference magnitude.
C Otherwise the reference magnitude's length is proportional
C to the default length assigned to the maximum magnitude.
C The min magnitude length is then determined proportionally
C to both these values.
C
      ELSE
         IF (VRLN .GT. 0.0) THEN
            DRL=VRLN*FW2W
            DVMX=DRL*UVMX/RMG
            VFR=DRL*UVMN/RMG
            DVMN=VFR
         ELSE
            DRL=DVMX*RMG/UVMX
            VFR=DVMX*UVMN/UVMX
            DVMN=VFR
         END IF
      END IF
      RLEN=DVMX*(WXMX-WXMN)/FW2W
C
C Compute scale factors.
C
      SXDC=DVMX/UVMX
      SYDC=DVMX/UVMX
C
C Calculate the sine and cosine of the arrow head half angle
C
      HSIN = SIN(PDTOR*HANG)
      HCOS = COS(PDTOR*HANG)
C
C Done
C
      RETURN
      END

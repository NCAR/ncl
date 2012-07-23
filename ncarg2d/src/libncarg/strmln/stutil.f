C
C       $Id: stutil.f,v 1.5 2009-04-22 19:26:37 dbrown Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
C
      SUBROUTINE STARDR(XUS,YUS,XND,YND,TA,VMF,SV,IAM,STUMSL,IST)
C
C This routine draws the arrow. Calculations are in fractional
C coordinates to ensure uniform arrows irrespective of the 
C mapping in effect.
C A small fraction of the differential change is used to find the
C tangent angle at the current position. Once the angle is known the
C arrow can be drawn at a fixed size independent of the mapping
C routine currently employed.
C
C Input parameters:
C
C XUS,YUS - current position in user space
C XND,YND - current position in NDC space
C TA    - Angle in NDC
C VMF   - magnitude of vector relative to maximum magnitude
C SV    - scalar value used to determine arrowhead color
C IAM   - Area mask array
C STUMSL - User defined masked streamline drawing routine
C
C Output parameters:
C
C IST - Status code, indicates success or failure
C
      DIMENSION  IAM(*)
      EXTERNAL STUMSL
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
C ---------------------------------------------------------------------
C
C Point buffers
C
      DIMENSION AX(3), AY(3), AS(3)
C
C Local variables
C
C AX, AY   - Arrow head point buffers
C AS       - Arrow head scalar value
C DXW, DYW - Change in X,Y in window coordinates
C XF, YF   - Arrow head position in the fractional system
C DXF,DYF  - Incremental change in the fractional system
C PHI      - Tangent angle
C K        - Loop index and sign factor for each edge of the arrow
C KK       - Index for the arrow head array, within the loop
C D30      - Half the angle of the point of the arrow head (about 30 o)
C XX,YY    - Ends of the arrow in window coordinates
C
C Parameters:
C
C PHFANG - Half the angle of the arrow head (0.5 in radians is 
C          approximately equivalent to 30 degrees)
C PLWFCT - Linewidth factor, arrow size is increased by this 
C          much when the linewidth is greater than 1.0

      PARAMETER (PHFANG=0.5, PLWFCT=0.15)
C
C ---------------------------------------------------------------------
C
      IST=0
C
      AX(2) = XUS
      AY(2) = YUS
      AS(2) = SV
      FLW = 1.0 + PLWFCT*MAX(0.0,WDLV-1.0)
      SIZ = RNDA * RAFR
      SIZ = SIZ + (RNDA - SIZ) * VMF
C
      DO 10 K = -1,1,2
C
C K serves as a sign determining factor; KK indexes the point array.
C
         KK=K+2
         D30 = -(P1D2PI-TA)+REAL(K)*PHFANG
         XX = +SIZ*FLW*SIN(D30)+XND
         YY = -SIZ*FLW*COS(D30)+YND
         AX(KK) = CFUX(XX)
         AY(KK) = CFUY(YY)
         AS(KK) = SV
C
 10   CONTINUE
C
      CALL STLNSG(AX,AY,AS,3,IAM,STUMSL)
C
C Done
C
      RETURN
      END
C
C ---------------------------------------------------------------------
C
      SUBROUTINE STLNSG(X,Y,P,IPC,IAM,STUMSL)
C
C This routine draws colored streamline segments for each different
C color represented in the scalar value buffer.
C If masking is in effect the area line drawing subroutine, 
C ARDRLN is called. Otherwise CURVE is invoked. 
C  
C Input parameters:
C
C X,Y - Point arrays
C P   - Scalar value array
C IPC - Number of points
C IAM   - Area mask array
C STUMSL - User-defined masked streamline drawing routine
C
      DIMENSION X(IPC), Y(IPC), P(IPC)
      DIMENSION  IAM(*)
      EXTERNAL STUMSL
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
      DIMENSION IAI(IPGRCT),IAG(IPGRCT)
      DIMENSION XO(IPNPTS), YO(IPNPTS)
      DIMENSION IC(IPNPTS)
C
      IF (ICTV .EQ. 0) THEN
         IF (IMSK.LT.1) THEN
            CALL CURVE(X,Y,IPC)
         ELSE
            CALL ARDRLN(IAM, X, Y, IPC, XO, YO, IPC, 
     +           IAI, IAG, IPGRCT, STUMSL)
         END IF
C
         RETURN
C
      END IF
C
C Colored streamlines:
C First convert the scalar values into color indexes.
C
      DO 20 I = 1,IPC,1
         IF (ISPC.GE.0 .AND. P(I).EQ.RPSV) THEN
            IC(I) = ISPC
            GO TO 20
         END IF
         DO 10 J = 1,NLVL,1
            IF (P(I).LE.TVLU(J) .OR. J.EQ.NLVL) THEN
               IC(I) = ICLR(J)
               GO TO 20
            END IF
 10      CONTINUE
 20   CONTINUE

      ICI = IC(1)
      IS = 1
      DO 30 I = 1,IPC,1
         IF (IC(I) .NE. ICI) THEN
            NPT = I - IS + 1
            IF (NPT .EQ. 1) THEN
               NPT = NPT + 1
               IF (IS .EQ. IPC) THEN
                  IS = IPC - 1
               END IF
            END IF
            CALL GSPLCI(ICI)
            IF (IMSK.LT.1) THEN
               CALL CURVE(X(IS),Y(IS),NPT)
            ELSE
               CALL ARDRLN(IAM, X(IS), Y(IS), NPT, XO, YO, IPC, 
     +              IAI, IAG, IPGRCT, STUMSL)
            END IF
            ICI = IC(I)
            IS = I
         END IF
 30   CONTINUE
C
      NPT = IPC - IS + 1
      IF (NPT .EQ. 1) THEN
         NPT = NPT + 1
         IF (IS .EQ. IPC) THEN
            IS = IPC - 1
         END IF
      END IF
      CALL GSPLCI(ICI)
      IF (IMSK.LT.1) THEN
         CALL CURVE(X(IS),Y(IS),NPT)
      ELSE
         CALL ARDRLN(IAM, X(IS), Y(IS), NPT, XO, YO, IPC, 
     +        IAI, IAG, IPGRCT, STUMSL)
      END IF
C            
C Done
C 
      RETURN
      END
C
C ---------------------------------------------------------------------
C
      SUBROUTINE STSVCK(U,V,I,J,IST)
C
      DIMENSION  U(IUD1,*), V(IVD1,*)
C
C Checks for special values in the vicinity of I,J
C
C Input parameters
C
C U,V - vector field components array
C I,J - current array position
C
C Output parameters:
C
C IST - status value, 0 if no special values in neighborhood
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
C ---------------------------------------------------------------------
C
      IST = 0
C
      IF (U(I,J).EQ.RUSV) THEN
         IST = -1
         RETURN
      ELSE IF (V(I,J).EQ.RVSV) THEN
         IST = -1
         RETURN
      END IF
C
      IF (I.EQ.IXDM .OR. J.EQ.IYDN) THEN
         RETURN
      END IF
C
      IF (U(I,J+1).EQ.RUSV) THEN
         IST = -1
      ELSE IF (U(I+1,J).EQ.RUSV) THEN
         IST = -1
      ELSE IF (U(I+1,J+1).EQ.RUSV) THEN
         IST = -1
      ELSE IF (V(I,J+1).EQ.RVSV) THEN
         IST = -1
      ELSE IF (V(I+1,J).EQ.RVSV) THEN
         IST = -1
      ELSE IF (V(I+1,J+1).EQ.RVSV) THEN
         IST = -1
      END IF
C
C Done
C
      RETURN
      END
C
C ---------------------------------------------------------------------
C
      SUBROUTINE STMPUV(UI,VI,UO,VO,IST)
C
C Maps the U,V vector component values
C
C Input parameters:
C
C UI,VI  - Input values of U,V
C
C     Output parameters:
C
C UO,VO  - Output mapped component values
C IST    - Status value
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
C Statement functions for field tranformations
C
      FU(X,Y) = X
      FV(X,Y) = Y
C
C ---------------------------------------------------------------------
C
      IST = 0
C
C Input array polar mode
C
      IF (IPLR .LT. 1) THEN
         UT=UI
         VT=VI
      ELSE IF (IPLR .EQ. 1) THEN
         UT = UI*COS(PDTOR*VI)
         VT = UI*SIN(PDTOR*VI)
      ELSE IF (IPLR .GT. 1) THEN
         UT = UI*COS(VI)
         VT = UI*SIN(VI)
      END IF
C
C Allow mapping using FU,FV functions
C
      UO = FU(UT,VT)
      VO = FV(UT,VT)
C
C Done
C
      RETURN
      END
C
C ---------------------------------------------------------------------
C
      SUBROUTINE STZERO
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
      IF (CZFT(1:1) .EQ. ' ') THEN
         RETURN
      END IF
C
      CALL GQPLCI(IER,IOC)
      CALL GQTXCI(IER,IOT)
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
C     Restore clipping and the set transformation.
C     
      CALL GSCLIP(ICL)
      CALL SET(VPL,VPR,VPB,VPT,WDL,WDR,WDB,WDT,ILG)
C
C Done
C
      RETURN
      END
C
C ---------------------------------------------------------------------
C
      SUBROUTINE STITSV(PS,I,J,X,Y,DP)
C
C Input parameters:
C
C PS     - array containing scalar field values
C I,J    - the current grid indices
C X,Y    - the X,Y position relative to the grid
C
C Output parameters:
C
C DP     - Interpolated value of the scalar field value at the 
C          specified point
C
C Interpolation routine to calculate the interpolated scalar value.
C The philosphy here is to utilize as many points as possible
C (within reason) in order to obtain a pleasing and accurate plot.
C Interpolation schemes desired by other users may easily be
C substituted if desired.
C
      DIMENSION PS(IPD1,*)
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
C FDLI  - Double linear interpolation formula
C FBESL - Bessel 16 pt interpolation formula ( most used formula )
C FQUAD - Quadratic interpolation formula
C
      FDLI(Z,Z1,Z2,Z3,DX,DY) = (1.-DX)*((1.-DY)*Z +DY*Z1)
     +                         +     DX *((1.-DY)*Z2+DY*Z3)
      FBESL(Z,ZP1,ZP2,ZM1,DZ)=Z+DZ*(ZP1-Z+0.25*(DZ-1.)*((ZP2-ZP1-Z+ZM1)
     +                        +0.666667*(DZ-0.5)*(ZP2-3.*ZP1+3.*Z-ZM1)))
      FQUAD(Z,ZP1,ZM1,DZ)=Z+0.5*DZ*(ZP1-ZM1+DZ*(ZP1-2.*Z+ZM1))
C
C ---------------------------------------------------------------------
C
      DX = X-AINT(X)
      DY = Y-AINT(Y)
      ITF = 1
      IM1 = I-1
      IP2 = I+2
C
C Determine which interpolation formula to use 
C depending on I,J location or the special flags
C
      IF (I.GE.IXDM .OR. J.GE.IYDN) THEN
C
C This branch should never be taken if STDRAW is correct, but is 
C included for safety
C
         RETURN
C
      ELSE IF(ISPC.GE.0) THEN
         ITF = 1
C
C Check for special values in any of the interpolation cells
C and return the special value if necessary.
C
         IF (PS(I,J).EQ.RPSV .OR. PS(I,J+1).EQ.RPSV
     +        .OR. PS(I+1,J).EQ.RPSV .OR. PS(I+1,J+1).EQ.RPSV) THEN
            DP = RPSV
            RETURN
         END IF
      ELSE IF (ITRP.NE.0 .OR. ISVF.NE.0) THEN
         ITF = 1
      ELSE IF (J.GT.IYD1 .AND. J.LT.IYM1 
     +        .AND. I.GT.IXD1 .AND. I.LT.IXM1) THEN
         ITF = 2
      ELSE IF (J.EQ.IYM1 .AND. I.GT.IXD1 .AND. I.LT.IXM1) THEN
         ITF = 3
      ELSE IF (J.EQ.IYD1) THEN
         ITF = 1
      ELSE IF (ICYK.NE.1) THEN
         IF (I.EQ.IXD1) THEN
            ITF = 1
         ELSE IF (I.EQ.IXM1) THEN
            ITF = 4
         END IF
      ELSE IF (I.EQ.IXD1 .AND. J.LT.IYM1) THEN 
         IM1 = IXM1
         ITF = 2
      ELSE IF (I.EQ.IXM1 .AND. J.LT.IYM1) THEN
         IP2 = IXD1+1
         ITF = 2
      ELSE IF (J.EQ.IYM1 .AND. I.EQ.IXD1) THEN
         IM1 = IXM1
         ITF = 3
      ELSE IF (J.EQ.IYM1 .AND. I.EQ.IXM1) THEN
         IP2 = IXD1+1
         ITF = 3
      END IF
C
      IF (ITF .EQ. 1) THEN
C
C Double linear interpolation formula. This scheme works at all points
C but the resulting streamlines are not as pleasing as those drawn
C by FBESL or FQUAD. Currently this is utilized
C only at certain boundary points or if ITRP is not equal to zero,
C or if special value processing is turned on.
C
         DP = FDLI(PS(I,J),PS(I,J+1),PS(I+1,J),PS(I+1,J+1),DX,DY)
C
      ELSE IF (ITF .EQ. 2) THEN
C
C 16 point bessel interpolation scheme.
C
         UJM1 = FBESL(PS(I,J-1),PS(I+1,J-1),PS(IP2,J-1),PS(IM1,J-1),DX)
         UJ   = FBESL(PS(I,J),PS(I+1,J),PS(IP2,J),PS(IM1,J),DX)
         UJP1 = FBESL(PS(I,J+1),PS(I+1,J+1),PS(IP2,J+1),PS(IM1,J+1),DX)
         UJP2 = FBESL(PS(I,J+2),PS(I+1,J+2),PS(IP2,J+2),PS(IM1,J+2),DX)
         DP   = FBESL(UJ,UJP1,UJP2,UJM1,DY)
C
      ELSE IF (ITF .EQ. 3) THEN
C
C 12 point interpolation scheme applicable to one row from top boundary
C
         UJM1 = FBESL(PS(I,J-1),PS(I+1,J-1),PS(IP2,J-1),PS(IM1,J-1),DX)
         UJ   = FBESL(PS(I,J),PS(I+1,J),PS(IP2,J),PS(IM1,J),DX)
         UJP1 = FBESL(PS(I,J+1),PS(I+1,J+1),PS(IP2,J+1),PS(IM1,J+1),DX)
         DP   = FQUAD(UJ,UJP1,UJM1,DY)
C
      ELSE IF (ITF .EQ. 4) THEN
C
C 9 point interpolation scheme for use in the non-cyclic case
C at I=IXM1; J > IYD1 and J <= IYM1
C
         UJP1 = FQUAD(PS(I,J+1),PS(I+1,J+1),PS(IM1,J+1),DX)
         UJ   = FQUAD(PS(I,J),PS(I+1,J),PS(IM1,J),DX)
         UJM1 = FQUAD(PS(I,J-1),PS(I+1,J-1),PS(IM1,J-1),DX)
         DP   = FQUAD(UJ,UJP1,UJM1,DY)
C
      END IF
C
C Done
C
      RETURN
      END

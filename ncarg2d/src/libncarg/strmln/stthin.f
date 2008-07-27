C
C       $Id: stthin.f,v 1.4 2008-07-27 00:17:28 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE STTHIN (U,V,P,UFR,VFR)
C
C Argument dimensions
C
      DIMENSION U(IUD1,*), V(IVD1,*), P(IPD1,*)
      DIMENSION UFR(IXDM,IYDN),VFR(IXDM,IYDN)
C
C Input parameters
C
C U,V    - 2-d arrays holding the component values of a vector field
C P  - A 2-d array containing a scalar data field. The contents
C          of this array may be used to color the vectors 
C
C Output parameters:
C
C UFR,VFR  - 2-d arrays in which are placed the NDC locations of each
C          vector. After the positions are calculated, for vectors that
C          are not to be drawn, the UFR array element is replaced with
C          the value -1.0.
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
C Local variables
C
C
C The following status and count variables are used to gather
C statistics that are not currently available to the user
C
C IST - Status flag returned from the mapping routine
C
C Vector length adjustment
C
C I,J - loop indices for traversing the vector arrays
C UI,VI - local copies of the current vector values
C UVM   - magnitude of the current vector
C XB,XE,YB,XE - the beginning/ending points of the vector in 
C               the fractional system
C X,Y - mapping of the array indices to a coordinate system
C XGV,YGV - X and Y grid value, the scaled distance between each
C           array grid point
C SMD - streamline min distance (NDC): the primary thinning parameter
C ---------------------------------------------------------------------
C
C Calculate the grid interval represented by adjacent array
C elements along each axis
C
      XGV=(XHIV-XLOV)/REAL(MAX(1,IXDM-1))
      YGV=(YHIV-YLOV)/REAL(MAX(1,IYDN-1))
C
C Calculate the vector locations.
C Vectors not to be plotted are marked with the following special codes:
C      -1.0: vector removed by thinning process
C      -2.0: special value
C      -3.0: below minimum magnitude
C      -5.0: zero-length vector
C      -6.0: rejected by mapping routine
C
      DO 201 J=1,IYDN,IYIN
         DO 200 I=1,IXDM,IXIN
C
C Cull out special values
C
            CALL STSVCK(U,V,I,J,IST)
            IF (IST .NE. 0) GO TO 199
C
            UI = U(I,J)
            VI = V(I,J)
C
C            IF (ISVF .GT. 0) THEN
C               IF (UI .EQ. RUSV .OR. VI .EQ. RVSV) GO TO 199
C            END IF
C
C Calculate the vector magnitude or if the polar flag is set
C compute the cartesian component values
C
            IF (IPLR .LE. 0) THEN
               UVM = SQRT(UI*UI+VI*VI)
            ELSE
               UVM = UI
               IF (IPLR .EQ. 1) VI = PDTOR * VI
               UI = UVM * COS(VI)
               VI = UVM * SIN(VI)
            END IF
C
C Bypass vectors that fall outside the user-specified range.
C
            IF (UVM .LT. UVMN) GO TO 196
C
            IF (UVM .LE. 0.0) GO TO 198
C
C Map the vector.
C
C
            X=XLOV+REAL(I-1)*XGV
            Y=YLOV+REAL(J-1)*YGV
            CALL HLUSTMPXY(X,Y,XUS,YUS,IST)
            IF (IST .NE. 0) GO TO 195
            XND = CUFX(XUS)
            YND = CUFY(YUS)
C
            UFR(I,J)=XND
            VFR(I,J)=YND
            GOTO 200
C
 195        CONTINUE
C
C Vectors rejected by mapping routine
C
            UFR(I,J) = -6.0
            GO TO 200
C
 196        CONTINUE
C
C Vectors under minimum magnitude
C
            UFR(I,J) = -3.0
            GO TO 200
C
C Zero length vectors cannot be drawn even if UVMN is 0.0, but
C need to be treated as if they were drawn.
C
 198        CONTINUE
C
            UFR(I,J) = -5.0
            GO TO 200
C
C Special values
C
 199        CONTINUE
            UFR(I,J) = -2.0
C
 200     CONTINUE
 201  CONTINUE
C
C End of location loop.
C
C For each vector determine the vectors nearby that are too close;
C eliminate these vectors      
C
      SMD=RSMD*FW2W
      VSQ = SMD*SMD
      DO 501 J=1,IYDN,IYIN
         DO 500 I=1,IXDM,IXIN
            IF (UFR(I,J) .LT. 0.0) GO TO 500
C
            MXI = I+IXIN
            DO 401 JJ=J,IYDN,IYIN
               IF (MXI.LE.I) THEN
                  GO TO 405
               END IF
               ICM = I
               DO 400 II=I,IXDM,IXIN
                  IF (I.EQ.II .AND. J.EQ.JJ) THEN
                     ICM = I+IXIN
                     GO TO 400
                  END IF
                  IF (UFR(II,JJ).LT.0.0) THEN
                     ICM = II + IXIN
                     GO TO 400
                  END IF
                  UT = UFR(I,J) - UFR(II,JJ)
                  VT = VFR(I,J) - VFR(II,JJ)
                  VLS = UT*UT+VT*VT
                  IF (VLS .LT. VSQ) THEN
                     UFR(II,JJ) = -1.0
                     ICM = II + IXIN
                     GO TO 400
                  ELSE 
                     MXI = ICM
                     GO TO 401
                  END IF
 400           CONTINUE
 401        CONTINUE
C
 405        CONTINUE
C
            JJMX = MIN(JJ,IYDN)
            MNI = I - 2*IXIN
            DO 411 JJ=J+IYIN,JJMX,IYIN
               IF (MNI.GE.I) THEN
                  GO TO 500
               END IF
               ICM = I - IXIN
               DO 410 II = I - IXIN,1,-IXIN
                  IF (UFR(II,JJ).EQ.-1.0) THEN
                     GO TO 411
                  END IF
                  IF (UFR(II,JJ).LT.0.0) THEN
                     ICM = II - IXIN
                     GO TO 410
                  END IF
                  UT = UFR(I,J) - UFR(II,JJ)
                  VT = VFR(I,J) - VFR(II,JJ)
                  VLS = UT*UT+VT*VT
                  IF (VLS .LT. VSQ) THEN
                     UFR(II,JJ) = -1.0
                     ICM = II - IXIN
                     GO TO 410
                  ELSE 
                     MNI = ICM
                     GO TO 411
                  END IF
 410           CONTINUE
 411        CONTINUE
C
 500     CONTINUE
 501  CONTINUE
C
C Check the edge elements backwards in case the grid wraps
C First along the left edge
C
      MXX = (IXDM-1)/IXIN * IXIN + 1
C
      DO 700 J=1,IYDN,IYIN
         IF (UFR(1,J) .LT. 0.0) THEN
            GO TO 700
         END IF
         DO 650 I=1,IXDM,IXIN
            IF (UFR(I,J) .LT. 0.0) THEN
               IF (I.EQ.MXX) GO TO 700
               GO TO 650
            END IF
            GO TO 651
 650     CONTINUE
 651     CONTINUE
C
         MNI = MXX - IXIN
         DO 601 JJ=J,IYDN,IYIN
            IF (MNI.GE.MXX) THEN
               GO TO 700
            END IF
            ICM = MXX
            DO 600 II=MXX,1,-IXIN
               IF (JJ.EQ.J) THEN
                  IF (II.EQ.1) THEN
                     GO TO 601
                  END IF
                  ICM = MXX - IXIN
               END IF
               IF (UFR(II,JJ).LT.0.0) THEN
                  ICM = II - IXIN
                  GO TO 600
               END IF
               UT = UFR(1,J) - UFR(II,JJ)
               VT = VFR(1,J) - VFR(II,JJ)
               VLS = UT*UT+VT*VT
               IF (VLS .LT. VSQ) THEN
                  ICM = II - IXIN
                  UFR(II,JJ) = -1.0
                  GO TO 600
               ELSE IF (II.GT.MNI) THEN
                  GO TO 600
               ELSE
                  MNI = ICM
                  GO TO 601
               END IF
 600        CONTINUE
 601        CONTINUE
C
 700  CONTINUE
C
      MXY = (IYDN-1)/IYIN * IYIN + 1
C
      DO 900 I=  1,IXDM,IXIN
         ITMP = 0
         DO 750 J=1,IYDN,IYIN
            IF (UFR(I,J) .LT. 0.0) THEN
               IF (J.EQ.MXY) GO TO 900
               GO TO 750
            END IF
            GO TO 751
 750     CONTINUE
 751     CONTINUE
C
         MNJ = MXY - IYIN
         DO 801 II=I,IXDM,IXIN
            IF (MNJ.GE.MXY) THEN
               GO TO 900
            END IF
            ICM = MXY
            DO 800 JJ=MXY,1,-IYIN
               IF (II.EQ.I) THEN
                  ICM = MXY - IXIN
                  IF (JJ.EQ.J) THEN
                     GO TO 800
                  END IF
               END IF   
               IF (UFR(II,JJ).LT.0.0) THEN
                  ICM = JJ - IYIN
                  GO TO 800
               END IF
               UT = UFR(I,J) - UFR(II,JJ)
               VT = VFR(I,J) - VFR(II,JJ)
               VLS = UT*UT+VT*VT
               IF (VLS .LT. VSQ) THEN
                  ICM = JJ - IYIN
                  UFR(II,JJ) = -1.0
                  GO TO 800
               ELSE IF (JJ.GE.MNJ) THEN
                  GO TO 801
               ELSE
                  MNJ = ICM
                  GO TO 801
               END IF
 800        CONTINUE
 801        CONTINUE
C
 900  CONTINUE
C
C Done
C
      RETURN
      END
C
      SUBROUTINE STTHND(I,J,UFR,IS)
C
C Argument dimensions
C
      DIMENSION UFR(IXDM,IYDN)
C
C Input parameters
C I,J    - Indexes of grid point
C UFR    - A 2-d array containing the value -1.0 for all vectors 
C          to be thinned from the plot
C Output parameters:
C
C IS     - set to 1 if vector is to be culled, 0 otherwise.
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
      IF (UFR(I,J) .LE. -1.0) THEN
         IS = 1
      ELSE
         IS = 0
      END IF
C
C Done
C
      RETURN
      END
            

C
C       $Id: stinit.f,v 1.15 2008-07-27 00:17:28 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE STINIT (U,LU,V,LV,P,LP,M,N,WRK,LW)
C
C Argument dimensions.
C
      DIMENSION       U(LU,N)    ,V(LV,N)    ,P(LP,N)
      DIMENSION       WRK(LW)
C
C Input parameters
C
C U,V   - 2-d arrays holding the component values of a vector field
C LU,LV - The first dimensions of the U and V arrays, respectively
C ----------------
C P     - A 2-d array containing a scalar data field. The contents
C         of this array may be used to color the streamlines. 
C LP    - The first dimension of the P array
C NOTE:
C Coloring by means of the P scalar data field is not yet
C implemented
C ----------------
C M     - The first data dimension (must be less than or equal to
C         MIN(LU,LV) (or MIN(LU,LV,LP) if the P array is used
C WRK   - an internally used work array
C LW    - dimension of the work array (must be at least 2*M*N; 3*M*N if
C         coloring by magnitude) 
C
C Output parameters:
C
C None
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
C Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
      CALL STDATA
C
C Write the array sizes into the common block
C 
      IUD1=LU
      IVD1=LV
      IF (ABS(ICTV).EQ.2) THEN
         IPD1=LP
      ELSE 
         IPD1=M
      END IF
      IWKD=LW
C
C Error if M > LU or M > LV?
C
      IF (LU.LT.M .OR. LV.LT.M) THEN
         CSTR(1:45)='STINIT - U AND/OR V ARRAY DIMENSIONS EXCEEDED'
         CALL SETER (CSTR(1:45),1,1)
         RETURN
      END IF
      IXDM=M
      IYDN=N
      IXD1=1
      IYD1=1
      IXM1=IXDM-1
      IXM2=IXDM-2
      IYM1=IYDN-1
      IYM2=IYDN-2
      IF (LW .LT. 2*IXDM*IYDN) THEN
         CSTR(1:37)='STINIT - WRK ARRAY DIMENSION TOO SMALL'
         CALL SETER (CSTR(1:37),2,1)
         RETURN
      END IF
C
C Initialize and transfer some arguments to local variables.
C
      IBIG = I1MACH(9)
      RBIG = R1MACH(2)
C
C Decide what the range of values in X and Y should be.
C
      IF (UXC1.EQ.UXCM) THEN
        XLOV=1.
        XHIV=REAL(IXDM)
      ELSE
        XLOV=UXC1
        XHIV=UXCM
      END IF
C
      IF (UYC1.EQ.UYCN) THEN
        YLOV=1.
        YHIV=REAL(IYDN)
      ELSE
        YLOV=UYC1
        YHIV=UYCN
      END IF
C
      IXIN = MAX(IXIN,1)
      IYIN = MAX(IYIN,1)
C
      NXCT = MAX(IXDM/IXIN,1)
      NYCT = MAX(IYDN/IYIN,1)
C
C If the user has done a SET call, retrieve the arguments; if he hasn't
C done a SET call, do it for him.
C
      IF (ISET .EQ .0) THEN
C
        CALL GETSET (XVPL,XVPR,YVPB,YVPT,WXMN,WXMX,WYMN,WYMX,LNLG)
C
      ELSE
C
        LNLG=1
C
        IF (UWDL.EQ.UWDR) THEN
          WXMN=XLOV
          WXMX=XHIV
        ELSE
          WXMN=UWDL
          WXMX=UWDR
        END IF
C
        IF (UWDB.EQ.UWDT) THEN
          WYMN=YLOV
          WYMX=YHIV
        ELSE
          WYMN=UWDB
          WYMX=UWDT
        END IF
C
C Determine the viewport based on the setting of the viewport
C shape and viewport extent parameters
C
        IF (UVPS.LT.0.) THEN
          AR=ABS(UVPS)
        ELSE IF (UVPS.EQ.0.) THEN
          AR=(UVPR-UVPL)/(UVPT-UVPB)
        ELSE IF (UVPS.LE.1.) THEN
          AR=ABS((WXMX-WXMN)/(WYMX-WYMN))
          IF (MIN(AR,1./AR).LT.UVPS) AR=(UVPR-UVPL)/(UVPT-UVPB)
        ELSE
          AR=ABS((WXMX-WXMN)/(WYMX-WYMN))
          IF (MAX(AR,1./AR).GT.UVPS) AR=1.
        END IF
C
        IF (AR.LT.(UVPR-UVPL)/(UVPT-UVPB)) THEN
          XVPL=.5*(UVPL+UVPR)-.5*(UVPT-UVPB)*AR
          XVPR=.5*(UVPL+UVPR)+.5*(UVPT-UVPB)*AR
          YVPB=UVPB
          YVPT=UVPT
        ELSE
          XVPL=UVPL
          XVPR=UVPR
          YVPB=.5*(UVPB+UVPT)-.5*(UVPR-UVPL)/AR
          YVPT=.5*(UVPB+UVPT)+.5*(UVPR-UVPL)/AR
        END IF
C
        CALL SET (XVPL,XVPR,YVPB,YVPT,WXMN,WXMX,WYMN,WYMX,LNLG)
C
      END IF
C    
C Calculate fraction of VP width to fractional size factor.
C Calculate fraction of VP height to fractional size factor.
C These are for convenience.
C
      FW2W = XVPR - XVPL
      FH2H = YVPT - YVPB
      TX=(XVPR-XVPL)/REAL(NXCT)
      TY=(YVPT-YVPB)/REAL(NYCT)
      RDMX=SQRT((TX*TX+TY*TY))
      RDMN = 0.0
C
C Swap window rectangle if it is inverted, but keep track
C This makes it easier to exclude out-of-bounds points in the
C projection mapping routines
C
      INVX=0
      INVY=0
      IF (WXMN .GT. WXMX) THEN
         T=WXMN
         WXMN=WXMX
         WXMX=T
         INVX=1
      END IF
      IF (WYMN .GT. WYMX) THEN
         T=WYMN
         WYMN=WYMX
         WYMX=T
         INVY=1
      END IF
C
C If cyclic data specified check to ensure the cyclic condition exists.
C The error flag is set if necessary within STCYCL
C
      IF (ICYK.NE.0) CALL STCYCL(U,V)
C
C Calculate the grid size
C
      XGDS=(XHIV-XLOV)/(REAL(NXCT)-1.0)
      YGDS=(YHIV-YLOV)/(REAL(NYCT)-1.0)
C
C If coloring by magnitude a work array containing the vector magnitudes
C needs to be created in the third section of the work array. Do it 
C while calculating the max and min magnitudes.
C
      IF (ABS(ICTV).EQ.1) THEN
         IF (LW .LT. 3*IXDM*IYDN) THEN
            CSTR(1:37)='STINIT - WRK ARRAY DIMENSION TOO SMALL'
            CALL SETER (CSTR(1:37),2,1)
            RETURN
         END IF
         CALL STCMAG(U,V,WRK(2*IXDM*IYDN+1),1)
         PMIN=UVMN
         PMAX=UVMX
         IP=1
      ELSE
         CALL STCMAG(U,V,RDM,0)
      END IF
C
C If using the scalar array...
C
      IF (ABS(ICTV).EQ.2) THEN
C
C Check for error condition
C
         IF (IPD1 .LT. IXDM) THEN
            CSTR(1:31)='STINIT - SCALAR ARRAY TOO SMALL'
            CALL SETER (CSTR(1:31),2,1)
            RETURN
         END IF
C
C Find the max and min scalar values when necessary
C
         IF (ICTV.GE.2 .OR. PMIN.EQ.PMAX) THEN
            PMIN=RBIG
            PMAX=-RBIG
            IP=0
            DO 121 J=1,IYDN,IYIN
               DO 120 I=1,IXDM,IXIN
                  IF (ISPC .GE. 0 .AND. P(I,J) .EQ. RPSV) GO TO 120  
                  IP=1
                  PMIN = MIN(PMIN, P(I,J))
                  PMAX = MAX(PMAX, P(I,J))
 120           CONTINUE
 121        CONTINUE
         END IF
C
      END IF
C
C Determine the threshold values. If there were no non-special
C values in the P array, then set all threshold values to the
C maximum possible.
C
      IF (NLVL.GT.0 .AND. ICTV.GT.0) THEN
C
         IF (IP .NE. 0) THEN
            TV=(PMAX-PMIN)/NLVL
            DO 125 I=1,NLVL,1
               TVLU(I)=PMIN+REAL(I)*TV
 125        CONTINUE
         ELSE
            DO 130 I=1,NLVL,1
               TVLU(I)=RBIG
 130        CONTINUE
         END IF
C
      END IF
C
C Done.
C
      RETURN
C
      END
C ---------------------------------------------------------------------
C
      SUBROUTINE STCMAG(U,V,RM,MFL)
C
C This routine finds the maximum and minimum magnitude vectors.
C If there are no special values use simpler code for efficiency.
C If coloring by magnitude also populates an array with magnitudes
C
C Input parameters:
C
C U,V - 2-d arrays holding the component values of a vector field
C RM  - 2-d array for the magnitude
C MFL - flag for performing magnitude operation
C
C Output parameters:
C

      DIMENSION  U(IUD1,*),V(IVD1,*)
      DIMENSION  RM(IXDM,*)
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
      UVMN=RBIG
      UVMX=0.0
      IF (ISVF .LE. 0) THEN
C
         DO 111 J=1,IYDN,IYIN
            DO 110 I=1,IXDM,IXIN
               TU=U(I,J)
               TV=V(I,J)
               IF (IPLR.NE.0) THEN
                  UVMN = MIN(UVMN, TU)
                  UVMX = MAX(UVMX, TU)
                  IF (MFL .EQ. 1) THEN
                     RM(I,J) = TU
                  END IF
               ELSE
                  TM = TU*TU+TV*TV
                  UVMN = MIN(UVMN, TM)
                  UVMX = MAX(UVMX, TM)
                  IF (MFL .EQ. 1) THEN
                     RM(I,J) = SQRT(TM)
                  END IF
               END IF
 110        CONTINUE
 111     CONTINUE
C
      ELSE
C
         DO 116 J=1,IYDN,IYIN
            DO 115 I=1,IXDM,IXIN
               TU=U(I,J)
               TV=V(I,J)
C
               IF (ISVF .GT. 0) THEN
                  IF (TU .EQ. RUSV .OR. TV .EQ. RVSV) GO TO 115
               END IF
C
               IF (IPLR.NE.0) THEN
                  UVMN = MIN(UVMN, TU)
                  UVMX = MAX(UVMX, TU)
                  IF (MFL .EQ. 1) THEN
                     RM(I,J) = TU
                  END IF
               ELSE
                  TM = TU*TU+TV*TV
                  UVMN = MIN(UVMN,TM)
                  UVMX = MAX(UVMX,TM)
                  IF (MFL .EQ. 1) THEN
                     RM(I,J) = SQRT(TM)
                  END IF
               END IF
C
 115        CONTINUE
 116     CONTINUE
C
      END IF
C
      IF (IPLR .LE. 0) THEN
         UVMX = SQRT(UVMX)
         UVMN = SQRT(UVMN)
      END IF
C
C DONE
C
      RETURN
C
      END


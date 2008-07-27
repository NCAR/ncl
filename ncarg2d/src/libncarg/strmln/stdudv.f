C
C       $Id: stdudv.f,v 1.10 2008-07-27 00:17:28 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE STDUDV (UX,VY,I,J,X,Y,DU,DV)
C
C Input parameters:
C
C UX,VY  - the arrays containing normalized vector field data
C I,J    - the current grid indices
C X,Y    - the X,Y position relative to the grid
C
C Output parameters:
C
C DU,DV  - Interpolated value of the vector field components
C          at the specified point 
C
C Interpolation routine to calculate the displacemant components.
C The philosphy here is to utilize as many points as possible
C (within reason) in order to obtain a pleasing and accurate plot.
C Interpolation schemes desired by other users may easily be
C substituted if desired.
C
      DIMENSION UX(IXDM,*), VY(IXDM,*)
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
      ELSE IF(ISVF.NE.0 .OR. ITRP.NE.0) THEN
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
         DU = FDLI(UX(I,J),UX(I,J+1),UX(I+1,J),UX(I+1,J+1),DX,DY)
         DV = FDLI(VY(I,J),VY(I,J+1),VY(I+1,J),VY(I+1,J+1),DX,DY)
C
      ELSE IF (ITF .EQ. 2) THEN
C
C 16 point bessel interpolation scheme.
C
         UJM1 = FBESL(UX(I,J-1),UX(I+1,J-1),UX(IP2,J-1),UX(IM1,J-1),DX)
         UJ   = FBESL(UX(I,J),UX(I+1,J),UX(IP2,J),UX(IM1,J),DX)
         UJP1 = FBESL(UX(I,J+1),UX(I+1,J+1),UX(IP2,J+1),UX(IM1,J+1),DX)
         UJP2 = FBESL(UX(I,J+2),UX(I+1,J+2),UX(IP2,J+2),UX(IM1,J+2),DX)
         DU   = FBESL(UJ,UJP1,UJP2,UJM1,DY)
         VJM1 = FBESL(VY(I,J-1),VY(I+1,J-1),VY(IP2,J-1),VY(IM1,J-1),DX)
         VJ   = FBESL(VY(I,J),VY(I+1,J),VY(IP2,J),VY(IM1,J),DX)
         VJP1 = FBESL(VY(I,J+1),VY(I+1,J+1),VY(IP2,J+1),VY(IM1,J+1),DX)
         VJP2 = FBESL(VY(I,J+2),VY(I+1,J+2),VY(IP2,J+2),VY(IM1,J+2),DX)
         DV   = FBESL(VJ,VJP1,VJP2,VJM1,DY)
C
      ELSE IF (ITF .EQ. 3) THEN
C
C 12 point interpolation scheme applicable to one row from top boundary
C
         UJM1 = FBESL(UX(I,J-1),UX(I+1,J-1),UX(IP2,J-1),UX(IM1,J-1),DX)
         UJ   = FBESL(UX(I,J),UX(I+1,J),UX(IP2,J),UX(IM1,J),DX)
         UJP1 = FBESL(UX(I,J+1),UX(I+1,J+1),UX(IP2,J+1),UX(IM1,J+1),DX)
         DU   = FQUAD(UJ,UJP1,UJM1,DY)
         VJM1 = FBESL(VY(I,J-1),VY(I+1,J-1),VY(IP2,J-1),VY(IM1,J-1),DX)
         VJ   = FBESL(VY(I,J),VY(I+1,J),VY(IP2,J),VY(IM1,J),DX)
         VJP1 = FBESL(VY(I,J+1),VY(I+1,J+1),VY(IP2,J+1),VY(IM1,J+1),DX)
         DV   = FQUAD(VJ,VJP1,VJM1,DY)
C
      ELSE IF (ITF .EQ. 4) THEN
C
C 9 point interpolation scheme for use in the non-cyclic case
C at I=IXM1; J > IYD1 and J <= IYM1
C
         UJP1 = FQUAD(UX(I,J+1),UX(I+1,J+1),UX(IM1,J+1),DX)
         UJ   = FQUAD(UX(I,J),UX(I+1,J),UX(IM1,J),DX)
         UJM1 = FQUAD(UX(I,J-1),UX(I+1,J-1),UX(IM1,J-1),DX)
         DU   = FQUAD(UJ,UJP1,UJM1,DY)
         VJP1 = FQUAD(VY(I,J+1),VY(I+1,J+1),VY(IM1,J+1),DX)
         VJ   = FQUAD(VY(I,J),VY(I+1,J),VY(IM1,J),DX)
         VJM1 = FQUAD(VY(I,J-1),VY(I+1,J-1),VY(IM1,J-1),DX)
         DV   = FQUAD(VJ,VJP1,VJM1,DY)
C
      END IF
C
C Done
C
      RETURN
      END

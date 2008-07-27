C
C       $Id: velvct.f,v 1.22 2008-07-27 00:17:34 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE VELVCT (U,LU,V,LV,M,N,FLO,HI,NSET,LENGTH,ISPV,SPV)
C
C This is the old form of the velocity vector subroutine
C
C
C DIMENSION OF           U(LU,N),V(LV,N),SPV(2)
C ARGUMENTS
C
      DIMENSION U(LU,N),V(LV,N),SPV(2)
C
C The original discussion explaining the routine follows the code
C Note that to use of this function does not preclude use of other
C new added features. By default the FX,FY,MXF,MYF routines are used
C but the new mapping function VVMPXY may be called with the proper
C setting of the compatibility flag parameter, 'CPM'. Look in vvdata.f
C for usage of this parameter.
C
C The original VELVCT common blocks. All values contained in these
C common blocks that are intended to be modified by the user are
C transferred into the VVCOM common block used by the new routines in
C the VELVCT package. Since the VEC1 and VEC2 blocks now appear
C only in this routine, the data are initialized here.
C
      COMMON /VEC1/   ASH        ,EXT        ,ICTRFG     ,ILAB       ,
     +                IOFFD      ,IOFFM      ,ISX        ,ISY        ,
     +                RMN        ,RMX        ,SIDE       ,SIZE       ,
     +                XLT        ,YBT        ,ZMN        ,ZMX
C     
      COMMON /VEC2/   BIG        ,INCX       ,INCY
C     
      SAVE /VEC1/, /VEC2/
C
C Internal parameters of VELVCT are as follows.
C
C                        Name   Default  Function
C                        ----   -------  --------
C
C                        BIG   R1MACH(2) No longer needed
C
C                        EXT     0.25    The lengths of the sides of the
C                                        plot are proportional to M and
C                                        N when ISET is less than or
C                                        equal to zero, except when
C                                        MIN(M,N)/MAX(M,N) is less than
C                                        EXT, in which case a square
C                                        graph is plotted.
C
C                        ICTRFG    1     Flag to control the position of
C                                        the arrow relative to  a base
C                                        point at (MX,MY).
C
C                                        Zero - center at (MX,MY)
C
C                                        Positive - tail at (MX,MY)
C
C                                        Negative -  head at (MX,MY)
C
C                        ILAB      0     Flag to control the drawing of
C                                        line labels.
C
C                                        Zero - do not draw the labels
C
C                                        Non-zero - draw the labels
C
C                        INCX      1     X-coordinate step size for less
C                                        dense arrays.
C
C                        INCY      1     Y-coordinate step size.
C
C                        IOFFD     0     Flag to control normalization
C                                        of label numbers.
C
C                                        Zero - include a decimal point
C                                        when possible
C
C                                        Non-zero - normalize all label
C                                        numbers by ASH
C
C                        IOFFM     0     Flag to control plotting of
C                                        the message below the plot.
C
C                                        Zero - plot the message
C
C                                        Non-zero - do not plot it
C
C                        RMN     160.    Arrow size below which the
C                                        head no longer shrinks, on a
C                                        2**15 x 2**15 grid.
C
C                        RMX    6400.    Arrow size above which the
C                                        head no longer grows larger,
C                                        on a 2**15 x 2**15 grid.
C
C                        SIDE    0.90    Length of longer edge of plot.
C                                        (see also EXT.)
C
C                        SIZE    256.    Width of the characters in
C                                        vector labels, on a 2**15 x
C                                        2**15 grid.
C
C                        XLT     0.05    Left hand edge of the plot.
C                                        (0 is the left edge of the
C                                        frame, 1 the right edge.)
C
C                        YBT     0.05    Bottom edge of the plot (0 is
C                                        the bottom of the frame, 1 the
C                                        top of the frame.)
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
C Local variables:
C
C IPM             - parameter use flag
C ICB             - VEC1,VEC2 common blocks use flag
C VXL,VXR,VYB,VYT - saved viewport boundary
C WXL,WXR,WYB,WYT - saved window boundary
C X1,X2,Y1,Y2     - temporary viewport boundary
C X3,Y3,X4,Y4     - temporary window boundary
C SVM             - Size of Viewport, Metacode coords
C LEN             - maximum vector size in Metacode coords
C RDA             - real dummy array variable
C IDA             - integer dummy array variable
C XP              - X coordinate position
C YP              - Y coordinate position
C TSZ             - text size
C
C Saved values of internal parameters (Note that these do not follow
C the usually observed rule of using 3 characters or less for local
C variables):
C SVLC,SVHC,ISSET,SVRL,ISSVF,SUSV,SVSV,SVPS,ISVPO,ISLBL,ISDPF,
C SAMN,SAMX,SLBS,ISXIN,ISYIN,ISMXP,SMXX,SMXY
C
C The character variables must be declared:
C
      CHARACTER*(IPCHSZ) CSMNT, CSMXT
      EXTERNAL VVDUMB
C
C This is an empirically derived factor for converting from the
C WTSTR text size to PLCHHQ text
C
      PARAMETER (PHQCVT = 0.8)
C
C Dummy variable
C
      DIMENSION RDA(1)
      DIMENSION IDA(1)
      DATA RDA / 0.0 /
      DATA IDA / 0 / 
C
C -------------------------------------------------------------------
C
C Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
        CALL VELDAT
C
C Save the values of all internal parameters that might be reset
C
      CALL VVGETR('VLC - Vector Low Cutoff Value', SVLC)
      CALL VVGETR('VHC - Vector High Cutoff Value',SVHC)
      CALL VVGETI('SET - Do SET Call Flag', ISSET)
      CALL VVGETR('VRL - Maximum Vector Realized Length', SVRL)
      CALL VVGETI('SVF - Special Value Flag', ISSVF)
      CALL VVGETR('USV - U Array Special Value', SUSV)
      CALL VVGETR('VSV - V Array Special Value', SVSV)
      CALL VVGETR('VPS - Viewport Mode Setting', SVPS)
      CALL VVGETI('VPO - Vector Position Method', ISVPO)
      CALL VVGETI('LBL - Vector Labelling Flag', ISLBL)
      CALL VVGETI('DPF - Label Normalization', ISDPF)
      CALL VVGETR('AMN - Arrow Head Minimum Size', SAMN)
      CALL VVGETR('AMX - Arrow Head Maximum Size', SAMX)
      CALL VVGETR('LBS - Vector Label Text Size', SLBS)
      CALL VVGETI('XIN - X Grid Increment', ISXIN)
      CALL VVGETI('YIN - Y Grid Increment', ISYIN)
      CALL VVGETC('MNT - Minimum Vector Text', CSMNT)
      CALL VVGETC('MXT - Maximum Vector Text', CSMXT)
      CALL VVGETI('MXP - Maximum Text Position Mode', ISMXP)
      CALL VVGETR('MXX - Maximum Text X Position', SMXX)
      CALL VVGETR('MXY - Maximum Text Y Position', SMXY)
C
C Set up the scaling of the plot.
C
      CALL GETSET (VXL,VXR,VYB,VYT,WXL,WXR,WYB,WYT,LL)
C
      X1=VXL
      X2=VXR
      Y1=VYB
      Y2=VYT
      SVM = REAL(KFMX(X2) - KFMX(X1))
C
C Set the parameter and common block use flags
C Note that the value of ICPM is temporarily modified if it
C is equal to the default value. It will be restored as soon as
C the vectors are drawn
C
      IPM = 0
      ICB = 0
      IF (ICPM .GE. -2 .AND. ICPM .LE. 2) IPM = 1
      IF (ABS(ICPM).EQ.1 .OR. ABS(ICPM).EQ.3 .OR. ICPM.EQ.0) ICB = 1 
      IF (ICPM.EQ.0) ICPM = 99
C
      IF (IPM .EQ. 1) THEN
C
C VELVCT parameters override Vectors internal parameters
C
         IF (NSET .LT. 0) THEN
C
            X3 = 1.
            X4 = REAL(M)
            Y3 = 1.
            Y4 = REAL(N)
            CALL SET(X1,X2,Y1,Y2,X3,X4,Y3,Y4,1)
C     
         ELSE IF (NSET .EQ. 0) THEN
C     
            X1 = XLT
            X2 = XLT+SIDE
            Y1 = YBT
            Y2 = YBT+SIDE
            X3 = 1.
            Y3 = 1.
            X4 = REAL(M)
            Y4 = REAL(N)
            IF (MIN(X4,Y4)/MAX(X4,Y4) .GE. EXT) THEN
               IF (M .GT. N) THEN
                  Y2 = YBT+SIDE*Y4/X4
               ELSE 
                  X2 = XLT+SIDE*X4/Y4
               END IF
            END IF
C     
            CALL SET(X1,X2,Y1,Y2,X3,X4,Y3,Y4,1)
            CALL PERIM (1,0,1,0)
            SVM = REAL(KFMX(X2) - KFMX(X1))
C     
         END IF
C
C Use VVSET calls to transfer parameter values into the common block
C 
C Use of FLO and HI correspond to negative values for VLC and VHC
C
         CALL VVSETR('VLC - Vector Low Cutoff Value', MIN(0.0,-FLO))
         CALL VVSETR('VHC - Vector High Cutoff Value', MIN(0.0,-HI))
C
C Since the set call has been handled VVINIT should not do a SET
C
         CALL VVSETI('SET - Do SET Call Flag', 0)
C
C Convert the maximum vector length from plotter address units to
C a fraction of the viewport
C
         CALL GETUSV('XF',ISX)
         ISX = 2**(15-ISX)
         IF (LENGTH .EQ. 0) THEN
            VRL=0.0
         ELSE
            VRL=CMFX(LENGTH*ISX)/(X2-X1)
         ENDIF
         CALL VVSETR('VRL - Maximum Vector Realized Length', VRL)
C
C     Special values
C
         CALL VVSETI('SVF - Special Value Flag', ISPV)
         IF (ISPV .EQ. 1 .OR. ISPV .EQ. 3 .OR. ISPV .EQ. 4) THEN
            CALL VVSETR('USV - U Array Special Value', SPV(1))
         END IF
         IF (ISPV .EQ. 2 .OR. ISPV .EQ. 3 .OR. ISPV .EQ. 4) THEN
            CALL VVSETR('VSV - V Array Special Value', SPV(2))
         END IF
      END IF
C
C Common block values
C
      IF (ICB .EQ. 1) THEN
C
C VEC1, VEC2 values override Vectors internal parameters
C
         CALL VVSETR('VPS - Viewport Mode Setting', EXT)
         CALL VVSETI('VPO - Vector Position Method', ICTRFG)
         CALL VVSETI('LBL - Vector Labelling Flag', ILAB)
         CALL VVSETI('DPF - Label Normalization', IOFFD)
C
C Convert minimum and maximum arrow head sizes and the vector 
C label character size to fraction of viewport width.
C
         AMN = RMN / SVM
         AMX = RMX / SVM
         TSZ = PHQCVT * SIZE / SVM
C
         CALL VVSETR('AMN - Arrow Head Minimum Size', AMN)
         CALL VVSETR('AMX - Arrow Head Maximum Size', AMX)
         CALL VVSETR('LBS - Vector Label Text Size', TSZ)
C
C Viewport values (XLT,YBT,SIDE) do not need to be set since they
C have been handled above.
C
         CALL VVSETI('XIN - X Grid Increment', INCX)
         CALL VVSETI('YIN - Y Grid Increment', INCY)
C
C Depending on the value of the message flag, set up the 
C min/max text blocks to emulate the old VELVCT. 
C
         CALL VVSETC('MNT - Minimum Vector Text', ' ')
         IF (IOFFM .NE. 0) THEN
            CALL VVSETC('MXT - Maximum Vector Text', ' ')
         ELSE
C
C Calculate text position values to place text at lower right of
C plotter frame (0.05,0.005 NDC units from the corner)
C rather than under lower left of viewport.
C Also calculate a fixed text size based on the size hardcoded 
C in the old version of VELVCT.
C
            XP=1.0+(1.0-X2-0.05)/(X2-X1)
            YP=(0.005-Y1)/(Y2-Y1)
            TSZ=CPFX(MAX(256/ISX,8))/(X2-X1)
            CALL VVSETR('MXS - Maximum Text Size', TSZ)
            CALL VVSETI('MXP - Maximum Text Position Mode', -2)
            CALL VVSETR('MXX - Maximum Text X Position', XP)
            CALL VVSETR('MXY - Maximum Text Y Position', YP)
            CALL VVSETC('MXT - Maximum Vector Text', 'MAXIMUM VECTOR')
         END IF
C
      END IF
C
C Initialize the vector plotting routine and plot the vectors
C
      CALL VVINIT(U,LU,V,LV,FDA,0,M,N,FDA,0)
C
      CALL VVECTR(U,V,FDA,IDA,VVDUMB,FDA)
C
C Restore original SET values, if required.
C
      IF (NSET .LE. 0) THEN
         CALL SET (VXL,VXR,VYB,VYT,WXL,WXR,WYB,WYT,LL)
      END IF
C
C Restore the compatibility flag value if necessary
C
      IF (ICPM .EQ. 99) ICPM = 0
C
C Reset all internal parameters that may have been modified
C
      CALL VVSETR('VLC - Vector Low Cutoff Value', SVLC)
      CALL VVSETR('VHC - Vector High Cutoff Value',SVHC)
      CALL VVSETI('SET - Do SET Call Flag', ISSET)
      CALL VVSETR('VRL - Maximum Vector Realized Length', SVRL)
      CALL VVSETI('SVF - SpeciaL Value Flag', ISSVF)
      CALL VVSETR('USV - U Array Special Value', SUSV)
      CALL VVSETR('VSV - V Array Special Value', SVSV)
      CALL VVSETR('VPS - Viewport Mode Setting', SVPS)
      CALL VVSETI('VPO - Vector Position Method', ISVPO)
      CALL VVSETI('LBL - Vector Labelling Flag', ISLBL)
      CALL VVSETI('DPF - Label Normalization', ISDPF)
      CALL VVSETR('AMN - Arrow Head Minimum Size', SAMN)
      CALL VVSETR('AMX - Arrow Head Maximum Size', SAMX)
      CALL VVSETR('LBS - Vector Label Text Size', SLBS)
      CALL VVSETI('XIN - X Grid Increment', ISXIN)
      CALL VVSETI('YIN - Y Grid Increment', ISYIN)
      CALL VVSETC('MNT - Minimum Vector Text', CSMNT)
      CALL VVSETC('MXT - Maximum Vector Text', CSMXT)
      CALL VVSETI('MXP - Maximum Text Position Mode', ISMXP)
      CALL VVSETR('MXX - Maximum Text X Position', SMXX)
      CALL VVSETR('MXY - Maximum Text Y Position', SMXY)
C
      RETURN
      END
C
C ------------------------------------------------------------------
C Original discussion of the VELVCT routine follows:
C
C USAGE                  If the following assumptions are met, use
C
C                               CALL EZVEC (U,V,M,N)
C
C                          Assumptions -
C
C                            --The whole array is processed.
C                            --The scale factor is chosen internally.
C                            --The perimeter is drawn.
C                            --FRAME is called after plotting.
C                            --There are no special values.
C
C                        If these assumptions are not met, use
C
C             CALL VELVCT(U,LU,V,LV,M,N,FLO,HI,NSET,LENGTH,ISPV,SPV)
C
C ARGUMENTS
C
C ON INPUT               U,V
C
C                          The (origins of the) two-dimensional arrays
C                          containing the velocity field to be plotted.
C                          The vector at the point (I,J) has magnitude
C                          SQRT(U(I,J)**2+V(I,J)**2) and direction
C                          ATAN2(V(I,J),U(I,J)).  Other representations,
C                          such as (R,THETA), can be plotted by
C                          changing statement functions in this routine.
C
C                        LU
C
C                          The first dimension of U in the calling
C                          program.
C
C                        LV
C
C                          The first dimension of V in the calling
C                          program.
C
C
C                        M
C
C                          The number of data values to be plotted in
C                          the X-direction (the first subscript
C                          direction).  When plotting the entire array,
C                          LU = LV = M. (PV would also = M).
C
C                        N
C
C                          The number of data values to be plotted in
C                          the Y-direction (the second subscript
C                          direction).
C
C                        FLO
C
C                          The minimum vector magnitude to be shown.
C
C                        HI
C
C                          The maximum vector magnitude to be shown. (A
C                          value less than or equal to zero causes the
C                          maximum value of SQRT(U**2+V**2) to be used.)
C
C                        ISET
C
C                          Flag to control scaling -
C
C                          If ISET is zero, VELVCT establishes the
C                          window and viewport to properly
C                          scale plotting instructions to the standard
C                          configuration.  PERIM is called to draw a
C                          border.
C
C                          If ISET is greater than zero, VELVCT assumes
C                          that the user has established the window
C                          and viewport in such a way as to properly
C                          scale the plotting instructions generated
C                          by VELVCT.  PERIM is not called.
C
C                          If ISET is less than zero, VELVCT
C                          places the contour plot
C                          within the limits of the user's current
C                          window and viewport.  PERIM is not called.
C
C                        LENGTH
C
C                          The length, in Plotter Address Units (PAUs),
C                          of a vector having magnitude HI
C                          (or, if HI=0, the length in PAUs
C                          of the longest vector).  If LENGTH=0, a
C                          value is chosen such that the longest vector
C                          could just reach to the tail of the next
C                          vector.  If the horizontal and vertical
C                          resolutions of the plotter are different,
C                          LENGTH should be non-zero and specified as a
C                          horizontal distance.
C
C                        ISVF
C
C                          Flag to control the special value feature.
C
C                             0 means that the feature is not in use.
C
C                             1 means that if the value of
C                               U(I,J)=UUSV the vector will not be
C                               plotted.
C
C                             2 means that if the value of
C                               V(I,J)=UVSV the vector will not be
C                               plotted.
C
C                             3 means that if either U(I,J)=UUSV or
C                               V(I,J)=UVSV then the vector will not
C                               be plotted.
C
C                             4 means that if U(I,J)=UUSV
C                               and V(I,J)=UVSV, the vector
C                               will not be plotted.
C
C                        SPV
C
C                        An array of length 2 which gives the value
C                        in the U array and the value in the V array
C                        which denote missing values.
C                        This argument is ignored if ISVF=0.
C
C
C ON OUTPUT              All arguments remain unchanged.
C
C NOTE                   The endpoints of each arrow drawn are (FX(X,Y),
C                        FY(X,Y)) and (MXF(X,Y,U,V,SFX,SFY,MX,MY),
C                        MYF(X,Y,U,V,SFX,SFY,MX,MY)) where X=I, Y=J,
C                        U=U(I,J), V=V(I,J), and SFX and SFY are scale
C                        factors.  Here I is the X-index and J is the
C                        Y-index.  (MX,MY) is the location of the tail.
C                        Thus the actual length of the arrow is
C                        SQRT(DX**2+DY**2) and the direction is
C                        ATAN2(DX,DY), where DX=MX-MXF(...) and
C                        DY=MY-MYF(...).
C
C ENTRY POINTS           VELVCT,EZVECT,VELVEC
C
C COMMON BLOCKS          VEC1,VEC2
C
C I/O                    Plots the vector field.
C
C PRECISION              Single
C
C LANGUAGE               FORTRAN
C
C REQUIRED LIBRARY       GRIDAL and the SPPS
C ROUTINES
C
C REQUIRED GKS LEVEL     0A
C
C HISTORY                Written and standardized in November 1973.
C                        Revised in May, 1975, to include MXF and MYF.
C                        Revised in March, 1981, to fix certain errors;
C                        to use FL2INT and PLOTIT instead of MXMY,
C                        FRSTPT, and VECTOR; and to make the arrowheads
C                        narrower.  Converted to FORTRAN77 and GKS
C                        in July 1984.
C
C ALGORITHM              Each vector is examined, possibly transformed,
C                        then plotted.
C
C PORTABILITY            FORTRAN77
C
C ---------------------------------------------------------------------
C
C SPECIAL NOTE -
C
C Using this routine to put vectors on an arbitrary background drawn by
C SUPMAP is a bit tricky.  The arithmetic statement functions FX and FY
C are easy to replace.  The problem arises in replacing MXF and MYF.
C The following example may be helpful. (SUPMAP is an entry point in
C the EZMAP package.)
C
C Suppose that we have two arrays, CLON(36,9) and CLAT(36,9), which
C contain the E-W and N-S components of a wind flow field on the surface
C of the earth.  CLON(I,J) is the magnitude of the easterly flow.
C CLAT(I,J) is the magnitude of the northerly flow at a longitude (I-1)
C *10 degrees east of Greenwich and a latitude (J-1)*10 degrees north of
C the equator.  SUPMAP is to be used to draw a polar projection of the
C earth and VELVCT is to be used to superimpose vectors representing the
C flow field on it.  The following steps would be necessary:
C
C     1.  CALL SUPMAP (1,90.,0.,-90.,90.,90.,90.,90.,-4,10,0,1,IER)
C         to draw the map.
C
C     2.  CALL VELVCT (CLON,36,CLAT,36,36,9,0.,0.,1,50,0,0.) to put
C         vectors on it.  Notice that ISET has the value 1 to tell
C         VELVCT that SUPMAP has done the required SET call.
C
C     3.  In order to ensure that step 2 will work properly, delete
C         the arithmetic statement functions FX, FY, MXF, and MYF
C         from VELVCT and include the following functions.
C
C     FUNCTION FX(XX,YY)
C     CALL MAPTRN (10.*(YY-1.),10.*(XX-1.),X,Y)
C     FX=X
C     RETURN
C     END
C
C     FUNCTION FY(XX,YY)
C     CALL MAPTRN (10.*(YY-1.),10.*(XX-1.),X,Y)
C     FY=Y
C     RETURN
C     END
C
C     FUNCTION MXF(XX,YY,UU,VV,SFX,SFY,MX,MY)
C     CFCT=COS(.17453292519943*(YY-1.))
C     CALL MAPTRN(10.*(YY-1.)         ,10.*(XX-1.)              ,X1,Y1)
C     CALL MAPTRN(10.*(YY-1.)+1.E-6*VV,10.*(XX-1.)+1.E-6*UU/CFCT,X2,Y2)
C     U=((X2-X1)/SQRT((X2-X1)**2+(Y2-Y1)**2))*SQRT(UU**2+VV**2)
C     MXF=MX+INT(SFX*U)
C     RETURN
C     END
C
C     FUNCTION MYF(XX,YY,UU,VV,SFX,SFY,MX,MY)
C     CFCT=COS(.17453292519943*(YY-1.))
C     CALL MAPTRN(10.*(YY-1.)         ,10.*(XX-1.)              ,X1,Y1)
C     CALL MAPTRN(10.*(YY-1.)+1.E-6*VV,10.*(XX-1.)+1.E-6*UU/CFCT,X2,Y2)
C     V=((Y2-Y1)/SQRT((X2-X1)**2+(Y2-Y1)**2))*SQRT(UU**2+VV**2)
C     MYF=MY+INT(SFY*V)
C     RETURN
C     END
C
C The basic notion behind the coding of the MXF and MYF functions is as
C follows.Since UU and VV are the longitudinal and latitudinal components,
C respectively, of a velocity vector having units of distance over time,
C 1.E-6*UU/COS(latitude) and 1.E-6*VV represent the change in longitude
C and latitude, respectively, of a particle moving with the flow field
C for a very short period of time.  The routine MAPTRN is used to find
C the position of the particle's projection at the beginning and end of
C that tiny time slice and, therefore, the direction in which to draw
C the arrow representing the velocity vector so that it will be tangent
C to a projected flow line of the field at that point.  The values U
C and V are computed so as to give the arrow the length implied by UU
C and VV.  (The code ensures that SQRT(U**2+V**2) is equal to
C SQRT(UU**2+VV**2).)  The length of the arrow represents the magnitude
C of the velocity vector, unaffected by perspective.  The scaling set
C up by VELVCT will therefore be appropriate for the arrows drawn.
C
C This method is rather heuristic and has three inherent problems.
C First, the constant 1.E-6 may need to be made larger or smaller,
C depending on the magnitude of your U/V data.  Second, the north and
C south poles must be avoided.  At either pole, CFCT goes to zero,
C giving a division by zero; in a small region near the pole, the
C method may try to use MAPTRN with a latitude outside the range
C (-90,+90).  Third, the projection must be set up so as to avoid
C having vector basepoints at the exact edge of the map.  Vectors
C there will be of the correct length, but they may be drawn in the
C wrong direction (when the projected particle track determining the
C direction crosses the edge and reappears elsewhere on the map).
C With a little care, the desired results may be obtained.
C
C
C ---------------------------------------------------------------------
C End of discussion


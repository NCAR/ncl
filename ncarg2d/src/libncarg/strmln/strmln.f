C
C       $Id: strmln.f,v 1.17 2000-07-12 16:26:09 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU Lesser General Public License as
C published by the Free Software Foundation; either version 2.1 of the
C License, or (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C Lesser General Public License for more details.
C
C You should have received a copy of the GNU Lesser General Public
C License along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
      SUBROUTINE STRMLN (U,V,WORK,IMAX,IPTSX,JPTSY,NSET,IER)
C
C This is the old form of the main streamline entry point
C
C
C SUBROUTINE STRMLN (U,V,WORK,IMAX,IPTSX,JPTSY,NSET,IER)
C
C DIMENSION OF           U(IMAX,JPTSY) , V(IMAX,JPTSY) ,
C ARGUMENTS              WORK(2*IMAX*JPTSY)
C
C PURPOSE                STRMLN draws a streamline representation of
C                        the flow field. The representation is
C                        independent of the flow speed.
C
C USAGE                  If the following assumptions are met, use
C
C                            CALL EZSTRM  (U,V,WORK,IMAX,JMAX)
C
C                          Assumptions:
C                            --The whole array is to be processed.
C                            --The arrays are dimensioned
C                              U(IMAX,JMAX) , V(IMAX,JMAX) and
C                              WORK(2*IMAX*JMAX).
C                            --Window and viewport are to be chosen
C                              by STRMLN.
C                            --PERIM is to be called.
C
C                        If these assumptions are not met, use
C
C                            CALL STRMLN (U,V,WORK,IMAX,IPTSX,JPTSY,
C                                         NSET,IER)
C
C                        The user must call FRAME in the calling
C                        routine.
C
C                        The user may change various internal
C                        parameters via common blocks. See below.
C
C ARGUMENTS
C
C ON INPUT               U, V
C                          Two dimensional arrays containing the
C                          velocity fields to be plotted.
C
C                          Note:  If the U AND V components
C                          are, for example, defined in Cartesian
C                          coordinates and the user wishes to plot them
C                          on a different projection (i.e., stereo-
C                          graphic), then the appropriate
C                          transformation must be made to the U and V
C                          components via the functions FU and FV
C                          (located in STDRAW).
C
C                        WORK
C                          User provided work array.  The dimension
C                          of this array must be .GE. 2*IMAX*JPTSY.
C
C                          Caution:  This routine does not check the
C                          size of the work array.
C
C                        IMAX
C                          The first dimension of U and V in the
C                          calling program. (X-direction)
C
C                        IPTSX
C                          The number of points to be plotted in the
C                          first subscript direction.  (X-direction)
C
C                        JPTSY
C                          The number of points to be plotted in the
C                          second subscript direction. (Y-direction)
C
C                        NSET
C                          Flag to control scaling
C                          > 0  STRMLN assumes that the window
C                               and viewport have been set by the
C                               user in such a way as to properly
C                               scale the plotting instructions
C                               generated by STRMLN. PERIM is not
C                               called.
C                          = 0  STRMLN will establish the window and
C                               viewport to properly scale the
C                               plotting instructions to the standard
C                               configuration. PERIM is called to draw
C                               the border.
C                          < 0  STRMLN establishes the window
C                               and viewport so as to place the
C                               streamlines within the limits
C                               of the user's window.  PERIM is
C                               not called.
C
C ON OUTPUT              Only the IER argument may be changed. All
C                        other arguments are unchanged.
C
C
C                        IER
C                          =  0 when no errors are detected
C                          = -1 when the routine is called with ICYC
C                               .NE. 0  and the data are not cyclic
C                               (ICYC is an internal parameter
C                               described below); in this case the
C                               routine will draw the
C                               streamlines with the non-cyclic
C                               interpolation formulas.
C
C ENTRY POINTS           STRMLN, STDRAW, EZSTRM, STNEWP, STCYCL
C
C COMMON BLOCKS          STR01, STR02, STR03, STR04
C
C REQUIRED LIBRARY       GRIDAL, GBYTES, and the SPPS
C ROUTINES
C
C REQUIRED GKS LEVEL     0A
C
C I/O                    None
C
C PRECISION              Single
C
C LANGUAGE               FORTRAN 77
C
C The remainder of the original STRMLN discussion follows the code
C
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
      PARAMETER (IPLVLS = 64)
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
     +                ITHN       ,IPLR       ,ISST       ,
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
     +                RDFM       ,RSMD       ,RAMD       ,IGBS
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
C ------------------------------------------------------------------
C
C The old STRMLN common blocks
C (Now isolated to this compatibility routine)
C Note that STR01 and STR04 have been completely eliminated
C
      COMMON /STR02/  EXT , SIDE , XLT , YBT
C
      COMMON /STR03/  INITA , INITB , AROWL , ITERP , ITERC , IGFLG
     +             ,  IMSG , UVMSG , ICYC , DISPL , DISPC , CSTOP
C
      SAVE  /STR02/, /STR03/
C
C Local variables:
C
C Saved values of common block variables:
C (These violate the general rule of using only 1-3 chars for a
C  local variable)
C
C ISSGD, ISAGD, SARL, ISCKP, 
C ISCKX, ISTRP, ISCYK, SVNL, 
C ISSVF, SUSV, SVSV, SDFM, SCDS, SSSP
C
C IPM             - parameter use flag
C ICB             - common blocks use flag
C VXL,VXR,VYB,VYT - saved viewport boundary
C WXL,WXR,WYB,WYT - saved window boundary
C X1,X2,Y1,Y2     - temporary viewport boundary
C X3,Y3,X4,Y4     - temporary window boundary
C LEN             - maximum vector size in Metacode coords
C
      EXTERNAL STUMSL
      DIMENSION RDA(1)
      DIMENSION IDA(1)
      DATA RDA / 0.0 /
      DATA IDA / 0 /
C
C Save the values of all parameters that may get changed
C
      CALL STGETI('SET - Do SET Call Flag', ISSET)
      CALL STGETI('SGD - Stream Start Grid Increment', ISSGD)
      CALL STGETI('AGD - Arrow Placement Grid Increment', ISAGD)
      CALL STGETR('ARL - Arrow Length, Fraction Of Grid', SARL)
      CALL STGETI('CKP - Check Progress at Iteration Mod', ISCKP)
      CALL STGETI('CKX - Check Streamline Crossover Mod', ISCKX)
      CALL STGETI('TRP - Interpolation Method', ISTRP)
      CALL STGETI('CYK - Cyclical Data Flag', ISCYK)
      CALL STGETR('VNL - Normalized Vector Magnitude', SVNL)
      CALL STGETI('SVF - Special Value Flag', ISSVF)
      CALL STGETR('USV - U Array Special Value', SUSV)
      CALL STGETR('VSV - V Array Special Value', SVSV)
      CALL STGETR('DFM - Differential magnitude', SDFM) 
      CALL STGETR('CDS - Critical Displacement', SCDS)
      CALL STGETR('SSP - Streamline Spacing', SSSP)
      CALL STGETI('MSK - Masking Flag', ISMSK)
C
C Load the communication common block with parameters
C
C
      IF (IPTSX.LE.1 .OR. JPTSY.LE.1 .OR. IMAX.LT.IPTSX) THEN
         CSTR(1:26)='STRMLN - INVALID ARGUMENTS'
         CALL SETER (CSTR(1:26),1,2)
         STOP
      END IF
      LU=IMAX
      LV=IMAX
      M=IPTSX
      N=JPTSY
      LW=2*LU*N
      IS = 1
      IEND = IPTSX
      JS = 1
      JEND = JPTSY
C
C Set up the scaling of the plot.
C
      CALL GETSET (VXL,VXR,VYB,VYT,WXL,WXR,WYB,WYT,LL)
C
      X1=VXL
      X2=VXR
      Y1=VYB
      Y2=VYT
C
C Set the parameter and common block use flags
C Note that the value of ICPM is temporarily modified if it
C is equal to the default value. It will be restored as soon as
C the streamlines are drawn
C
      IPM = 0
      IF (ICPM.GE.-2 .AND. ICPM.LE.2) IPM = 1
      ICB = 0
      IF (ABS(ICPM).EQ.1 .OR. ABS(ICPM).EQ.3 .OR. ICPM.EQ.0) ICB = 1
      IF (ICPM .EQ. 0) ICPM = 99
C
      IF (IPM .EQ. 1) THEN
C
         ILL = 1
         IF (NSET .LT. 0) THEN
C     
            X3 = FLOAT(IS)
            X4 = FLOAT(IEND)
            Y3 = FLOAT(JS)
            Y4 = FLOAT(JEND)
            CALL SET(X1,X2,Y1,Y2,X3,X4,Y3,Y4,ILL)
C     
         ELSE IF (NSET .EQ. 0) THEN
C     
            X1 = XLT
            X2 = XLT+SIDE
            Y1 = YBT
            Y2 = YBT+SIDE
            X3 = FLOAT(IS)
            X4 = FLOAT(IEND)
            Y3 = FLOAT(JS)
            Y4 = FLOAT(JEND)
            IF (AMIN1(X4,Y4)/AMAX1(X4,Y4).GE.EXT) THEN
               IF (X4.GT.Y4)  THEN
                  Y2=SIDE*(Y4/X4)+YBT
               ELSE IF (Y4.GT.X4) THEN
                  X2=SIDE*(X4/Y4)+XLT
               END IF
            END IF
C
C Center the plot
C
            DX = 0.25*( 1. - (X2-X1) )
            DY = 0.25*( 1. - (Y2-Y1) )
            X1 = (XLT+DX)
            X2 = (X2+DX )
            Y1 = (YBT+DY)
            Y2 = (Y2+DY )
C
            CALL SET(X1,X2,Y1,Y2,X3,X4,Y3,Y4,ITYPE)
C     
            CALL PERIM (1,0,1,0)
C     
         END IF
C
C Since the set call has been handled STINIT should not do a SET
C
         CALL STSETI('SET - Do SET Call Flag', 0)
C
      END IF
C
C Common block values (STR03) (Save them first)
C
      IF (ICB .EQ. 1) THEN
C
         CALL STSETI('SGD - Stream Start Grid Increment', INITA)
         CALL STSETI('AGD - Arrow Placement Grid Increment', INITB)
         CALL STSETI('CKP - Check Progress at Iteration Mod', ITERP)
         CALL STSETI('CKX - Check Streamline Crossover Mod', ITERC)
         CALL STSETI('TRP - Interpolation Method', IGFLG)
         CALL STSETI('CYK - Cyclical Data Flag', ICYC)
         CALL STSETI('SVF - Special Value Flag', IMSG)
         CALL STSETR('USV - U Array Special Value', UVMSG)
         CALL STSETR('VSV - V Array Special Value', UVMSG)
         CALL STSETR('VNL - Normalized Vector Magnitude', DISPL)
C
C These parameters are roughly mapped to the new method of
C using viewport fraction rather than grid fraction.
C
         CALL STSETR('ARL - Arrow length, viewport fraction', 
     +        AROWL/FLOAT(M))
         CALL STSETR('SSP - Streamline spacing, VP fraction', 
     +        CSTOP/FLOAT(M))
         CALL STSETR('DFM - Differential magnitude, VP fraction', 
     +        DISPL/FLOAT(M))
         CALL STSETR('CDS - Critical displacement multiplier', 
     +        DISPC/DISPL)
C
      END IF
C
C Turn off masking in all cases: w/o an areamap it can't work
C
      CALL STSETI('MSK - mask to area map', 0)
C
C Initialize the stream plotting routine and draw the streamlines
C
      CALL STINIT(U,LU,V,LV,RDA,0,M,N,WORK,LW)
C
      CALL STREAM(U,V,RDA,IDA,STUMSL,WORK)
C
C Fetch the error value into the output parameter, IER
C
      CALL STGETI('ERR - Error identifier', IER)
C
C Restore original SET values, if required.
C
      IF (NSET .LE. 0) THEN
         CALL SET (VXL,VXR,VYB,VYT,WXL,WXR,WYB,WYT,LL)
      END IF
C
      CALL STSETI('SET - Do SET Call Flag', ISSET)
      CALL STSETI('SGD - Stream Start Grid Increment', ISSGD)
      CALL STSETI('AGD - Arrow Placement Grid Increment', ISAGD)
      CALL STSETR('ARL - Arrow Length, Fraction Of Grid', SARL)
      CALL STSETI('CKP - Check Progress at Iteration Mod', ISCKP)
      CALL STSETI('CKX - Check Streamline Crossover Mod', ISCKX)
      CALL STSETI('TRP - Interpolation Method', ISTRP)
      CALL STSETI('CYK - Cyclical Data Flag', ISCYK)
      CALL STSETR('VNL - Normalized Vector Magnitude', SVNL)
      CALL STSETI('SVF - Special Value Flag', ISSVF)
      CALL STSETR('USV - U Array Special Value', SUSV)
      CALL STSETR('VSV - V Array Special Value', SVSV)
      CALL STSETR('DFM - Differential magnitude', SDFM) 
      CALL STSETR('CDS - Critical Displacement', SCDS)
      CALL STSETR('SSP - Streamline Spacing', SSSP)
      CALL STGETI('MSK - Masking Flag', ISMSK)
C
C
C Restore the compatibility flag value if necessary
C
      IF (ICPM .EQ. 99) ICPM = 0
C
      RETURN
      END
C
C --------------------------------------------------------------------
C Original disucussion of the STRMLN algorithm follows:
C
C HISTORY                Written and standardized in November 1973.
C
C                        Converted to FORTRAN 77 and GKS in June, 1984.
C
C
C PORTABILITY            FORTRAN 77
C
C ALGORITHM              Wind components are normalized to the value
C                        of DISPL. The least significant two
C                        bits of the work array are
C                        utilized as flags for each grid box. Flag 1
C                        indicates whether any streamline has
C                        previously passed through this box.  Flag 2
C                        indicates whether a directional arrow has
C                        already appeared in a box. Judicious use
C                        of these flags prevents overcrowding of
C                        streamlines and directional arrows.
C                        Experience indicates that a final pleasing
C                        picture is produced when streamlines are
C                        initiated in the center of a grid box. The
C                        streamlines are drawn in one direction then
C                        in the opposite direction.
C
C REFERENCE              The techniques utilized here are described
C                        in an article by Thomas Whittaker (U. of
C                        Wisconsin) which appeared in the notes and
C                        correspondence section of Monthly Weather
C                        Review, June 1977.
C
C TIMING                 Highly variable
C                          It depends on the complexity of the
C                          flow field and the parameters:  DISPL,
C                          DISPC , CSTOP , INITA , INITB , ITERC ,
C                          and IGFLG. (See below for a discussion
C                          of these parameters.) If all values
C                          are default, then a simple linear
C                          flow field for a 40 x 40 grid will
C                          take about 0.4 seconds on the CRAY1-A;
C                          a fairly complex flow field will take about
C                          1.5 seconds on the CRAY1-A.
C
C
C INTERNAL PARAMETERS
C
C                        NAME     DEFAULT         FUNCTION
C                        ----     -------         --------
C
C                        EXT       0.25   Lengths of the sides of the
C                                         plot are proportional to
C                                         IPTSX and JPTSY except in
C                                         the case when MIN(IPTSX,JPT)
C                                         / MAX(IPTSX,JPTSY) .LT. EXT;
C                                         in that case a square
C                                         graph is plotted.
C
C                        SIDE      0.90   Length of longer edge of
C                                         plot. (See also EXT.)
C
C                        XLT       0.05   Left hand edge of the plot.
C                                         (0.0 = left edge of frame)
C                                         (1.0 = right edge of frame)
C
C                        YBT       0.05   Bottom edge of the plot.
C                                         (0.0 = bottom ; 1.0 = top)
C
C                                         (YBT+SIDE and XLT+SIDE must
C                                         be .LE. 1. )
C
C                        INITA     2      Used to precondition grid
C                                         boxes to be eligible to
C                                         start a streamline.
C                                         For example, a value of 4
C                                         means that every fourth
C                                         grid box is eligible ; a
C                                         value of 2 means that every
C                                         other grid box is eligible.
C                                         (see INITB)
C
C                        INITB     2      Used to precondition grid
C                                         boxes to be eligible for
C                                         direction arrows.
C                                         If the user changes the
C                                         default values of INITA
C                                         and/or INITB, it should
C                                         be done such that
C                                         MOD(INITA,INITB) = 0 .
C                                         For a dense grid try
C                                         INITA=4 and INITB=2 to
C                                         reduce the CPU time.
C
C                        AROWL     0.33   Length of direction arrow.
C                                         For example, 0.33 means
C                                         each directional arrow will
C                                         take up a third of a grid
C                                         box.
C
C                        ITERP     35     Every 'ITERP' iterations
C                                         the streamline progress
C                                         is checked.
C
C                        ITERC     -99    The default value of this
C                                         parameter is such that
C                                         it has no effect on the
C                                         code. When set to some
C                                         positive value, the program
C                                         will check for streamline
C                                         crossover every 'ITERC'
C                                         iterations. (The routine
C                                         currently does this every
C                                         time it enters a new grid
C                                         box.)
C                                         Caution:  When this
C                                         parameter is activated,
C                                         CPU time will increase.
C
C                        IGFLG     0      A value of zero means that
C                                         the sixteen point Bessel
C                                         Interpolation Formula will
C                                         be utilized where possible;
C                                         when near the grid edges,
C                                         quadratic and bi-linear
C                                         interpolation  will be
C                                         used. This mixing of
C                                         interpolation schemes can
C                                         sometimes cause slight
C                                         raggedness near the edges
C                                         of the plot.  If IGFLG.NE.0,
C                                         then only the bilinear
C                                         interpolation formula
C                                         is used; this will generally
C                                         result in slightly faster
C                                         plot times but a less
C                                         pleasing plot.
C
C                        IMSG      0      If zero, then no missing
C                                         U and V components are
C                                         present.
C                                         If .NE. 0, STRMLN will
C                                         utilize the
C                                         bi-linear interpolation
C                                         scheme and terminate if
C                                         any data points are missing.
C
C                        UVMSG     1.E+36 Value assigned to a missing
C                                         point.
C
C                        ICYC      0      Zero means the data are
C                                         non-cyclic in the X
C                                         direction.
C                                         If .NE 0, the
C                                         cyclic interpolation
C                                         formulas will be used.
C                                         (Note:  Even if the data
C                                         are cyclic in X, leaving
C                                         ICYC = 0 will do no harm.)
C
C                        DISPL     0.33   The wind speed is
C                                         normalized to this value.
C                                         (See the discussion below.)
C
C                        DISPC     0.67   The critical displacement.
C                                         If after 'ITERP' iterations
C                                         the streamline has not
C                                         moved this distance, the
C                                         streamline will be
C                                         terminated.
C
C                        CSTOP     0.50   This parameter controls
C                                         the spacing between
C                                         streamlines.  The checking
C                                         is done when a new grid
C                                         box is entered.
C
C DISCUSSION OF          Assume a value of 0.33 for DISPL.  This
C DISPL,DISPC            means that it will take three steps to move
C AND CSTOP              across one grid box if the flow was all in the
C                        X direction. If the flow is zonal, then a
C                        larger value of DISPL is in order.
C                        If the flow is highly turbulent, then
C                        a smaller value is in order.  The smaller
C                        DISPL, the more the CPU time.  A value
C                        of 2 to 4 times DISPL is a reasonable value
C                        for DISPC.  DISPC should always be greater
C                        than DISPL. A value of 0.33 for CSTOP would
C                        mean that a maximum of three stream-
C                        lines will be drawn per grid box. This max
C                        will normally only occur in areas of singular
C                        points.
C
C                                            ***************************
C                                            Any or all of the above
C                                            parameters may be changed
C                                            by utilizing common blocks
C                                            STR02 and/or STR03
C                                            ***************************
C
C                        UXSML               A number which is small
C                                            compared to the average
C                                            normalized u component.
C                                            Set automatically.
C
C                        NCHK      750       This parameter is located
C                                            in STDRAW. It specifies the
C                                            length of the circular
C                                            lists  used for checking
C                                            for STRMLN crossovers.
C                                            For most plots this number
C                                            may be reduced to 500
C                                            or less and the plots will
C                                            not be altered.
C
C                        ISKIP               Number of bits to be
C                                            skipped to get to the
C                                            least two significant bits
C                                            in a floating point number.
C                                            The default value is set to
C                                            I1MACH(5) - 2 . This value
C                                            may have to be changed
C                                            depending on the target
C                                            computer; see subroutine
C                                            STDRAW.
C
C --------------------------------------------------------------------

C
C $Id: tdez2d.f,v 1.4 2000-07-12 16:26:32 haley Exp $
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
      SUBROUTINE TDEZ2D(NX,NY,X,Y,Z,RMULT,THETA,PHI,IST)
C
C  This subroutine uses the NCAR Graphics functions in Tdpack
C  to draw a surface plot.  TDEZ2D is meant to be a simplified 
C  interface to Tdpack for the purpose of quickly drawing a plot.  
C  TDEZ2D sacrifices much of the flexibility and functionality of 
C  Tdpack in its attempt to be simple.
C
C  The data values plotted are:
C
C      Z(I,J) = data value at (X(I),Y(J)) for I=1,NX and J=1,NY.
C
C  The point looked at is the midpoint of the surface.
C
C  The eye position is calculated from the 3D coordinate
C  (RMULT,THETA,PHI) as provided in the argument list:
C
C      RMULT  is a multiplier of the diagonal length (DL) of 
C             the smallest box containing the surface to be drawn.
C      THETA  is an angle (in degrees) in the XY plane measured
C             positive counter-clockwise from the X axis
C      PHI    is an angle (in degrees) measured from the positive Z 
C             axis toward the XY plane.  
C
C  Thus, the coordinate (RMULT*DL,THETA,PHI) is the spherical 
C  coordinate for the eye position.  If RMULT = THETA = PHI = 0., 
C  a default eye position ( 2.5,-55.,70.) is chosen.
C
C      IST    is a style index defining the colors used to shade the
C             surface as per:
C
C               1  -  wire frame
C               2  -  gray shades underneath; gray shades on top.
C               3  -  gray shades underneath; red shades on top.
C               4  -  gray shades underneath; green shades on top.
C               5  -  gray shades underneath; blue shades on top.
C               6  -  gray shades underneath; cyan shades on top.
C               7  -  gray shades underneath; magenta shades on top.
C               8  -  gray shades underneath; yellow shades on top.
C
C             If IST is positive, then black is used for the background
C             color and white for the foreground color; if IST is 
C             the negative of any of the above values, then white
C             is used for the background color and black for the
C             foreground color.
C  
C  When TDEZ2D is called, a color table is defined for all open
C  workstations that implements IST as described above.  *This 
C  color table will supersede any color table that has been 
C  previously defined.*  The color table that is defined is:
C
C      Color 
C      index   Colors
C    -------   ----------------------------------------------------
C          0   black if IST is positive; white if IST is negative
C          1   white if IST is positive; black if IST is negative
C          2   red
C          3   green
C          4   blue
C          5   cyan
C          6   magenta
C          7   yellow
C      8- 37   grayscale from white to black.
C     38- 67   shades of gray
C     68- 97   shades of red
C     98-127   shades of green
C    128-157   shades of blue
C    158-187   shades of cyan
C    188-217   shades of magenta
C    218-247   shades of yellow
C         
C
C  TDEZ2D does not call FRAME.
C
C  If the image is too small, decrease the size of RMULT; if the
C  image is too large, increase the size of RMULT.
C
C  Example:
C
C    CALL TDEZ2D(NX,NY,X,Y,Z,0.,0.,0.,6)
C
C    would draw a surface in shades of cyan with a black background and
C    with a default eye position selected.
C
C------------------------------------------------------------------------------
C
C  Set the maximum number of triangles.
C
      PARAMETER (MTRI=110000)
C
C  Dimension the input arrays.
C
      DIMENSION X(NX),Y(NY),Z(NX,NY)
C
C  Dimension the Tdpack work arrays.
C
      DIMENSION RTRI(10,MTRI),RTWK(MTRI,2),ITWK(MTRI)
      COMMON /DSTDDT/ RTRI,RTWK,ITWK
C
C  Define the spherical coordinates for the default eye position.
C
      DATA ANG1,ANG2,RMUL / -55.,70.,2.5 /
C
C  Set the desired values of the shading parameters.  Values of SHDE
C  near 0 give brighter colors and values near 1 give pastel shades.
C  Values of SHDR near 0 give a narrow range of shades and values near
C  1 give a wide range of shades.
C
      DATA SHDE,SHDR / 0.0 , 0.8 /
C
C  Specify the number of shades for each color, the starting 
C  color index for the color shades part of the color table,
C  the color index of the first gray value, and the color index
C  of the last gray value.
C
      PARAMETER (NSHD=30, ICST=8, IGS=ICST+NSHD, IGL=IGS+NSHD-1)
C
C  Factor for converting from degrees to radians.
C
      DATA DTOR / .017453292519943 /
C
C  Determine the number of open workstations (NUMOP).
C
      CALL GQOPWK(1,IER,NUMOP,IWKID)
C
C  Loop through the open workstations and define a color table.
C
      IFC = ICST
      ILC = ICST+NSHD-1
      DO 10 I=1,NUMOP
        CALL GQOPWK(I,IER,NUMTMP,IWKID)
C
C  Set colors only for workstations of the correct category.
C
        CALL GQWKC(IWKID,IER,ICON,ITYP)
        CALL GQWKCA(ITYP,IER,ICAT)
        IF (ICAT.EQ.0 .OR. ICAT.EQ.2 .OR. ICAT.EQ.4) THEN
          CALL TDCLRS(IWKID,1-(SIGN(1,IST)+1)/2,SHDE,SHDR,IFC,ILC,8)
        ENDIF
   10 CONTINUE
C
C  Find data mins and maxs.
C
      XMIN = X(1)
      XMAX = X(NX)
      YMIN = Y(1)
      YMAX = Y(NY)
      ZMIN = Z(1,1)
      ZMAX = Z(1,1)
      DO 130 I=1,NX
        DO 140 J=1,NY
          ZMIN = MIN(ZMIN,Z(I,J))
          ZMAX = MAX(ZMAX,Z(I,J))
  140   CONTINUE
  130 CONTINUE
      XRNG = XMAX-XMIN
      YRNG = YMAX-YMIN
      ZRNG = ZMAX-ZMIN
      XMID = 0.5*(XMIN+XMAX)
      YMID = 0.5*(YMIN+YMAX)
      ZMID = 0.5*(ZMIN+ZMAX)
C
C  Define TDPACK rendering styles 1 through 8.  The 
C  indices 1-8 can then be used as final arguments in 
C  calls to TDITRI, TDSTRI, and TDMTRI.
C
      XSL = 0.05*XRNG
      YSL = 0.05*YRNG
      ZSL = 0.00*ZRNG
      CALL TDSTRS (1,  0,  0,         0,         0,1,1,0,XSL,YSL,ZSL)
      CALL TDSTRS (2,IGS,IGL,IFC+1*NSHD,ILC+1*NSHD,1,1,0,XSL,YSL,ZSL)
      CALL TDSTRS (3,IGS,IGL,IFC+2*NSHD,ILC+2*NSHD,1,1,0,XSL,YSL,ZSL)    
      CALL TDSTRS (4,IGS,IGL,IFC+3*NSHD,ILC+3*NSHD,1,1,0,XSL,YSL,ZSL)    
      CALL TDSTRS (5,IGS,IGL,IFC+4*NSHD,ILC+4*NSHD,1,1,0,XSL,YSL,ZSL)    
      CALL TDSTRS (6,IGS,IGL,IFC+5*NSHD,ILC+5*NSHD,1,1,0,XSL,YSL,ZSL)    
      CALL TDSTRS (7,IGS,IGL,IFC+6*NSHD,ILC+6*NSHD,1,1,0,XSL,YSL,ZSL)    
      CALL TDSTRS (8,IGS,IGL,IFC+7*NSHD,ILC+7*NSHD,1,1,0,XSL,YSL,ZSL)    
C
C  Create the triangle list representing the surface.  If the specified
C  value for IST is out of range, set it to 6.
C
      NTRI=0
      IF (ABS(IST).GT.8 .OR. ABS(IST).LT.1) THEN
        JST = 6
      ELSE
        JST = ABS(IST)
      ENDIF
      CALL TDSTRI (X,NX,Y,NY,Z,NX,RTRI,MTRI,NTRI,JST)
      IF (NTRI .EQ. MTRI) THEN
        PRINT * , 'Triangle list overflow in TDITRI'
        STOP
      END IF
C
C  Determine a default eye position if none is specified.
C
      IF (THETA.EQ.0. .AND. PHI.EQ.0. .AND. RMULT.EQ.0.) THEN
        R = RMUL*SQRT(XRNG*XRNG + YRNG*YRNG + ZRNG*ZRNG)
        XEYE = XMID+R*SIN(DTOR*ANG2)*COS(DTOR*ANG1)
        YEYE = YMID+R*SIN(DTOR*ANG2)*SIN(DTOR*ANG1)
        ZEYE = ZMID+R*COS(DTOR*ANG2)
      ELSE
C
C  Convert the user-specified eye position to Cartesian coordinates.
C
        R = RMULT*SQRT(XRNG*XRNG + YRNG*YRNG + ZRNG*ZRNG)
        XEYE = XMID+R*SIN(DTOR*PHI)*COS(DTOR*THETA)
        YEYE = YMID+R*SIN(DTOR*PHI)*SIN(DTOR*THETA)
        ZEYE = ZMID+R*COS(DTOR*PHI)
      ENDIF
C
C Initialize TDPACK.
C
      CALL TDINIT (XEYE, YEYE, ZEYE, XMID, YMID, ZMID,
     +                   XMID, YMID, ZMID+0.1*ZRNG, 0)
C
C Order the triangles.
C
      CALL TDOTRI (RTRI,MTRI,NTRI,RTWK,ITWK,1)
      IF (NTRI .EQ. MTRI) THEN
        PRINT * , 'Triangle list overflow in TDOTRI'
        STOP
      END IF
C
C  Draw the triangles.
C
      CALL TDDTRI (RTRI,MTRI,NTRI,ITWK)
C
      RETURN
      END

C
C $Id: wmgtso.f,v 1.5 2000-07-12 16:27:04 haley Exp $
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
      SUBROUTINE WMGTSO(X1,Y1,X2,Y2,NOUT,XIN,YIN,IFLG)
C
C  Returns the symbol outlines:
C
C  Returns points along the Bezier curve for a warm front symbol.  
C  This is an arch beginning at (X2,Y2) and traversing to (X1,Y1).
C  NOUT must be greater than 2.
C
C  Returns points of a triangle for cold front symbols.
C
C  If IFLG has absolute value "1", then warm front symbols are 
C  returned; if IFLG has absolute value "2", then cold front symbols 
C  are returned.  The algebraic sign of IFLG determines which side
C  the symbol points to (the two possibilities are mirror-imaged
C  about the line connecting the points (X1,Y1) and (X2,Y2).
C  
      PARAMETER (NDIM=7)
C
      include 'wmcomn.h'
C
      DIMENSION XIN(NOUT),YIN(NOUT)
      DIMENSION WSBZCX(NDIM),WSBZCY(NDIM)
      DIMENSION WSXTMP(NDIM),WSYTMP(NDIM),WXTRAN(NDIM),WYTRAN(NDIM)
C
C  X and Y Bezier control points for the warm front symbol.
C
      DATA WSBZCX/ 1.00000,  0.94737,  0.36842,  0.00000,
     +            -0.36842, -0.94737, -1.00000           /
      DATA WSBZCY/ 0.00000, -1.05263, -1.42100, -1.42100,
     +            -1.42100, -1.05263,  0.00000           /
C
C  Scale the control points, and flip depending of the direction
C  the symbols are to be drawn in.
C
      D  = SQRT((Y2-Y1)**2+(X2-X1)**2)
      SS = 0.5*D
      DO 10 I=1,NDIM
        WXTRAN(I) = SS*WSBZCX(I)
        WYTRAN(I) = SS*ISIGN(1,IFLG)*WSBZCY(I)
   10 CONTINUE
C
C  Make the cold front symbols a little larger.
C
      IF (IABS(IFLG) .EQ. 1) THEN
        WYTRAN(4) = 1.1*WYTRAN(4)
      ENDIF
C
C  Rotate the control points.
C
      COSANG = (X2-X1)/D
      SINANG = (Y2-Y1)/D
      DO 20 I=1,NDIM
        WSXTMP(I) = WXTRAN(I)*COSANG-WYTRAN(I)*SINANG
        WSYTMP(I) = WXTRAN(I)*SINANG+WYTRAN(I)*COSANG
   20 CONTINUE
      DO 30 I=1,NDIM
        WXTRAN(I) = WSXTMP(I)
        WYTRAN(I) = WSYTMP(I)
   30 CONTINUE
C
C  Translate the control points.
C
      XM = 0.5*(X1+X2)
      YM = 0.5*(Y1+Y2)
      DO 40 I=1,NDIM
        WXTRAN(I) = XM+WXTRAN(I)
        WYTRAN(I) = YM+WYTRAN(I)
   40 CONTINUE
C
      IF (IABS(IFLG).EQ.1 .OR. IABS(IFLG).EQ.3) THEN
C
C  Return cold front symbol.
C
        XIN(1) =  WXTRAN(1)
        YIN(1) =  WYTRAN(1)
        XIN(2) =  WXTRAN(4)
        YIN(2) =  WYTRAN(4)
        XIN(3) =  WXTRAN(7)
        YIN(3) =  WYTRAN(7)
      ELSE
C
C  Calculate the points along the Bezier curve for a warm front symbol.
C
        NHALF = NOUT/2
        CALL BCFCRV(WXTRAN(1),WYTRAN(1),NHALF+1,XIN(1),YIN(1))
        IF (MOD(NOUT,2) .EQ. 0) THEN
          CALL BCFCRV(WXTRAN(4),WYTRAN(4),NHALF,
     +                XIN(NHALF+1),YIN(NHALF+1))       
        ELSE
          CALL BCFCRV(WXTRAN(4),WYTRAN(4),NHALF+1,
     +                XIN(NHALF+1),YIN(NHALF+1))
        ENDIF
      ENDIF
C
      RETURN
      END

C
C $Id: nadraw.f,v 1.3 2000-08-22 15:37:36 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published
C by the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
      SUBROUTINE NADRAW (NUMV,VORX,VORY,IWORK,WORK)
C
C  Draw a Voronoi polygon whose vertices are (VORX(I),VORY(I),I=1,NUMV), 
C  but are not in any particular order.
C
      DIMENSION VORX(NUMV),VORY(NUMV),IWORK(NUMV),WORK(NUMV)
      DIMENSION XX(2),YY(2)
      PARAMETER (D2R = 57.29578)
C
C  Find the extreme values.
C
      XMN =  VORX(1)
      XMX =  VORX(1)
      YMN =  VORY(1)
      YMX =  VORY(1)
C
      DO 10 I=2,NUMV
        XMN = MIN(XMN,VORX(I))
        XMX = MAX(XMX,VORX(I))
        YMN = MIN(YMN,VORY(I))
        YMX = MAX(YMX,VORY(I))
   10 CONTINUE
C
C  Find a point in the middle of the polygon as a reference point.
C
      XMID = 0.5*(XMX+XMN)
      YMID = 0.5*(YMX+YMN)
C
C  Find the angles to the vertices from the reference point and sort 
C  the vertices in ascending angular order.
C
      DO 15 I=1,NUMV
        WORK(I) = ATAN2(VORY(I)-YMID,VORX(I)-XMID)*D2R
   15 CONTINUE
C
      CALL NASORT(WORK,IWORK,NUMV)
C
C  Draw lines between the vertices.
C
      CALL GQPLCI(IER,ICOL)
      CALL GQLN(IER,ILN)
      CALL SFLUSH()
      CALL GSPLCI(5)
      CALL GSLN(1)
      DO 20 I=1,NUMV-1
        IDX1 = IWORK(I)
        IDX2 = IWORK(I+1)
        XX(1) = VORX(IDX1)
        YY(1) = VORY(IDX1)
        XX(2) = VORX(IDX2)
        YY(2) = VORY(IDX2)
        CALL GPL(2,XX,YY)
   20 CONTINUE
      IDX1 = IWORK(NUMV)
      IDX2 = IWORK(1)
      XX(1) = VORX(IDX1)
      YY(1) = VORY(IDX1)
      XX(2) = VORX(IDX2)
      YY(2) = VORY(IDX2)
      CALL GPL(2,XX,YY)
      CALL SFLUSH()
      CALL GSPLCI(ICOL)
      CALL GSLN(ILN)
C    
      RETURN
      END

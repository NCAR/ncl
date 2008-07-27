C
C $Id: nadraw.f,v 1.4 2008-07-27 03:11:53 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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

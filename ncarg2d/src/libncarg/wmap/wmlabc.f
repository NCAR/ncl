C
C	$Id: wmlabc.f,v 1.5 2000-08-22 15:07:46 haley Exp $
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
      SUBROUTINE WMLABC(X,Y,CITY,TEMPS) 
C
C  Plot the city name in CITY and the daily temperatures
C  in TEMPS in two lines, all centered at (X,Y).  The
C  text will be surrounded with a box in the background
C  color.
C
      include 'wmcomn.h'
C
      CHARACTER*(*) CITY,TEMPS
C
C  Save the current line and fill colors and set them to ICOLOR.
C
      CALL GQFAIS(IER,IFAISO)
      CALL GQFACI(IER,IFCLRO)
      CALL GQPLCI(IER,ILCLRO)
      CALL GSFACI(ICOLOR)
      CALL GSPLCI(ICOLOR)
      CALL GSFAIS(1)
C
C  Convert X and Y to NDC and work in NDC space.
C
      CALL WMW2NX(1,X,XNDC)
      CALL WMW2NY(1,Y,YNDC)
      CALL GQCNTN(IER,NTRO)
      CALL GSELNT(0)
C
      SIZEL = WSIZEC
C
C  Save Plotchar parameters.
C
      CALL PCGETI ('FN - font name',IFNO)
C
C  Draw the city label and daily hi/lows.
C
      GAP = .9*SIZEL
      CALL PCSETI ('FN - font name',22)
C
      CALL WMGETI ('RBS',IRBSO)
      CALL WMGETI ('RLS',IRLSO)
      CALL WMGETI ('ROS',IROSO)
C
      CALL WMSETI ('RBS',IBGCTY)
      CALL WMSETI ('RLS',-1)
      CALL WMSETI ('ROS',-1)
C
      CALL WMCHBG (XNDC,YNDC+0.5*(GAP+SIZEL),CITY,SIZEL)
      CALL WMCHBG (XNDC,YNDC-0.5*(GAP+SIZEL),TEMPS,0.9*SIZEL)
      CALL WMSETI ('RBS',IRBSO)
      CALL WMSETI ('RLS',IRLSO)
      CALL WMSETI ('ROS',IROSO)
C
C  Restore Plotchar parameters.
C
      CALL PCSETI ('FN',IFNO)
C
C  Restore GKS environment.
C
      CALL GSFAIS(IFAISO)
      CALL GSFACI(IFCLRO)
      CALL GSPLCI(ILCLRO)
      CALL GSELNT(NTRO)
C
      RETURN
      END

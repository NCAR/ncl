C
C	$Id: wmlabc.f,v 1.6 2008-07-27 00:17:36 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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

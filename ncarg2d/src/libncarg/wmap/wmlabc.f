C
C	$Id: wmlabc.f,v 1.1 1994-09-09 23:55:09 fred Exp $
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
      CALL PCGETI ('CC - character color',ICCO)
      CALL PCGETI ('FN - font name',IFNO)
C
C  Draw the city label and daily hi/lows.
C
      GAP = .9*SIZEL
      CALL PCSETI ('CC - character color',ICOLOR)
      CALL PCSETI ('FN - font name',22)
      CALL WMCHBG (XNDC,YNDC+0.5*(GAP+SIZEL),CITY,SIZEL,CTYMRG,IBGCTY)
      CALL WMCHBG (XNDC,YNDC-0.5*(GAP+SIZEL),TEMPS,0.9*SIZEL,CTYMRG,
     +             IBGCTY)
C
C  Restore Plotchar parameters.
C
      CALL PCSETI ('CC',ICCO)
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

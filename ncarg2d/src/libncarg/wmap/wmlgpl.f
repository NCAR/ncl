C
C	$Id: wmlgpl.f,v 1.3 1994-09-23 17:13:58 fred Exp $
C
      SUBROUTINE WMLGPL(N,X,Y)
C
C  Draws polylines either using GPL (IWDTYP=0) or WMDRFL
C  (IWDTYP=1).
C
      DIMENSION X(N),Y(N)
C
      include 'wmcomn.h'
C
C  Save the current line attributes and reset.
C
      CALL GQPLCI(IER,ICOLD)
      CALL GSPLCI(ICOLOR)
C
C  Handle the squall lines (IFRONT = 5), tropical fronts (IFRONT = 6), 
C  and convergence lines (IFRONT = 7) separately.
C
      IF (IFRONT .EQ. 5) THEN
        CALL GQLWSC(IER,OSCL)
        CALL GSLWSC(DLINWD)
        CALL DPGETI('DPS',IDPS)
        CALL DPGETR('WOG',OWOG)
        CALL DPGETR('WOS',OWOS)
        CALL DPGETR('LS1',OLS1)
C
        CALL DPSETR('WOG',.003)
        CALL DPSETR('WOS',.0035)
        CALL DPSETR('LS1',0.0)
        CALL DPSETC('DPT','___:F34X150Y150:7_:F34X150Y150:7___$$$$$$$$$$
     +$$$$$$')
        CALL GSLWSC(2.)
        CALL DPCURV(X,Y,N)
C
        CALL GSLWSC(OSCL)
        CALL DPSETI('DPS',IDPS)
        CALL DPSETC('DPT',' ')
        CALL DPSETR('LS1',OLS1)
        CALL DPSETR('WOG',OWOG)
        CALL DPSETR('WOS',OWOS)
        RETURN
      ELSE IF (IFRONT .EQ. 6) THEN
        CALL GQLWSC(IER,OSCL)
        CALL GSLWSC(DLINWD)
        CALL DPGETI('DPS',IDPS)
        CALL DPGETI('DPT',IDPT)
        CALL DPGETR('WOG',OWOG)
        CALL DPGETR('WOS',OWOS)
C
        CALL DPSETI('DPS',-7)
        CALL DPSETR('WOG',.0075)
        CALL DPSETR('WOS',.0075)
        CALL DPSETI('DPT',31)
        CALL DPCURV(X,Y,N)
C
        CALL GSLWSC(OSCL)
        CALL DPSETI('DPS',IDPS)
        CALL DPSETI('DPT',IDPT)
        CALL DPSETR('WOG',OWOG)
        CALL DPSETR('WOS',OWOS)
        RETURN
      ELSE IF (IFRONT .EQ. 7) THEN
        CALL GQLWSC(IER,OSCL)
        CALL GSLWSC(DLINWD)
        CALL DPGETI('DPS',IDPS)
        CALL DPGETI('DPT',IDPT)
        CALL DPGETR('WOG',OWOG)
        CALL DPGETR('WOS',OWOS)
C
        CALL DPSETI('DPS',-5)
        CALL DPSETR('WOG',.0075)
        CALL DPSETR('WOS',.0075)
        CALL DPSETI('DPT',31)
        CALL DPCURV(X,Y,N)
C
        CALL GSLWSC(OSCL)
        CALL DPSETI('DPS',IDPS)
        CALL DPSETI('DPT',IDPT)
        CALL DPSETR('WOG',OWOG)
        CALL DPSETR('WOS',OWOS)
        RETURN
      ENDIF
C
      IF (IWDTYP .EQ. 1) THEN
        CALL WMDRFL(N,X,Y)
      ELSE
        CALL GPL(N,X,Y)
      ENDIF
C
C  Restore line attributes.
C
      CALL GSPLCI(ICOLD)
C
      RETURN
      END

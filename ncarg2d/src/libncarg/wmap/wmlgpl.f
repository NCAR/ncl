C
C	$Id: wmlgpl.f,v 1.6 2000-08-22 15:07:47 haley Exp $
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
      SUBROUTINE WMLGPL(N,X,Y)
C
C  Draws polylines either using GPL (IWDTYP=0) or WMDRFL (IWDTYP=1).
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
        CALL DPSETI('DPS',-22)
        CALL DPSETR('WOG',.007)
        CALL DPSETR('WOS',.007)
        CALL DPSETI('DPT',4177920)
        CALL GSPLCI(ITRO1C)
        CALL DPCURV(X,Y,N)
        CALL DPSETI('DPT',2040)
        CALL GSPLCI(ITRO2C)
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

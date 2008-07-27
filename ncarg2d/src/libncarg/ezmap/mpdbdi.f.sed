C
C $Id: mpdbdi.f.sed,v 1.4 2008-07-27 01:15:50 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MPDBDI (FLNM,ISTA)
        CHARACTER*(*) FLNM
        FLNM=' '
	CALL GNGPAT (FLNM,'SED_DBDIR',ISTA)
	RETURN
      END

C
C $Id: mputis.f,v 1.6 2008-07-27 00:17:09 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MPUTIS (UVAL,VVAL,RLAT,RLON)
        REAL UVAL,VVAL,RLAT,RLON
        CALL MDUTIS (UVAL,VVAL,RLAT,RLON)
        RETURN
      END

C
C $Id: mputfs.f,v 1.6 2008-07-27 00:17:09 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MPUTFS (RLAT,RLON,UVAL,VVAL)
        REAL RLAT,RLON,UVAL,VVAL
        CALL MDUTFS (RLAT,RLON,UVAL,VVAL)
        RETURN
      END

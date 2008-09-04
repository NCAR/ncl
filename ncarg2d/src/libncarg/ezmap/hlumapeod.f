C
C $Id: hlumapeod.f,v 1.10 2008-09-04 19:56:57 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE HLUMAPEOD (NOUT,NSEG,IDLS,IDRS,NPTS,PNTS)
        INTEGER NOUT,NSEG,IDLS,IDRS,NPTS
        REAL    PNTS(*)
        CALL MAPEOD (NOUT,NSEG,IDLS,IDRS,NPTS,PNTS)
        RETURN
      END

C
C $Id: hlumapeod.f,v 1.13 2008-09-18 00:42:16 kennison Exp $
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

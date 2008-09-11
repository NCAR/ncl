C
C $Id: hlumapeod.f,v 1.11 2008-09-11 04:11:35 kennison Exp $
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

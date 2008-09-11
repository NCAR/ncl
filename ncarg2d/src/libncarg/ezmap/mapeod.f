C
C $Id: mapeod.f,v 1.15 2008-09-11 22:53:32 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MAPEOD (NOUT,NSEG,IDLS,IDRS,NPTS,PNTS)
        INTEGER NOUT,NSEG,IDLS,IDRS,NPTS
        REAL    PNTS(*)
        CALL MDPEOD (NOUT,NSEG,IDLS,IDRS,NPTS,PNTS)
        RETURN
      END

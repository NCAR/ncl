C
C $Id: mputin.f,v 1.8 2008-07-27 00:17:09 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MPUTIN (IPRJ,IZON,ISPH,PADP,UMIN,UMAX,VMIN,VMAX)
        DOUBLE PRECISION PADP(15),UMIN,UMAX,VMIN,VMAX
        CALL MDUTIN (IPRJ,IZON,ISPH,PADP,UMIN,UMAX,VMIN,VMAX)
        RETURN
      END

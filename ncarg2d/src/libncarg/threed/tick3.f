C
C $Id: tick3.f,v 1.6 2008-07-27 00:17:34 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE TICK3 (MAG,MIN)
      CALL TICK43 (MAG,MIN,MAG,MIN,MAG,MIN)
      RETURN
      END

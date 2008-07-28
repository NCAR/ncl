C
C	$Id: pcffsp.h,v 1.5 2008-07-28 14:03:54 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      PARAMETER (IBZL = 129)
      COMMON /PCBZSP/BCNTLX(4), BCNTLY(4), BZXC(IBZL), BZYC(IBZL)
      REAL           BCNTLX   , BCNTLY   , BZXC      , BZYC
      SAVE   /PCBZSP/

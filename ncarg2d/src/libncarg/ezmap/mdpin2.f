C
C $Id: mdpin2.f,v 1.6 2008-09-11 04:11:36 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDPIN2 (TST1,TST2,TST3)
        REAL TST1,TST2,TST3
        TST1=1.
        TST2=TST1+1.E-10
        TST3=TST2+1.E-10
        RETURN
      END

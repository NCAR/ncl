C
C $Id: gagtrn.f,v 1.4 2008-07-27 00:17:13 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GAGTRN (NSDR,TMP1,TMP2,TMP3)
C
C This routine computes some quantities needed by GRIDAL; the code is
C here so as to ensure that, on machines on which arithmetic is done
C in double-precision registers, these quantities will be truncated to
C real precision before being used in tests.
C
        TMP1=10.**(-NSDR)
        TMP2=  1.+TMP1
        TMP3=TMP2+TMP1
C
C Done.
C
        RETURN
C
      END

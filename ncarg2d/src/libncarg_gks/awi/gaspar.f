C
C	$Id: gaspar.f,v 1.5 2008-07-27 00:20:56 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GASPAR(IOS,STATUS)
C
C  Parse the ASPECT SOURCE FLAG elements.
C
      include 'trstat.h'
      include 'trpars.h'
      include 'trinst.h'
C
      INTEGER IOS, STATUS
      PARAMETER (ICVMAX=15)
      INTEGER TMPASF(2), ASFCNT, II, ICONV(ICVMAX)
      DATA ICONV/1,2,3,4,5,6,7,7,8,9,10,11,13,12,12/
C
C  Compute the number of aspect source flags defined.
C
      ASFCNT = (LEN / (MENCPR/BYTSIZ)) / 2
C
C  Extract the ASF pointer and value (since the ASFs are stored in
C  the segments as CGM elements, the values are reversed, so 
C  complement them here).
C
      DO 10 II = 1,ASFCNT
        CALL GOPDEC(TMPASF,MENCPR,2,IOS,STATUS)
        IF (TMPASF(1).GE.0 .AND. TMPASF(1).LT.ASFMAX) THEN
          IF(II .LE. ICVMAX) GASFSF(ICONV(II)) = 1-TMPASF(2)
        END IF
 10   CONTINUE
      CALL GSASF(GASFSF)
C
      RETURN
      END

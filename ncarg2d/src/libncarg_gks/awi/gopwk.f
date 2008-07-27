C
C	$Id: gopwk.f,v 1.10 2008-07-27 00:20:58 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GOPWK(WKID,CONID,WTYPE)
C
      INTEGER WKID,CONID,WTYPE
C
      CALL GZOPWK(WKID,CONID,WTYPE)
C
      RETURN
      END

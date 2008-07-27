C
C $Id: nnuser_f.f,v 1.5 2008-07-27 03:10:13 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE NNSETC(PNAM,CVAL)
C
      CHARACTER*(*) PNAM,CVAL
C
      ILEN = LEN(CVAL)
      CALL FNNSETC(PNAM,CVAL,ILEN)
C
      RETURN
      END
      SUBROUTINE NNGETC(PNAM,CVAL)
C
      CHARACTER*(*) PNAM,CVAL
C
      ILEN = LEN(CVAL)
      CALL FNNGETC(PNAM,CVAL,ILEN)
C
      RETURN
      END

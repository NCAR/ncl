C
C $Id: dsuser_f.f,v 1.4 2008-07-27 03:10:09 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE DSSETC(PNAM,CVAL)
C
      CHARACTER*(*) PNAM,CVAL
C
      ILEN = LEN(CVAL)
      CALL FDSSETC(PNAM,CVAL,ILEN)
C
      RETURN
      END
      SUBROUTINE DSGETC(PNAM,CVAL)
C
      CHARACTER*(*) PNAM,CVAL
C
      ILEN = LEN(CVAL)
      CALL FDSGETC(PNAM,CVAL,ILEN)
C
      RETURN
      END

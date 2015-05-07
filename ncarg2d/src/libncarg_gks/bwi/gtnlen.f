C
C	$Id: gtnlen.f,v 1.7 2008-07-27 00:21:06 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
        SUBROUTINE GTNLEN(FNAME,ILEN,IER)
C
C  Determine the length of the name in FNAME.  The length is the
C  longest initial character sequence without a blank or null.
C  Maximum length is 1023.  IER is returned as 0 if the length is
C  less than 1024; IER is returned as 1 otherwise.
C
        CHARACTER*(*) FNAME
C
        IER  = 0
        ILEN = 0
        DO 10 I=1,1024
        IF (FNAME(I:I).EQ.' ' .OR. FNAME(I:I).EQ.CHAR(0)) THEN
          ILEN = I-1
          RETURN
        ENDIF
   10   CONTINUE
        IER = 1
        RETURN
        END

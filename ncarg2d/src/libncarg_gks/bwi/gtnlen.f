C
C	$Id: gtnlen.f,v 1.1.1.1 1992-04-17 22:34:02 ncargd Exp $
C
        SUBROUTINE GTNLEN(FNAME,ILEN,IER)
C
C  Determine the length of the name in FNAME.  The length is the
C  longest initial character sequence without a blank or null.
C  Maximum length is 79.  IER is returned as 0 if the length is
C  less than 80; IER is returned as 1 otherwise.
C
        CHARACTER*(*) FNAME
C
        IER  = 0
        ILEN = 0
        DO 10 I=1,80
        IF (FNAME(I:I).EQ.' ' .OR. FNAME(I:I).EQ.CHAR(0)) THEN
          ILEN = I-1
          RETURN
        ENDIF
   10   CONTINUE
        IER = 1
        RETURN
        END

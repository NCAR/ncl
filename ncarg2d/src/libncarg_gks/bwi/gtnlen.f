C
C	$Id: gtnlen.f,v 1.4 1994-04-28 23:35:54 fred Exp $
C
        SUBROUTINE GTNLEN(FNAME,ILEN,IER)
C
C  Determine the length of the name in FNAME.  The length is the
C  longest initial character sequence without a blank or null.
C  Maximum length is 255.  IER is returned as 0 if the length is
C  less than 256; IER is returned as 1 otherwise.
C
        CHARACTER*(*) FNAME
C
        IER  = 0
        ILEN = 0
        DO 10 I=1,256
        IF (FNAME(I:I).EQ.' ' .OR. FNAME(I:I).EQ.CHAR(0)) THEN
          ILEN = I-1
          RETURN
        ENDIF
   10   CONTINUE
        IER = 1
        RETURN
        END

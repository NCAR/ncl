C
C	$Id: openpw.F,v 1.1.1.1 1992-04-17 22:31:33 ncargd Exp $
C
      SUBROUTINE OPENPW (IUNIT)
      CHARACTER*128 PARANM,FILENM
      SAVE IOPEN
      DATA PARANM / 'DBDIR' /
      DATA IOPEN / 0 /
	IF (IOPEN.EQ.0) THEN
	  CALL GETNGP (PARANM,FILENM)
	  DO 101 I=1,119
	    IF (FILENM(I:I).EQ.' ') THEN
	      FILENM(I:I+9)='/pwritdata'
	      GO TO 102
	    END IF
  101     CONTINUE
	  GO TO 103
#if defined(ultrix) && defined(mips)
  102     OPEN (UNIT=IUNIT,FILE=FILENM,STATUS='OLD',FORM='UNFORMATTED',
     +                                                READONLY,ERR=103)
#else
  102     OPEN (UNIT=IUNIT,FILE=FILENM,STATUS='OLD',FORM='UNFORMATTED',
     +                                                         ERR=103)
#endif
	  IOPEN=1
	END IF
	RETURN
  103   WRITE (6,*) 'ERROR IN OPENING pwritdata.'
	STOP
      END

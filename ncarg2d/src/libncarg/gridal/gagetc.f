C
C	$Id: gagetc.f,v 1.2 1992-09-04 20:40:37 ncargd Exp $
C

      SUBROUTINE GAGETC (PNAM,CVAL)
C
        CHARACTER*(*) PNAM,CVAL
C
C The subroutine GAGETC may be used to get GRIDAL parameters which have
C values of type CHARACTER.
C
C Declare the common block containing character parameters.
C
        COMMON /GACHAR/ FNLX,FNLY
        CHARACTER*10    FNLX,FNLY
        SAVE   /GACHAR/
C
C Get the parameter value desired.
C
        IF      (PNAM(1:3).EQ.'XLF'.OR.PNAM(1:3).EQ.'xlf') THEN
          CVAL=FNLX
        ELSE IF (PNAM(1:3).EQ.'YLF'.OR.PNAM(1:3).EQ.'ylf') THEN
          CVAL=FNLY
        ELSE
          CALL SETER ('GAGETC - UNRECOGNIZED PARAMETER NAME',1,2)
        END IF
C
C Done.
C
        RETURN
C
      END

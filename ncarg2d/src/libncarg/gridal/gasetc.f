
      SUBROUTINE GASETC (PNAM,CVAL)
C
        CHARACTER*(*) PNAM,CVAL
C
C The subroutine GASETC may be used to set GRIDAL parameters which have
C values of type CHARACTER.
C
C Declare the common block containing character parameters.
C
        COMMON /GACHAR/ FNLX,FNLY
        CHARACTER*10    FNLX,FNLY
        SAVE   /GACHAR/
C
C Set the specified parameter.
C
        IF      (PNAM(1:3).EQ.'XLF'.OR.PNAM(1:3).EQ.'xlf') THEN
          FNLX=CVAL
        ELSE IF (PNAM(1:3).EQ.'YLF'.OR.PNAM(1:3).EQ.'ylf') THEN
          FNLY=CVAL
        ELSE
          CALL SETER ('GASETC - UNRECOGNIZED PARAMETER NAME',1,2)
        END IF
C
C Done.
C
        RETURN
C
      END

C
C $Id: gasetc.f,v 1.3 1994-03-17 17:27:41 kennison Exp $
C
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
C Check for an uncleared prior error.
C
        IF (ICFELL('GASETC - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Set the specified parameter.
C
        IF      (PNAM(1:3).EQ.'XLF'.OR.PNAM(1:3).EQ.'xlf') THEN
          FNLX=CVAL
        ELSE IF (PNAM(1:3).EQ.'YLF'.OR.PNAM(1:3).EQ.'ylf') THEN
          FNLY=CVAL
        ELSE
          CALL SETER ('GASETC - UNRECOGNIZED PARAMETER NAME',2,1)
        END IF
C
C Done.
C
        RETURN
C
      END

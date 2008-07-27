C
C $Id: gasetc.f,v 1.6 2008-07-27 00:17:13 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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

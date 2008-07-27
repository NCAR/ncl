C
C $Id: tdseti.f,v 1.4 2008-07-27 00:17:33 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE TDSETI (PNAM,IVAL)
C
      CHARACTER*(*) PNAM
C
C The subroutine TDSETI may be used to set TDPACK parameters which have
C values of type INTEGER.
C
C Check for an uncleared prior error.
C
        IF (ICFELL('TDSETI - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Just convert it into a call to the routine TDSETR.
C
        CALL TDSETR (PNAM,REAL(IVAL))
        IF (ICFELL('TDSETI',2).NE.0) RETURN
C
C Done.
C
        RETURN
C
      END

C
C $Id: idseti.f,v 1.4 2008-07-27 00:17:30 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE IDSETI (PNAM,IVAL)
C
C Set the integer value of the BIVAR parameter named PNAM from IVAL.
C
        CHARACTER*(*) PNAM
C
C Check for an uncleared prior error.
C
        IF (ICFELL('IDSETI (BIVAR) - UNCLEARED PRIOR ERROR',1).NE.0)
     +                                                        RETURN
C
C Pass IDSETR the real equivalent of the integral value and let it do
C the work.
C
        CALL IDSETR (PNAM,REAL(IVAL))
        IF (ICFELL('IDSETI',2).NE.0) RETURN
C
C Done.
C
        RETURN
C
      END

C
C $Id: pwrit.f,v 1.7 2008-07-27 00:17:25 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE PWRIT (PX,PY,CH,NC,IS,IO,IC)
      CHARACTER*(*) CH
C
C PWRIT is called to draw a character string in a specified position.
C It is just like WTSTR, but has one extra argument.  NC is the number
C of characters to be written from the string CH.
C
      IF (ICFELL('PWRIT - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
      CALL WTSTR (PX,PY,CH(1:NC),IS,IO,IC)
      IF (ICFELL('PWRIT',2).NE.0) RETURN
C
C Done.
C
      RETURN
C
      END

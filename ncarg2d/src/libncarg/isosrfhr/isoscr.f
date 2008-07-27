C
C	$Id: isoscr.f,v 1.4 2008-07-27 00:17:17 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE ISOSCR()
C
C OPEN SCRATCH FILE : ./tmp.isosrfhr
C
      COMMON /UNITS/ IUNIT
      IUNIT = 4
      OPEN(UNIT=4,STATUS='SCRATCH',FORM='UNFORMATTED')
      END

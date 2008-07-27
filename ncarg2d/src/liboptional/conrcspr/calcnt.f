C
C	$Id: calcnt.f,v 1.5 2008-07-27 00:23:01 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CALCNT (Z,M,N,A1,A2,A3,I1,I2,I3)
C
C THIS ENTRY POINT IS FOR USERS WHO ARE TOO LAZY TO SWITCH OLD DECKS
C TO THE NEW CALLING SEQUENCE.
C
      SAVE
      DIMENSION       Z(M,N)
C
C THE FOLLOWING CALL IS FOR GATHERING STATISTICS ON LIBRARY USE AT NCAR
      CALL Q8QST4 ('GRAPHX', 'CONRECSUPR', 'CALCNT', 'VERSION 01')
C
      CALL CONREC (Z,M,M,N,A1,A2,A3,I1,I2,-ABS(I3))
      RETURN
      END

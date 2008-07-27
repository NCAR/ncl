C
C	$Id: ezcntr.f,v 1.4 2008-07-27 00:23:02 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE EZCNTR (Z,M,N)
C
C USER ENTRY.  SEE COMMENT CARD DESCRIPTION CONTAINED IN CONEC.
C
      SAVE
      DIMENSION       Z(M,N)
      DATA NSET,NHI,NDASH/0,0,682/
C
C                             682=1252B
C THE FOLLOWING CALL IS FOR GATHERING STATISTICS ON LIBRARY USE AT NCAR
      CALL Q8QST4 ('GRAPHX', 'CONRECSUPR', 'EZCNTR', 'VERSION 01')
C
      CALL CONREC (Z,M,M,N,0.,0.,0.,NSET,NHI,-NDASH)
      CALL FRAME
      RETURN
      END

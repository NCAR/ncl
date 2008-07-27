C
C	$Id: gqmk.f,v 1.5 2008-07-27 00:20:59 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GQMK(ERRIND,MTYPE)
C 
C  INQUIRE MARKERTYPE
C
      include 'gkscom.h'
C
      INTEGER ERRIND,MTYPE
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .EQ. 0) THEN
        MTYPE = CMK
      ELSE
        MTYPE = -1
      ENDIF
C
      RETURN
      END

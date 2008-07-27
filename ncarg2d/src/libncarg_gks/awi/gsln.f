C
C	$Id: gsln.f,v 1.6 2008-07-27 00:21:02 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GSLN (LTYPE)
C
C  SET LINETYPE
C
      INTEGER ESLN
      PARAMETER (ESLN=19)
C
      include 'gkscom.h'
C
      INTEGER LTYPE
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,ESLN,IER)
      IF (IER .NE. 0) RETURN
C
C  Check that the linetype is valid.
C
      IF (LTYPE .LE. 0) THEN
        ERS = 1
        CALL GERHND(63,ESLN,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Set the current linetype (default it if it is out of range).
C
      IF (LTYPE .LE. 5) THEN
        CLN = LTYPE
      ELSE
        CLN = 1
      ENDIF
C
C  Invoke the workstation interface.
C
      FCODE = 22
      CONT  = 0
      CALL GZROI(0)
      IL1   = 1
      IL2   = 1
      ID(1) = CLN
      CALL GZTOWK
      IF (RERR .NE. 0) THEN
        ERS = 1
        CALL GERHND(RERR,ESLN,ERF)
        ERS = 0
      ENDIF
C
      RETURN
      END

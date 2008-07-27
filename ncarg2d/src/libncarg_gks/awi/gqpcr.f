C
C	$Id: gqpcr.f,v 1.5 2008-07-27 00:21:00 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GQPCR(WTYPE,PCI,ERRIND,RED,GREEN,BLUE)
C
C  INQUIRE PREDEFINED COLOUR REPRESENTATION
C
      include 'gkscom.h'
C
      INTEGER WTYPE,PCI,ERRIND
      REAL    RED,GREEN,BLUE
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .NE. 0) GOTO 100
C
C  Check that the workstation type is valid.
C
      CALL GZCKWK(22,-1,IDUM,WTYPE,ERRIND)
      IF (ERRIND .NE. 0) GO TO 100
C
C  Check if the workstation category is OUTPUT or OUTIN.
C
      CALL GQWKCA(WTYPE,ERRIND,ICAT)
      IF (ICAT.NE.GOUTPT .AND. ICAT.NE.GOUTIN) THEN
        ERRIND = 39
        GO TO 100
      ENDIF
C
C  Check if color index is valid.
C
      IF (PCI .LT. 0) THEN
        ERRIND = 93
        GO TO 100
      ENDIF
C
C  Invoke interface
C
      FCODE = -116
      CONT  = 0
      CALL GZROI(0)
      IL1   = 2
      IL2   = 2
      ID(1) = WTYPE
      ID(2) = PCI
      IWK   = -1
      CALL GZIQWK(WTYPE,IWK)
      IF (RERR .NE. 0) THEN
        ERRIND = RERR
        GOTO 100
      ENDIF
      RED   = RX(1)
      GREEN = RX(2)
      BLUE  = RX(3)
      RETURN
C
  100 CONTINUE
      RED   = -1.
      GREEN = -1.
      BLUE  = -1.
      RETURN
      END

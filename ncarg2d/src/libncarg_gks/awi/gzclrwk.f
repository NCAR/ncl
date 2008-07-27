C
C	$Id: gzclrwk.f,v 1.5 2008-07-27 00:21:03 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GZCLRWK(WKID,COFL)
C
C  CLEAR WORKSTATION
C
      INTEGER ECLRWK
      PARAMETER (ECLRWK=6)
C
      include 'gkscom.h'
C
      INTEGER WKID,COFL,FCODEO,CONTO
C
C  Check that GKS is in the proper state.
C
      CALL GZCKST(6,ECLRWK,IER)
      IF (IER .NE. 0) RETURN
C
C  Check if workstation identifier is valid.
C
      CALL GZCKWK(20,ECLRWK,WKID,IDUM,IER)
      IF (IER .NE. 0) RETURN
C
C  Check if the specified workstation is open.
C
      CALL GZCKWK(25,ECLRWK,WKID,IDUM,IER)
      IF (IER .NE. 0) RETURN
C
C  Check on workstation category.
C
      CALL GQWKC(WKID,IER,ICONID,ITYPE)
      IF (IER .NE. 0) THEN
        ERS = 1
        CALL GERHND(IER,ECLRWK,ERF)
        ERS = 0
        RETURN
      ENDIF
      CALL GQWKCA(ITYPE,IER,ICAT)
      IF (IER .NE. 0) THEN
        ERS = 1
        CALL GERHND(IER,ECLRWK,ERF)
        ERS = 0
        RETURN
      ENDIF
      IF (ICAT .EQ. GMI) THEN
        IER = 33
        ERS = 1
        CALL GERHND(IER,ECLRWK,ERF)
        ERS = 0
        RETURN
      ELSE IF (ICAT .EQ. GINPUT) THEN
        IER = 35
        ERS = 1
        CALL GERHND(IER,ECLRWK,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Put out new picture initialization if CGM and the picture is empty.        
C
      IF (ITYPE .EQ. GCGM) THEN
        IF (NOPICT .LE. 0) THEN
          FCODEO = FCODE
          CONTO  = CONT
          FCODE = 91
          CONT  =  0
          CALL GZROI(0)
          CALL G01WDR(WKID,' ')
          FCODE  = FCODEO
          CONT   = CONTO
          NOPICT = 1
        ENDIF
      ENDIF
C
C  Invoke the workstation interface.
C
C  Set the flag CUFLAG to indicate that the interface call should go
C  only to the specifically designated workstation.
C
      CUFLAG = WKID
      FCODE = 1
      CONT  = 0
      CALL GZROI(0)
      IL1 = 2
      IL2 = 2
      ID(1) = WKID
      ID(2) = COFL
      CALL GZTOWK
      IF (RERR.NE.0) THEN
        ERS = 1
        CALL GERHND(RERR,ECLRWK,ERF)
        ERS = 0
      ENDIF
      CUFLAG = -1
C
C  Set flag to indicate that the current picture is empty if CGM.
C
      IF (ITYPE .EQ. GCGM) NOPICT = 0
C
      RETURN
      END

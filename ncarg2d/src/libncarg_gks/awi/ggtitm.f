C
C	$Id: ggtitm.f,v 1.5 2008-07-27 00:20:57 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GGTITM (WKID,TYPE,LDR)
C
C  GET ITEM TYPE FROM GKSM
C
      INTEGER EGTITM
      PARAMETER (EGTITM=102)
C
      include 'gkscom.h'
C
      INTEGER WKID,TYPE,LDR
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(7,EGTITM,IER)
      IF (IER .NE. 0) RETURN
C
C  Check if the workstation ID is valid.
C
      CALL GZCKWK(20,EGTITM,WKID,IDUM,IER)
      IF (IER .NE. 0) RETURN
C
C  Check if the workstation is currently open.
C
      CALL GZCKWK(25,EGTITM,WKID,IDUM,IER)
      IF (IER .NE. 0) RETURN
C
C  Set function code and put out WKID.
C
C  Set the flag CUFLAG to indicate that the interface call should go
C  only to the specifically designated workstation.
C
      CUFLAG = WKID
      FCODE = 102
      CONT  = 0
      CALL GZROI(0)
      IL1 = 1
      IL2 = 1
      ID(1) = WKID
      CALL GZTOWK
      IF (RERR.NE.0) THEN
        ERS = 1
        CALL GERHND(RERR,EGTITM,ERF)
        ERS = 0
        RETURN
      ELSE
C
C  Read returned data.
C
        TYPE = ID(2)
        LDR  = ID(3)
      ENDIF
      CUFLAG = -1
C
      RETURN
      END

C
C	$Id: gsclip.f,v 1.6 2009-04-08 23:18:21 fred Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GSCLIP(CLSW)
C
C  Set clipping indicator.
C
      INTEGER ESCLIP
      PARAMETER (ESCLIP=53)
C
      include 'gkscom.h'
C
      INTEGER CLSW
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,ESCLIP,IER)
      IF (IER .NE. 0) RETURN
C
C  Check that CLSW is in range.
C
      IF (CLSW.LT.0.OR.CLSW.GT.1) THEN
        ERS = 1
        CALL GERHND(2000,ESCLIP,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Set clipping indicator in the GKS state list.
C
      CCLIP = CLSW
C
C  Invoke the workstation interface.  Send the clipping indicator
C  in ID(1) and the viewport of the current normalization transformation
C  in RX, RY.
C
      FCODE = 61
      CONT  = 0
      CALL GZROI(0)
C
C  Current transformation number increased by one so it can
C  be used as an index.
C
      ICNT = CNT+1
      IL1 = 2
      IL2 = 2
      ID(1) = CLSW
      ID(2) = CNT
      RL1   = 2
      RL2   = 2
C
C  Current transformation number increased by one so it can
C  be used as an index.
C
      ICNT = CNT+1
      RX(1) = NTVP(ICNT,1)
      RX(2) = NTVP(ICNT,2)
      RY(1) = NTVP(ICNT,3)
      RY(2) = NTVP(ICNT,4)
      CALL GZTOWK
      IF (RERR.NE.0) THEN
        ERS = 1
        CALL GERHND(RERR,ESCLIP,ERF)
        ERS = 0
      ENDIF
C
      RETURN
      END

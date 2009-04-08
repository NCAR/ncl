C
C	$Id: gschup.f,v 1.7 2009-04-08 23:18:21 fred Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GSCHUP (CHUX,CHUY)
C
C  SET CHARACTER UP VECTOR
C
      INTEGER ESCHUP
      PARAMETER (ESCHUP=32)
C
      include 'gkscom.h'
C
      REAL CHUX,CHUY
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,ESCHUP,IER)
      IF (IER .NE. 0) RETURN
C
C  Check that the vector is non-zero.
C
      IF (CHUX.EQ.0.AND.CHUY.EQ.0) THEN
        ERS = 1
        CALL GERHND(79,ESCHUP,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Set the current character up vector in the GKS state list.
C  (This vector remains in world coordinates here).
C
      CCHUP(1) = CHUX
      CCHUP(2) = CHUY
C
C  Invoke the workstation interface.  Two real vectors are
C  passed through the interface.  The first vector is
C  passed via (RX(1),RY(1)) and is a vector parallel to the
C  current character up vector with length equal to the
C  recently defined character height (the character height
C  having been transformed to NDC space).  The second vector
C  is passed via (RX(2),RY(2)) and is a vector parallel to
C  the character base vector scaled in accordance with the
C  appropriate aspect ratio.
C
      FCODE = 34
      CONT  = 0
      CALL GZROI(0)
      RL1   = 2
      RL2   = 2
      SCL = 1./SQRT(CCHUP(1)*CCHUP(1)+CCHUP(2)*CCHUP(2))
C
C  Construct a vector (XP,YP) that is parallel to the up vector
C  and has height equal to the character height.
C
      XP = CCHH*SCL*CCHUP(1)
      YP = CCHH*SCL*CCHUP(2)
C
C  Construct a base vector perpendicular to the height vector.
C
      XB =  YP
      YB = -XP
 
C
C  Transform the height and base vectors using the current normalization
C  transformation.
C
      CALL GZW2NX(1,XP,XTMP)
      CALL GZW2NY(1,YP,YTMP)
      CALL GZW2NX(1,0.,ZXTMP)
      CALL GZW2NY(1,0.,ZYTMP)
      RX(1) = XTMP-ZXTMP 
      RY(1) = YTMP-ZYTMP 
C
C  If the character height transformed to NDC is large, reduce it
C  to avoid possible overflow.
C
      IF (ABS(RX(1)).GT.1000. .OR. ABS(RY(1)).GT.1000.) THEN
        AMX = MAX(ABS(RX(1)),ABS(RY(1)))
        RX(1) = 1.E5*RX(1)/AMX
        RY(1) = 1.E5*RY(1)/AMX
      ENDIF
      CALL GZW2NX(1,XB,XTMP)
      CALL GZW2NY(1,YB,YTMP)
      RX(2) = XTMP-ZXTMP 
      RY(2) = YTMP-ZYTMP 
      IF (ABS(RX(2)).GT.1000. .OR. ABS(RY(2)).GT.1000.) THEN
        AMX = MAX(ABS(RX(2)),ABS(RY(2)))
        RX(2) = 1.E5*RX(2)/AMX
        RY(2) = 1.E5*RY(2)/AMX
      ENDIF
C
      CALL GZTOWK
      IF (RERR.NE.0) THEN
        ERS = 1
        CALL GERHND(RERR,ESCHUP,ERF)
        ERS = 0
      ENDIF
C
      RETURN
      END

C
C	$Id: gevtm.f,v 1.1 1993-01-09 01:58:34 fred Exp $
C
      SUBROUTINE GEVTM(X0,Y0,DX,DY,PHI,SX,SY,SW,MOUT)
C
C  EVALUATE TRANSFORMATION MATRIX
C
      INTEGER EEVTM
      PARAMETER (EEVTM=105)
C
      REAL X0,Y0,DX,DY,PHI,MOUT(2,3)
      INTEGER SW
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,EEVTM,IER)
      IF (IER .NE. 0) RETURN
C
C  The transformation takes place in NDC space, so do the
C  appropriate conversions.
C
      IF (SW .EQ. 1) THEN
        CALL GZW2NX(1,X0,XN0)
        CALL GZW2NY(1,Y0,YN0)
        CALL GZW2NX(1,DX,DNX)
        CALL GZW2NX(1,DY,DNY)
      ELSE
        XN0 = X0
        YN0 = Y0
        DNX = DX
        DNY = DY
      ENDIF        
C
C  Calculate the transformtion matrix.
C
      IF (PHI .EQ. 0.) THEN
        MOUT(1,1) = SX
        MOUT(1,2) = 0.
        MOUT(1,3) = DNX+XN0-XN0*SX
        MOUT(2,1) = 0.
        MOUT(2,2) = SY
        MOUT(2,3) = DNY+YN0-YN0*SY
        GO TO 10
      ELSE
        CPHI = COS(PHI)
        SPHI = SIN(PHI)
C
        MOUT(1,1) =  SX*CPHI
        MOUT(1,2) = -SY*SPHI
        MOUT(1,3) =  DNX+XN0-XN0*SX*CPHI+YN0*SY*SPHI
        MOUT(2,1) =  SX*SPHI
        MOUT(2,2) =  SY*CPHI
        MOUT(2,3) =  DNY+YN0-XN0*SX*SPHI-YN0*SY*CPHI
        GO TO 10
      ENDIF
C
   10 CONTINUE
      RETURN
      END

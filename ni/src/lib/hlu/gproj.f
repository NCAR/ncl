      SUBROUTINE HLUGPROJ(PHI0, LAMBDA0, PHI, LAMBDA, X, Y)
C
C  Find the projection onto the plane tangent to the globe at
C  (PHI0,LAMBDA0) of the point on the globe (PHI,LAMBDA).
C  The projected point is returned in Cartesian coordinate
C  (X,Y).
C
      DOUBLE PRECISION PHI0, LAMBDA0, PHI, LAMBDA, X, Y
      DOUBLE PRECISION SIN0, COS0, SINP, COSP, SIND, COSD, DENOM
      DOUBLE PRECISION D2R
      PARAMETER (D2R = 0.017453292519943D0)
C
      SIN0 = SIN(D2R*PHI0)
      COS0 = COS(D2R*PHI0)
      SINP = SIN(D2R*PHI)
      COSP = COS(D2R*PHI)
      SIND = SIN(D2R*(LAMBDA-LAMBDA0))
      COSD = COS(D2R*(LAMBDA-LAMBDA0))
      DENOM = SIN0*SINP + COS0*COSP*COSD
      X = (COSP*SIND)/DENOM
      Y = (COS0*SINP - SIN0*COSP*COSD)/DENOM
C
      RETURN
      END


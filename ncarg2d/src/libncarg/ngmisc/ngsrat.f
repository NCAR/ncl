C
C       $Id: ngsrat.f,v 1.4 2008-07-27 00:17:18 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE NGSRAT(IOPT, IAT, RAT)
C
C Save/restore/set NCAR GKS attributes.
C
C ARGUMENTS
C
C ON INPUT  
C    
C   IOPT  An integer argument indicating the desired action:
C
C           = 0, then save the current settings of all attributes 
C                internal to the subroutine, i.e. not in the IAT and
C                RAT arrays.
C           = 1, then restore the context from the most recent
C                saved state.
C           = 2, then return the current context in the output arrays
C                IAT (integer attributes) and RAT (floating-point
C                attributes) as follows:
C
C                IAT - An array containing GKS integer state variables 
C                      as follows:
C
C                        IAT( 1) = Clip indicator
C                        IAT( 2) = Line type
C                        IAT( 3) = Polyline color index
C                        IAT( 4) = Marker type
C                        IAT( 5) = Polymarker color index
C                        IAT( 6) = Text font
C                        IAT( 7) = Text precision
C                        IAT( 8) = Text color index
C                        IAT( 9) = Text path
C                        IAT(10) = Text horizontal alignment
C                        IAT(11) = Text vertical alignment
C                        IAT(12) = Fill area interior style
C                        IAT(13) = Fill are style index
C                        IAT(14) = Fill area color index
C
C                RAT - A REAL array containing GKS floating-point
C                      state variables as follows:
C
C                        RAT( 1) = Linewidth scale factor
C                        RAT( 2) = Marker scale factor
C                        RAT( 3) = Character expansion factor
C                        RAT( 4) = Character spacing
C                        RAT( 5) = Character height in world coordinates
C                        RAT( 6) = Character up vector, X component in
C                                  world coordinates
C                        RAT( 7) = Character up vector, Y component in
C                                  world coordinates
C 
C           = 3, then set the context to the values specified
C                in the IAT and RAT arrays (as described above).
C
C  IAT   An integer array that has meaning only if IOPT equals 2 or 3
C        as indicated above in the description of IOPT.
C
C  RAT   A real array that has meaning only if IOPT equals 2 or 3
C        as indicated above in the description of IOPT.
C
      DIMENSION IAT(14),RAT(7)
      CHARACTER*80 IDR(3),ODR(3)
C
      WRITE(IDR(1)(1:5),500) IOPT
  500 FORMAT(I5)
      IF (IOPT.EQ.0 .OR. IOPT.EQ.1) THEN
        CALL GESC(-1388,1,IDR,3,3,ODR)
      ELSE IF (IOPT .EQ. 2) THEN
        CALL GESC(-1388,1,IDR,3,3,ODR)
        READ (ODR(1),510) (IAT(LL),LL=1,14)
  510   FORMAT(14I5)
        READ (ODR(2),520) (RAT(LL),LL=1,5)
  520   FORMAT(5E16.7)
        READ (ODR(3),530) (RAT(LL),LL=6,7)
  530   FORMAT(2E16.7)
      ELSE IF (IOPT.EQ.3) THEN
        WRITE(IDR(1)(6:75),510) (IAT(LL),LL=1,14)
        WRITE(IDR(2),520) (RAT(LL),LL=1,5)
        WRITE(IDR(3),530) (RAT(LL),LL=6,7)
        CALL GESC(-1388,1,IDR,3,3,ODR)
      ENDIF
C
      RETURN
      END

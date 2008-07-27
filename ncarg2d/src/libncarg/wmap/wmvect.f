C
C	$Id: wmvect.f,v 1.3 2008-07-27 00:17:37 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE WMVECT(X,Y,U,V)
C
C  This subroutine plots wind vectors with the origin of the vector at
C  (X,Y).  U and V are the components of the wind vector.
C
      include 'wmcomn.h'
C
      PARAMETER (R2D=57.2957795131)
C
C  Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
      CALL WMBLDA
C
C  Save the current line and fill colors and set them to ICOLOR.
C
      CALL GQPLCI(IER,ILCLRO)
      CALL GQLWSC(IER,RLNWTH)
C
      CALL GSPLCI(VCCOLR)
      CALL GSLWSC(VCLWID)
C
      SPEED  = SQRT(U*U+V*V)
      CALL WMGETR('VRS',VRS)
      CALL WMGETR('VRN',VRN)
C
      UP =  U
      VP =  V
      COSANG = UP/SPEED
      SINANG = VP/SPEED
      IF (VP .GE. 0.) THEN
        ALPHA = ACOS(UP/SPEED)*R2D
      ELSE
        ALPHA = -ACOS(UP/SPEED)*R2D
      ENDIF
      CALL WMSETR('VCD',ALPHA)
      CALL WMSETR('VCS',SPEED)
C
C  Draw the vector.
C
      CALL WMLABS(X,Y,'Vector')
C
C  Restore the original environment.
C
      CALL GSPLCI(ILCLRO)
C     CALL GSELNT(NTRO)
      CALL GSLWSC(RLNWTH)
      CALL PCSETI ('FN',IFNO)
C
      RETURN
      END

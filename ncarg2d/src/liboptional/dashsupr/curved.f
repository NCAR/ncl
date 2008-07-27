C
C	$Id: curved.f,v 1.4 2008-07-27 00:23:03 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CURVED (X,Y,N)
C USER ENTRY POINT.
C
      DIMENSION X(N),Y(N)
C
      CALL FRSTD (X(1),Y(1))
      DO 10 I=2,N
         CALL VECTD (X(I),Y(I))
   10 CONTINUE
C
      CALL LASTD
C
      RETURN
      END

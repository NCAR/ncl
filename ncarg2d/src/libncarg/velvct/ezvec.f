C
C       $Id: ezvec.f,v 1.6 2008-07-27 00:17:34 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE EZVEC (U,V,M,N)
C
C THIS SUBROUTINE IS FOR THE USER WHO WANTS A QUICK-AND-DIRTY VECTOR
C PLOT WITH DEFAULT VALUES FOR MOST OF THE ARGUMENTS.
C
        SAVE
C
      DIMENSION       U(M,N)     ,V(M,N)     ,SPVAL(2)
C
      DATA FLO,HI,NSET,LENGTH,ISPV,SPVAL(1),SPVAL(2) /
     +      0.,0.,   0,     0,   0,      0.,      0. /
C
      CALL VELVCT (U,M,V,M,M,N,FLO,HI,NSET,LENGTH,ISPV,SPVAL)
      CALL FRAME
      RETURN
      END

C
C       $Id: velvec.f,v 1.6 2008-07-27 00:17:34 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE VELVEC (U,LU,V,LV,M,N,FLO,HI,NSET,ISPV,SPV)
C
C THIS ROUTINE SUPPORTS USERS OF THE OLD VERSION OF THIS PACKAGE.
C
      DIMENSION       U(LU,N)    ,V(LV,N)    ,SPV(2)
C
      CALL VELVCT (U,LU,V,LV,M,N,FLO,HI,NSET,0,ISPV,SPV)
C
      RETURN
      END

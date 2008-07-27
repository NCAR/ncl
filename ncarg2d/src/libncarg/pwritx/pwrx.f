C
C $Id: pwrx.f,v 1.5 2008-07-27 00:17:21 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE PWRX (X,Y,ID,N,SIZE,THETA,ICNT)
C
C PWRX IS AN OLD ENTRY POINT AND HAS BEEN REMOVED - USE PLOTCHAR
C
      WRITE (I1MACH(4),1001)
      WRITE (I1MACH(4),1002)
      STOP
C
 1001 FORMAT ('1'//////////)
 1002 FORMAT (' ****************************************'/
     1        ' *                                      *'/
     2        ' *                                      *'/
     3        ' *   THE ENTRY POINT PWRX IS NO LONGER  *'/
     4        ' *   SUPPORTED.  PLEASE USE PLOTCHAR.   *'/
     5        ' *                                      *'/
     6        ' *                                      *'/
     7        ' ****************************************')
      END

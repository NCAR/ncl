C
C	$Id: pwry.f,v 1.1.1.1 1992-04-17 22:31:34 ncargd Exp $
C
      SUBROUTINE PWRY (X,Y,ID,N,SIZE,THETA,ICNT)
C
C PWRY IS AN OLD ENTRY POINT AND HAS BEEN REMOVED - USE PWRITY
C ENTRY POINT
C
      WRITE (I1MACH(4),1001)
      WRITE (I1MACH(4),1002)
      STOP
C
 1001 FORMAT ('1'//////////)
 1002 FORMAT (' ****************************************'/
     1        ' *                                      *'/
     2        ' *                                      *'/
     3        ' *   THE ENTRY POINT PWRY IS NO LONGER  *'/
     4        ' *   SUPPORTED.  PLEASE USE THE MORE    *'/
     5        ' *   RECENT VERSION  PWRITY.            *'/
     6        ' *                                      *'/
     7        ' *                                      *'/
     8        ' ****************************************')
      END

C
C	$Id: pwrz.f,v 1.1.1.1 1992-04-17 22:31:48 ncargd Exp $
C
      SUBROUTINE PWRZ (X,Y,Z,ID,N,ISIZE,LIN3,ITOP,ICNT)
      WRITE (6,1001)
      WRITE (6,1002)
      STOP
C
 1001 FORMAT (1H1//////////)
 1002 FORMAT (' *****************************************'/
     1        ' *                                       *'/
     2        ' *                                       *'/
     3        ' *   THE ENTRY POINT PWRZ IS NO LONGER   *'/
     4        ' *   SUPPORTED.  THE CAPABILITIES OF     *'/
     5        ' *   THIS OLD ENTRY ARE NOW AVAILABLE    *'/
     6        ' *   IN THE NEW PORTABLE VERSIONS        *'/
     7        ' *                                       *'/
     8        ' *        PWRZS  FOR USE WITH SRFACE     *'/
     9        ' *        PWRZI  FOR USE WITH ISOSRF     *'/
     +        ' *        PWRZT  FOR USE WITH THREED     *'/
     1        ' *                                       *'/
     2        ' *   FOR USAGE OF THESE ROUTINES, SEE    *'/
     3        ' *   THE DOCUMENTATION FOR THE DESIRED   *'/
     4        ' *   ROUTINE.                            *'/
     5        ' *                                       *'/
     6        ' *                                       *'/
     7        ' *****************************************')
C
      END

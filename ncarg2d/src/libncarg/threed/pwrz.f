C
C	$Id: pwrz.f,v 1.2 2000-07-12 16:26:45 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU Lesser General Public License as
C published by the Free Software Foundation; either version 2.1 of the
C License, or (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C Lesser General Public License for more details.
C
C You should have received a copy of the GNU Lesser General Public
C License along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
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

C
C	$Id: gzputs.f,v 1.3 2000-07-12 16:40:06 haley Exp $
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
      SUBROUTINE GZPUTS(ST,IER)
C
C  This subroutine fills the string character variable of the 
C  workstation interface common block and invokes the workstation
C  interface subroutine (N is the length of string ST).
C  If an error is returned from the workstation interface,
C  IER is set to that error number and a return is executed.
C
C
      include 'gkscom.h'
C
      CHARACTER*(*) ST
      N = LEN(ST)
      J = (N-1)/80
      STRL1 = N
      IF (J .EQ. 0) THEN
C
C  Case where there is no continuation.
C
        CONT = 0
        STRL2 = N
        STR(1:N) = ST(1:N)
        CALL GZTOWK
        IF (RERR .NE. 0) THEN
          IER = RERR
          RETURN
        ENDIF
      ELSE
C
C  Case with contination.
C
        CONT = 1
        STRL2 = 80
C
C  Loop through parts with continuation flag set.
C
        DO 200 K=1,J
          KPNT = (K-1)*80
          STR(1:80) = ST(KPNT+1:KPNT+80+1)
          IF (K .GT. 1) THEN
            IL2 = 0
            RL2 = 0
            IC2 = 0
          ENDIF
          CALL GZTOWK
          IF (RERR .NE. 0) THEN
            IER = RERR
            RETURN
          ENDIF
  200   CONTINUE
C
C  Put out last part of string with continuation flag set to last.
C
        CONT = 0
        STRL2 = N-J*80
        KPNT = J*80
        STR(1:STRL2) = ST(KPNT+1:KPNT+STRL2+1)
        CALL GZTOWK
      ENDIF
C
      RETURN
      END

C
C	$Id: geclks.f,v 1.4 2000-08-22 15:07:58 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published
C by the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
      SUBROUTINE GECLKS
C
C  EMERGENCY CLOSE GKS
C
      include 'gkscom.h'
C
C  Update all open workstations.
C
      IF (OPS .GE. GWSOP) THEN
        IF (NOPWK .NE. 0) THEN
          DO 200 I=1,NOPWK
            CALL GUWK(SOPWK(I),1)
  200     CONTINUE
        ENDIF
      ENDIF
C
C  Close the open segment if in state GSGOP.
C
      IF (OPS .EQ. GSGOP) THEN
        CALL GQOPSG(IER,ISGNE)
        IF (IER .EQ. 0) THEN
          CALL GCLSG
        ENDIF
      ENDIF
C
C  Deactivate all active workstations.
C
      IF (OPS .EQ. GWSAC) THEN
        IF (NACWK .NE. 0) THEN
          NACWKT = NACWK
          DO 201 I=1,NACWKT
            CALL GDAWK(SACWK(NACWK))
  201     CONTINUE
        ENDIF
      ENDIF
C
C  Close all open workstations.
C
      IF (OPS .GE. GWSOP) THEN
        IF (NOPWK .NE. 0) THEN
          NOPWKT = NOPWK
          DO 202 I=1,NOPWKT
            CALL GCLWK(SOPWK(NOPWK))
  202     CONTINUE
        ENDIF
      ENDIF
C
C  Mark GKS closed.
C
      OPS = GGKCL
C
      RETURN
      END

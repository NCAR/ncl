C
C $Id: sflush.f,v 1.6 2000-08-22 15:06:15 haley Exp $
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
      SUBROUTINE SFLUSH
C
C SFLUSH flushes the GKS pen-move buffer as well as executing
C an UPDATE WORKSTATION on all open workstations.  In particular,
C for a CGM workstation, this effects a flush of the system level
C I/O buffers.
C
      INTEGER WKID
C
C Check for an uncleared prior error.
C
      IF (ICFELL('SFLUSH - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Flush the GKS pen move buffer.
C
      CALL PLOTIF (0.,0.,2)
      IF (ICFELL('SFLUSH',2).NE.0) RETURN
C
C Execute UPDATE WORKSTATION on all open workstations.
C
C If no workstations are open, return.
C
      CALL GQOPWK (1,IE,NO,ID)
      IF (IE.NE.0) THEN
        CALL SETER ('SFLUSH - ERROR EXIT FROM GQOPWK',3,1)
        RETURN
      END IF
      IF (NO .EQ. 0) RETURN
C
C Update all workstations.
C
      DO 200 I=1,NO
        CALL GQOPWK (I,IE,NO,WKID)
        IF (IE.NE.0) THEN
          CALL SETER ('SFLUSH - ERROR EXIT FROM GQOPWK',4,1)
          RETURN
        END IF
        CALL GUWK(WKID,0)
  200 CONTINUE
C
C Done.
C
      RETURN
C
      END

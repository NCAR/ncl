C
C	$Id: gxlate.f,v 1.2 2000-07-12 16:40:02 haley Exp $
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
      SUBROUTINE GXLATE(WKID,IOS,STATUS)
C
C  This subroutine is the driver for the translation of segments
C  stored as CGM elements to GKS calls.
C
C  Error messages for unexpected EOF or invalid opcode are flagged.
C
      include 'trinst.h'
C
      INTEGER IOS, STATUS, WKID
C
C  Loop until end of file.
C
 10   CONTINUE
C
      CALL GNINST(IOS,STATUS) 
C
C  Test if valid instruction found.
C
      IF (STATUS .NE. 0) RETURN
C
C  Jump to proper element processor.
C
      IF (OPCL .LE. 2) THEN
        STATUS = 302
        RETURN
      END IF
C
      GO TO (1300,1400,1500,1600,1700),(OPCL-2)
C
 1300 CONTINUE
C
C  Class code 3 control element codes.
C
      CALL GCELCD(WKID,IOS,STATUS)
      GO TO 9999
C
 1400 CONTINUE
C
C  Class code 4 graphical primitive elements.
C
      CALL GELMS(IOS,STATUS)
      GO TO 9999
C
 1500 CONTINUE
C
C  Class code 5 attribute elements.
C
      CALL GATELM(IOS,STATUS)
      GO TO 9999
C
 1600 CONTINUE
C
C  Class code 6 escape functions.
C
      CALL GESCFN(IOS,STATUS)
      GO TO 9999
C
 1700 CONTINUE
C
C  Class code 7 external elements (ignore for now).
C
      GO TO 9999
C
 9999 CONTINUE
      IF (STATUS .EQ. 0) GO TO 10
C
      RETURN
      END

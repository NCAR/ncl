C
C	$Id: gzw2gk.f,v 1.3 2000-08-22 15:08:36 haley Exp $
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
      SUBROUTINE GZW2GK(WKID,CONID,IER)
C
C  Copy the segment which has been attached to Fortran unit
C  CONID to the workstation WKID by way of parsing the CGM
C  elements in the segment and making the appropriate GKS
C  calls.
C
      include 'gkscom.h'
C
      INTEGER WKID,CONID
      INTEGER IOS,STATUS
      REAL TWIND(4),TVIEW(4)
C
      EXTERNAL GSEGDT
      include 'trbufr.h'
C
C  Use normalization transformation 1 set to the identity (we use 1
C  so that the window can be changed for clipping).
C
      CALL GQCNTN(IER,NTOLD)
      CALL GQNT(NTOLD,IER,TWIND,TVIEW)
      CALL GSWN(1,0.,1.,0.,1.)
      CALL GSVP(1,0.,1.,0.,1.)
      CALL GSELNT(1)
C
C  Read in the first record.
C
      MCONID = CONID
      METREC = 1
      CALL GSEGRD(IOS,STATUS)
      IF (STATUS .NE. 0) THEN
        IER = 302
        RETURN
      ENDIF
C
C  Set the default operand lengths before reading any instructions.
C
      CALL GXOPDF
C
C  Set defaults.
C
      CALL GXMDEF
C
C  Loop until we run out of data, or error.
C
      CALL GXLATE(WKID,IOS,STATUS)
      IF (STATUS .EQ. -1) THEN
C
C  Normal exit, restore normalization transformation.
C
        IF (NTOLD .NE. 0) THEN
          CALL GSWN(NTOLD,TWIND(1),TWIND(2),TWIND(3),TWIND(4))
          CALL GSVP(NTOLD,TVIEW(1),TVIEW(2),TVIEW(3),TVIEW(4))
        ENDIF
        CALL GSELNT(NTOLD)
        IER = 0
      ELSE
        IER = STATUS
      ENDIF
C
      RETURN
      END

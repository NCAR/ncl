C
C	$Id: gzxid.f,v 1.3 2000-07-12 16:40:07 haley Exp $
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
      SUBROUTINE GZXID(WKID,XID,IER)
C
C  Return the local X window identifier associated with the
C  WKID supplied.  IER is 0 if success.
C
      include 'gkscom.h'
C
      INTEGER WKID,XID,IER
C
      IER = 0
C
C  Check to make sure that WKID flags an X workstation type.
C
      IF (WKID .LT. 0) THEN
        IER = 20
        RETURN
      ENDIF
C
      CALL GQWKC(WKID,IER,ICONID,ITYPE)
      IF (IER .NE. 0) RETURN
      CALL GQWKCA(ITYPE,IER,ICAT)
      IF (IER .NE. 0) RETURN
C
      DO 10 I=1,NOPWK
        IF (WKID .EQ. SOPWK(I)) THEN
          XID = LXWKID(I)
          IF (XID .EQ. -1) IER = 25
          RETURN
        ENDIF
   10 CONTINUE
      IER = 25
C
      RETURN
      END

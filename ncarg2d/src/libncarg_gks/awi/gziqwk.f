C
C	$Id: gziqwk.f,v 1.5 2000-07-12 16:40:05 haley Exp $
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
      SUBROUTINE GZIQWK(WTYPE,WKID)
C
C  This subroutine is used to handle inquiry functions which
C  involve a particular workstation.
C
C  WTYPE should always be a valid workstation type, but WKID
C  need be supplied only when necessary.  If WKID is negative,
C  then it should not be necessary.   
C
      include 'gkscom.h'
C
      INTEGER XID,WTYPE,WKID,IDUM(1)
C
C  Invoke workstation driver for all inquiry functions.
C
      IF (WTYPE .EQ. GCGM) THEN
        CALL G01WDR(WKID,' ')
        RETURN
      ELSE IF (WTYPE.EQ.GXWC  .OR. WTYPE.EQ.GDMP  .OR.
     +         WTYPE.EQ.GXWE  .OR.
     +        (WTYPE.GE.GPSMIN .AND. WTYPE.LE.GPSMAX)) THEN
        CALL GZXID(WKID,XID,RERR)
        IF (RERR .NE. 0) RETURN
        IL1 = 0
        IL2 = 0
        IC1 = 1
        IC2 = 1
        IC(1) = ID(2)
        CALL GGKWDR(XID,FCODE,CONT,IL1,IL2,ID,IC1,IC2,IC,
     +              RL1,RL2,RX,RY,STRL1,STRL2,IDUM,RERR,XERMSG) 
        IF (RERR .NE. 0) THEN
          ERS = 1
          CALL GERHND(RERR,0,ERF)
          ERS = 0
          RERR = 0
        ENDIF
        RETURN
      ELSE IF (WTYPE .NE. GXWC) THEN
        RERR = 22
        RETURN
      ELSE
        RERR = -109
        RETURN
      ENDIF  
C
      END

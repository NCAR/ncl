C
C	$Id: gqpxad.f,v 1.4 2000-08-22 15:08:13 haley Exp $
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
      SUBROUTINE GQPXAD(WKID,PX,PY,QX,QY,ERRIND,N,M)
C
C  INQUIRE PIXEL ARRAY DIMENSIONS
C
      include 'gkscom.h'
C
      INTEGER WKID,ERRIND,N,M
      REAL    PX,PY,QX,QY
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(7,-1,ERRIND)
      IF (ERRIND .NE. 0) GOTO 100
C
C  Check that the workstation identifier is valid.
C
      CALL GZCKWK(20,-1,WKID,IDUM,ERRIND)
      IF (ERRIND .NE. 0) GO TO 100
C
C  Check if the specified workstation is open.
C
      CALL GZCKWK(25,-1,WKID,IDUM,ERRIND)
      IF (ERRIND .NE. 0) GO TO 100
C
C  Check for invalid workstation categories.
C
      CALL GQWKC(WKID,ERRIND,ICONID,ITYPE)
      IF (ERRIND .NE. 0) GO TO 100
      CALL GQWKCA(ITYPE,ERRIND,ICAT)
      IF (ICAT.NE.GOUTPT .AND. ICAT.NE.GOUTIN) THEN
        ERRIND = 39
        GO TO 100
      ENDIF
C
C  Invoke interface.
C
      FCODE = -292
      CONT  = 0
      CALL GZROI(0)
      IL1   = 1
      IL2   = 1
      RL1   = 2
      RL2   = 2
      ID(1) = WKID
      CALL GZW2NX(1,PX,PXD)
      CALL GZW2NY(1,PY,PYD)
      CALL GZW2NX(1,QX,QXD)
      CALL GZW2NY(1,QY,QYD)
      RX(1) = PXD
      RY(1) = PYD
      RX(2) = QXD
      RX(2) = QYD
      CALL GZIQWK(ITYPE,WKID)
      IF (RERR .NE. 0) THEN
        ERRIND = RERR
        GOTO 100
      ENDIF
      N = ID(2)
      M = ID(3)
      RETURN
C
  100 CONTINUE
      N = -1
      M = -1
      RETURN
      END

C
C	$Id: gqwkdu.f,v 1.3 2000-07-12 16:39:54 haley Exp $
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
      SUBROUTINE GQWKDU(WKID,ERRIND,DEFMOD,REGMOD,DEMPTY,NFRAME)
C
C  INQUIRE WORKSTATION DEFERRAL AND UPDATE STATES
C
      include 'gkscom.h'
C
      INTEGER WKID,ERRIND,DEFMOD,REGMOD,DEMPTY,NFRAME
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
      IF (ERRIND .NE. 0) GO TO 100
      IF (ICAT .EQ. GMI) THEN
        ERRIND = 33
        GO TO 100
      ELSE IF (ICAT .EQ. GINPUT) THEN
        ERRIND = 35
        GO TO 100
      ELSE IF (ICAT .EQ. GWISS) THEN
        ERRIND = 36
        GO TO 100
      ENDIF
C
C  Invoke interface.
C
      FCODE = -200
      CONT  = 0
      CALL GZROI(0)
      IL1   = 1
      IL2   = 1
      ID(1) = WKID
      CALL GZIQWK(ITYPE,WKID)
      IF (RERR .NE. 0) THEN
        ERRIND = RERR
        GOTO 100
      ENDIF
      DEFMOD = ID(2)
      REGMOD = ID(3)
      DEMPTY = ID(4)
      NFRAME = ID(5)
      RETURN
C
  100 CONTINUE
      DEFMOD = -1
      REGMOD = -1
      DEMPTY = -1
      NFRAME = -1
      RETURN
      END

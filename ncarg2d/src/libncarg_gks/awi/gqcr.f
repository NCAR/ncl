C
C	$Id: gqcr.f,v 1.4 2000-08-22 15:08:05 haley Exp $
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
      SUBROUTINE GQCR(WKID,COLI,TYPE,ERRIND,RED,GREEN,BLUE)
C
C  INQUIRE COLOUR REPRESENTATION
C
      include 'gkscom.h'
C
      INTEGER WKID,COLI,TYPE,ERRIND
      REAL    RED,GREEN,BLUE
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(7,-1,ERRIND)
      IF (ERRIND .NE. 0) GOTO 100
C
C  Check on workstation ID.
C
      CALL GZCKWK(20,-1,WKID,IDUM,ERRIND)
      IF (ERRIND .GT. 0) GOTO 100
C
C  Check if the specified workstation is open.
C
      CALL GZCKWK(25,-1,WKID,IDUM,ERRIND)
      IF (ERRIND .GT. 0) GOTO 100
C
C  Check on the range of the TYPE parameter.
C
      IF (TYPE.LT.0 .OR. TYPE.GT.1) THEN
        ERRIND = 2000
        GO TO 100
      ENDIF
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
C  Check on color index.
C
      IF (COLI.LT.0) THEN
        ERRIND = 93
        GO TO 100
      ENDIF
C
C  Invoke the workstation interface routine.
C
      FCODE = -256
      CONT  = 0
      CALL GZROI(0)
      IL1   = 3
      IL2   = 3
      ID(1) = WKID
      ID(2) = COLI
      ID(3) = TYPE
      CALL GZIQWK(ITYPE,WKID)
      IF (RERR.NE.0) THEN
        ERRIND = RERR
        GOTO 100
      ENDIF
      RED   = RX(1)
      GREEN = RX(2)
      BLUE  = RX(3)
      RETURN
C
  100 CONTINUE
      RED   = -1.
      GREEN = -1.
      BLUE  = -1.
C
      RETURN
      END

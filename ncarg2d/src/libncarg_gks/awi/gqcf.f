C
C	$Id: gqcf.f,v 1.4 2000-08-22 15:08:04 haley Exp $
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
      SUBROUTINE GQCF(WTYPE,ERRIND,NCOLI,COLA,NPCI)
C
C  INQUIRE COLOUR FACILITIES
C
      include 'gkscom.h'
C
      INTEGER WTYPE,ERRIND,NCOLI,COLA,NPCI
C
C  Check if GKS is in proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .NE. 0) GOTO 100
C
C  Check if the workstation type is valid.
C
      CALL GZCKWK(22,-1,IDUM,WTYPE,ERRIND)
      IF (ERRIND .NE. 0) GO TO 100
C
C  Check if the workstation category is OUTPUT or OUTIN.
C
      CALL GQWKCA(WTYPE,ERRIND,ICAT)
      IF (ICAT.NE.GOUTPT .AND. ICAT.NE.GOUTIN) THEN
        ERRIND = 39
        GO TO 100
      ENDIF
C
      IF (ERRIND .NE. 0) GO TO 100
      ERRIND = 0
      IF (WTYPE.EQ.GXWC .OR. WTYPE.EQ.GXWE .OR. WTYPE.EQ.GDMP) THEN
        NCOLI  = 256
        COLA   = 1
        NPCI   = 4
      ELSE
        GO TO 100
      ENDIF
      RETURN
C
  100 CONTINUE
      NCOLI = -1
      COLA  = -1
      NPCI  = -1
C
      RETURN
      END

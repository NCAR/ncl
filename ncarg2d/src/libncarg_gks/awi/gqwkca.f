C
C	$Id: gqwkca.f,v 1.6 2000-08-22 15:08:15 haley Exp $
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
      SUBROUTINE GQWKCA(WTYPE,ERRIND,WKCAT)
C
C  INQUIRE WORKSTATION CATEGORY
C
      include 'gkscom.h'
C
      INTEGER WTYPE,ERRIND,WKCAT
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .NE. 0) GOTO 100
C
C  Check that the workstation type is valid.
C
      CALL GZCKWK(22,-1,IDUM,WTYPE,ERRIND)
      IF (ERRIND .NE. 0) GO TO 100
C
C  Provide the requested information.
C
      IF (WTYPE .EQ. GWSS) THEN
        WKCAT = GWISS
      ELSE IF (WTYPE.EQ.GCGM) THEN
        WKCAT = GMO
      ELSE IF (WTYPE.GE.GPSMIN .AND. WTYPE.LE.GPSMAX) THEN
        WKCAT = GMO
      ELSE IF (WTYPE.EQ.GXWC .OR. WTYPE.EQ.GXWE) THEN       
        WKCAT = GOUTIN
      ELSE IF (WTYPE .EQ. GDMP) THEN
        WKCAT = GOUTPT
      ELSE
        WKCAT = -1
      ENDIF
      RETURN
C
  100 CONTINUE
      WKCAT = -1
      RETURN
      END

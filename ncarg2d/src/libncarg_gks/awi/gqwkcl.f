C
C	$Id: gqwkcl.f,v 1.3 2000-07-12 16:39:54 haley Exp $
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
      SUBROUTINE GQWKCL(WTYPE,ERRIND,VRTYPE)
C
C  INQUIRE WORKSTATION CLASSIFICATION
C
      include 'gkscom.h'
C
      INTEGER WTYPE,ERRIND,VRTYPE
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
C  Invoke interface.
C
      FCODE = -128
      CONT  = 0
      CALL GZROI(0)
      IL1   = 1
      IL2   = 1
      ID(1) = WTYPE
      IWK   = -1
      CALL GZIQWK(WTYPE,IWK)
      IF (RERR .NE. 0) THEN
        ERRIND = RERR
        GOTO 100
      ENDIF
      VRTYPE = ID(2)
      RETURN
C
  100 CONTINUE
      VRTYPE = -1
      RETURN
      END

C
C	$Id: gqfaf.f,v 1.4 2000-08-22 15:08:07 haley Exp $
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
      SUBROUTINE GQFAF(WTYPE,NI,NH,ERRIND,NIS,IS,NHS,HS,NPFAI)
C
C  INQUIRE FILL AREA FACILITIES
C
      include 'gkscom.h'
C
      INTEGER WTYPE,NI,NH,ERRIND,NIS,IS,NHS,HS,NPFAI
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
C  Check on bounds for NI and NH.
C
      IF (NI.LT.0 .OR. NI.GT.3 .OR. NH.LT.0) THEN
        ERRIND = 2000
        GO TO 100
      ENDIF
C
C  Check if the workstation category is OUTPUT or OUTIN.
C
      CALL GQWKCA(WTYPE,ERRIND,ICAT)
      IF (ICAT.NE.GOUTPT .AND. ICAT.NE.GOUTIN) THEN
        ERRIND = 39
        GO TO 100
      ENDIF
C
C  Invoke interface.
C
      FCODE = -112
      CONT  = 0
      CALL GZROI(0)
      IL1   = 3
      IL2   = 3
      ID(1) = WTYPE
      ID(2) = NI
      ID(3) = NH
      IWK   = -1
      CALL GZIQWK(WTYPE,IWK)
      IF (RERR.NE.0) THEN
        ERRIND = RERR
        GOTO 100
      ENDIF
      NIS   = ID(4)
      IS    = ID(5)
      NHS   = ID(6)
      HS    = ID(7)
      NPFAI = ID(8)
      RETURN
C
  100 CONTINUE
      NIS   = -1
      IS    = -1
      NHS   = -1
      HS    = -1
      NPFAI = -1
      RETURN
      END

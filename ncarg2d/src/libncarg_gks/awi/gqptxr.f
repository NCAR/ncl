C
C	$Id: gqptxr.f,v 1.4 2000-08-22 15:08:13 haley Exp $
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
      SUBROUTINE GQPTXR(WTYPE,PTXI,ERRIND,FONT,PREC,CHARXP,CHARSP,COLI)
C
C  INQUIRE PREDEFINED TEXT REPRESENTATION
C
      include 'gkscom.h'
C
      INTEGER WTYPE,PTXI,ERRIND,FONT,PREC,COLI
      REAL    CHARXP,CHARSP
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .NE. 0) GOTO 100
C
C  Check that workstation type is valid.
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
C     CHECK THAT INDEX IS POSITIVE
C
      IF (PTXI .LT. 1) THEN
        ERRIND = 72
        GO TO 100
      ENDIF
C
C  Invoke interface.
C
      FCODE = -123
      CONT  = 0
      CALL GZROI(0)
      IL1   = 2
      IL2   = 2
      ID(1) = WTYPE
      ID(2) = PTXI
      IWK   = -1
      CALL GZIQWK(WTYPE,IWK)
      IF (RERR .NE. 0) THEN
        ERRIND = RERR
        GOTO 100
      ENDIF
      FONT   = ID(3)
      PREC   = ID(4)
      COLI   = ID(5)
      CHARXP = RX(1)
      CHARSP = RX(2)
      RETURN
C
  100 CONTINUE
      FONT   = -1
      PREC   = -1
      COLI   = -1
      CHARXP = -1.
      CHARSP = -1.E20
      RETURN
      END

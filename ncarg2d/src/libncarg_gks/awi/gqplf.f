C
C	$Id: gqplf.f,v 1.3 2000-07-12 16:39:51 haley Exp $
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
      SUBROUTINE GQPLF(WTYPE,N,ERRIND,NLT,LT,NLW,NOMLW,
     +                 RLWMIN,RLWMAX,NPPLI)
C
C  INQUIRE POLYLINE FACILITIES
C
      include 'gkscom.h'
C
      INTEGER WTYPE,N,ERRIND,NLT,LT,NLW,NPPLI
      REAL    NOMLW,RLWMIN,RLWMAX
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
C  Check if the workstation category is OUTPUT or OUTIN.
C
      CALL GQWKCA(WTYPE,ERRIND,ICAT)
      IF (ICAT.NE.GOUTPT .AND. ICAT.NE.GOUTIN) THEN
        ERRIND = 39
        GO TO 100
      ENDIF
C
C  Check if index non-negative.
C
      IF (N .LT. 0) THEN
        ERRIND = 2002
        GO TO 100
      ENDIF
C
C  Invoke interface.
C
      FCODE = -118
      CONT  = 0
      CALL GZROI(0)
      IL1   = 2
      IL2   = 2
      ID(1) = WTYPE
      ID(2) = N
      IWK   = -1
      CALL GZIQWK(WTYPE,IWK)
      IF (RERR.NE.0) THEN
        ERRIND = RERR
        GOTO 100
      ENDIF
      NLT    = ID(3)
      LT     = ID(4)
      NLW    = ID(5)
      NPPLI  = ID(6)
      NOMLW  = RX(1)
      RLWMIN = RX(2)
      RLWMAX = RX(3)
      RETURN
C
  100 CONTINUE
      NLT    = -1
      LT     = -1
      NLW    = -1
      NPPLI  = -1
      NOMLW  = -1.
      RLWMIN = -1.
      RLWMAX = -1.
      RETURN
      END

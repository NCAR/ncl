C
C	$Id: gzclrwk.f,v 1.3 2000-07-12 16:40:03 haley Exp $
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
      SUBROUTINE GZCLRWK(WKID,COFL)
C
C  CLEAR WORKSTATION
C
      INTEGER ECLRWK
      PARAMETER (ECLRWK=6)
C
      include 'gkscom.h'
C
      INTEGER WKID,COFL,FCODEO,CONTO
C
C  Check that GKS is in the proper state.
C
      CALL GZCKST(6,ECLRWK,IER)
      IF (IER .NE. 0) RETURN
C
C  Check if workstation identifier is valid.
C
      CALL GZCKWK(20,ECLRWK,WKID,IDUM,IER)
      IF (IER .NE. 0) RETURN
C
C  Check if the specified workstation is open.
C
      CALL GZCKWK(25,ECLRWK,WKID,IDUM,IER)
      IF (IER .NE. 0) RETURN
C
C  Check on workstation category.
C
      CALL GQWKC(WKID,IER,ICONID,ITYPE)
      IF (IER .NE. 0) THEN
        ERS = 1
        CALL GERHND(IER,ECLRWK,ERF)
        ERS = 0
        RETURN
      ENDIF
      CALL GQWKCA(ITYPE,IER,ICAT)
      IF (IER .NE. 0) THEN
        ERS = 1
        CALL GERHND(IER,ECLRWK,ERF)
        ERS = 0
        RETURN
      ENDIF
      IF (ICAT .EQ. GMI) THEN
        IER = 33
        ERS = 1
        CALL GERHND(IER,ECLRWK,ERF)
        ERS = 0
        RETURN
      ELSE IF (ICAT .EQ. GINPUT) THEN
        IER = 35
        ERS = 1
        CALL GERHND(IER,ECLRWK,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Put out new picture initialization if CGM and the picture is empty.        
C
      IF (ITYPE .EQ. GCGM) THEN
        IF (NOPICT .LE. 0) THEN
          FCODEO = FCODE
          CONTO  = CONT
          FCODE = 91
          CONT  =  0
          CALL GZROI(0)
          CALL G01WDR(WKID,' ')
          FCODE  = FCODEO
          CONT   = CONTO
          NOPICT = 1
        ENDIF
      ENDIF
C
C  Invoke the workstation interface.
C
C  Set the flag CUFLAG to indicate that the interface call should go
C  only to the specifically designated workstation.
C
      CUFLAG = WKID
      FCODE = 1
      CONT  = 0
      CALL GZROI(0)
      IL1 = 2
      IL2 = 2
      ID(1) = WKID
      ID(2) = COFL
      CALL GZTOWK
      IF (RERR.NE.0) THEN
        ERS = 1
        CALL GERHND(RERR,ECLRWK,ERF)
        ERS = 0
      ENDIF
      CUFLAG = -1
C
C  Set flag to indicate that the current picture is empty if CGM.
C
      IF (ITYPE .EQ. GCGM) NOPICT = 0
C
      RETURN
      END

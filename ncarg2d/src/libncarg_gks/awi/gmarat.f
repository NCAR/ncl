C
C	$Id: gmarat.f,v 1.3 2000-07-12 16:39:43 haley Exp $
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
        SUBROUTINE GMARAT(IOS,STATUS)
C
C  Set the marker attributes.
C
C  Valid attributes:
C    POLYMARKER INDEX
C    MARKER TYPE
C    MARKER SIZE 
C    MARKER COLOR
C
      include 'trinst.h'
      include 'trstat.h'
      include 'trpars.h'
      include 'trcode.h'
      include 'gkscom.h'
C
      INTEGER IOS, STATUS
C
      IF (OPID .EQ. ATELMB) THEN
C
C  POLYMARKER BUNDLE INDEX
C
        CALL GOPDEC(MARIDX,MIXCPR,1,IOS,STATUS)
        CALL GSPMI(MARIDX)
      ELSE IF (OPID .EQ. ATELMT) THEN
C
C  MARKER TYPE
C
        CALL GOPDEC(MARTYP,MIXCPR,1,IOS,STATUS)
        CALL GSMK(MARTYP)
      ELSE IF (OPID .EQ. ATELMZ) THEN
C
C  MARKER SIZE
C
        CALL GTFLT(MARSIZ,MFLCPR,IOS,STATUS)
C
C  Scale the marker size by the current transformation matrix.
C  Take a vector (0,MARSIZ), transform it with the
C  current transformation matrix, then find the length of the
C  transformed vector.
C
        YSCALE = SQRT(CURTM(1,2)*CURTM(1,2)+CURTM(2,2)*CURTM(2,2))
        HS = MARSIZ*YSCALE
        IF (HS .LT. 0.00004) HS = 0.00004
        CALL GSMKSC(HS)
      ELSE IF (OPID .EQ. ATELMC) THEN
C
C  Set the marker color
C
        CALL GOPDEC(MARCOL,MCICPR,1,IOS,STATUS)
        CALL GSPMCI(MARCOL)
      ENDIF
C
      RETURN
      END

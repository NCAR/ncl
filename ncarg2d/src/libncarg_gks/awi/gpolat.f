C
C	$Id: gpolat.f,v 1.2 2000-07-12 16:39:44 haley Exp $
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
        SUBROUTINE GPOLAT(IOS,STATUS)
C
C  Set POLYLINE attributes.
C
C  Valid attributes:
C    POLYLINE INDEX
C    LINE TYPE
C    LINE WIDTH
C    LINE COLOR
C
      include 'trcode.h'
      include 'trinst.h'
      include 'trstat.h'
      include 'trpars.h'
C
      INTEGER IOS, STATUS
C
      IF (OPID .EQ. ATELLI) THEN
C
C  POLYLINE BUNDLE INDEX
C
        CALL GOPDEC(POLIDX,MIXCPR,1,IOS,STATUS)
        CALL GSPLI(POLIDX)
      ELSE IF (OPID .EQ. ATELLT) THEN
C
C  LINE TYPE
C
        CALL GOPDEC(LINTYP,MIXCPR,1,IOS,STATUS)
        CALL GSLN(LINTYP)
      ELSE IF (OPID .EQ. ATELLW) THEN
C
C  LINE WIDTH
C
        CALL GTFLT(LINWTH,MFLCPR,IOS,STATUS)
        CALL GSLWSC(LINWTH)
      ELSE IF (OPID .EQ. ATELLC) THEN
C
C  LINE COLOR
C
        CALL GOPDEC(LINCOL,MCICPR,1,IOS,STATUS)
        CALL GSPLCI(LINCOL)
      ENDIF
C
      RETURN
      END

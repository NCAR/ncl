C
C	$Id: gelms.f,v 1.2 2000-07-12 16:39:41 haley Exp $
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
        SUBROUTINE GELMS(IOS,STATUS)
C
C  This routine invokes the processors for the valid 
C  graphical elements.
C
C  Supported graphical elements: 
C       POLYLINE
C       POLYMARKER
C       TEXT
C       POLYGON
C       CELL ARRAY
C       GENERALIZED DRAWING PRIMITIVE
C
      include 'trinst.h'
      include 'trcode.h'
C
      INTEGER IOS, STATUS
C
      IF (OPID .EQ. GPELPL) THEN
C
C  POLYLINE
C
        CALL GPUTPT(11)
C
      ELSE IF (OPID .EQ. GPELPM) THEN
C
C  POLYMARKER
C
        CALL GPUTPT(12)
C
      ELSE IF (OPID .EQ. GPELPG) THEN
C
C  POLYGON
C
        CALL GPUTPT(14)
C
      ELSE IF (OPID .EQ. GPELTX) THEN 
C
C  TEXT
C
        CALL GTXDRW(IOS,STATUS)
C
      ELSE IF (OPID .EQ. GPELCA) THEN
C
C  CELL ARRAY
C
        CALL GCELDR(IOS,STATUS)
C
      ELSE IF (OPID.EQ.GPELGP) THEN
C
C  GENERALIZED DRAWING PRIMITIVE (Currently not supported).
C
        CALL GSKPOP(8,LEN,IOS,STATUS)
      END IF
C
      RETURN
      END

C
C	$Id: gatelm.f,v 1.2 2000-07-12 16:39:38 haley Exp $
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
        SUBROUTINE GATELM(IOS,STATUS)
C
C  Set the attributes. 
C
C  Supported attribute sets:
C       POLYLINES
C       POLYMARKERS
C       TEXT AND CHARACTERS
C       FILL OPTIONS
C       COLOR TABLE
C       ASPECT SOURCE FLAGS
C
      include 'trinst.h'
      include 'trcode.h'
C
      INTEGER IOS, STATUS
C
      STATUS = 0
C
      IF (OPID.GE.ATELLI .AND. OPID.LT.ATELMB) THEN
C
C  POLYLINE set
C
        CALL GPOLAT(IOS,STATUS)
C
      ELSE IF (OPID.GE.ATELMB .AND. OPID.LT.ATELTI) THEN
C
C  MARKER set
C
        CALL GMARAT(IOS,STATUS)
C
      ELSE IF (OPID.GE.ATELTI .AND. OPID.LT.ATELFI) THEN
C
C  TEXT set
C
        CALL GTXTAT(IOS,STATUS)
C
      ELSE IF (OPID.GE.ATELFI .AND. OPID.LT.ATELCB) THEN
C
C  FILL attribute set.
C
        CALL GFILAT(IOS,STATUS)
C
      ELSE IF (OPID .EQ. ATELCB) THEN
C
C  Color table attributes (skip them, they are not supposed to
C  be in a segment).
C
        CALL GSKPOP(8,LEN,IOS,STATUS)
C
      ELSE IF (OPID .EQ. ATELAS) THEN
C
C  Set the aspect source flags.
C
         CALL GASPAR(IOS,STATUS)
      END IF
C
      RETURN
      END

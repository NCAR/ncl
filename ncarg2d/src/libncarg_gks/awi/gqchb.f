C
C	$Id: gqchb.f,v 1.3 2000-07-12 16:39:45 haley Exp $
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
      SUBROUTINE GQCHB (ERRIND,CHBX,CHBY)
C
C  INQUIRE CHARACTER BASE VECTOR
C
      include 'gkscom.h'
C
      INTEGER ERRIND
      REAL    CHBX,CHBY
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
C
      IF (ERRIND .EQ. 0) THEN
        CHBX =  CCHUP(2)
        CHBY = -CCHUP(1)
      ELSE
        CHBX = 0.
        CHBY = 0.
      ENDIF
C
      RETURN
      END

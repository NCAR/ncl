C
C	$Id: frm.f,v 1.2 2000-07-12 17:04:34 haley Exp $
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
      SUBROUTINE FRM(IOS,STATUS)
C
C  EXECUTE A NEW FRAME ACTION
C
      COMMON /TRFRAM/ METALL, FRINIT, FRCNT
      LOGICAL METALL, FRINIT
      INTEGER  FRCNT
      INTEGER IOS, STATUS
      INTEGER BYTE8
C
      DATA BYTE8/8/
C
C  GET THE NEXT BIT SET TO REACH A 16 BIT BOUNDARY
C
      CALL MNINST(BYTE8,IOS,STATUS)
C
C  INCREMENT THE FRAME COUNT
C
      FRCNT = FRCNT + 1
C
      CALL FRAME
C
C  RESET MINIMAL DATA
C
      CALL MMDEF
C
      RETURN
      END

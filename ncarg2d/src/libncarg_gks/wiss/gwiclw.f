C
C	$Id: gwiclw.f,v 1.3 2000-07-12 16:54:41 haley Exp $
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
      SUBROUTINE GWICLW
C
C CLEAR WORKSTATION
C
      include 'gwiarq.h'
      include 'gksin.h'
      include 'gwiwsl.h'
      include 'gwiio.h'
      include 'gwiins.h'
      include 'gwiast.h'
      include 'gwiadf.h'
      include 'gwiopc.h'
      include 'gwienu.h'
C
      SAVE
C
C  Flush buffer.
C
      CALL GWIFLB (RERR)
      IF (RERR.NE.0)  GO TO 77
C
C  Reset all attribute deferral control variables.
C
      CALL GWPDVA
C
C  Set WSL entry "DISPLAY SURFACE EMPTY" to "EMPTY".
C
      MDEMPT = GEMPTY
C
C  Set WSL entry "NEW FRAME ACTION NECESSARY AT UPDATE" to "NO".
C
      MNFRAM = GNO
C
C  If update state is pending, set current window and viewport
C  entries to those requested.
C
      IF (MTUS .EQ. GPEND) THEN
         CWINDO(1) = RWINDO(1)
         CWINDO(2) = RWINDO(2)
         CWINDO(3) = RWINDO(3)
         CWINDO(4) = RWINDO(4)
         CWKVP(1) = RWKVP(1)
         CWKVP(2) = RWKVP(2)
         CWKVP(3) = RWKVP(3)
         CWKVP(4) = RWKVP(4)
C
C  Set workstation update state to "NOT PENDING".
C
         MTUS = GNPEND
      ENDIF
C
   77 RETURN
C
      END

C
C	$Id: pltit.f,v 1.2 2000-07-12 17:04:35 haley Exp $
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
      SUBROUTINE PLTIT(NX,NY,PEN,IOS,STATUS)
C
C  CONTROL THE LINE DRAWING BUFFERS
C
C  INPUT
C       NX-X COORDINATE IN PLOTTER ADDRESS UNITS (0-32767)
C       NY-Y COORDINATE IN PLOTTER ADDRESS UNITS (0-32767)
C       PEN-PEN UP DOWN COMMAND
C               0-PEN UP (MOVE COMMAND)
C               1-PEN DOWN (DRAW COMMAND)
C  OUTPUT
C       IOS-IO STATUS FLAG VALID ONLY IF STATUS = I/O ERROR
C       STATUS-STATUS FLAG DEFINED BY THE COMMON TREROR
C
      INTEGER NX, NY, PEN, IOS, STATUS
C
      CALL PLOTIT(NX,NY,PEN)
C
      RETURN
      END

C
C	$Id: ffppkt.f,v 1.3 2000-08-22 03:53:12 haley Exp $
C                                                                      
C			     Copyright (C)  1997
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published
C by the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C

      SUBROUTINE FFPPKT(BITPTR,PKTFLG,XVAL,YVAL)
C
      include 'fntcom.h'
C
      INTEGER BITPTR,PKTFLG,XVAL,YVAL
C
C  Writes a packet to the output buffer starting at bit
C  position BITPTR.  The packet contains the packet flag
C  PKTFLG and the values XVAL and YVAL.  
C
      IOFF = BITPTR
      CALL SBYTES(BUFFER,PKTFLG,IOFF,PKFLWD,0,1)
      IOFF = IOFF+PKFLWD
      CALL SBYTES(BUFFER,XVAL,IOFF,XBITWD,0,1)
      IOFF = IOFF+XBITWD
      CALL SBYTES(BUFFER,YVAL,IOFF,YBITWD,0,1)
C
      RETURN
      END


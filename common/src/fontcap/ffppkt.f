C
C	$Id: ffppkt.f,v 1.4 2008-07-27 12:23:42 haley Exp $
C                                                                      
C			     Copyright (C)  1997
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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


C
C	$Id: ffppkt.f,v 1.1 1997-01-24 21:59:51 haley Exp $
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


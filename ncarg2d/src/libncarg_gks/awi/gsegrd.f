C
C	$Id: gsegrd.f,v 1.2 2000-07-12 16:39:56 haley Exp $
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
      SUBROUTINE GSEGRD(IOS,STATUS)
C
C  Read a record from the segment.
C
      include 'trbufr.h'
      include '../bwi/g01io.h'
      include 'gkscom.h'
C
      PARAMETER (MYBITS=11520)
C
      INTEGER IOS, STATUS
      INTEGER TEMP, IER
      CHARACTER*1 DNAME
C
      STATUS = 0
      MOBFSZ = 1 + (MYBITS-1)/I1MACH(5)
C
C  Read in a record.
C
      MRCNMO = MRECNM
      MRECNM = METREC 
      IF (METREC .GT. MXSREC) THEN
        METREC = MRECNM
        MRECNM = MRCNMO
        STATUS = -1
        RETURN
      ENDIF
      CALL G01MIO (4, MCONID, DNAME, MBUFER, MOBFSZ, IER)
      IF (IER .NE. 0) GO TO 1000
      MRECNM = MRCNMO
      METREC = METREC + 1
C
C  Reset the bit pointer.
C
      METBIT = MBITST
C
C  Get the useful data count and convert from 8 bit bytes to bits.
C
      CALL GBYTES(MBUFER,TEMP,RECLOC,RECSIZ,0,1)
      MRECLN = TEMP * 8 + MBUFOF
C
C  If the bit count is out of range, then set error and return.
C
      IF (MRECLN.LE.MBUFOF .OR. MRECLN.GT.1440*8) THEN
        STATUS = 302
        RETURN
      END IF
C
C  Normal exit.
C
      RETURN
C
C  Error exit.
C
 1000 CONTINUE
      STATUS = IER
C
      RETURN
      END

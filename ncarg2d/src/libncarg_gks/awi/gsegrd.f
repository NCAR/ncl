C
C	$Id: gsegrd.f,v 1.4 2008-07-27 00:21:01 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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

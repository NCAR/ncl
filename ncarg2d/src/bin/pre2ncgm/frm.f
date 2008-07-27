C
C	$Id: frm.f,v 1.4 2008-07-27 00:59:04 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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

C
C	$Id: grqst.f,v 1.5 2008-07-27 00:21:01 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GRQST (WKID,STDNR,STAT,LOSTR,STR)
C
C  REQUEST STRING
C
      INTEGER ERQST
      PARAMETER (ERQST=86)
C
      INTEGER WKID,STDNR,STAT,LOSTR
      CHARACTER*(*) STR
C
      CHARACTER*80 IDR,ODR
C
C  The only reason this subroutine is in the NCAR GKS package is
C  to support the pause feature of FRAME and NGPICT in a standard
C  manner so that those two subroutines can work with a any level
C  2B GKS package.
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(7,ERQST,IER)
      IF (IER .NE. 0) RETURN
C
C  Check if the workstation identifier is valid.
C
      CALL GZCKWK(20,ERQST,WKID,IDUM,IER)
      IF (IER .NE. 0) RETURN
C
C  Check if the specified workstation is open.
C
      CALL GZCKWK(25,ERQST,WKID,IDUM,IER)
      IF (IER .NE. 0) RETURN
C
C  Other checks should be added if this subroutine is ever fully
C  implemented for NCAR GKS.
C
C
C  Make the interface call to pause.
C
      WRITE(IDR,500) WKID
  500 FORMAT(I5)
      CALL GESC(-1396,1,IDR,1,1,ODR)
C
      RETURN
      END

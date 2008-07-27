C
C	$Id: gsstm.f,v 1.4 2008-07-27 00:21:02 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GSSTM (WKID,STDNR,MODE,ESW)
C
C  SET STRING MODE
C
      INTEGER ESSTM
      PARAMETER (ESSTM=80)
C
      INTEGER WKID,STDNR,MODE,ESW
C
C  The only reason this subroutine is in the NCAR GKS package is
C  to support the pause feature of FRAME and NBPICT in a standard
C  manner so that those two subroutines can work with a any level
C  2B GKS package.
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(7,ESSTM,IER)
      IF (IER .NE. 0) RETURN
C
C  Check if the workstation identifier is valid.
C
      CALL GZCKWK(20,ESSTM,WKID,IDUM,IER)
      IF (IER .NE. 0) RETURN
C
C  Check if the specified workstation is open.
C
      CALL GZCKWK(25,ESSTM,WKID,IDUM,IER)
      IF (IER .NE. 0) RETURN
C
C  Other checks should be added if this subroutine is ever fully
C  implemented for NCAR GKS.
C
      RETURN
      END

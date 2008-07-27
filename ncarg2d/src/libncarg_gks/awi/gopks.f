C
C	$Id: gopks.f,v 1.9 2008-07-27 00:20:58 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GOPKS(ERRFIL,BUFA)
C
C  OPEN GKS
C
C  Force load of all BLOCKDATAs.
C
      COMMON /SECOMI/ IERRU,IERRF,IRECF,LOMSG
      INTEGER EOPKS
      PARAMETER (EOPKS=0)
      INTEGER ERRFIL,BUFA
C
      include 'gkscom.h'
C
C  Do calls forcing BLOCKDATAs to be loaded from a binary library.
C
        CALL GKSBD
        CALL G01BKD
        CALL GWIBKD
        CALL GSEGDT
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(1,EOPKS,IER)
      IF (IER .NE. 0) RETURN
C
C  Initialize the error state list and local names.
C
      CALL GZINES
C
C  Specify the error file in the GKS error state list.
C
      ERF = ERRFIL
C
C  Also change the error unit for SETER calls if ERRFIL is
C  non-zero.
C
      IF (ERRFIL .NE. 0) IERRU = ERRFIL
C
C  Initialize the GKS state list.
C
      CALL GZINSL
C
C  Set the GKS operating state to GKSOP.
C
      OPS = GGKOP
C
      RETURN
      END

C
C	$Id: gsasf.f,v 1.5 2008-07-27 00:21:01 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GSASF(LASF)
C
C  SET ASPECT SOURCE FLAGS
C
      INTEGER ESASF
      PARAMETER (ESASF=41)
C
      include 'gkscom.h'
C
      INTEGER LASF(13)
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,ESASF,IER)
      IF (IER .NE. 0) RETURN
C
C  Check if flags are 0 or 1.
C
      DO 200 I=1,13
      IF (LASF(I).NE.0 .AND. LASF(I).NE.1) THEN
        ERS = 1
        CALL GERHND(2000,ESASF,ERF)
        ERS = 0
      ENDIF
  200 CONTINUE
C
C  Set the current aspect source flags in the GKS state list.
C
      CLNA   = LASF( 1)
      CLWSCA = LASF( 2)
      CPLCIA = LASF( 3)
      CMKA   = LASF( 4)
      CMKSA  = LASF( 5)
      CPMCIA = LASF( 6)
      CTXFPA = LASF( 7)
      CCHXPA = LASF( 8)
      CCHSPA = LASF( 9)
      CTXCIA = LASF(10)
      CFAISA = LASF(11)
      CFASIA = LASF(12)
      CFACIA = LASF(13)
C
C  Invoke the workstation interface.
C
      FCODE = 43
      CONT  = 0
      CALL GZROI(0)
      IL1   = 13
      IL2   = 13
      DO 201 I=1,13
        ID(I) = LASF(I)
  201 CONTINUE
      CALL GZTOWK
      IF (RERR.NE.0 .AND. RERR.NE.-109) THEN
        ERS = 1
        CALL GERHND(RERR,ESASF,ERF)
        ERS = 0
      ENDIF
C
      RETURN
      END

C
C	$Id: gqasf.f,v 1.5 2008-07-27 00:20:58 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GQASF(ERRIND,LASF)
C
C  INQUIRE ASPECT SOURCE FLAGS
C
      include 'gkscom.h'
C
      INTEGER ERRIND,LASF(13)
C
C  Check if GKS is in proper state.
C
      CALL GZCKST(8,-1,ERRIND)
C
      IF (ERRIND.NE.0) THEN
        DO 200 I=1,13
          LASF(I) = -1
  200   CONTINUE
      ELSE
        LASF( 1) = CLNA
        LASF( 2) = CLWSCA
        LASF( 3) = CPLCIA
        LASF( 4) = CMKA
        LASF( 5) = CMKSA
        LASF( 6) = CPMCIA
        LASF( 7) = CTXFPA
        LASF( 8) = CCHXPA
        LASF( 9) = CCHSPA
        LASF(10) = CTXCIA
        LASF(11) = CFAISA
        LASF(12) = CFASIA
        LASF(13) = CFACIA
      ENDIF
C
      RETURN
      END

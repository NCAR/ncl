C
C       $Id: chkcyc.f,v 1.5 2008-07-27 00:17:27 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CHKCYC  (U,V,IMAX,JPTSY,IER)
C
C CHECK FOR CYCLIC CONDITION
C
      DIMENSION       U(IMAX,JPTSY)          ,V(IMAX,JPTSY)
      COMMON /STR01/  IS         ,IEND      ,JS        ,JEND
     1             ,  IEND1      ,JEND1     ,I         ,J
     2             ,  X          ,Y         ,DELX      ,DELY
     3             ,  ICYC1      ,IMSG1     ,IGFL1
C
        SAVE
      DO  10 J=JS,JEND
      IF (U(IS,J).NE.U(IEND,J)) GO TO  20
      IF (V(IS,J).NE.V(IEND,J)) GO TO  20
   10 CONTINUE
C
C MUST BE CYCLIC
C
      RETURN
   20 CONTINUE
C
C MUST NOT BE CYCLIC
C . CHANGE THE PARAMETER AND SET IER = -1
C
      ICYC1 = 0
      IER = -1
      RETURN
C
C-----------------------------------------------------------------------
C REVISION HISTORY
C
C OCTOBER, 1979     FIRST ADDED TO ULIB
C
C OCTOBER, 1980     ADDED BUGS SECTION
C
C JUNE, 1984        REMOVED STATEMENT FUNCTIONS ANDF AND ORF,
C                   CONVERTED TO FORTRAN77 AND GKS.
C
C MAY, 1988         CHANGED CODE (IN SUBROUTINE DRWSTR) WHICH PROTECTS
C                   UX ELEMENTS FROM BECOMING ZERO.  THE ORIGINAL CODE
C                   CAUSED UNDERFLOW ON IBM MACHINES.  (DJK)
C
C MARCH, 1990       FIXED SAVING AND RESTORING OF SET CALL.
C-----------------------------------------------------------------------
C
      END

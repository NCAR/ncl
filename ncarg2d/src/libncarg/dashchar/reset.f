C
C	$Id: reset.f,v 1.4 2008-07-27 00:16:58 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE RESET
C
C  USER ENTRY POINT
C  USED IN DASHCHAR AND DASHSMTH FOR COMPATIBILITY WITH CONREC AND
C  AUTOGRAPH WHICH ALSO USE DASHSUPR.  THUS SUBROUTINE PERFORMS NO
C  FUNCTION.
C
C-----------------------------------------------------------------------
C
C REVISION HISTORY--
C
C MAY 1984         CONVERTED TO FORTRAN 77 AND GKS.  CHANGED ENTRY
C                  DASHD TO DASHDB AND DASHDC.  DISALLOWED REAL
C                  ISIZE ARGUMENTS.
C
C DECEMBER 1979    ADDED A LIBRARY STATISTICS CALL AND A REVISION
C                  HISTORY
C
C JUNE 1988        CHANGED THE NAME OF A COMMON BLOCK TO GET RID OF A
C                  WARNING FROM SEGLDR.  (DJK)
C
C-----------------------------------------------------------------------
      END

C
C       $Id: chkcyc.f,v 1.4 2000-08-22 15:06:40 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published
C by the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
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

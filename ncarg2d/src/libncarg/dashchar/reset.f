C
C	$Id: reset.f,v 1.2 2000-07-12 16:23:01 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU Lesser General Public License as
C published by the Free Software Foundation; either version 2.1 of the
C License, or (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C Lesser General Public License for more details.
C
C You should have received a copy of the GNU Lesser General Public
C License along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
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

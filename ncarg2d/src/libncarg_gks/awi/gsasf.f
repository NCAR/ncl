C
C	$Id: gsasf.f,v 1.3 2000-07-12 16:39:55 haley Exp $
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

C
C	$Id: gopks.f,v 1.5 2000-07-12 16:39:44 haley Exp $
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
      SUBROUTINE GOPKS(ERRFIL,BUFA)
C
C  OPEN GKS
C
C  Force load of all BLOCKDATAs.
C
      EXTERNAL GKSBD,G01BKD,GWIBKD,GSEGDT
      INTEGER EOPKS
      PARAMETER (EOPKS=0)
      INTEGER ERRFIL,BUFA
C
      include 'gkscom.h'
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

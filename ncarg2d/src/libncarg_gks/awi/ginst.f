C
C	$Id: ginst.f,v 1.3 2000-07-12 16:39:42 haley Exp $
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
      SUBROUTINE GINST (WKID,STDNR,LSTR,ISTR,PET,XMIN,XMAX,YMIN,
     +                  YMAX,BUFLEN,INIPOS,LDR,DATREC)
C
C  INITIALISE STRING
C
      INTEGER EINST
      PARAMETER (EINST=74)
C
      INTEGER WKID,STDNR,LSTR,PET,BUFLEN,INIPOS,LDR
      REAL XMIN,XMAX,YMIN,YMAX
      CHARACTER*(*) ISTR
      CHARACTER*80 DATREC(LDR)
C
C  The only reason this subroutine is in the NCAR GKS package is
C  to support the pause feature of FRAME and NGPICT in a standard
C  manner so that those two subroutines can work with a any level
C  2B GKS package.
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(7,EINST,IER)
      IF (IER .NE. 0) RETURN
C
C  Check if the workstation identifier is valid.
C
      CALL GZCKWK(20,EINST,WKID,IDUM,IER)
      IF (IER .NE. 0) RETURN
C
C  Check if the specified workstation is open.
C
      CALL GZCKWK(25,EINST,WKID,IDUM,IER)
      IF (IER .NE. 0) RETURN
C
C  Other checks should be added if this subroutine is ever fully
C  implemented for NCAR GKS.
C
      RETURN
      END

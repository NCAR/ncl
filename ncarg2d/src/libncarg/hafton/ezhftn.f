C
C	$Id: ezhftn.f,v 1.3 2000-08-22 15:04:44 haley Exp $
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
      SUBROUTINE EZHFTN (Z,M,N)
C
      DIMENSION       Z(M,N)
      SAVE
C
C HALF-TONE PICTURE VIA SHORTEST ARGUMENT LIST.
C ASSUMPTIONS--
C     ALL OF THE ARRAY IS TO BE DRAWN,
C     LOWEST VALUE IN Z WILL BE AT LOWEST INTENSITY ON READER/PRINTER
C     OUTPUT, HIGHEST VALUE IN Z WILL BE AT HIGHEST INTENSITY, VALUES IN
C     BETWEEN WILL APPEAR LINEARLY SPACED, MAXIMUM POSSIBLE NUMBER OF
C     INTENSITIES ARE USED, THE PICTURE WILL HAVE A PERIMETER DRAWN,
C     FRAME WILL BE CALLED AFTER THE PICTURE IS DRAWN, Z IS FILLED WITH
C     NUMBERS THAT SHOULD BE USED (NO UNKNOWN VALUES).
C IF THESE CONDITIONS ARE NOT MET, USE HAFTON.
C EZHFTN ARGUMENTS--
C     Z   2 DIMENSIONAL ARRAY TO BE USED TO GENERATE A HALF-TONE PLOT.
C     M   FIRST DIMENSION OF Z.
C     N   SECOND DIMENSION OF Z.
C
      DATA FLO,HI,NLEV,NOPT,NPRM,ISPV,SPV/0.0,0.0,0,0,0,0,0.0/
C
C THE FOLLOWING CALL IS FOR GATHERING STATISTICS ON LIBRARY USE AT NCAR
C
      CALL Q8QST4 ('GRAPHX','HAFTON','EZHFTN','VERSION  1')
C
      CALL HAFTON (Z,M,M,N,FLO,HI,NLEV,NOPT,NPRM,ISPV,SPV)
      CALL FRAME
      RETURN
      END

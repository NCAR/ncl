C
C	$Id: ezcntr.f,v 1.3 2000-08-22 15:10:10 haley Exp $
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
      SUBROUTINE EZCNTR (Z,M,N)
      SAVE
      DIMENSION       Z(M,N)
C
C CONTOURING VIA SHORTEST POSSIBLE ARGUMENT LIST
C ASSUMPTIONS --
C     ALL OF THE ARRAY IS TO BE CONTOURED,
C     CONTOUR LEVELS ARE PICKED INTERNALLY,
C     CONTOURING ROUTINE PICKS SCALE FACTORS,
C     HIGHS AND LOWS ARE MARKED,
C     ALL LINES ARE DRAWN WITH A SOLID DASH-LINE PATTERN.
C     EZCNTR CALLS FRAME AFTER DRAWING THE CONTOUR MAP.
C IF THESE ASSUMPTIONS ARE NOT MET, USE CONREC.
C
C ARGUMENTS
C     Z   ARRAY TO BE CONTOURED
C     M   FIRST DIMENSION OF Z
C     N   SECOND DIMENSION OF Z
C***CRAYLIB HAS THIS BLANK COMMENT
C
      DATA NSET,NHI,NDASH/0,0,0/
C
C***CRAYLIB HAS THIS BLANK COMMENT
C THE FOLLOWING CALL IS FOR GATHERING STATISTICS ON LIBRARY USE AT NCAR
C
      CALL Q8QST4 ('GRAPHX','CONRECQCK','EZCNTR','VERSION 01')
C
C***CRAYLIB HAS THIS BLANK COMMENT
C
      CALL CONREC (Z,M,M,N,0.,0.,0.,NSET,NHI,NDASH)
      CALL FRAME
      RETURN
      END

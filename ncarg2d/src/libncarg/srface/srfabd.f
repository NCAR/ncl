C
C	$Id: srfabd.f,v 1.3 2000-08-22 15:06:27 haley Exp $
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
      BLOCKDATA SRFABD
      COMMON /SRFBLK/ LIMU(1024) ,LIML(1024) ,CL(41)     ,NCL        ,
     1                LL         ,FACT       ,IROT       ,NDRZ       ,
     2                NUPPER     ,NRSWT      ,BIGD       ,UMIN       ,
     3                UMAX       ,VMIN       ,VMAX       ,RZERO      ,
     4                IOFFP      ,NSPVAL     ,SPVAL      ,BIGEST
      COMMON /SRFIP1/ IFR        ,ISTP       ,IROTS      ,IDRX       ,
     1                IDRY       ,IDRZ       ,IUPPER     ,ISKIRT     ,
     2                NCLA       ,THETA      ,HSKIRT     ,CHI        ,
     3                CLO        ,CINC       ,ISPVAL
      COMMON /SRFINT/ ISRFMJ     ,ISRFMN     ,ISRFTX
C
C  INITIALIZATION OF INTERNAL PARAMETERS
C
      DATA ISPVAL/-999/
      DATA IFR,ISTP,IROTS,IDRX,IDRY,IDRZ,IUPPER,ISKIRT,NCLA/
     1       1,   0,    0,   1,   1,   0,     0,     0,   6/
      DATA THETA,HSKIRT,CHI,CLO,CINC/
     1       .02,    0., 0., 0.,  0./
      DATA NRSWT/0/
      DATA IOFFP,SPVAL/0,0.0/
C LINE COLOR INDEX
      DATA ISRFMJ/1/
C
C REVISION HISTORY----------
C FEBURARY 1979    ADDED REVISION HISTORY
C                  MODIFIED CODE TO CONFORM TO FORTRAN 66 STANDARD
C
C DECEMBER 1979    CHANGED LATEST REVISION DATE AND CHANGED LIBRARY
C                  STATISTICS CALL TO NSSL INSTEAD OF CRAYLIB
C
C MAY 1980         INSERTED MODS TO CORRECT THE ERROR THAT ENOUGH
C                  SPACE WAS NOT BEING ALLOWED FOR SKIRTS IN
C                  CERTAIN CASES.
C                  CHANGED THE NAMES OF ENTRY POINTS DRAW AND
C                  TRN32 TO DRAWS AND TRN32S TO BE CONSISTENT
C                  WITH USAGE IN CONJUNCTION WITH PWRZS.
C
C MARCH 1984       CONVERTED CODE TO FORTRAN 77 AND GKS.
C
C MARCH 1986       IMPLEMENTED MODS TO CORRECT VISIBILITY ERRORS
C                  IN CERTAIN OBJECTS HAVING LARGE GRADIENTS.
C
C NOVEMBER 1987    CORRECTED CODE TO GET RID OF SPURIOUS VERTICAL
C                  LINES PROBABLY CAUSED BY THE PREVIOUS MODS.
C
C NOVEMBER 1987    CORRECTED HACK'S PROBLEM BY MARKING THE VISIBILITY
C                  HORIZONS WHILE DRAWING THE SKIRTS.
C
C DECEMBER 1987    FIXED THE TEST IN THE ROUTINE SRFACE DETERMINING
C                  WHETHER THE I LOOP OR THE J LOOP IS OUTERMOST.
C
C MARCH 1990       FIXED THE USE OF SET CALLS.
C
C-----------------------------------------------------------------------
C
      END

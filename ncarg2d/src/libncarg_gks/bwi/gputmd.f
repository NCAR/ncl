C
C	$Id: gputmd.f,v 1.6 2000-08-22 15:09:42 haley Exp $
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
      SUBROUTINE GPUTMD (ERROR)
C
C  Generate metafile descriptor elements.
C
      INTEGER  ERROR
C
      include 'g01prm.h'
      include 'g01wdt.h'
      include 'g01opc.h'
      include 'g01ins.h'
C
      INTEGER  NBYTES, DRSET(2), G01PBL
C
C  CLASS and ID for drawing-set and control set pseudo op.
C
      DATA  DRSET /-1, 1/
C
      ERROR = 0
C
C  Put out METAFILE VERSION element.
C
      NBYTES = 1 + (MINTFW-1)/8
      CALL GPUTNI (CLMVER, IDMVER, NBYTES, ERROR)
      CALL GPUTPR (MVERSN, MINTFW, 1, ERROR)
      IF (ERROR.NE.0)  RETURN
C
C  Put out METAFILE DESCRIPTION.
C
      NBYTES = G01PBL(24,0)
      CALL GPUTNI (CLDSCR, IDDSCR, NBYTES, ERROR)
      CALL GPUTPS ('NCAR_GKS0A--VERSION_4.1 ', 24, 24, 0, ERROR)
      IF (ERROR.NE.0)  RETURN
C
C  Put out METAFIlE ELEMENTS LIST, drawing-set pseudo op.
C
      NBYTES = 1 + (MINTFW + 2*MIXFW - 1)/8
      CALL GPUTNI (CLMELT, IDMELT, NBYTES, ERROR)
      CALL GPUTPR (1, MINTFW, 1, ERROR)
      CALL GPUTPR (DRSET, MIXFW, 2, ERROR)
C
C  Put out MAXIMUM COLOR INDEX.
C
      NBYTES = 1 + (MCIXFW-1)/8
      ICLTMP = 1
      IELTMP = 9
      CALL GPUTNI (ICLTMP,IELTMP,NBYTES,ERROR)
      IMXTMP = 255
      CALL GPUTPR (IMXTMP,MCIXFW,1,ERROR)
C
C  Put out FONT LIST.  There are 20 fonts.
C
      NBYTES = 20 + ( 7+26+26+21+21+22+21+21+22+22+24+20+21+22+21+
     -               22+22+20+19+19)
      CALL GPUTNI(CLFLST,IDFLST,NBYTES,ERROR)
      CALL GPUTPS('DEFAULT'                   , 7, 7,0,ERROR)
      CALL GPUTPS('HERSHEY:CARTOGRAPHIC_ROMAN',26,26,0,ERROR)
      CALL GPUTPS('HERSHEY:CARTOGRAPHIC_GREEK',26,26,0,ERROR)
      CALL GPUTPS('HERSHEY:SIMPLEX_ROMAN'     ,21,21,0,ERROR)
      CALL GPUTPS('HERSHEY:SIMPLEX_GREEK'     ,21,21,0,ERROR)
      CALL GPUTPS('HERSHEY:SIMPLEX_SCRIPT'    ,22,22,0,ERROR)
      CALL GPUTPS('HERSHEY:COMPLEX_ROMAN'     ,21,21,0,ERROR)
      CALL GPUTPS('HERSHEY:COMPLEX_GREEK'     ,21,21,0,ERROR)
      CALL GPUTPS('HERSHEY:COMPLEX_SCRIPT'    ,22,22,0,ERROR)
      CALL GPUTPS('HERSHEY:COMPLEX_ITALIC'    ,22,22,0,ERROR)
      CALL GPUTPS('HERSHEY:COMPLEX_CYRILLIC'  ,24,24,0,ERROR)
      CALL GPUTPS('HERSHEY:DUPLEX_ROMAN'      ,20,20,0,ERROR)
      CALL GPUTPS('HERSHEY:TRIPLEX_ROMAN'     ,21,21,0,ERROR)
      CALL GPUTPS('HERSHEY:TRIPLEX_ITALIC'    ,22,22,0,ERROR)
      CALL GPUTPS('HERSHEY:GOTHIC_GERMAN'     ,21,21,0,ERROR)
      CALL GPUTPS('HERSHEY:GOTHIC_ENGLISH'    ,22,22,0,ERROR)
      CALL GPUTPS('HERSHEY:GOTHIC_ITALIAN'    ,22,22,0,ERROR)
      CALL GPUTPS('HERSHEY:MATH_SYMBOLS'      ,20,20,0,ERROR)
      CALL GPUTPS('HERSHEY:SYMBOL_SET1'       ,19,19,0,ERROR)
      CALL GPUTPS('HERSHEY:SYMBOL_SET2'       ,19,19,0,ERROR)
C
      RETURN
      END

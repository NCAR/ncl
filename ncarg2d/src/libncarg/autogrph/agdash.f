C
C $Id: agdash.f,v 1.4 2000-08-22 15:02:11 haley Exp $
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
      SUBROUTINE AGDASH (DASH,WODQ,WOCD,SCWP)
C
C AGDASH sets up the DASHD call required to establish the dash pattern
C desired for the next curve.  The arguments are as follows:
C
C -- DASH specifies the desired dash pattern.  A positive value implies
C    that a binary dash pattern is to be used, a negative value that a
C    character-string dash pattern is to be used.
C
C -- WODQ is the width of the solid-line segment specified by a dollar
C    sign and the gap specified by a quote, expressed as a fraction of
C    the smaller side of the curve window.
C
C -- WOCD is the width of a character which is to be a part of the dash
C    pattern, expressed in the same units as WODQ.
C
C -- SCWP is the length of the smaller side of the curve window, in
C    plotter coordinate units.
C
C The following common block contains other AUTOGRAPH variables, both
C real and integer, which are not control parameters.  The only ones
C used here are MWCD and MWDQ - the minimum widths of characters and
C spaces, respectively, in the dash pattern.
C
      COMMON /AGORIP/ SMRL , ISLD , MWCL,MWCM,MWCE,MDLA,MWCD,MWDQ ,
     +                INIF
      SAVE /AGORIP/
C
C The following common block contains other AUTOGRAPH variables, of type
C character.
C
      COMMON /AGOCHP/ CHS1,CHS2
      CHARACTER*504 CHS1,CHS2
      SAVE /AGOCHP/
C
C The AUTOGRAPH function AGFPBN is of type integer.
C
      INTEGER AGFPBN
C
      IWCD=MAX0(MWCD,IFIX(WOCD*SCWP))
      IWDQ=MAX0(MWDQ,IFIX(WODQ*SCWP))
C
      IF (DASH.GE.0.) THEN
        CALL DASHDB (AGFPBN(DASH))
      ELSE
        CALL AGGTCH (IFIX(DASH),CHS1,LNC1)
        CALL DASHDC (CHS1(1:LNC1),IWDQ,IWCD)
      END IF
C
      RETURN
C
      END

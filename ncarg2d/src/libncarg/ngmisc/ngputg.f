C
C $Id: ngputg.f,v 1.4 2000-07-12 16:24:46 haley Exp $
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
      SUBROUTINE NGPUTG(IFNT,PCHAR,X,Y,SIZE,ICOLOR)
C
C  Draw the glyph associated with the ASCII character CHAR from
C  font number IFNT.
C
      CHARACTER*1 PCHAR,TCHAR
C
C  Disable the function code flag since it is never used and since
C  we want to allow for plotting all printable ASCII characters.
C  Save and restore the current flag as well as the font and
C  character color.
C
      CALL PCGETC('FC',TCHAR)
      CALL PCGETI('FN',IFNTO)
      CALL PCGETI('CC',ICLRO)
C
      CALL PCSETC('FC',CHAR(30))
      CALL PCSETI('FN',IFNT) 
      CALL PCSETI('CC',ICOLOR)
      CALL PLCHHQ(X,Y,PCHAR,6.*SIZE/7.,0.,0.) 
C
      CALL PCSETC('FC',TCHAR)
      CALL PCSETI('FN',IFNTO)
      CALL PCSETI('CC',ICLRO)
C
      RETURN
      END

C
C $Id: agpwrt.f,v 1.3 2000-07-12 16:22:00 haley Exp $
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
      SUBROUTINE AGPWRT (XPOS,YPOS,CHRS,NCHS,ISIZ,IORI,ICEN)
C
      CHARACTER*(*) CHRS
C
C This routine just passes its arguments along to the character-drawing
C routine PWRIT, in the system plot package.  By substituting his/her
C own version of AGPWRT, the user can cause a fancier character-drawer
C to be used.
C
      CALL PWRIT (XPOS,YPOS,CHRS,NCHS,ISIZ,IORI,ICEN)
C
C Done.
C
      RETURN
C
      END

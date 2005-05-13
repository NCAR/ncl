C
C $Id: plchhz.f,v 1.1 2005-05-13 19:55:17 kennison Exp $
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
      SUBROUTINE PLCHHZ (XPOS,YPOS,SLEN,CHRS,SIZE,ANGD,CNTR)
C
        CHARACTER CHRS*(*)
C
C This routine is just like PLCHHQ (which it eventually calls), but it
C has an additional argument SLEN which is the desired length, in the
C fractional coordinate system, of the character string written.  If
C the length of the string without zooming is between 4/5ths and 5/4ths
C of the desired length, the capability of zooming in the X direction
C is used to make the string have exactly the desired length.  This can
C be used to generate paragraphs that are justified on both left and
C right.
C
C First, save current values of some internal parameters of PLOTCHAR.
C
        CALL PCGETI ('TE',ISTE)
        CALL PCGETR ('ZX',ZOOX)
        CALL PCGETR ('ZY',ZOOY)
        CALL PCGETR ('ZZ',ZOOZ)
C
C Reset the parameters.
C
        CALL PCSETI ('TE',1)
        CALL PCSETR ('ZX',1.)
        CALL PCSETR ('ZY',1.)
        CALL PCSETR ('ZZ',1.)
C
C Find the length of the unzoomed character string.
C
        CALL PLCHHQ (XPOS,YPOS,CHRS,SIZE,360.,CNTR)
        CALL PCGETR ('DL',DLFT)
        CALL PCGETR ('DR',DRGT)
C
C If the zoom factor required to make the character string have the
C desired length is in the acceptable range, use it.
C
        ZOOM=SLEN/(DLFT+DRGT)
        IF (ZOOM.GT..8.AND.ZOOM.LT.1.25) THEN
          CALL PCSETR ('ZX',ZOOM)
        END IF
C
C Write the character string.
C
        CALL PLCHHQ (XPOS,YPOS,CHRS,SIZE,MOD(ANGD,360.),CNTR)
C
C Reset internal parameters of PLOTCHAR that might have been changed.
C
        CALL PCSETI ('TE',ISTE)
        CALL PCSETR ('ZX',ZOOX)
        CALL PCSETR ('ZY',ZOOY)
        CALL PCSETR ('ZZ',ZOOZ)
C
C Done.
C
        RETURN
C
      END

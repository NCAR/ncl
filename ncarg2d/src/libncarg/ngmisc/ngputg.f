C
C $Id: ngputg.f,v 1.6 2008-07-27 00:17:18 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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

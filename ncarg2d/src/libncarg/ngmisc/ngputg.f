      SUBROUTINE NGPUTG(IFNT,CHAR,X,Y,SIZE,ICOLOR)
C
C  Draw the glyph associated with the ASCII character CHAR from
C  font number IFNT.
C
      CHARACTER*1 CHAR
C
      CALL PCSETI('FN',IFNT) 
      CALL PCSETI('CC',ICOLOR)
      CALL PLCHHQ(X,Y,CHAR,6.*SIZE/7.,0.,0.) 
C
      RETURN
      END

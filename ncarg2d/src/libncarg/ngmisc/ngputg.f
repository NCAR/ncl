      SUBROUTINE NGPUTG(IFNT,PCHAR,X,Y,SIZE,ICOLOR)
C
C  Draw the glyph associated with the ASCII character CHAR from
C  font number IFNT.
C
      CHARACTER*1 PCHAR,TCHAR
C
C  Disable the function code flag since it is never used and since
C  we want to allow for plotting all printable ASCII characters.
C  Save and restore the current flag.
C
      CALL PCGETC('FC',TCHAR)
      CALL PCSETC('FC',CHAR(30))
      CALL PCSETI('FN',IFNT) 
      CALL PCSETI('CC',ICOLOR)
      CALL PLCHHQ(X,Y,PCHAR,6.*SIZE/7.,0.,0.) 
      CALL PCSETC('FC',TCHAR)
C
      RETURN
      END

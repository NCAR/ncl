C
C $Id: pccfff.f,v 1.1 1992-11-17 18:46:09 kennison Exp $
C
      SUBROUTINE PCCFFF (IPSS,IBNU,NFNT,NASC,CHGT,SIZE,RDGU,LDGU,NDGU)        
C
C
      PARAMETER (NUMNTR=29)
      COMMON /PCMTRC/TYPFLG, CHRSTR, CHREND, FRIGHT, FTOP  , FCAPOV,
     +               FCAP  , FXHOV , FXH   , FHALF , FBASE , FBOT  ,
     +               FCHSWD, FCVSWD, FLLX  , FLLY  , FURX  , FURY  ,
     +               FLLEX , FLLEY , FUREX , FUREY , TABPNT, XBITWD, 
     +               YBITWD, XBIAS , YBIAS , PKFLWD, LSTPNT
      INTEGER        TYPFLG, CHRSTR, CHREND, FRIGHT, FTOP  , FCAPOV,
     +               FCAP  , FXHOV , FXH   , FHALF , FBASE , FBOT  ,
     +               FCHSWD, FCVSWD, FLLX  , FLLY  , FURX  , FURY  ,
     +               FLLEX , FLLEY , FUREX , FUREY , TABPNT, XBITWD, 
     +               YBITWD, XBIAS , YBIAS , PKFLWD, LSTPNT
      INTEGER FNINFO(NUMNTR)
      EQUIVALENCE (FNINFO,TYPFLG)
      SAVE   /PCMTRC/
C
      PARAMETER (IFCLEN=3000, ICLEN=150)
      COMMON /PCINDX/IBFC(IFCLEN)    ,SFLGS(ICLEN)   ,CHRPNT(128),
     +               IXC(ICLEN)      ,IYC(ICLEN)     ,XC(ICLEN)  ,
     +               YC(ICLEN)       ,OUTLIN         ,SCALE
      INTEGER        IBFC            ,SFLGS          ,CHRPNT     , 
     +               IXC             ,IYC            ,OUTLIN
      REAL           XC              ,YC             ,SCALE
      SAVE   /PCINDX/
C
      DIMENSION RDGU(LDGU)
C
C The subroutine PCCFFF retrieves the digitization of a specified
C character from one of the locally-defined NCAR Graphics fontcaps
C that are defined for filled fonts.
C
C IPSS is the number of the pass being executed by PLCHHQ.
C
C IBNU is the number of a unit to be used in reading the fontcaps.
C
C NFNT is the number of the desired font.
C
C NASC is the ASCII decimal equivalent of the desired character.
C
C CHGT is a REAL specifying the desired character height in digitization
C      units (this is the value into which  FNTCAP-FNTBAS is mapped.)
C
C SIZE is a REAL specifying the approximate desired character height
C      in NDC units.
C
C RDGU is a real array in which the digitization is to be returned.
C
C LDGU is the length of RDGU.
C
C NDGU is the number of elements returned in RDGU.
C
C The digitization is to be in the form expected by PLCHHQ, in a
C coordinate system centered at the center of the character.  RDGU(1)
C must be a negative number whose magnitude is the distance from the
C center of the character to its left edge.  RDGU(2) must be a positive
C number whose magnitude is the distance from the center of the
C character to its right edge.
C
C For each odd value of I greater than 2, RDGU(I) is an X coordinate
C and RDGU(I+1) is a Y coordinate of some point on a polyline that is
C to be drawn or filled, with the following exception:  each polyline
C is is terminated by a "point" with X coordinate -2048. or -2047.; 
C the value -2048. implies that the polyline is to be drawn and -2047. 
C that it is to be filled.  This point is not actually part of the 
C polyline, it simply marks the end of the polyline.  The Y coordinate
C paired with it says what color is to be used for the draw or fill.  
C If the Y coordinate is 0., no special color is implied; drawing or
C filling will be done using whatever color is implied by internal
C parameters of PLOTCHAR determining the colors to be used for the 
C various parts of normal characters.  If the Y coordinate is positive,
C a special color is implied (as, for example in state and federal
C highway signs).  
C
C ---------------------------------
C
C The variable NFRL, if non-zero, defines the number of the font read
C last.
C
      SAVE NFRL
C
      DATA NFRL / 0/
C
C Zero the count of digitization elements returned.
C
      NDGU=0
      NFNTL = NFNT
C
C If the desired font is not the one read last, read it.  If there is
C any problem with reading it, just return with NDGU zeroed to indicate
C that the desired character was not available.
C
      IF (NFRL .NE. NFNT) THEN
C
C  Set the OUTLIN flag for outline fonts and reassign the font number.
C
        IF (NFNT.EQ.121 .OR. NFNT.EQ.122 .OR. NFNT.EQ.125 .OR.
     +      NFNT.EQ.126 .OR. NFNT.EQ.129 .OR. NFNT.EQ.130 .OR.
     +      NFNT.EQ.133 .OR. NFNT.EQ.134 .OR. NFNT.EQ.135 .OR.
     +      NFNT.EQ.136 .OR. NFNT.EQ.137 .OR. NFNT.EQ.199) THEN
          OUTLIN = 1
          NFNTL = NFNT - 100
        ELSE
          OUTLIN = 0
        ENDIF
C
C Open the fontcap file.
C
        IBNS=IBNU
        CALL PCFOPN (IBNU,NFNTL)
C
C Read the fontcap.
C
        CALL PCFRED (IBNU,NFNTL,IBFC,3000)
C
C Close the fontcap file.
C
        CALL PCFCLS (IBNU,NFNTL)
        IBNU=IBNS
C
C Set the variable NFRL to reflect the number of the font last read.
C
        NFRL=NFNT
C
C Get the font metric information from the fontcap and store in common
C PCINDX.
C
        CALL PCFFME (CHGT)
C
      END IF
C
C  Calculate the scale factor to go from the font coordinate space 
C  to the height specified in the call to PCCFFF.
C
      SCALE = CHGT/REAL(FCAP-FBASE)
C
C Get the digitization.
C
      CALL PCFFGD (IPSS,NASC,CHGT,SIZE,RDGU,LDGU,NDGU)
C
C Done.
C
      RETURN
C
      END

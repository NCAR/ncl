C
C	$Id: gflas3.f,v 1.6 2008-07-27 00:17:13 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GFLAS3(INAME)
C
C_BEGIN PROLOGUE GFLAS3
C
C_PURPOSE
C    A call to this subroutine will insert all plotting instructions
C    saved in a previous GFLAS1/GFLAS2 sequence (with identifier
C    INAME) into the output pipeline.
C
C_DESCRIPTION
C    A call to this subroutine will insert all plotting instructions
C    saved in a previous GFLAS1/GFLAS2 sequence (with identifier
C    INAME) into the output pipeline.  The plotting instructions
C    saved in the previous GFLAS1/GFLAS2 sequence are displayed
C    exactly as they were created.
C
C_ARGUMENTS
C
C INAME   (INPUT,SINGLE,VARIABLE)  An integer variable in the range
C         [0,99] which is used as a user-defined identifier for the
C         dataset of graphics instructions to be inserted into the
C         output pipeline.  This identifier must be an identifier
C         which was used either as an argument to a previous GFLAS1
C         call, or, if one is using the NCAR GKS package, a previous
C         GFLAS4 call.
C
C_I/O
C    GFLAS3 writes the plotting instructions saved in a previous
C    GFLAS1/GFLAS2 sequence to all active worksations.  If one is
C    using the NCAR GKS package, this results in opening the file
C    created by GFLAS1, reading all records from that file and 
C    writing them to the output files, and closing the input file.
C
C_ROUTINES CALLED
C    PLOTIT, SETER
C
C_COMMON BLOCKS
C    GFLASH
C
C_MAXIMUM GKS LEVEL
C    2A
C
C_LANGUAGE
C    FORTRAN
C
C_REVISION DATE
C    890117 (YYMMDD)
C
C_HISTORY
C    This subroutine was coded at NCAR by Fred Clare.  Its purpose
C    is to duplicate the functionality of the previous NCAR System
C    Plot Package entry FLASH3 in a GKS environment.
C
C_REFERENCES
C    "The System Plot Package", NCAR Technical Note number
C    NCAR-TN/162+IA, May, 1983.
C
C    "NCAR Graphics User's Guide", Version 2.00, NCAR Technical Note
C    number NCAR/TN-283+IA, August, 1987.
C
C_LONG DESCRIPTION
C    A call to GFLAS3 will insert all the instructions saved in
C    a previous GFLAS1/GFLAS2 sequence into the output pipeline.
C    The instructions are an exact snapshot of what would have
C    been sent to an active workstation if it had been active
C    during the GFLAS1/GFLAS2 sequence.  A call to GFLAS3 is not
C    affected by the clipping rectangle in effect at the time
C    GFLAS3 is called, nor does a call to GFLAS3 have any effect
C    on the currently active clipping rectangle.  Similarly, a
C    call to GFLAS3 has no effect on the current state of GKS
C    attributes.
C
C_EXAMPLES
C    Example 1 --  Save a text string in a flash buffer and insert
C                  it into the output pipeline.
C          SUBROUTINE SVTXT
C          CALL GFLAS1(9)
C          CALL GTX(.5,.5,'STRING')
C          CALL GFLAS2
C          CALL GFLAS3(9)
C          RETURN
C          END
C
C_END PROLOGUE GFLAS3
C
C---------------------------------------------------------------
C
C  Common for FLASH package.
C
      COMMON /GFLASH/MODEF,IOPWKS(100),IOACT(100),NUMOP,IWISSI
C
C     CHARACTER*80 IDR(1),ODR(1)
C
      SAVE
C
C  Check on proper mode value.
C
      IF (MODEF .LT. 2) GO TO 101
C
C  Set mode flag.
C
      MODEF = 3
C
C  Copy the named segment to all currently active workstations of
C  type output, output/input, or metafile output.
C
      CALL GQACWK(1,IER,IAL,ID)
      DO 20 I=1,IAL
      CALL GQACWK(I,IER,IAL,ID)
      CALL GQWKC(ID,IER,ICON,IT)
      CALL GQWKCA(IT,IER,ICAT)
      IF (ICAT.EQ.0 .OR. ICAT.EQ.2 .OR. ICAT.EQ.4) THEN
C
C  Force a new picture initialization if necessary.
C
C       WRITE(IDR(1),110) ID
C 110   FORMAT('2',I8)
C       IFID = -1394
        CALL PLOTIT(0,0,2)
C       CALL GESC(IFID,1,IDR,1,IDUM,ODR)
        CALL GCSGWK(ID,INAME)
        CALL PLOTIT(0,0,2)
      ENDIF
   20 CONTINUE
      RETURN
C
  101 CONTINUE
      CALL GECLKS
      CALL SETER(' GFLAS3 -- GFLAS3 CALLED WITHOUT CALLING GFLAS2 OR GFL
     -AS4',7,2)
      STOP
C
      END

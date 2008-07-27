C
C	$Id: g01ctb.f,v 1.7 2008-07-27 00:21:05 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
        SUBROUTINE G01CTB
C
C  Set color representation (define color table entry).
C
      include 'g01prm.h'
      include 'gksin.h'
      include 'g01wsl.h'
      include 'g01ins.h'
      include 'g01opc.h'
      include 'gksenu.h'
C
      INTEGER  NCIX, NBYTES, I, IPTR
C
C  Store the flag indicatiing if a color table entry should
C  be written to the metafile.  Do not write a color table
C  entry if the picture is umpty.
C
      IWRITE = ID(2)
C
C  Scan the color table to see if the index already exists;
C  Find its position in the table otherwise (binary search
C  should be used here).
C
      NCIX = IC(1)
      DO 10 I=1,MOL
        IPTR = I
        IF (MCOLI(I)-NCIX)  10, 20, 15
   10 CONTINUE
C
C  Fall thru means that the index not defined yet and it is
C  higher than the highest defined index.
C
      IPTR = MOL + 1
C
C  Index is undefined -- it belongs at position IPTR in the table.
C  Test for room in the table, move table down.
C
   15 CONTINUE
      IF (MOL .GE. MOLMAX) THEN
C
C  Error -- table is full, set error, mark overflow, and return.
C
        RERR = 93
        MCOVFL = 1
        RETURN
      ELSE
C
C  Move table down, increment count of defined indices.
C
        DO 18 I=MOL,IPTR,-1
          MCOLI(I+1)  = MCOLI(I)
          SRED(I+1)   = SRED(I)
          SGREEN(I+1) = SGREEN(I)
          SBLUE(I+1)  = SBLUE(I)
   18   CONTINUE
        MOL = MOL + 1
      END IF
C
C  Insert/replace new color table entry.
C
   20 CONTINUE
      MCOLI(IPTR)  = NCIX
      SRED(IPTR)   = RX(1)
      SGREEN(IPTR) = RX(2)
      SBLUE(IPTR)  = RX(3)
C
C  Mark change to color table.  mark background color change 
C  if appropriate.
C
      MCTCHG = GYES
      IF (NCIX .EQ. 0)  MBCCHG = GYES
C
C  Normalize real components to integer range.
C
      ID(1) = RX(1)*MDCCRG
      ID(2) = RX(2)*MDCCRG
      ID(3) = RX(3)*MDCCRG
C
C  Write out a color table entry only if the current picture is
C  not empty.  If the current picture is empty, then the color table
C  will ultimately be put out by G01SNP.
C
      IF (IWRITE .EQ. 1) THEN
C
C  Generate metafile element with new entry.
C
C  Compute parameter list length -- number of bytes to hold
C       START INDEX + RED + GREEN + BLUE
C
        NBYTES = 1 + (3*MDCCFW + MCIXFW - 1)/8
C
C  Put out opcode (class and ID) and length.
C
        CALL GPUTNI (CLCTBL, IDCTBL, NBYTES, RERR)
        IF (RERR .NE. 0)  RETURN
C
C  Put out color index.
C
        CALL GPUTPR (NCIX, MCIXFW, 1, RERR)
        IF (RERR .NE. 0)  RETURN
C
C  Put out three color components.
C
        CALL GPUTPR (ID, MDCCFW, 3, RERR)
      ENDIF
C
      RETURN
      END

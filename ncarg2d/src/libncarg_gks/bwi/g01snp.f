C
C	$Id: g01snp.f,v 1.3 1994-03-30 02:08:27 fred Exp $
C
      SUBROUTINE G01SNP (ERROR)
C
C  Start new picture.
C
      INTEGER ERROR
      INTEGER  G01PBL, HOLD(3), NBYTES, ITMP(4)
C
      include 'g01prm.h'
      include 'g01io.h'
      include 'g01opc.h'
      include 'g01ins.h'
      include 'g01wsl.h'
      include 'gksenu.h'
C
C  Set 'NEW FRAME' flag.
C
      MNFFLG = GYES
C
C  Put out 'BEGIN PICTURE' element using the picture name in MPNAME.
C
      CALL GPUTNI (CLBEGP, IDBEGP, G01PBL(60,0), ERROR)
      CALL GPUTPS (MPNAME(1:60), 60, 60, 0, ERROR)
C
C  Reset picture name to blank.
C
      MPNAME = ' '
      IF (ERROR.NE.0)  GO TO 77
C
C  Put out the PICTURE DESCRIPTOR.
C
C  Put out background color, conditionally depending upon
C  whether it has been explicitly changed.
C
      IF (MBCCHG .EQ. GYES)  THEN
         NBYTES = 1 + (3*MDCCFW-1)/8
         CALL GPUTNI (CLBKGC, IDBKGC, NBYTES, ERROR)
         HOLD(1) = SRED(1)*MDCCRG
         HOLD(2) = SGREEN(1)*MDCCRG
         HOLD(3) = SBLUE(1)*MDCCRG
         CALL GPUTPR (HOLD, MDCCFW, 3, ERROR)
         IF (ERROR.NE.0)  GOTO 77
      ENDIF
C
C  Put out 'BEGIN PICTURE BODY' element.
C
      CALL GPUTNI (CLBGPB, IDBGPB, 0, ERROR)
      IF (ERROR.NE.0)  GO TO 77
C
 77   CONTINUE
C
C  Send the color table.
C
      NBYTES = 1 + (3*MDCCFW + MCIXFW - 1)/8
      DO 10 I=1,MOL
C
C  Put out opcode (CLASS and ID) and length.
C
        CALL GPUTNI (CLCTBL, IDCTBL, NBYTES, RERR)
        IF (RERR.NE.0)  GO TO 77
C
C  Put out parameter index (DATA, PRECIS, COUNT)
C
        CALL GPUTPR (MCOLI(I), MCIXFW, 1, RERR)
        IF (RERR.NE.0)  GO TO 77
C
C  Put out the color components.
C
        ITMP(1) = SRED(I)*REAL(MDCCRG)
        ITMP(2) = SGREEN(I)*REAL(MDCCRG)
        ITMP(3) = SBLUE(I)*REAL(MDCCRG)
        CALL GPUTPR (ITMP, MDCCFW, 3, RERR)
   10 CONTINUE
C
C  Put out clipping indicator and rectangle.
C
C  Put out opcode (CLASS and ID) and length.
C
      NBYTES = 1+(MIXFW-1)/8
      CALL GPUTNI (CLCLIN, IDCLIN, NBYTES, RERR)
      IF (RERR.NE.0)  GO TO 77
C
C  Put out clipping indicator parameter (DATA, PRECIS, COUNT).
C
      CALL GPUTPR (MRCLIP, MIXFW,     1, RERR)
      IF (RERR.NE.0)  GO TO 77
C
C  Total byte length, based on VDC bit precision.
C
      NBYTES = 1 + (4*MVDCFW-1)/8
C
C  Put out opcode (CLASS and ID) and LENGTH.
C
      CALL GPUTNI (CLCREC, IDCREC, NBYTES, RERR)
      IF (RERR.NE.0)  GO TO 77
C
C  Put out clipping rectangle parameters (XMIN,XMAX,YMIN,YMAX).
C
      CALL GPUTPR (MRCREC, MVDCFW,     4, RERR)
C
      RETURN
      END

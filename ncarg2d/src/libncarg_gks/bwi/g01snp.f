C
C	$Id: g01snp.f,v 1.10 2008-07-27 00:21:06 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
C  Put out the clipping indicator and rectangle.
C
       CALL G01CLP(1)
C
C  Send the color table.
C
      NBYTES = 1 + (3*MDCCFW + MCIXFW - 1)/8
      DO 10 I=1,MOL
C
C  Put out opcode (CLASS and ID) and length.
C
        CALL GPUTNI (CLCTBL, IDCTBL, NBYTES, ERROR)
        IF (ERROR.NE.0)  GO TO 77
C
C  Put out parameter index (DATA, PRECIS, COUNT)
C
        CALL GPUTPR (MCOLI(I), MCIXFW, 1, ERROR)
        IF (ERROR.NE.0)  GO TO 77
C
C  Put out the color components.
C
        ITMP(1) = SRED(I)*REAL(MDCCRG)
        ITMP(2) = SGREEN(I)*REAL(MDCCRG)
        ITMP(3) = SBLUE(I)*REAL(MDCCRG)
        CALL GPUTPR (ITMP, MDCCFW, 3, ERROR)
   10 CONTINUE
C
C  Set the flag indicating that the new picture information
C  has been put out.  This is to inform G01CLP to issue its
C  clipping information if G01SNP has been called.
C
      NPFLG = 1
C
      RETURN
      END

C
C	$Id: gskpop.f,v 1.1 1993-01-09 02:02:47 fred Exp $
C
      SUBROUTINE GSKPOP(OPNLEN,COUNT,IOS,STATUS)
C
C  This subroutine will skip the requested number of operands.
C  It will cross pratition boundries without intervention from a 
C  higher level. The LEN variable which higher level routines utilize
C  for knowlage of the size and remainder in the partition is modified.
C
C  INPUT
C    OPNLEN -- Length of each operand in bits.
C    COUNT  -- Number of operands to skip.
C
      include 'trinst.h'
      include 'trbufr.h'
C
      INTEGER OPNLEN, COUNT,  IOS, STATUS
      INTEGER BYTSIZ, PARBIT, CUR, LEFT  , RL
C       
      DATA BYTSIZ/8/
C
      STATUS = 0
C
C  INITIALIZE THE OPERAND POINTERS
C
      PARBIT = (OPNLEN*COUNT)
C
C  LOOP FOR ALL PARTITIONS
C
 10   CONTINUE
C
      LEFT = MAX0((PARBIT-LEN*BYTSIZ),0)
      CUR = MIN0(PARBIT, LEN*BYTSIZ)
      LEN = LEN - (CUR/BYTSIZ)
C
C  SKIP THE OPERANDS
C
 20   CONTINUE
C
C       LOOP FOR RECORDS WITHIN A PARTITION
C
        RL = CUR+MOPRST
        CUR = MAX0(CUR-(MRECLN-MOPRST),0)
        IF (CUR .GT.0) THEN
                CALL GSEGRD(IOS, STATUS)
                IF (STATUS .NE. 0) RETURN
                MOPRST = METBIT
                GO TO 20
        ELSE
                MOPRST = RL
                METBIT = RL
        END IF
C
C       LOOP IF THERE ARE MORE OPERANDS IN ANOTHER PARTITION
C
        PARBIT = LEFT
        IF (PARBIT .GT. 0) THEN
                CALL GNPART(IOS,STATUS)
                IF (STATUS .NE. 0) RETURN
                GO TO 10
        END IF
C
      RETURN
      END

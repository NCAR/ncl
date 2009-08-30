C
C	$Id: gputpt.f,v 1.5 2009-08-30 05:54:47 fred Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GPUTPT(ICODE)
C
C  Put out coordinate points from segment copy.
C
      include 'trpars.h'
      include 'trinst.h'
      include 'trbufr.h'
      include 'gkscom.h'
      include '../bwi/g01io.h'
C
      LOGICAL CNTINO
C
      INTEGER CONVAL, NOPNS, TNOPNS, TMAX, NBYTS, IOS, STATUS
      PARAMETER (MAXTMP=600,MAXTMH=300)
      INTEGER ITEMPS(MAXTMP)
      REAL TEMPP(MAXTMH),TEMPQ(MAXTMH)
      CHARACTER*1 DNAME
C
C  Set up interface values.
C
      FCODE = ICODE
      CALL GZROI(0)
C
C  Set TMAX to maximum dimension of temporary space.
C
      TMAX = MAXTMP
C
C  Compute the operand length.
C
      NBYTS = MOPLEN/8
C
C  Save the file pointers.
C
      MRECNO = MRECNM
      METREO = METREC
      METBIO = METBIT
      MRECLO = MRECLN
      MOPRSO = MOPRST
      LENO   = LEN
      CNTINO = CNTINU
      IF (CNTINU) THEN
        DO 50 I=1,MNWRDS
          MBFSAV(I) = MBUFER(I)
   50   CONTINUE
      ENDIF
C
C  Determine the total number of points to be passed.
C
      NPTOT = LEN/(2*NBYTS)
   20 CONTINUE
      IF (CNTINU) THEN
        CALL GSKPOP(MOPLEN,LEN/2,IOS,STATUS)
C
C  Get the continue flag value.
C
        CALL GBYTES(MBUFER,CONVAL,METBIT,MCOLEN,0,1)
        METBIT = METBIT + MCOLEN
        IF (CONVAL .EQ. CONFLG) THEN
          CNTINU = .TRUE.
        ELSE
          CNTINU = .FALSE.
        END IF
C
C  Get the long operand 8 bit byte count field.
C
        CALL GBYTES(MBUFER,LEN,METBIT,MLOLEN,0,1)
        METBIT = METBIT + MLOLEN
        MOPRST = METBIT
        NPTOT = NPTOT+LEN/(2*NBYTS)
        GO TO 20
      ENDIF
C
C  Reposition the input file and record pointers.
C
      IF (METREC .GT. METREO) THEN
        IBK = METREC - METREO
        MRECNO = MRECNM
        MRECNM = METREC
        DO 30 J=1,IBK 
          CALL G01MIO(6, MCONID, DNAME, IDUM1, IDUM2, IER)
   30   CONTINUE
        METREC = MRECNM
        MRECNM = MRECNO
      ENDIF
C
      MRECNM = MRECNO
      METREC = METREO
      METBIT = METBIO
      MRECLN = MRECLO
      MOPRST = MOPRSO
      LEN = LENO
      CNTINU = CNTINO
      IF (CNTINU) THEN
        DO 60 I=1,MNWRDS 
          MBUFER(I) = MBFSAV(I)
   60   CONTINUE
      ENDIF
C
C  Jump here if instruction continued from end of routine.
C
    5 CONTINUE
C
C  Compute the number of operands in the buffer.
C
      NOPNS = LEN/NBYTS
C
C  Number of operands must be greater than one.
C
      IF (NOPNS .GT. 1) THEN
    6   CONTINUE
C
C  Move only enough operands for the ITEMPS array.
C
        TNOPNS = NOPNS
        IF (TNOPNS .GT. TMAX) THEN
          TNOPNS = TMAX
          NOPNS = NOPNS - TMAX
        ELSE
          NOPNS = 0
        END IF
C
C  Get the operands.
C
        CALL GOPDEC(ITEMPS,MOPLEN,TNOPNS,IOS,STATUS)
        IF (STATUS .NE. 0) RETURN
C
C  Separate the coordinates into P and Q arrays.
C
        NP = TNOPNS/2
        DO 40 I=1,NP
          PTMP = REAL(ITEMPS(2*I-1))/32767.
          QTMP = REAL(ITEMPS(2*I))/32767.
C
C  Transform the points using the current segment transformation CURTM.
C
          TEMPP(I) = CURTM(1,1)*PTMP+CURTM(1,2)*QTMP+CURTM(1,3)        
          TEMPQ(I) = CURTM(2,1)*PTMP+CURTM(2,2)*QTMP+CURTM(2,3)        
   40   CONTINUE
C
C  Put the points out.
C
        CALL GZPUTR(NPTOT,NP,TEMPP,TEMPQ,0,IER)
C
C  If there was not enough room in the operand buffer, then move
C  the next set of this partition into the buffer.
C
        IF (NOPNS .NE. 0) GO TO 6
      END IF
C
C  Get the next part of the instruction if a continue.
C
      IF (CNTINU) THEN
        CALL GNPART(IOS,STATUS)
        IF (STATUS.NE.0) RETURN
        GO TO 5
      END IF
C
      RETURN
      END

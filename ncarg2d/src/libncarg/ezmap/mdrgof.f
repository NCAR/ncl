C
C $Id: mdrgof.f,v 1.9 2008-09-18 00:42:18 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDRGOF (IRGL,ICAT,ICEL,IRIM)
C
        INTEGER IRGL,ICAT,ICEL,IRIM
C
C This routine is called to open the files containing RANGS/GSHHS data
C at a specified resolution level IRGL.  The file descriptors come back
C in ICAT, ICEL, and IRIM.
C
C FLNM is a character variable in which to build the names of files to
C be opened by a call to NGOFRO.
C
        CHARACTER*1025 FLNM
C
C NCAT, NCEL, and NRIM hold the names of the catalog file, the cell
C file, and the rim data file to be read.
C
        CHARACTER*13  NCAT,NCEL,NRIM
C
C Declare other local variables.
C
        INTEGER       I,INAM,ISTA
C
C Get the name of the directory the RANGS/GSHHS files are in.
C
        CALL MDRGDI (FLNM)
        IF (ICFELL('MDRGOF',1).NE.0) RETURN
C
        DO 101 I=1,LEN(FLNM)-17
          IF (FLNM(I:I).EQ.' '.OR.FLNM(I:I).EQ.CHAR(0)) THEN
            INAM=I
            IF (I.NE.1) THEN
              IF (FLNM(I-1:I-1).NE.'/') THEN
                FLNM(I:I)='/'
                INAM=I+1
              END IF
            END IF
            GO TO 102
          END IF
  101   CONTINUE
C
        CALL SETER ('MDRGOF - MDRGDI RETURNED BAD DIRECTORY NAME',2,1)
C
C Define the names of the data files to be read.
C
  102   NCAT = 'rangs('//CHAR(ICHAR('0')+IRGL)//').cat'//CHAR(0)
        NCEL = 'rangs('//CHAR(ICHAR('0')+IRGL)//').cel'//CHAR(0)
        NRIM = 'gshhs('//CHAR(ICHAR('0')+IRGL)//').rim'//CHAR(0)
C
C Open all the data files.
C
        FLNM(INAM:INAM+12)=NCAT
        CALL NGOFRO (FLNM,ICAT,ISTA)
C
        IF (ISTA.NE.0) THEN
          CALL SETER ('MDRGOF - ERROR OPENING RANGS/GSHHS CAT FILE',3,1)
          RETURN
        END IF
C
        FLNM(INAM:INAM+12)=NCEL
        CALL NGOFRO (FLNM,ICEL,ISTA)
C
        IF (ISTA.NE.0) THEN
          CALL SETER ('MDRGOF - ERROR OPENING RANGS/GSHHS CEL FILE',4,1)
          CALL NGCLFI (ICAT)
          RETURN
        END IF
C
        FLNM(INAM:INAM+12)=NRIM
        CALL NGOFRO (FLNM,IRIM,ISTA)
C
        IF (ISTA.NE.0) THEN
          CALL SETER ('MDRGOF - ERROR OPENING RANGS/GSHHS RIM FILE',5,1)
          CALL NGCLFI (ICAT)
          CALL NGCLFI (ICEL)
          RETURN
        END IF
C
C Done.
C
        RETURN
C
      END

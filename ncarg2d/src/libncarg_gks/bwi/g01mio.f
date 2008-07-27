C
C	$Id: g01mio.f,v 1.12 2008-07-27 00:21:05 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE G01MIO (OP, UNIT, FNAME, BUFFER, LENGTH, ERROR)
C------------------------------------------------------------------------------
C
C	g01mio.f
C
C	The UNIX version of this routine allows the metafile
C	output to be specified using environment variables.
C	It does not actually use FORTRAN I/O.
C
C	Two environment variables play a part in this.
C
C	If NCARG_GKS_OUTPUT is not defined, the default file output
C	is determined by the procedure that calls G01MIO().
C
C	NCARG_GKS_OUTPUT can also be defined in two ways:
C
C	1. As a filename - CGM output will be directed to the
C	   file. (e.g. setenv NCARG_GKS_OUTPUT myfile)
C
C	2. A process, the name of which is preceeded by a pipe
C	   symbol. (e.g. setenv NCARG_GKS_OUTPUT "| translator")
C	   If no name follows the pipe symbol, a default translator
C	   is invoked.
C
C	NOTE: stdout to a translator is not used because of
C	potential conflicts with user code writing to logical unit 6.
C
C------------------------------------------------------------------------------
C
C  Central I/O routine for NCAR GKS.
C
C    INPUT PARAMETERS
C
C      OP     - Operation:
C                 =  1, open workstation for reading and writing on 
C                       ABS(UNIT) - truncates existing files.
C                 =  2, close workstation on ABS(UNIT).
C                 =  3, write buffer to ABS(UNIT).
C                 =  4, read ABS(UNIT) to buffer.
C                 =  5, position the record pointer to the beginning
C                       of the file.
C                 =  6, position the record pointer to the previous 
C                       record.
C                 =  7, flush the I/O buffers for UNIT.
C                 =  8, open workstation for reading only on ABS(UNIT).
C                 =  9, delete the segment whose file name is in FNAME.
C                 = 10, open a segment.
C                 = 11, open an existing CGM for appending.
C      UNIT   - ABS(UNIT) is the Fortran LUN on which OP is to occur.
C      FNAME  - filename used for open.
C      BUFFER - buffer containing data for a read/write operation.
C      LENGTH - length of data in BUFFER.
C
C    OUTPUT PARAMETERS
C
C      ERROR  - error indicator  =    0 if no errors.
C                                =   -1 if EOF.
C                                = -105 if open error.
C                                =  302 if read error.
C                                =  303 if write error.
C
C
      INTEGER  OP, UNIT, LENGTH, BUFFER(LENGTH), ERROR
      CHARACTER*(*) FNAME
C
C  Local variables:  IAUNIT is the Fortran LUN.
C                    IOPENF is a flag for the file open:
C                             = 0  open segment for read only.
C                             = 1  truncate and open for reading and writing.
C                             = 2  open a segment for writing.
C                             = 3  open CGM for appending.
C
      INTEGER IAUNT,IOPENF
C
      ERROR = 0
      IAUNT = ABS(UNIT)

C Open the workstation with filename FNAME

      IF (OP.EQ.1) THEN
        IOPENF = 1
	CALL OPNWKS(IAUNT, IOPENF, FNAME, ERROR)

C Close the workstation attached to IAUNT

      ELSE IF (OP.EQ.2) THEN
	CALL CLSWKS(IAUNT, ERROR)

C Write the BUFFER to the workstation

      ELSE IF (OP.EQ.3) THEN
	CALL WRTWKS(IAUNT, BUFFER, LENGTH, ERROR)

C Read from current location into BUFFER

      ELSE IF (OP.EQ.4) THEN
	CALL RDWKS(IAUNT, BUFFER, LENGTH, ERROR)

C Move file pointer to beginning

      ELSE IF (OP.EQ.5) THEN
	CALL BEGWKS(IAUNT, ERROR)

C Move file pointer back one record

      ELSE IF (OP.EQ.6) THEN
	CALL LSTWKS(IAUNT, ERROR)

C Flush the I/O buffers for a given unit

      ELSE IF (OP.EQ.7) THEN
	CALL FLSWKS(IAUNT, ERROR)

      ELSE IF (OP.EQ.8) THEN
        IOPENF = 0
	CALL OPNWKS(IAUNT, IOPENF, FNAME, ERROR)

      ELSE IF (OP.EQ.9) THEN
	CALL DELFIL(FNAME, ERROR)

      ELSE IF (OP.EQ.10) THEN
        IOPENF = 2
	CALL OPNWKS(IAUNT, IOPENF, FNAME, ERROR)

      ELSE IF (OP.EQ.11) THEN
        IOPENF = 3
	CALL OPNWKS(IAUNT, IOPENF, FNAME, ERROR)

      ENDIF

      END

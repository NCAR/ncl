C
C	$Id: g01mio.f,v 1.2 1993-01-11 20:53:53 don Exp $
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
C	(NOTE: stdout to a translator is not used because of
C	potential conflicts with user code writing to LU 6.
C
C------------------------------------------------------------------------------
C
C  CENTRAL I/O ROUTINE FOR METAFILE GENERATOR.
C
C    INPUT PARAMETERS
C      OP     - OPERATION,
C               =1, OPEN WORKSTATION FOR OUTPUT ON IABS(UNIT).
C               =2, CLOSE WORKSTATION FOR OUTPUT ON IABS(UNIT).
C               =3, WRITE BUFFER TO IABS(UNIT).
C               =4, READ IABS(UNIT) TO BUFFER
C               =5, POSITION THE RECORD POINTER TO THE BEGINNING
C                   OF THE FILE.
C               =6, POSITION THE RECORD POINTER TO THE PREVIOUS RECORD.
C               =7, FLUSH THE I/O BUFFERS FOR UNIT
C      UNIT   - IABS(UNIT) IS THE FORTRAN LUN ON WHICH OP IS
C               TO OCCUR.
C      FNAME  - FILENAME USED FOR OPEN.
C      BUFFER - BUFFER CONTAINING DATA FOR A WRITE OPERATION.
C      LENGTH - LENGTH OF DATA IN BUFFER.
C
C    OUTPUT PARAMETERS
C      ERROR  - ERROR INDICATOR, =    0 IF NO ERRORS.
C                                =   -1 IF EOF
C                                = -105 IF OPEN ERROR
C                                =  302 IF READ ERROR
C                                =  303 IF WRITE ERROR
C
C
C  SUBROUTINE ARGUMENTS.
C
      INTEGER  OP, UNIT, LENGTH, BUFFER(LENGTH), ERROR
      CHARACTER*(*) FNAME
C
C  COMMON BLOCKS.
C
      COMMON  /G01IO/   MIOFLG  ,MRECNM ,MPXYSZ ,MPXPY(256)     ,
     +                  MOBFSZ  ,MOUTBF(720)    ,MBFPOS ,
     +                  MFGLUN  ,MXBITS         ,MDTYPE ,
     +                  MNFFLG  ,MBMFLG ,MEMFLG
        INTEGER         MIOFLG  ,MRECNM ,MPXYSZ ,MPXPY  ,MOBFSZ ,
     +                  MBFPOS  ,MFGLUN ,MOUTBF ,MXBITS ,MDTYPE ,
     +                  MNFFLG  ,MBMFLG ,MEMFLG
      COMMON  /G01CHA/  MFNAME  ,MPNAME
      CHARACTER*80      MFNAME  ,MPNAME
C
C  LOCAL VARIABLES.
C
      INTEGER IAUNT
C
C
      ERROR = 0

      IAUNT = IABS(UNIT)

C Open the workstation with filename FNAME

      IF (OP.EQ.1) THEN
	CALL OPNWKS(IAUNT, FNAME, ERROR)

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

      ENDIF

      END

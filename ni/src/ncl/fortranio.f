	SUBROUTINE NCL_FORTRANREAD(PPATH,ARR,N,ERR)
	CHARACTER* (*)  PPATH
	CHARACTER ARR(N)
	INTEGER ERR

	OPEN(STATUS='old',FILE=PPATH ,IOSTAT=ios,form='unformatted',
     + ERR=10)
	INQUIRE (FILE=PPATH,NUMBER=IU,RECL=IR)
	READ(IU,ERR=20) ARR
	CLOSE(IU)

	ERR = -1
	RETURN
 10	ERR = -4
	CALL NHLFPERROR('FATAL',1000,'fbinread: An error occured '
     +  //'opening the requested file')
	RETURN
 20	ERR = -4
	CALL NHLFPERROR('FATAL',1000,'fbinread: An error occured while '
     +  //'reading the requested file check dimension size information')
	RETURN
	END

	SUBROUTINE NCL_FORTRANWRITE(PPATH,ARR,N,ERR)
	CHARACTER* (*) PPATH
	CHARACTER ARR(N)
	INTEGER ERR

	OPEN(STATUS='new',FILE=PPATH,IOSTAT=ios,FORM='unformatted',
     +  ERR=30)
	INQUIRE (FILE=PPATH,NUMBER=IU,RECL=IR)
	WRITE(IU,ERR=40) ARR
	CLOSE(IU)

	ERR = -1
	RETURN
 30	ERR = -4
	CALL NHLFPERROR('FATAL',1000,'fbinread: An error occured '
     +  //'opening the requested file')
	RETURN
 40	ERR = -4
	CALL NHLFPERROR('FATAL',1000,'fbinread: An error occured '
     +  //'while writing the requested file')
	RETURN
	END

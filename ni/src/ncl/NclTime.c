/************************************************************************
*									*
*			     Copyright (C)  1994			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
#include <sys/resource.h>
#include <sys/time.h>
#ifdef USE_MPI
#include "mpi.h"
#endif
#ifdef NIO_LIB_ONLY
#include "niohlu.h"
#include "nioNresDB.h"
#else
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#endif
#include "defs.h"
#include "NclDataDefs.h"
#include "NclFileInterfaces.h"
#include "NclVar.h"
#include "VarSupport.h"
#include "NclData.h"

NhlErrorTypes NclGetCPUTime(float *time)
{
	struct rusage usage;
	int status;

	*time = 0.0;
	status = getrusage(RUSAGE_SELF, &usage);
	if (status) {
		NhlPError(NhlWARNING, NhlEUNKNOWN,
			"unable to get process resourse usage info: %d", status);
		return(NhlWARNING);
	}

	*time = (usage.ru_stime.tv_sec + usage.ru_utime.tv_sec) +
					(usage.ru_stime.tv_usec + usage.ru_utime.tv_usec) / 1000000.;

	return NhlNOERROR;	
}

#ifdef USE_MPI
NhlErrorTypes NclGetWTime(double *time)
{
	*time = MPI_Wtime();
	return NhlNOERROR;
}
#else
NhlErrorTypes NclGetWTime(double *time)
{
	struct timeval wtime;
	int status;

	*time = 0.0;
	status = gettimeofday(&wtime, NULL);
	if(status == 0) {
		*time = wtime.tv_sec + wtime.tv_usec / 1000000.0 ;	
		return NhlNOERROR;	
	}
	else {
		NhlPError(NhlWARNING, NhlEUNKNOWN,
					"Unable to get time, gettimeofday() failed: %d", status);
		return NhlWARNING;
	}
}
#endif

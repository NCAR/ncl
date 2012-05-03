/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
 * Development of this software was sponsored by the Office of Biological  *
 * and Environmental Research of the U.S. Department of Energy's Office    *
 * of Science.                                                             *
 *                                                                         *
 * Copyright 2011 UChicago Argonne, LLC.                                   *
 *                                                                         *
 * Licensed under the Apache License, Version 2.0 (the "License"); you     *
 * may not use this file except in compliance with the License.  You may   *
 * obtain a copy of the License at                                         * 
 *                                                                         *
 *   http://www.apache.org/licenses/LICENSE-2.0                            *
 *                                                                         *
 * Unless required by applicable law or agreed to in writing, software     *
 * distributed under the License is distributed on an "AS IS" BASIS,       *
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or         *
 * implied.  See the License for the specific language governing           *
 * permissions and limitations under the License.                          *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include <stdio.h>
/* FIXME: Get rid of unused headers */
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
#if defined(USE_MPI)
#include "mpi.h"
#endif

#define PROF_ENV_NAME	"NCARG_PROF"
#define MAX_FILENAME_LEN 100
#define MAX_PROFINFO_FNAMES_CHUNK 32

struct _NclProfInfo{
	int enable_print;
	FILE *ostr;
	double stime;
	NclQuark *fnamesq;
	int fnamesq_idx;
	int fnamesq_len;
} NclProfInfo;

char *get_prof_logname(char *input_filename, char *output_filename, int olen)
{
	snprintf(output_filename, olen, "%s.prof.log", input_filename);
	return output_filename;
}

void NclProfInit(char *filename)
{
	char prof_logname[MAX_FILENAME_LEN];
	char *prof_env = NULL;
#if defined(USE_MPI)
	int rank=-1, retval = MPI_SUCCESS;
#endif
	/* FIXME: Add code here to check env 
	 * - specify file to write output
	 * - print from root/all_procs
	 * ...etc */

	NclProfInfo.fnamesq_idx = 0;
	NclProfInfo.fnamesq_len = 0;
	NclProfInfo.fnamesq = NULL;
	NclProfInfo.stime = 0.0;

	if(prof_env = getenv(PROF_ENV_NAME)){
		int prof_sw = 0;
		prof_sw = atoi(prof_env);
		if(prof_sw){
			NclProfInfo.enable_print = 1;
		}
		else{
			NclProfInfo.enable_print = 0;
		}
	}
	else{
		NclProfInfo.enable_print = 1;
	}

	if(!NclProfInfo.enable_print) return;

	get_prof_logname(filename, prof_logname, MAX_FILENAME_LEN);
#if defined(USE_MPI)
	retval = MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	if((retval == MPI_SUCCESS) && (rank == 0)){
		NclProfInfo.ostr = fopen(prof_logname, "w");
		if(NclProfInfo.ostr == NULL){
			NclProfInfo.ostr = stdout;
		}
	}
	else{
		NclProfInfo.enable_print = 0;
	}
#else
	NclProfInfo.ostr = fopen(prof_logname, "w");
	if(NclProfInfo.ostr == NULL){
		NclProfInfo.ostr = stdout;
	}
#endif
	if(NclProfInfo.enable_print){
		NclGetWTime(&(NclProfInfo.stime));
		fprintf(NclProfInfo.ostr, "\\FL_EN %d\n", (int )NrmStringToQuark(filename));
	}
	return;
}

void NclProfFinalize(void )
{
	if(NclProfInfo.enable_print){
		int i;
		fprintf(NclProfInfo.ostr, "/FL_EX\n");
		fprintf(NclProfInfo.ostr, "\\MDATA\n");
		for(i=0; i<NclProfInfo.fnamesq_idx; i++){
			fprintf(NclProfInfo.ostr, "FL_NM %d %s\n", (int ) NclProfInfo.fnamesq[i], NrmQuarkToString(NclProfInfo.fnamesq[i]));
		}
		fprintf(NclProfInfo.ostr, "/MDATA\n");
		if(NclProfInfo.ostr != stdout){
			fclose(NclProfInfo.ostr);
		}
	}
}

void NclProfPFEnter(char *funcname)
{
	double time=0.0;

	if(!NclProfInfo.enable_print) return;

	/*
	 * Ignore the return val here - Worst case we just have an
	 * uninitialized value of time
	 */
	NclGetWTime(&time);
	
	fprintf(NclProfInfo.ostr, "\\F_EN %s (%.10f)\n", funcname, time - NclProfInfo.stime);
}

void NclProfPFExit(char *funcname)
{
	double time=0.0;

	if(!NclProfInfo.enable_print) return;

	/*
	 * Ignore the return val here - Worst case we just have an
	 * uninitialized value of time
	 */
	NclGetWTime(&time);

	fprintf(NclProfInfo.ostr, "/F_EX %s (%.10f)\n", funcname, time - NclProfInfo.stime);
}

void NclProfLEnter(char *filename, int line)
{
	double time=0.0;

	if(!NclProfInfo.enable_print) return;

	/*
	 * Ignore the return val here - Worst case we just have an
	 * uninitialized value of time
	 */
	NclGetWTime(&time);
	
	fprintf(NclProfInfo.ostr, "\\L_EN %d %d (%.10f)\n", line, (int )NrmStringToQuark(filename), time - NclProfInfo.stime);
}

void NclProfLExit(char *filename, int line)
{
	double time=0.0;

	if(!NclProfInfo.enable_print) return;

	/*
	 * Ignore the return val here - Worst case we just have an
	 * uninitialized value of time
	 */
	NclGetWTime(&time);

	fprintf(NclProfInfo.ostr, "/L_EX %d %d (%.10f)\n", line, (int )NrmStringToQuark(filename), time - NclProfInfo.stime);
}

/* The only type registered now is file name infos */
void NclProfRegisterMData(int type, const char *str)
{
	if(!NclProfInfo.enable_print) return;
	switch(type){
		case 0:
			if(NclProfInfo.fnamesq_idx >= NclProfInfo.fnamesq_len){
				/* realloc mem */
				NclProfInfo.fnamesq_len += MAX_PROFINFO_FNAMES_CHUNK;
				NclProfInfo.fnamesq = (NclQuark *)NclRealloc(NclProfInfo.fnamesq, sizeof(NclQuark) * NclProfInfo.fnamesq_len);
			}
			NclProfInfo.fnamesq[NclProfInfo.fnamesq_idx++] = NrmStringToQuark(str);
			
			break;
		default:
			NhlPError(NhlWARNING, NhlEUNKNOWN, "Unknown meta data type");
	}
}

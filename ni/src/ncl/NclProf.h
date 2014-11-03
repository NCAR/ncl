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

#ifndef NclProf_h
#define NclProf_h

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

#ifdef ENABLE_PROFILING

void NclProfInit(char *filename);
void NclProfFinalize(void );

/* Specialized prof enter/exit funcs */
void NclProfPFEnter(char *funcname);
void NclProfPFExit(char *funcname);
void NclProfLEnter(char *filename, int line);
void NclProfLExit(char *filename, int line);
void NclProfRegisterMData(int type, char *str);

/* FIXME: There should be a generic version too...
 * void NclProfEnter(...);
 * void NclProfExit(...);
 */

/*
NhlErrorTypes NclGetWTime(float *time);
*/

#define NCL_PROF_INIT(filename) NclProfInit(filename)
#define NCL_PROF_FINALIZE() NclProfFinalize()
#define NCL_PROF_PFENTER(funcname) NclProfPFEnter(funcname)
#define NCL_PROF_PFEXIT(funcname) NclProfPFExit(funcname)
#define NCL_PROF_LENTER(filename, line) NclProfLEnter(filename, line)
#define NCL_PROF_LEXIT(filename, line) NclProfLExit(filename, line)
#define NCL_PROF_REGISTER_MDATA(type, str) NclProfRegisterMData(type, str)

#else

	#define NCL_PROF_INIT(filename)
	#define NCL_PROF_FINALIZE()
	#define NCL_PROF_PFENTER(funcname)
	#define NCL_PROF_PFEXIT(funcname)
	#define NCL_PROF_LENTER(filename, line)
	#define NCL_PROF_LEXIT(filename, line)
	#define NCL_PROF_REGISTER_MDATA(type, str)

#endif


#endif

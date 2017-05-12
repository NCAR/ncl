/*
 * 
 * 
 */

#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>
#include <ncarg/hlu/Error.h>
#include <ncarg/hlu/NresDB.h>
#include "defs.h"
#include "NclDataDefs.h"
#include "NclBuiltInSupport.h"
#include "NclMultiDValData.h"
#include "NclVar.h"
#include "Symbol.h"
#include "Machine.h"
#include "VarSupport.h"

/*
 * It is not expected that there will be large numbers of concurrent processes at any one time, so 
 * we'll use a simple table scheme to keep track of them (i.e, sequential lookup; holes denoted by pid=-1).
 */
typedef struct SubprocEntry {
    int  pid;
} SubprocEntry;

static SubprocEntry *subprocTab;
static const int SUBPROC_TAB_GROW = 10;
static int subprocTabSize = 0;

/* forward references */
static void addSubProc(int pid);
static void removeSubProc(int pid);

/*
 * _Nclsubprocess()
 * 
 * Implements the "subprocess" built-in command.
 * 
 */
NhlErrorTypes _Nclsubprocess(void)
{
    NclStackEntry arg = _NclGetArg(0, 1, DONT_CARE);
    
    /*
     * Should be constrained to be a SCALAR MDVal
     */
    NclMultiDValData cmdMD = NULL;
    switch(arg.kind) {
        case NclStk_VAL:
            cmdMD = arg.u.data_obj;
            break;
        case NclStk_VAR:
                cmdMD = _NclVarValueRead(arg.u.data_var, NULL, NULL);
                break;
        default:
                return(NhlFATAL);
    }
    
    if((cmdMD != NULL) && (cmdMD->multidval.type->type_class.type & Ncl_Typestring)) {
	int pid = fork();
	if (pid < 0) {
            NhlPError(NhlFATAL, NhlEUNKNOWN, "subprocess: cannot create child process");
            return(NhlFATAL);
	}
		
        if(pid == 0) {
            char * command = NrmQuarkToString(*(NclQuark*)cmdMD->multidval.val); 
            /*
             * Note: the child should use _exit() rather than exit() to avoid calling the
             * registered atexit() functions prematurely
             */
	    int status = execl("/bin/sh", "sh", "-c", command, NULL);
            NhlPError(NhlFATAL, NhlEUNKNOWN, "subprocess: failed to execute command: %s", command);
            _exit(status);
        }
  
        else {
            addSubProc(pid);
            ng_size_t dimSizes = 1;
            NclReturnValue((void*)&pid, 1, &dimSizes, (NclScalar*)NULL, NCL_int, 1);
        }
    }
    
    return(NhlNOERROR);
}

/*
 * _NclsubprocessWait()
 * 
 * Implements the "subprocess_wait" built-in command.
 * 
 */
NhlErrorTypes _NclsubprocessWait(void)
{

    NclMultiDValData pidMD = NULL;
    NclStackEntry arg = _NclGetArg(0, 2, DONT_CARE);    
    switch(arg.kind) {
        case NclStk_VAL:
            pidMD = arg.u.data_obj;
            break;
        case NclStk_VAR:
                pidMD = _NclVarValueRead(arg.u.data_var, NULL, NULL);
                break;
        default:
                return(NhlFATAL);
    }

    NclMultiDValData isBlockingMD = NULL;
    arg = _NclGetArg(1, 2, DONT_CARE);    
    switch(arg.kind) {
        case NclStk_VAL:
            isBlockingMD = arg.u.data_obj;
            break;
        case NclStk_VAR:
                isBlockingMD = _NclVarValueRead(arg.u.data_var, NULL, NULL);
                break;
        default:
                return(NhlFATAL);
    }
    
    if((pidMD != NULL) && (pidMD->multidval.type->type_class.type & Ncl_Typeint) &&
            isBlockingMD != NULL && (isBlockingMD->multidval.type->type_class.type & Ncl_Typelogical)) 
    {       
        int status;
        int thePid = *(int*)pidMD->multidval.val;
        thePid = (thePid > 0) ? thePid : -1;
        int waitOptions = (*(logical*)isBlockingMD->multidval.val) ? 0 : WNOHANG;
        pid_t pid = waitpid(thePid, &status, waitOptions);
        if (pid < 0) {
            NhlPError(NhlWARNING, NhlEUNKNOWN, "subprocess_wait: unknown child process with id=%d", thePid);                
        }
        else if (pid != 0) {
            removeSubProc(pid);
            if (WIFEXITED(status)) {
                status = WEXITSTATUS(status);
                if (status) 
                    NhlPError(NhlWARNING, NhlEUNKNOWN, "subprocess_wait: process %d existed with non-zero status=%d", 
                            pid, status);
            }
            else if (WIFSIGNALED(status)) {
                NhlPError(NhlWARNING, NhlEUNKNOWN, "subprocess_wait: process %d terminated on signal=%d", 
                            pid, WTERMSIG(status));
            }
            else if (WIFSTOPPED(status)) {
                NhlPError(NhlWARNING, NhlEUNKNOWN, "subprocess_wait: process %d stopped, signal=%d", 
                            pid, WSTOPSIG(status));
            }                    
        }
        
        ng_size_t numDims = 1;
        NclReturnValue((void*)&pid, 1, &numDims, (NclScalar*)NULL, NCL_int, 1);
    }
}

/*
 * addSubProc()
 * 
 * A helper function that manages additions to and dynamic resizing of the process table.
 * 
 */
static void addSubProc(int pid) {
   
    /* look for a free entry in the process table... */
    int index = -1;
    for (int i=0; i<subprocTabSize; i++) {
        if (subprocTab[i].pid < 0) {
            index = i;
            break;
        }
    }
    
    if (index < 0) {  
        /* need to grow the table... */
        int newTableSize = subprocTabSize*sizeof(SubprocEntry) + SUBPROC_TAB_GROW*sizeof(SubprocEntry);
        SubprocEntry *newTable = (SubprocEntry*) realloc(subprocTab, newTableSize);
        if (!newTable) {
            NhlPError(NhlFATAL, NhlEUNKNOWN, "subprocess: failed to allocate process table of size %d", newTableSize);                
            return;
        }
        subprocTab = newTable;
        for (int i=subprocTabSize; i < (subprocTabSize + SUBPROC_TAB_GROW); i++) {
            subprocTab[i].pid = -1;
        }
        index = subprocTabSize;
        subprocTabSize += SUBPROC_TAB_GROW;
    }
    
    subprocTab[index].pid = pid;    
    return;
}
   
/*
 * removeSubProc()
 * 
 * A helper function that handles removing process IDs from the process table.
 * 
 */
static void removeSubProc(int pid) {
    for (int i=0; i<subprocTabSize; i++) {
        if (subprocTab[i].pid == pid) {
            subprocTab[i].pid = -1;
            return;
        }
    }
    
    NhlPError(NhlWARNING, NhlEUNKNOWN, "subprocess_wait: unable to find process %d in process table", pid);                
}

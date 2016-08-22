/* THIS WRAPPER IS NOT DONE! I'M CHECKING IT IN BEFORE THE SVN TO GIT TRANSITION */

#include <stdio.h>
#include "wrapper.h"
#include <eemd.h>

extern size_t emd_num_imfs(size_t);

NhlErrorTypes emd_num_imfs_W( void )
{
/*
 * Input variable
 */
  ng_size_t *nt;
  NclBasicDataTypes type_nt;

/*
 * Return variable
 */
  void *Mret;
  ng_size_t dsizes_M[1];
  NclBasicDataTypes type_M;

/*
 * Get argument # 0
 */
  nt = (ng_size_t*)NclGetArgValue(
           0,
           1,
           NULL,
           NULL,
           NULL,
           NULL,
	   &type_nt,
           DONT_CARE);

  if(type_nt != NCL_int && type_nt != NCL_long) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"emd_num_imfs: nt must be an integer or a long");
    return(NhlFATAL);
  }
/* 
 * Allocate space for output array.
 */
#if defined(NG32BIT)
  Mret = (void*)calloc(1, sizeof(int));
#else
  Mret = (void*)calloc(1, sizeof(long));
#endif
  if(Mret == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"emd_num_imfs: Unable to allocate memory for return value");
    return(NhlFATAL);
  }

#if defined(NG32BIT)
  type_M = NCL_int;
  ((int*)Mret)[0] = (int)emd_num_imfs((size_t)(*nt));
#else
  type_M = NCL_long;
  ((long*)Mret)[0] = (long)emd_num_imfs((size_t)(*nt));
#endif
  dsizes_M[0] = 1;
  return(NclReturnValue(Mret,1,dsizes_M,NULL,type_M,0));
}

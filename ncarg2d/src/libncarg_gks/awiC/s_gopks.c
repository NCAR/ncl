/*
 *	$Id: s_gopks.c,v 1.1 1997-03-05 19:12:51 haley Exp $
 */
/*
 *  Open GKS
 */

#include <ncarg/gks.h>

void gopen_gks
#ifdef NeedFuncProto
(
    const char   *err_file,   /* name of error file                */
    size_t        mem_unit    /* size_t units of memory available
                                 for buffer space                  */
)
#else
(err_file,mem_unit)
    char   *err_file;
    Gint   mem_unit;
#endif
{
    int unit;
/*
 *  If the err_file is "stderr" then pass unit 6 (which is  stdout in 
 *  Fortran) to "gopks".
 *
 *  Otherwise, get the file descriptor from the list and pass that to 
 *  "gopks".
 *
 */
    if( err_file ) {
        if( !strcmp( err_file, "stderr" ) || !strcmp( err_file, "stdout" ) ) {
           unit = 6;       
        }
        else {
            get_conn_id(&unit);
        }
    }
    else {
        get_conn_id(&unit);
	}
    NGCALLF(gopks,GOPKS)(&unit,&mem_unit);
}

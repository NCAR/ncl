#include <stdio.h>
#include "wrapper.h"

extern short NCLecho;
extern short load_stmt;
extern char *cur_load_file;

short   echo_val = 0;

NhlErrorTypes echo_on_W(void)
{
    echo_val = NCLecho;
    NCLecho = 1;
    load_stmt = 0;
    if (cur_load_file != (char *) NULL)
        (void) nclfprintf(stdout, "+ %s : %s\n", "echo_on()", cur_load_file);
    else
        (void) nclfprintf(stdout, "+ %s\n", "echo_on()");
    return(NhlNOERROR);
}

NhlErrorTypes echo_off_W(void)
{
    if (NCLecho) {
        NCLecho = echo_val;
        load_stmt = 1;
    }
    echo_val = 0;
    return(NhlNOERROR);
}

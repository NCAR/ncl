/*
 *  $Id: s_gopwk.c,v 1.7 2008-07-23 17:24:20 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/*
 *  Open workstation  
 */

#include <ncarg/gks.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/* this is the length of each row of the data	*/

#define MAX_ARRAY_LEN	80

extern void NGCALLF(gopwk,GOPWK)(Gint*,int*,Gint*);
extern void NGCALLF(gesc,GESC)(Gint*,int*,char*,int*,int*,
                               char out_array[MAX_ARRAY_LEN],int,int);
extern void NGCALLF(gerhnd,GERHND)(Gint*,Gint*,int*);

extern void get_conn_id(int*);
extern void add_conn_id(int*);

#define MAXSTRLEN 80

/* 
 * Fortran common block GKEROR
 */
#if defined(cray)
struct common {
#else
extern struct common {
#endif
    int ers, erf, cuflag, xermsg[160], mxermg;
} NGCALLC(gkeror,GKEROR);

void gopen_ws
#ifdef NeedFuncProto
(
    Gint        ws_id,      /* workstation identifier  */
    const char  *conn_id,   /* connection identifier   */
    Gint        ws_type     /* workstation type        */
)
#else
(ws_id,conn_id,ws_type)
    Gint  ws_id;
    char  *conn_id;
    Gint  ws_type;
#endif
{
    int i, iconn_id, lidr, mlodr, lodr, len;
    int error_num, fctid;
    char stmp[81];
    NGstring stmp2;
    char dum_array[MAX_ARRAY_LEN];
    if( ws_type == 1 ) {
/*
 *  If conn_id is non-NULL, then use it as the metafile name.
 *
 *  If the connection identifier is a non-NULL string, then call GESC
 *  with a function id of -1391 to establish the metafile name.
 */
        if( conn_id != NULL ) {
            if( strlen( conn_id ) < MAXSTRLEN ) {
                strcpy( stmp, conn_id );
                fctid = -1391;
                lidr = 1;
                mlodr = 0;
                for( i = strlen(stmp); i < MAXSTRLEN; i++ ) stmp[i] = ' ';
                stmp[MAXSTRLEN] = '\0';
                len = MAXSTRLEN;
                stmp2 = NGCstrToFstr(stmp,len);
                NGCALLF(gesc,GESC)(&fctid,&lidr,stmp2,&mlodr,&lodr,dum_array,
				   len,MAX_ARRAY_LEN);
            }
/*
 * metafiles can only be of length <= 80 for now.  In the future
 * will accept standard unix file names of up to length 256.
 */
            else {
                error_num = 163;
                fctid = 2;
                NGCALLC(gkeror,GKEROR).ers = 1;
                NGCALLF(gerhnd,GERHND)( &error_num, &fctid, 
                                        &NGCALLC(gkeror,GKEROR).erf );
                NGCALLC(gkeror,GKEROR).ers = 0;
                return;
            }
        }
        get_conn_id(&iconn_id);
        NGCALLF(gopwk,GOPWK)(&ws_id,&iconn_id,&ws_type);
	}
	else if( ws_type == 3 ) {
        get_conn_id(&iconn_id);
        NGCALLF(gopwk,GOPWK)(&ws_id,&iconn_id,&ws_type);
	}
	else if( ws_type == 7 ) {
/*
 *  if conn_id is non-NULL, then it better be an integer
 *  representing a connection identifier
 */
        error_num = 0;
        if( conn_id != NULL ) {
            for( i = 0; i < strlen( conn_id ); i++ ) {
                if( !isdigit( conn_id[i] ) ) {
                    error_num = 21;
                    fctid = 2;
                    NGCALLC(gkeror,GKEROR).ers = 1;
                    NGCALLF(gerhnd,GERHND)( &error_num, &fctid, 
                                            &NGCALLC(gkeror,GKEROR).erf );
                    NGCALLC(gkeror,GKEROR).ers = 0;
                    return;
                }
            }
            iconn_id = atoi( conn_id );
            add_conn_id( &iconn_id );
            NGCALLF(gopwk,GOPWK)(&ws_id,&iconn_id,&ws_type);
        }
        else {
            iconn_id = 0;
            NGCALLF(gopwk,GOPWK)(&ws_id,&iconn_id,&ws_type);
        }
	}
/*
 * Everybody else ends up here.
 */
	else {
	  iconn_id = 0;
	  NGCALLF(gopwk,GOPWK)(&ws_id,&iconn_id,&ws_type);
	}
}

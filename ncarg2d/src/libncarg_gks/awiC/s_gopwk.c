/*
 *  $Id: s_gopwk.c,v 1.1 1997-03-05 19:12:51 haley Exp $
 */
/*
 *  Open workstation  
 */

#include <ncarg/gks.h>

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
} NGCALLF(gkeror,GKEROR);

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
                len = NGSTRLEN(stmp);
                stmp2 = NGCstrToFstr(stmp,len);
                NGCALLF(gesc,GESC)(&fctid,&lidr,stmp2,&mlodr,&lodr,NULL,len);
            }
/*
 * metafiles can only be of length <= 80 for now.  In the future
 * will accept standard unix file names of up to length 256.
 */
            else {
                error_num = 163;
                fctid = 2;
                NGCALLF(gkeror,GKEROR).ers = 1;
                NGCALLF(gerhnd,GERHND)( &error_num, &fctid, 
                                        &NGCALLF(gkeror,GKEROR).erf );
                NGCALLF(gkeror,GKEROR).ers = 0;
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
                    NGCALLF(gkeror,GKEROR).ers = 1;
                    NGCALLF(gerhnd,GERHND)( &error_num, &fctid, 
                                            &NGCALLF(gkeror,GKEROR).erf );
                    NGCALLF(gkeror,GKEROR).ers = 0;
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
	else if( ws_type == 8 || ws_type == 10 || 
           ( ws_type >= 20 && ws_type <= 31) ) {
        iconn_id = 0;
        NGCALLF(gopwk,GOPWK)(&ws_id,&iconn_id,&ws_type);
	}
}

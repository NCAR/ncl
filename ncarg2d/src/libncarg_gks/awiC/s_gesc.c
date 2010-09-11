/*
 *	$Id: s_gesc.c,v 1.7 2008-07-23 17:24:19 haley Exp $
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
 *  Escape  
 */

#include <stdlib.h>
#include <sys/types.h>
#include <string.h>
#include <ncarg/gks.h>
#include <ncarg/gksP.h>
#include "gstore.h"

/* This values will change when fred allows filename's longer that 80 char's */
#define MAX_FILE_LEN    256

/* this is the length of each row of the data	*/

#define MAX_ARRAY_LEN	80

extern void NGCALLF(gesc,GESC)(Gint*,int*,char*,int*,int*,
                               char out_array[MAX_ARRAY_LEN],int,int);

extern void NGCALLF(gerhnd,GERHND)(Gint*,Gint*,int*);

#if defined(cray)
struct common {
#else
extern struct common {
#endif
    int ers, erf, cuflag, xermsg[160], mxermg;
} NGCALLC(gkeror,GKEROR);

void gescape
#ifdef NeedFuncProto
(
    Gint                   func_id,      /* escape function identifier    */
    const Gescape_in_data  *in_data,     /* escape input data record      */
    Gstore                 *store_data,  /* storage for output data       */
    Gescape_out_data       **out_data    /* OUT escape output data record */
)
#else
( func_id, in_data, store_data, out_data )
    Gint             func_id;
    Gescape_in_data  *in_data;
    Gstore           *store_data;
    Gescape_out_data **out_data;
#endif
{
	int	i, isize, error_num, eesc;

    /*
     * if only using a one dim array, then use the stack_array instead of
     * dynamic memory.
     */
	int	in_array_len;
	char	stack_in_array[MAX_ARRAY_LEN];
	char	*in_array;
    /*
     * Out array is currently only being used for -1390, returns < 80 chars
     */
	int	mx_out_array_len = 1;
	int	out_array_len = 1;
	char	out_array[MAX_ARRAY_LEN];

#if defined(cray)
	_fcd	in_cftstr;
	_fcd	out_cftstr;
#endif

	if(func_id == -1450){
		/*
		 * Private C "native" interface.  Totally hacked args.
		 * Only uses in_data:
		 * Uses data for the ptr to set in the gksc
		 */
		if(!in_data || !in_data->escape_r1.data){
			error_num = 182;
			eesc = 11;
			NGCALLC(gkeror,GKEROR).ers = 1;
			NGCALLF(gerhnd,GERHND)( &error_num, &eesc, &NGCALLC(gkeror,GKEROR).erf );
			NGCALLC(gkeror,GKEROR).ers = 0;
			return;
		}
		error_num = _NGCescape(func_id,
					(_NGCesc*)in_data->escape_r1.data);
		if(error_num != 0){
			eesc = 11;
			NGCALLC(gkeror,GKEROR).ers = 1;
			NGCALLF(gerhnd,GERHND)( &error_num, &eesc, &NGCALLC(gkeror,GKEROR).erf );
			NGCALLC(gkeror,GKEROR).ers = 0;
		}
		return;
	}

	if((in_data == NULL) || (in_data->escape_r1.size == 0)){
		/*
		 * The in_data is not used so just pass allocated space
		 * Unfortunately we need to pad it with spaces.
		 */
		in_array_len = 1;
		in_array = stack_in_array;
		for(i=0;i<MAX_ARRAY_LEN;i++) in_array[i] = ' ';
	}
	else{
		/*
		 * Get the size of the input data record
		 */
		isize = in_data->escape_r1.size;

		/*
		 * For func_id == -1391, only accept metafiles of length
		 * MAX_FILE_LEN or less.  In the future, will accept standard
		 * unix file names of up to length 256.
		 */
		if(func_id == -1391){
	       
			if( isize > MAX_FILE_LEN ) {
				error_num = 182;
				eesc = 11;
				NGCALLC(gkeror,GKEROR).ers = 1;
				NGCALLF(gerhnd,GERHND)( &error_num, &eesc, &NGCALLC(gkeror,GKEROR).erf );
				NGCALLC(gkeror,GKEROR).ers = 0;
				return;
			}
	      	}

		if(isize > MAX_ARRAY_LEN){
			/*
			 * Need to make sure array size is a multiple of
			 * MAX_ARRAY_LEN
			 */
			if((isize % MAX_ARRAY_LEN) == 0){
				in_array_len = isize / MAX_ARRAY_LEN;
			}
			else{
				in_array_len = (isize/MAX_ARRAY_LEN) + 1;
			}
			in_array = (char *)malloc(in_array_len * MAX_ARRAY_LEN
								* sizeof(char));
			(void)memcpy(in_array,in_data->escape_r1.data,isize);
		}
		else{
			in_array = stack_in_array;
			in_array_len = 1;
			(void)memcpy(in_array,in_data->escape_r1.data,isize);
		}

		/*
		 * pad the remainder of the array with spaces
		 */
		for(i=isize;i < in_array_len * MAX_ARRAY_LEN;i++)
			in_array[i] = ' ';
	}

	for(i=0;i < mx_out_array_len * MAX_ARRAY_LEN;i++)
		out_array[i] = ' ';

#if defined(cray)
	in_cftstr = _cptofcd(in_array,MAX_ARRAY_LEN);
	out_cftstr = _cptofcd(out_array,MAX_ARRAY_LEN);

	NGCALLF(gesc,GESC)(&func_id,&in_array_len,in_cftstr,&mx_out_array_len,
                       &out_array_len,out_cftstr);
#else
	NGCALLF(gesc,GESC)(&func_id,&in_array_len,in_array,&mx_out_array_len,
                       &out_array_len,out_array,MAX_ARRAY_LEN,MAX_ARRAY_LEN);
#endif

	if(func_id == -1390){
		char	*tchptr = NULL;

		if(	(out_array_len != 1) ||
			!store_data ||
			_gstore_clear(*store_data)){

			error_num = 2204;
			eesc = 11;
			NGCALLC(gkeror,GKEROR).ers = 1;
			NGCALLF(gerhnd,GERHND)( &error_num, &eesc, &NGCALLC(gkeror,GKEROR).erf );
			NGCALLC(gkeror,GKEROR).ers = 0;
			return;
		}

		/*
		 * after this loop i = position of last charactor.
		 */
		for(i=out_array_len*MAX_ARRAY_LEN-1;i >= 0; i--){
			if(out_array[i] != ' ')
				break;
		}

		if(i==0){
			*out_data = NULL;
			return;
		}

		*out_data = _gstore_alloc(*store_data,sizeof(Gescape_out_data));
		if(!*out_data){
			error_num = 2204;
			eesc = 11;
			NGCALLC(gkeror,GKEROR).ers = 1;
			NGCALLF(gerhnd,GERHND)( &error_num, &eesc, &NGCALLC(gkeror,GKEROR).erf );
			NGCALLC(gkeror,GKEROR).ers = 0;
			return;
		}

		tchptr = _gstore_alloc(*store_data,sizeof(char)*(i+2));
		if(!tchptr){
			error_num = 2204;
			eesc = 11;
			NGCALLC(gkeror,GKEROR).ers = 1;
			NGCALLF(gerhnd,GERHND)( &error_num, &eesc, &NGCALLC(gkeror,GKEROR).erf );
			NGCALLC(gkeror,GKEROR).ers = 0;
			return;
		}

		strncpy(tchptr,out_array,i+1);
		tchptr[i+1] = '\0';
		(*out_data)->escape_u1390.data = tchptr;
		(*out_data)->escape_u1390.size = i + 2;
	}

	if(in_array != stack_in_array){
		free(in_array);
	}

	return;
}

/*
 *      $Id$
 */
/************************************************************************
*									*
*			     Copyright (C)  1994			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Jul 13 10:15:21 MDT 1994
 *
 *	Description:	
 */

#ifdef NIO_LIB_ONLY
#include "niohlu.h"
#include "nioNresDB.h"
#else
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#endif
#include "defs.h"
#define HAVE_NETCDF
#include <mfhdf.h>
#include "NclData.h"
#include "NclFileInterfaces.h"
#include <math.h>
#include <ctype.h>

#include "NclHDF.h"

static NrmQuark Qmissing_val;
static NrmQuark Qfill_val;

#if 0
static void HDFGetAttrVal
#if	NhlNeedProto
(int ncid,HDFAttInqRec* att_inq)
#else
(ncid,att_inq)
int ncid,
HDFAttInqRec* att_inq
#endif
{
	char *tmp;
	int ret;

	if (att_inq->data_type < 1) {
		att_inq->value = NULL;
	}
	else if(att_inq->data_type == NC_CHAR && !(att_inq->name == Qfill_val || att_inq->name == Qmissing_val)) {
		tmp = (char*)NclMalloc(att_inq->len+1);
		tmp[att_inq->len] = '\0';
		ret = sd_ncattget(ncid,att_inq->varid,NrmQuarkToString(att_inq->name),tmp);
		att_inq->value = NclMalloc(sizeof(NclQuark));
		*(NclQuark *)att_inq->value = NrmStringToQuark(tmp);
		NclFree(tmp);
	} 
	else {
		att_inq->value = NclMalloc(sd_nctypelen(att_inq->data_type)*att_inq->len);
		ret = sd_ncattget(ncid,att_inq->varid,NrmQuarkToString(att_inq->name),att_inq->value);
	}
}
#endif

static void HDF_SDGetAttrVal
#if	NhlNeedProto
(int sd_id,HDFAttInqRec* att_inq)
#else
(sd_id,att_inq)
int sdid,
HDFAttInqRec* att_inq
#endif
{
	char *tmp;
	int ret;

	if (att_inq->data_type < 1) {
		att_inq->value = NULL;
	}
	else if(att_inq->data_type == NC_CHAR) {
		int llen = att_inq->len - 1;
		tmp = (char*)NclMalloc(att_inq->len+2);
		memset(tmp,0,att_inq->len+2);
		ret = SDreadattr(sd_id,att_inq->attr_ix,tmp);
		att_inq->value = NclMalloc(sizeof(NclQuark));
		while (llen > 1 && (tmp[llen] == '\0' || isspace(tmp[llen]) || ! isprint(tmp[llen]))) {
			llen--;
		}
		while (strlen(tmp) < llen) {
			tmp[strlen(tmp)] = ' ';
		}
		tmp[MIN(att_inq->len,llen+1)] = '\0';
		*(NclQuark *)att_inq->value = NrmStringToQuark(tmp);
		NclFree(tmp);
	} 
	else {
		att_inq->value = NclMalloc(sd_nctypelen(att_inq->data_type)*att_inq->len);
		ret = SDreadattr(sd_id,att_inq->attr_ix,att_inq->value);
		
	}
}


static void HDFCacheAttValue
#if	NhlNeedProto
(HDFAttInqRec *att_inq,void *value)
#else
(att_inq,value)
	HDFAttInqRec *att_inq;
	void *value;
#endif
{
	if (att_inq->data_type < 1 || value == NULL) {
		att_inq->value = NULL;
	}
	else if (att_inq->data_type == NC_CHAR && !(att_inq->name == Qfill_val || att_inq->name == Qmissing_val)) {
		char *tmp = NclMalloc(att_inq->len + 1);
		strncpy(tmp,value,att_inq->len);
		tmp[att_inq->len] = '\0';
		att_inq->value = NclMalloc(sizeof(NclQuark));
		*(NclQuark*)att_inq->value = NrmStringToQuark(tmp);
		NclFree(tmp);
	}
	else {
		att_inq->value = NclMalloc(sd_nctypelen(att_inq->data_type) * att_inq->len);
		memcpy(att_inq->value,value,sd_nctypelen(att_inq->data_type) * att_inq->len);
	}
	return;
}


static int32 HDFIsUnsigned (int hdf_type)
{

	switch (hdf_type) {
	case DFNT_UINT8:
	case DFNT_UINT16:
	case DFNT_UINT32:
	case DFNT_UINT64:
		return 1;
	default:
		return 0;
	}
}


static NclBasicDataTypes ToNclUnsigned(NclBasicDataTypes type)
{
	switch (type) {
	case NCL_byte:
		return NCL_ubyte;
	case NCL_short:
		return NCL_ushort;
	case NCL_int:
		return NCL_uint;
	case NCL_long:
		return NCL_ulong;
	case NCL_int64:
		return NCL_uint64;
	default:
		return type;
	}
}
		

static NclBasicDataTypes HDFMapToNcl 
#if	NhlNeedProto
(void* the_type)
#else
(the_type)
	void *the_type;
#endif
{
	static int first = 1;
	static NclBasicDataTypes long_type;
	if(first) {
		if(sizeof(nclong) == _NclSizeOf(NCL_int)) {
			long_type = NCL_int;
		} else if(sizeof(nclong) == _NclSizeOf(NCL_long)) {
			long_type = NCL_long;
		} 
		else {
			long_type = NCL_none;
		}
		first = 0;
	}
	switch(*(nc_type*)the_type) {
	case NC_BYTE:
		return(NCL_byte);
	case NC_CHAR:
		return(NCL_char);
	case NC_SHORT:
		return(NCL_short);
	case NC_LONG:
		return(long_type);
	case NC_FLOAT:
		return(NCL_float);
	case NC_DOUBLE:
		return(NCL_double);
	default:
		return(NCL_none);
	}
}

#ifndef NC_USHORT
#define NC_UOFFSET 101
#define NC_USHORT DFNT_UINT16 + NC_UOFFSET
#define NC_UINT   DFNT_UINT32 + NC_UOFFSET
#endif

static int32 NCMapToHDF(nc_type type)
{
/* this maps from NetCDF compatibility modes type to  HDF types */

	switch(type) {
	case NC_BYTE:
		return(DFNT_UCHAR);
	case NC_CHAR:
		return(DFNT_CHAR);
	case NC_SHORT:
		return(DFNT_INT16);
	case NC_USHORT:
		return(DFNT_UINT16);
	case NC_LONG:
		return(DFNT_INT32);
	case NC_UINT:
		return(DFNT_INT32);
	case NC_FLOAT:
		return(DFNT_FLOAT32);
	case NC_DOUBLE:
		return(DFNT_FLOAT64);
	default:
		return(DFNT_NONE);
	}
}


static nc_type HDFMapToNC(int32 hdf_type)
{
/* this maps from HDF types to the NetCDF compatibility mode types (strictly NetCDF 3) */

	switch (hdf_type) {
	case DFNT_INT8:
	case DFNT_UCHAR:
		return  NC_BYTE;
	case DFNT_UINT8:
	case DFNT_CHAR:
		return NC_CHAR;
	case DFNT_INT16:
	case DFNT_UINT16:
		return NC_SHORT;
	case DFNT_INT32:
	case DFNT_UINT32:
		return NC_LONG;
	case DFNT_FLOAT32:
		return NC_FLOAT;
	case DFNT_FLOAT64:
		return NC_DOUBLE;
	case DFNT_INT64:
	case DFNT_UINT64:
        default:
		NhlPError(NhlWARNING,NhlEUNKNOWN,"NclHDF.c: Can't map type");
		return (int) NhlWARNING;
	}
}
		
static void *HDFMapFromNcl
#if	NhlNeedProto
(NclBasicDataTypes the_type)
#else
(the_type)
	NclBasicDataTypes the_type;
#endif
{
	static int first = 1;
	static NclBasicDataTypes long_type;
	void *out_type = (void*)NclMalloc((unsigned)sizeof(nc_type));;
	if(first) {
		if(sizeof(nclong) == _NclSizeOf(NCL_long)) {
			long_type = NCL_long;
		} else if(sizeof(nclong) == _NclSizeOf(NCL_int)) {
			long_type = NCL_int;
		} else {
			long_type = NCL_none;
		}
		first = 0;
	}
	switch(the_type) {
	case NCL_byte:
		*(nc_type*)out_type = NC_BYTE;
                break;
	case NCL_char:
	case NCL_ubyte:
		*(nc_type*)out_type = NC_CHAR;
                break;
	case NCL_short:
		*(nc_type*)out_type = NC_SHORT;
                break;
	case NCL_ushort:
		*(int*)out_type = NC_USHORT;
                break;
	case NCL_int:
	case NCL_logical:
		*(nc_type*)out_type = NC_LONG;
                break;
	case NCL_uint:
		*(int*)out_type = NC_UINT;
                break;
	case NCL_long:
		if(long_type == the_type) {
			*(nc_type*)out_type = NC_LONG;
		} else {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"Can't map type, HDF 4 does not support 64 bit longs: try converting to integer or double");
			NclFree(out_type);
			out_type = NULL;
		}
		break;
	case NCL_ulong:
		if(long_type == the_type) {
			*(int*)out_type = NC_UINT;
		} else {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"Can't map type, HDF 4 does not support 64 bit longs: try converting to integer or double");
			NclFree(out_type);
			out_type = NULL;
		}
		break;
	case NCL_float:
		*(nc_type*)out_type = NC_FLOAT;
		break;
	case NCL_double:
		*(nc_type*)out_type = NC_DOUBLE;
		break;
	case NCL_int64:
	case NCL_uint64:		
                /* fall through */
        default:
		NhlPError(NhlINFO,NhlEUNKNOWN,"Can't map type");
		NclFree(out_type);
		out_type = NULL;
	}
	return(out_type);
}

static NclQuark HDFToNCLName
#if	NhlNeedProto
(
	char *hdf_name,
	char *ncl_class,
	NhlBoolean compress
)
#else
(hdf_name,hdf_class,compress)
	char *hdf_name;
	char *ncl_class;
	NhlBoolean compress;

#endif
{
	char buffer[MAX_NC_NAME];
	char *tp, *cp;
	int len = MIN(MAX_NC_NAME - 2,strlen(hdf_name));
	
	strncpy(buffer,hdf_name,len);
	cp = tp = buffer;

	if (! compress) {
		while (cp < buffer + len) {
			if (! isalnum(*cp)) {
				*cp = '_';
			}
			cp++;
		}
	}
	else {
		while (tp < buffer + len) {
			if (isalnum(*tp)) {
				*cp = *tp;
				cp++;
			}
			else {
				*cp = '_';
				if (cp == buffer || *(cp-1) != '_')
					cp++;
			}
			tp++;
		}
		if (cp > buffer && *(cp-1) == '_')
			cp--;
	}

	*cp = '\0';
	if (ncl_class) {
		len = MIN(MAX_NC_NAME - 1,len + 2 + strlen(ncl_class));
		strncat(cp,"__",len);
		cp += 2;
		*cp = '\0';
		strncat(buffer,ncl_class,len);
		cp += strlen(ncl_class);
		*cp = '\0';
	}
	buffer[len] = '\0';
	return NrmStringToQuark(buffer);
}
		  
static void 
ProcessVgroup(HDFFileRecord *hdf,int32 hid,int32 vg_ref,int level,char *vpath) 
{
	char vname[VGNAMELENMAX];
	char vclass[VGNAMELENMAX];
	int32 n_entries;
	int status;
	int i;
	int32 *tag_array,*ref_array;
	char levelstr[30];
	int vgid = Vattach(hid,vg_ref,"r");
	int len = strlen(vpath);

	status = Vgetclass(vgid,vclass);
	if (! strcmp(vclass,"CDF0.0")) {
		/* we're not interested in the SDS data vgroup, 
		   because it won't show the hierarchy */
		status = Vdetach(vgid);
		return;
	}
		
	status = Vinquire(vgid,&n_entries,vname);

	levelstr[0] = '\0';
	for (i = 0; i < level; i++) {
		strcat(levelstr,"\t");
	}
#if 0
	printf("%svgroup %d: %d %s %s\n",levelstr,vg_ref,n_entries,vclass,vname);
#endif
	tag_array = NclMalloc(n_entries*sizeof(int));
	ref_array = NclMalloc(n_entries*sizeof(int));
	status = Vgettagrefs(vgid,tag_array,ref_array,n_entries);
	if (len)
		strncat(vpath,"/",1023);
	strncat(vpath,vname,1023); 
	for (i = 0; i < n_entries; i++) {
		if (tag_array[i] == DFTAG_NDG) {
			HDFVarInqRecList *varp;
#if 0
			printf("%s\tDFTAG_NDG %d %d %d %d\n",levelstr,tag_array[i],ref_array[i],vg_ref,level);
#endif
			for (varp = hdf->vars; varp != NULL; varp = varp->next) {
				if (ref_array[i] == varp->var_inq->id_ref && varp->var_inq->var_path == NrmNULLQUARK) {
#if 0
					varp->var_inq->var_path = HDFToNCLName(vpath,NULL,True);
#endif
					varp->var_inq->var_path = NrmStringToQuark(vpath);
					varp->var_inq->vg_ref = vg_ref;
				}
			}
		}
/*
		else if (tag_array[i] == DFTAG_SD)
			printf("%s\tDFTAG_SD %d %d\n",levelstr,tag_array[i],ref_array[i]);
		else if (tag_array[i] == DFTAG_VG)
			printf("%s\tDGTAG_VG %d %d\n",levelstr,tag_array[i],ref_array[i]);
		else 
			printf("%s\tOTHER %d %d\n",levelstr,tag_array[i],ref_array[i]);
*/

		if (Visvg(vgid,ref_array[i])) {
			ProcessVgroup(hdf,hid,ref_array[i],level +1,vpath);
		}
	}
	vpath[len] = '\0';
	NclFree(tag_array);
	NclFree(ref_array);
	status = Vdetach(vgid);
	return;
}


static void ProcessVgroups(HDFFileRecord *hdf,char *path)
{
	int hid = Hopen(path,DFACC_READ,0);
	int32 *vls = NULL;
	int i;
	int vcount = Vlone(hid,vls,0);
	char vpath[1024];

	if (vcount) {
		vpath[0] = '\0';
		vls = NclMalloc(vcount * sizeof(int));
		/*
		 * get top level Vgroups
		 */
		vcount = Vlone(hid,vls,vcount);
		for (i = 0; i < vcount; i++) {
			ProcessVgroup(hdf,hid,vls[i],0,vpath);
		}
		NclFree(vls);
	}
}
#if 0	
static void GetSDInfo(char *path)
{
	
	int32 sd_id, sds_id, sds_ref;
      intn  status;
      int32 n_datasets, n_file_attrs, index;
      int32 dim_sizes[MAX_VAR_DIMS];
      int32 rank, data_type, n_attrs;
      char  name[MAX_NC_NAME];    int   i;    
      /********************* End of variable declaration ***********************/    
      /*    * Open the file and initialize the SD interface.    */    
      sd_id = SDstart (path, DFACC_READ); 

      /* Determine the number of data sets in the file and the number    
       * of file attributes.     */    
      status = SDfileinfo (sd_id, &n_datasets, &n_file_attrs);    
      /*      
       * Access every data set and print its name, rank, dimension sizes,    
       * data type, and number of attributes.     
       * The following information should be displayed:    *    
       *               name = SDStemplate    
       *               rank = 2    
       *               dimension sizes are : 16  5      
       *               data type is  24    
       *               number of attributes is  0    */    
      for (index = 0; index < n_datasets; index++)    {        
	      sds_id = SDselect (sd_id, index);     
	      sds_ref = SDidtoref(sds_id);
	      status = SDgetinfo (sds_id, name, &rank, dim_sizes,&data_type, &n_attrs);        
	      printf("----------------id %d ref %d\n",sds_id,sds_ref);
	      printf ("name = %s\n", name);        
	      printf ("rank = %d\n", rank);        
	      printf ("dimension sizes are : ");        
	      for (i=0; i< rank; i++) 
		      printf ("%d  ", dim_sizes[i]);        
	      printf ("\n");        
	      printf ("data type is  %d\n", data_type);        
	      printf ("number of attributes is  %d\n", n_attrs);        
	      /*        
	       * Terminate access to the data set.        
	       */        
	      status = SDendaccess (sds_id);    }    
      /*    
       * Terminate access to the SD interface and close the file.    
       */    
      status = SDend (sd_id); 
} 
#endif


static void ProcessDuplicateNames
#if	NhlNeedProto
(
	HDFFileRecord *tmp
)
#else
(tmp)
	HDFFileRecord *tmp;
#endif
{
	
	HDFVarInqRecList *varp,*tvarp;

	ProcessVgroups(tmp,NrmQuarkToString(tmp->file_path_q));
	for (varp = tmp->vars; varp != NULL; varp = varp->next) {
		char vgid_string[12];
		char *cp;
		int dup_count = 0;
		if (varp->var_inq->name != NrmNULLQUARK)
			continue;
		for (tvarp = varp->next; tvarp != NULL; tvarp = tvarp->next) {
			if (varp->var_inq->hdf_name != tvarp->var_inq->hdf_name) 
				continue;
			/*
			 * More than one variable with this name
			 * Must do something
			 */
			dup_count++;
			if (tvarp->var_inq->vg_ref > 0)
				sprintf(vgid_string,"%d",tvarp->var_inq->vg_ref);
			else 
				sprintf(vgid_string,"%d",dup_count);
			tvarp->var_inq->name = HDFToNCLName
				(NrmQuarkToString(tvarp->var_inq->hdf_name),vgid_string,True);

			if (tvarp->var_inq->var_path != NrmNULLQUARK) {
				HDFAttInqRecList **atlp;

				for (atlp = &(tvarp->var_inq->att_list);;atlp = &((*atlp)->next)) {
					if (*atlp != NULL)
						continue;
					(*atlp) = (HDFAttInqRecList*)NclMalloc((unsigned)sizeof(HDFAttInqRecList));
					(*atlp)->att_inq = (HDFAttInqRec*)NclMalloc((unsigned)sizeof(HDFAttInqRec));
					(*atlp)->next = NULL;
					(*atlp)->att_inq->att_num = tvarp->var_inq->natts;
					(*atlp)->att_inq->varid = tvarp->var_inq->varid;
					(*atlp)->att_inq->name = (*atlp)->att_inq->hdf_name = NrmStringToQuark("hdf_group");
					(*atlp)->att_inq->data_type = NC_CHAR;
					(*atlp)->att_inq->len = strlen(NrmQuarkToString(tvarp->var_inq->var_path));
					(*atlp)->att_inq->value = NULL;
					break;
				}
				tvarp->var_inq->natts++;
				atlp = &((*atlp)->next);
				(*atlp) = (HDFAttInqRecList*)NclMalloc((unsigned)sizeof(HDFAttInqRecList));
				(*atlp)->att_inq = (HDFAttInqRec*)NclMalloc((unsigned)sizeof(HDFAttInqRec));
				(*atlp)->next = NULL;
				(*atlp)->att_inq->att_num = tvarp->var_inq->natts;
				(*atlp)->att_inq->varid = tvarp->var_inq->varid;
				(*atlp)->att_inq->name = (*atlp)->att_inq->hdf_name = NrmStringToQuark("hdf_group_id");
				(*atlp)->att_inq->data_type = NC_LONG;
				(*atlp)->att_inq->len = 1;
				(*atlp)->att_inq->value = NULL;
				tvarp->var_inq->natts++;
			}

		}
		cp = &vgid_string[0];
		if (varp->var_inq->vg_ref > 0) {
			sprintf(vgid_string,"%d",varp->var_inq->vg_ref);
		}
		else if (dup_count > 0) {
			/* this is the first instance so call it 0 */ 
			sprintf(vgid_string,"%d",0);
		}
		else { /* no dups for this variable and no group name either */
			cp = NULL;
		}
		varp->var_inq->name = HDFToNCLName(NrmQuarkToString(varp->var_inq->hdf_name),cp,True);
		if (varp->var_inq->var_path != NrmNULLQUARK) {
			HDFAttInqRecList **atlp;

			for (atlp = &(varp->var_inq->att_list);;atlp = &((*atlp)->next)) {
				if (*atlp != NULL)
					continue;
				(*atlp) = (HDFAttInqRecList*)NclMalloc((unsigned)sizeof(HDFAttInqRecList));
				(*atlp)->att_inq = (HDFAttInqRec*)NclMalloc((unsigned)sizeof(HDFAttInqRec));
				(*atlp)->next = NULL;
				(*atlp)->att_inq->att_num = varp->var_inq->natts;
				(*atlp)->att_inq->varid = varp->var_inq->varid;
				(*atlp)->att_inq->name = (*atlp)->att_inq->hdf_name = NrmStringToQuark("hdf_group");
				(*atlp)->att_inq->data_type = NC_CHAR;
				(*atlp)->att_inq->len = strlen(NrmQuarkToString(varp->var_inq->var_path));
				(*atlp)->att_inq->value = NULL;
				break;
			}
			varp->var_inq->natts++;
			atlp = &((*atlp)->next);
			(*atlp) = (HDFAttInqRecList*)NclMalloc((unsigned)sizeof(HDFAttInqRecList));
			(*atlp)->att_inq = (HDFAttInqRec*)NclMalloc((unsigned)sizeof(HDFAttInqRec));
			(*atlp)->next = NULL;
			(*atlp)->att_inq->att_num = varp->var_inq->natts;
			(*atlp)->att_inq->varid = varp->var_inq->varid;
			(*atlp)->att_inq->name = (*atlp)->att_inq->hdf_name = NrmStringToQuark("hdf_group_id");
			(*atlp)->att_inq->data_type = NC_LONG;
			(*atlp)->att_inq->len = 1;
			(*atlp)->att_inq->value = NULL;
			varp->var_inq->natts++;
		}

			
	}
}

static void *HDFInitializeFileRec
#if	NhlNeedProto
(NclFileFormat *format)
#else
(format)
NclFileFormat *format;
#endif
{
	HDFFileRecord *therec = NULL;
	static int first = 1;

	if (first) {
		Qmissing_val = NrmStringToQuark("missing_value");
		Qfill_val = NrmStringToQuark("_FillValue");
		first = False;
	}

	therec = (HDFFileRecord*)NclCalloc(1, sizeof(HDFFileRecord));
	if (! therec) {
		NhlPError(NhlFATAL,ENOMEM,NULL);
		return NULL;
	}
	*format = _NclHDF;
	return (void *) therec;
}

void *HDFOpenFile(void *rec,NclQuark path,int wr_status)
{
	HDFFileRecord *tmp = (HDFFileRecord*) rec;
	int cdfid;
	int dummy;
	char buffer[MAX_NC_NAME];
	char buffer2[MAX_NC_NAME];
	int i,j,has_scalar_dim = 0,nvars = 0;
	long tmp_size;
	HDFAttInqRecList **stepalptr;
	HDFVarInqRecList **stepvlptr;
	HDFVarInqRecList *tmpvlptr;
	HDFVarInqRecList *tmpvlptr2;
	HDFDimInqRecList **stepdlptr;
	HDFDimInqRecList *tmpdlptr;
	int32 sd_id;
	intn status;
	
	if(tmp == NULL) {
		return(NULL);
	}
	tmp->file_path_q = path;
	tmp->wr_status = wr_status;
	tmp->n_vars = 0;
	tmp->vars= NULL;
	tmp->n_dims = 0;
	tmp->dims = NULL;
	tmp->n_file_atts = 0;
	tmp->file_atts= NULL;

#if 0
	/* print out SD dataset info using SD interface */
	GetSDInfo(NrmQuarkToString(path)); 
#endif

	if(wr_status > 0) {
		cdfid = sd_ncopen(NrmQuarkToString(path),NC_NOWRITE);
		sd_id = SDstart (NrmQuarkToString(path), DFACC_READ); 
	} else {
		cdfid = sd_ncopen(NrmQuarkToString(path),NC_WRITE);
		sd_id = SDstart (NrmQuarkToString(path), DFACC_WRITE); 
	}

	if(cdfid == -1) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"The specified HDF file (%s) does not exist or can't be opened\n",NrmQuarkToString(path));
		NclFree(tmp);
		return(NULL);
	}
#if 0
	SDfileinfo (sd_id, &tmp->n_vars, &tmp->n_file_atts);    
#endif
	sd_ncinquire(cdfid,&(tmp->n_dims),&(tmp->n_vars),&(tmp->n_file_atts),&dummy);


	stepdlptr = &(tmp->dims);
	if(tmp->n_dims != 0) {
		for(i = 0 ; i < tmp->n_dims; i++) {
			*stepdlptr = (HDFDimInqRecList*)NclMalloc(
					(unsigned) sizeof(HDFDimInqRecList));
			(*stepdlptr)->dim_inq = (HDFDimInqRec*)NclMalloc(
					(unsigned)sizeof(HDFDimInqRec));
			(*stepdlptr)->next = NULL;
			(*stepdlptr)->dim_inq->dimid = i;
			(*stepdlptr)->dim_inq->is_unlimited = (i==dummy)?1:0;

			sd_ncdiminq(cdfid,i,buffer,&((*stepdlptr)->dim_inq->size));
			if((*stepdlptr)->dim_inq->size == 0) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"HDF: %s is a zero length dimension some variables may be ignored",buffer);
			}
			
			(*stepdlptr)->dim_inq->hdf_name = NrmStringToQuark(buffer);
			(*stepdlptr)->dim_inq->name = HDFToNCLName(buffer,NULL,True);
			stepdlptr = &((*stepdlptr)->next);
		}
	} else {
		tmp->dims = NULL;
	}
	stepvlptr = &(tmp->vars);
	nvars = tmp->n_vars;
	if(tmp->n_vars != 0 ) {
		NhlBoolean has_duplicate_names = False;
		int32 sds_id;
		for(i = 0 ; i < nvars; i++) {
			int dim_sizes[32];
			int n_dims;
			int n_atts;
			*stepvlptr = (HDFVarInqRecList*)NclMalloc(
					(unsigned) sizeof(HDFVarInqRecList));
			(*stepvlptr)->var_inq = (HDFVarInqRec*)NclMalloc(
					(unsigned)sizeof(HDFVarInqRec));
			(*stepvlptr)->next = NULL;
			(*stepvlptr)->var_inq->varid = i;
			sd_ncvarinq(cdfid,i,buffer,
				&((*stepvlptr)->var_inq->data_type),
				&((*stepvlptr)->var_inq->n_dims),
				((*stepvlptr)->var_inq->dim),
				&((*stepvlptr)->var_inq->natts)
				);
			sds_id = SDselect(sd_id,i);
			(*stepvlptr)->var_inq->id_ref = SDidtoref(sds_id);
			(*stepvlptr)->var_inq->vg_ref = -1;
			(*stepvlptr)->var_inq->var_path = NrmNULLQUARK;
			(*stepvlptr)->var_inq->name = NrmNULLQUARK;
			(*stepvlptr)->var_inq->hdf_name = NrmStringToQuark(buffer);
			status = SDgetinfo(sds_id,buffer,
					   &n_dims,
					   dim_sizes,
					   &((*stepvlptr)->var_inq->hdf_type),
					   &n_atts);
				
                      /*
                        fprintf(stdout, "file: %s, line, %d\n", __FILE__, __LINE__);
                        fprintf(stdout, "\t(*stepvlptr)->var_inq->data_type: %ld\n", (long) (*stepvlptr)->var_inq->data_type);
                        fprintf(stdout, "\tdim_sizes[0]: %d \n", dim_sizes[0]);
                        fprintf(stdout, "\t(*stepvlptr)->var_inq->n_dims: %ld \n", (long) (*stepvlptr)->var_inq->n_dims);
                        fprintf(stdout, "\t(*stepvlptr)->var_inq->natts): %ld \n", (long) (*stepvlptr)->var_inq->natts);
                       */

			for(j = 0; j < ((*stepvlptr)->var_inq->n_dims); j++) {
				tmp_size = 0;
				sd_ncdiminq(cdfid,((*stepvlptr)->var_inq->dim)[j],buffer2,&tmp_size);
				if(tmp_size == 0 ) {
					NhlPError(NhlWARNING,NhlEUNKNOWN,"HDF: A zero length dimension was found in variable (%s), ignoring this variable",buffer);
					break;
				}
			}
			if(j != ((*stepvlptr)->var_inq->n_dims)) {
				tmpvlptr = *stepvlptr;
				*stepvlptr = NULL;
				tmp->n_vars--;
				NclFree(tmpvlptr->var_inq);
				NclFree(tmpvlptr);
				
			} else {
				if(((*stepvlptr)->var_inq->n_dims) == 0) {
					((*stepvlptr)->var_inq->dim)[0] = -5;
					((*stepvlptr)->var_inq->n_dims) = 1;
					has_scalar_dim = 1;
				}
				stepalptr = &((*stepvlptr)->var_inq->att_list);
				(*stepvlptr)->var_inq->natts++;
				if(((*stepvlptr)->var_inq->natts) != 0) {
					for(j = 0; j < ((*stepvlptr)->var_inq->natts); j++) {
						(*stepalptr) = (HDFAttInqRecList*)NclMalloc(
							(unsigned)sizeof(HDFAttInqRecList));
						(*stepalptr)->att_inq = (HDFAttInqRec*)NclMalloc(
							(unsigned)sizeof(HDFAttInqRec));
						(*stepalptr)->next = NULL;
						(*stepalptr)->att_inq->att_num = j;
						(*stepalptr)->att_inq->varid = i;
						if (j < (*stepvlptr)->var_inq->natts - 1) {
							sd_ncattname(cdfid,i,j,buffer);
							(*stepalptr)->att_inq->hdf_name = NrmStringToQuark(buffer);
							(*stepalptr)->att_inq->name = HDFToNCLName(buffer,NULL,False);
							SDattrinfo(sds_id,j,buffer,
									 &((*stepalptr)->att_inq->hdf_type),
									 &((*stepalptr)->att_inq->len));
							sd_ncattinq(cdfid,i,buffer,
								    &((*stepalptr)->att_inq->data_type),
								    &((*stepalptr)->att_inq->len));
							(*stepalptr)->att_inq->attr_ix = SDfindattr(sds_id,buffer);

							HDF_SDGetAttrVal(sds_id,(*stepalptr)->att_inq);
						}
						else {
							(*stepalptr)->att_inq->name =
								(*stepalptr)->att_inq->hdf_name = NrmStringToQuark("hdf_name");
							(*stepalptr)->att_inq->data_type = NC_CHAR;
							(*stepalptr)->att_inq->len = 
								strlen(NrmQuarkToString((*stepvlptr)->var_inq->hdf_name));
							(*stepalptr)->att_inq->value = NULL;
						}
						stepalptr = &((*stepalptr)->next);
					}
				} else {
					((*stepvlptr)->var_inq->att_list) = NULL;
				}
				status = SDendaccess (sds_id);
				stepvlptr = &((*stepvlptr)->next);
			}
		}
		if(has_scalar_dim) {
			tmp->has_scalar_dim = 1;
			tmpdlptr = tmp->dims;
			tmp->dims = (HDFDimInqRecList*)NclMalloc(
					(unsigned) sizeof(HDFDimInqRecList));
			tmp->dims->dim_inq = (HDFDimInqRec*)NclMalloc(
					(unsigned)sizeof(HDFDimInqRec));
			tmp->dims->next = tmpdlptr;
			tmp->dims->dim_inq->dimid = -5;
			tmp->dims->dim_inq->size = 1;
			tmp->dims->dim_inq->name = NrmStringToQuark("ncl_scalar");
			tmp->dims->dim_inq->hdf_name = NrmNULLQUARK;
			tmp->dims->dim_inq->is_unlimited = 0;
			tmp->n_dims++;
		} else {
			tmp->has_scalar_dim = 0;
		}
		/* see if there are duplicate names */

		for (tmpvlptr = tmp->vars; tmpvlptr != NULL; tmpvlptr = tmpvlptr->next) {
			for (tmpvlptr2 = tmpvlptr->next; tmpvlptr2 != NULL; tmpvlptr2 = tmpvlptr2->next) {
				if (tmpvlptr->var_inq->hdf_name == tmpvlptr2->var_inq->hdf_name) {
				        has_duplicate_names = True;
					break;
				}
			}
			if (has_duplicate_names)
				break;
		}
		if (has_duplicate_names) {
			ProcessDuplicateNames(tmp);
		}
		else {
			for (tmpvlptr = tmp->vars; tmpvlptr != NULL; tmpvlptr = tmpvlptr->next) {
				tmpvlptr->var_inq->name = HDFToNCLName(NrmQuarkToString(tmpvlptr->var_inq->hdf_name),NULL,True);
			}
		}

	} else {
		tmp->vars = NULL;
		tmp->has_scalar_dim = 0;
	}
	if(tmp->n_file_atts != 0 ) {
		stepalptr = &(tmp->file_atts);
		for(i = 0; i < tmp->n_file_atts; i++) {
			*stepalptr = (HDFAttInqRecList*)NclMalloc(
				(unsigned)sizeof(HDFAttInqRecList));
			(*stepalptr)->att_inq = (HDFAttInqRec*)NclMalloc(
				(unsigned)sizeof(HDFAttInqRec));
			(*stepalptr)->next = NULL;
			sd_ncattname(cdfid,NC_GLOBAL,i,buffer);
			(*stepalptr)->att_inq->att_num = i;
			(*stepalptr)->att_inq->hdf_name = NrmStringToQuark(buffer);
			(*stepalptr)->att_inq->name = HDFToNCLName(buffer,NULL,False);
			(*stepalptr)->att_inq->varid = NC_GLOBAL;
			(*stepalptr)->att_inq->attr_ix = SDfindattr(sd_id,buffer);
			SDattrinfo(sd_id,i,buffer,
				   &((*stepalptr)->att_inq->hdf_type),
				   &((*stepalptr)->att_inq->len));
			sd_ncattinq(cdfid,NC_GLOBAL,buffer,
					&((*stepalptr)->att_inq->data_type),
                                	&((*stepalptr)->att_inq->len));
			HDF_SDGetAttrVal(sd_id,(*stepalptr)->att_inq);
       	        	stepalptr = &((*stepalptr)->next);
		}
	} else {
		tmp->file_atts = NULL;
	}
	status = SDend (sd_id);
	sd_ncclose(cdfid);
	return((void*)tmp);
}

static void *HDFCreateFile
#if	NhlNeedProto
(void *rec,NclQuark path)
#else
(rec,path)
void *rec;
NclQuark path;
#endif
{
	int id = 0;
	
	id = sd_nccreate(NrmQuarkToString(path),NC_NOCLOBBER);

	if(id > -1) {
		sd_ncendef(id);
		sd_ncclose(id);
		return(HDFOpenFile(rec,path,-1));
	} else {
		return(NULL);
	}
}

void HDFFreeFileRec(void* therec)
{
	HDFFileRecord *rec = (HDFFileRecord*)therec;
	HDFAttInqRecList *stepal;
        HDFVarInqRecList *stepvl;
        HDFDimInqRecList *stepdl;

	stepal = rec->file_atts;
	while(rec->file_atts != NULL) {
		stepal = rec->file_atts;
		if (stepal->att_inq->value)
			NclFree(stepal->att_inq->value);
		NclFree(stepal->att_inq);
		rec->file_atts = rec->file_atts->next;
		NclFree(stepal);
	}
	stepdl = rec->dims;
	while(rec->dims != NULL) {
		stepdl = rec->dims;
		NclFree(stepdl->dim_inq);
		rec->dims= rec->dims->next;
		NclFree(stepdl);
	}

	while(rec->vars != NULL) {
		stepvl = rec->vars;
		while(stepvl->var_inq->att_list != NULL) {
			stepal = stepvl->var_inq->att_list;
			if (stepvl->var_inq->att_list->att_inq->value)
				NclFree(stepvl->var_inq->att_list->att_inq->value);
			NclFree(stepvl->var_inq->att_list->att_inq);
			stepvl->var_inq->att_list = stepal->next;
			NclFree(stepal);
		}
		NclFree(stepvl->var_inq);
		rec->vars = rec->vars->next;
		NclFree(stepvl);
	}
	NclFree(rec);
	return;
}

static NclQuark* HDFGetVarNames
#if	NhlNeedProto
(void* therec, int *num_vars)
#else
(therec, num_vars)
void* therec;
int *num_vars;
#endif
{
	HDFFileRecord *rec = (HDFFileRecord*)therec;
	NclQuark *out_quarks;
	HDFVarInqRecList *stepvl;
	int i;

	out_quarks = (NclQuark*)NclMalloc((unsigned)sizeof(NclQuark)*rec->n_vars);
	stepvl = rec->vars;
	for(i = 0; i < rec->n_vars; i++) {
		out_quarks[i] = stepvl->var_inq->name;
		stepvl=stepvl->next;
	}
	*num_vars = rec->n_vars;;
	return(out_quarks);
}

static NclFVarRec *HDFGetVarInfo
#if	NhlNeedProto
(void *therec, NclQuark var_name)
#else
(therec, var_name)
void *therec;
NclQuark var_name;
#endif
{
	HDFFileRecord *rec = (HDFFileRecord*)therec;
	HDFVarInqRecList *stepvl;
	HDFDimInqRecList *stepdl;
	NclFVarRec *tmp;
	int j;

	stepvl = rec->vars;
	while(stepvl != NULL) {
		if(stepvl->var_inq->name == var_name) {
			tmp = (NclFVarRec*)NclMalloc((unsigned)sizeof(NclFVarRec));
			tmp->var_name_quark = stepvl->var_inq->name;
			tmp->var_full_name_quark = stepvl->var_inq->name;
			tmp->var_real_name_quark = stepvl->var_inq->name;
			tmp->data_type = HDFMapToNcl((void*)&(stepvl->var_inq->data_type));
			if (HDFIsUnsigned(stepvl->var_inq->hdf_type)) {
				tmp->data_type = ToNclUnsigned(tmp->data_type);
			}
			tmp->num_dimensions = stepvl->var_inq->n_dims;
			for(j=0; j< stepvl->var_inq->n_dims; j++) {
				stepdl = rec->dims;
				while(stepdl->dim_inq->dimid != stepvl->var_inq->dim[j]) {
					stepdl = stepdl->next;
				}
/*
				tmp->dim_sizes[j] = stepdl->dim_inq->size;
*/
				if(stepdl->dim_inq->dimid == -5) {
					tmp->file_dim_num[j] = 0;
				} else if(rec->has_scalar_dim) {
					tmp->file_dim_num[j] = stepdl->dim_inq->dimid + 1;
				} else {
					tmp->file_dim_num[j] = stepdl->dim_inq->dimid;
				}
			}
			return(tmp);
		} else {
			stepvl = stepvl->next;
		}
	}
	return(NULL);
}

static NclQuark *HDFGetDimNames
#if	NhlNeedProto
(void* therec, int* num_dims)
#else
(therec,num_dims)
void *therec;
int *num_dims;
#endif
{
	HDFFileRecord *rec = (HDFFileRecord*)therec;
	NclQuark *out_quarks;
	HDFDimInqRecList *stepdl;
	int i;

	out_quarks = (NclQuark*)NclMalloc((unsigned)sizeof(NclQuark)*rec->n_dims);
	stepdl = rec->dims;
	for(i = 0; i < rec->n_dims; i++) {
		out_quarks[i] = stepdl->dim_inq->name;
		stepdl=stepdl->next;
	}
	*num_dims = rec->n_dims;;
	return(out_quarks);
}

static NclFDimRec *HDFGetDimInfo
#if	NhlNeedProto
(void* therec, NclQuark dim_name_q)
#else
(therec,dim_name_q)
void* therec;
NclQuark dim_name_q;
#endif
{
	HDFFileRecord* rec = (HDFFileRecord*)therec;
	NclFDimRec *tmp;
	HDFDimInqRecList *stepdl;

	stepdl = rec->dims;
	while(stepdl != NULL) {
		if(stepdl->dim_inq->name == dim_name_q) {
			tmp = (NclFDimRec*)NclMalloc((unsigned)sizeof(NclFDimRec));
			tmp->dim_name_quark = dim_name_q;
			tmp->dim_size = stepdl->dim_inq->size;
			tmp->is_unlimited  = stepdl->dim_inq->is_unlimited;
			return(tmp);
		} else {
			stepdl = stepdl->next;
		}
	}
	return(NULL);
}
static NclQuark *HDFGetAttNames
#if	NhlNeedProto
(void* therec,int *num_atts)
#else
(therec,num_atts)
void* therec;
int *num_atts;
#endif
{	
	HDFFileRecord* rec = (HDFFileRecord*)therec;
	HDFAttInqRecList *stepal;
	NclQuark *out_list = NULL;
	int i;

	out_list = (NclQuark*)NclMalloc((unsigned)sizeof(NclQuark)*rec->n_file_atts);
	stepal = rec->file_atts;
	for(i = 0; i< rec->n_file_atts; i++) {
		out_list[i] = stepal->att_inq->name;
		stepal = stepal->next;
	}
	*num_atts = rec->n_file_atts;
	return(out_list);
}

static NclFAttRec* HDFGetAttInfo
#if	NhlNeedProto
(void* therec, NclQuark att_name_q)
#else
(therec, att_name_q)
void* therec;
NclQuark att_name_q;
#endif
{
	HDFFileRecord* rec = (HDFFileRecord*)therec;
	HDFAttInqRecList *stepal;
	NclFAttRec *tmp;

	stepal = rec->file_atts;
	while(stepal != NULL) {
		if(stepal->att_inq->name == att_name_q) {
			tmp=(NclFAttRec*)NclMalloc((unsigned)sizeof(NclFAttRec));
			tmp->att_name_quark = att_name_q;
/*
* For convenience I make all character attributes strings (except if it's the _FillValue or missing_value of a character variable)
*/
			if(stepal->att_inq->data_type == NC_CHAR && !(stepal->att_inq->name == Qfill_val || stepal->att_inq->name == Qmissing_val)) {
				tmp->data_type = NCL_string;
				tmp->num_elements = 1;
			} else {
				tmp->data_type = HDFMapToNcl((void*)&(stepal->att_inq->data_type));
				if (HDFIsUnsigned(stepal->att_inq->hdf_type)) {
					tmp->data_type = ToNclUnsigned(tmp->data_type);
				}
				tmp->num_elements = stepal->att_inq->len;
			}
			return(tmp);
		} else {
			stepal = stepal->next;
		}
	}

	return(NULL);
}

static NclQuark *HDFGetVarAttNames
#if	NhlNeedProto
(void *therec , NclQuark thevar, int* num_atts)
#else
(therec , thevar, num_atts)
void *therec;
NclQuark thevar;
int* num_atts;
#endif
{
	HDFFileRecord* rec = (HDFFileRecord*)therec;
	HDFVarInqRecList *stepvl;
	HDFAttInqRecList *stepal;
	NclQuark *out_list = NULL;	
	int i;

	*num_atts = 0;
	stepvl = rec->vars;
	while(stepvl != NULL) {
		if(stepvl->var_inq->name== thevar) {
			stepal = stepvl->var_inq->att_list;
			out_list = (NclQuark*)NclMalloc((unsigned)sizeof(NclQuark) * stepvl->var_inq->natts);
			*num_atts = stepvl->var_inq->natts;
			for(i = 0; i< stepvl->var_inq->natts; i++) {
				out_list[i] = stepal->att_inq->name;
				stepal = stepal->next;
			}
			return(out_list);
		} else {
			stepvl = stepvl->next;
		}
	}
		
	return(NULL);
}

static NclFAttRec *HDFGetVarAttInfo
#if	NhlNeedProto
(void *therec, NclQuark thevar, NclQuark theatt)
#else
(therec, thevar, theatt)
void *therec;
NclQuark thevar;
NclQuark theatt;
#endif
{
	HDFFileRecord* rec = (HDFFileRecord*)therec;
	HDFVarInqRecList *stepvl;
	HDFAttInqRecList *stepal;
	NclFAttRec *tmp = NULL;

	stepvl = rec->vars;
	while(stepvl != NULL) {
		if(stepvl->var_inq->name == thevar) {
			stepal = stepvl->var_inq->att_list;
			while(stepal != NULL) {
				if(stepal->att_inq->name == theatt) {
					tmp= (NclFAttRec*)NclMalloc((unsigned)
						sizeof(NclFAttRec));
					tmp->att_name_quark = theatt;
					if(stepal->att_inq->data_type == NC_CHAR && !(stepal->att_inq->name == Qfill_val || stepal->att_inq->name == Qmissing_val)) {

						tmp->data_type = NCL_string;
						tmp->num_elements = 1;
					} else {
						tmp->data_type = HDFMapToNcl((void*)&stepal->att_inq->data_type);
						if (HDFIsUnsigned(stepal->att_inq->hdf_type)) {
							tmp->data_type = ToNclUnsigned(tmp->data_type);
						}
						tmp->num_elements = stepal->att_inq->len;
					}
					return(tmp);
				} else {
					stepal = stepal->next;
				}
			}
		} else {
			stepvl = stepvl->next;
		}
	}
		
	return(NULL);
}

static NclFVarRec *HDFGetCoordInfo
#if	NhlNeedProto
(void* therec, NclQuark thevar)
#else
(therec, thevar)
void* therec;
NclQuark thevar;
#endif
{
	return(HDFGetVarInfo(therec,thevar));
}


static void *HDFReadVar
#if	NhlNeedProto
(void* therec, NclQuark thevar, long* start, long* finish,long* stride,void* storage)
#else
(therec, thevar, start, finish,stride,storage)
void* therec;
NclQuark thevar;
long* start;
long* finish;
long* stride;
void* storage;
#endif
{
	HDFFileRecord *rec = (HDFFileRecord*) therec;
	HDFVarInqRecList *stepvl;
	void *out_data;
	ng_size_t n_elem = 1;
	int cdfid = -1;
	int ret = -1,i;
	int no_stride = 1;
	long count[MAX_NC_DIMS];

	stepvl = rec->vars;
	while(stepvl != NULL) {
		if(stepvl->var_inq->name == thevar) {
			for(i= 0; i< stepvl->var_inq->n_dims; i++) {
				count[i] = (long)((finish[i] - start[i])/stride[i]) + 1;
				n_elem *= count[i];
				if(stride[i] != 1) {
					no_stride = 0;
				}
			}
			out_data = storage;
			cdfid = sd_ncopen(NrmQuarkToString(rec->file_path_q),NC_NOWRITE);
				
			if(cdfid == -1) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"HDF: Could not reopen the file (%s) for reading",NrmQuarkToString(rec->file_path_q));
				return(NULL);
			}


			if(no_stride) {	
				ret = sd_ncvargetg(cdfid,
					stepvl->var_inq->varid,
					start,
					count,
					NULL,
					NULL,
					out_data);
			} else {
				ret = sd_ncvargetg(cdfid,
					stepvl->var_inq->varid,
					start,
					count,
					stride,
					NULL,
					out_data);
			}
	
			sd_ncclose(cdfid);
			if(ret == -1) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"HDF: An error occurred while attempting to read variable (%s) from file (%s)",NrmQuarkToString(thevar),NrmQuarkToString(rec->file_path_q));
				return(NULL);
			} else {
				return(storage);
			}
		} else {
			stepvl = stepvl->next;
		}
	}
	NhlPError(NhlFATAL,NhlEUNKNOWN,"HDF: Variable (%s) is not an element of file (%s)",NrmQuarkToString(thevar),NrmQuarkToString(rec->file_path_q));
	return(NULL);
}

static void *HDFReadCoord
#if	NhlNeedProto
(void* therec, NclQuark thevar, long* start, long* finish,long* stride,void* storage)
#else
(therec, thevar, start, finish,stride,storage)
void* therec;
NclQuark thevar;
long* start;
long* finish;
long* stride;
void* storage;
#endif
{
	return(HDFReadVar(therec,thevar,start,finish,stride,storage));
}


static void *HDFReadAtt
#if	NhlNeedProto
(void *therec,NclQuark theatt,void* storage)
#else
(therec,theatt,storage)
void *therec;
NclQuark theatt;
void* storage;
#endif
{
	HDFFileRecord *rec = (HDFFileRecord*)therec;
	HDFAttInqRecList *stepal;
	int cdfid,ret ;
	char *tmp;

	stepal = rec->file_atts;
	while(stepal != NULL) {
		if(stepal->att_inq->name == theatt) {
			if (stepal->att_inq->value != NULL) {
				if(stepal->att_inq->data_type == NC_CHAR && !(stepal->att_inq->name == Qfill_val || stepal->att_inq->name == Qmissing_val)) {
					*(NclQuark*)storage = *(NclQuark*)(stepal->att_inq->value);
				} else {
					memcpy(storage,stepal->att_inq->value,
					       sd_nctypelen(stepal->att_inq->data_type)*stepal->att_inq->len);
				}
				return(storage);
			}
			cdfid = sd_ncopen(NrmQuarkToString(rec->file_path_q),NC_NOWRITE);
			
			if(cdfid == -1) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"HDF: Could not reopen the file (%s) for reading",NrmQuarkToString(rec->file_path_q));
				return(NULL);
			}
			if(stepal->att_inq->data_type == NC_CHAR && !(theatt == Qfill_val || theatt == Qmissing_val)) {
				tmp = (char*)NclMalloc(stepal->att_inq->len+1);
				tmp[stepal->att_inq->len] = '\0';
				ret = sd_ncattget(cdfid,NC_GLOBAL,NrmQuarkToString(theatt),tmp);
				*(NclQuark*)storage = NrmStringToQuark(tmp);
				NclFree(tmp);
			} else {
				ret = sd_ncattget(cdfid,NC_GLOBAL,NrmQuarkToString(theatt),storage);
			}
			sd_ncclose(cdfid);
			return(storage);
		} else {
			stepal = stepal->next;
		}
	}
	NhlPError(NhlFATAL,NhlEUNKNOWN,"HDF: Attribute (%s) is not a global attribute of file (%s)",NrmQuarkToString(theatt),NrmQuarkToString(rec->file_path_q));
	return(NULL);
}

static void *HDFReadVarAtt
#if	NhlNeedProto
(void * therec, NclQuark thevar, NclQuark theatt, void * storage)
#else
(therec, thevar, theatt, storage)
void * therec;
NclQuark thevar;
NclQuark theatt;
void* storage;
#endif
{
	HDFFileRecord *rec = (HDFFileRecord*)therec;
	HDFAttInqRecList *stepal;
	HDFVarInqRecList *stepvl;
	int cdfid;
	int ret;
	char *tmp;

	stepvl = rec->vars;
	while(stepvl != NULL) {
		if(stepvl->var_inq->name == thevar) {
			if (theatt == NrmStringToQuark("hdf_name")) {
				*(NclQuark*)storage = stepvl->var_inq->hdf_name;
				return storage;
			}
			else if (theatt == NrmStringToQuark("hdf_group")) {
				*(NclQuark*)storage = stepvl->var_inq->var_path;
				return storage;
			}
			stepal = stepvl->var_inq->att_list;
			
			while(stepal != NULL) {
				if(stepal->att_inq->name == theatt) {
					if (theatt == NrmStringToQuark("hdf_group_id")) {
						memcpy(storage,&(stepvl->var_inq->vg_ref),
						       sd_nctypelen(stepal->att_inq->data_type)*stepal->att_inq->len);
						return storage;
					}
					if (stepal->att_inq->value != NULL) {
						if(stepal->att_inq->data_type == NC_CHAR && !(theatt == Qfill_val || theatt == Qmissing_val)) {
							*(NclQuark*)storage = *(NclQuark*)(stepal->att_inq->value);
						} else {
							memcpy(storage,stepal->att_inq->value,
							       sd_nctypelen(stepal->att_inq->data_type)*stepal->att_inq->len);
						}
						return(storage);
					}
					cdfid = sd_ncopen(NrmQuarkToString(rec->file_path_q),NC_NOWRITE);
			
					if(cdfid == -1) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"HDF: Could not reopen the file (%s) for reading",NrmQuarkToString(rec->file_path_q));
						return(NULL);
					}
					if(stepal->att_inq->data_type == NC_CHAR  && !(theatt == Qfill_val || theatt == Qmissing_val)) {
	
						tmp = (char*)NclMalloc(stepal->att_inq->len + 1);
						tmp[stepal->att_inq->len] = '\0';
						ret = sd_ncattget(cdfid,stepvl->var_inq->varid,NrmQuarkToString(theatt),tmp);
						*(NclQuark*)storage = NrmStringToQuark(tmp);
						NclFree(tmp);
					
						
						
					} else {
						ret = sd_ncattget(cdfid,stepvl->var_inq->varid,NrmQuarkToString(theatt),storage);
					}
					sd_ncclose(cdfid);
					if(ret != -1)
						return(storage);
				} else {
					stepal = stepal->next;
				}
			}
			break;
		} else {
			stepvl = stepvl->next;
		}
	}
	NhlPError(NhlFATAL,NhlEUNKNOWN,"HDF: Attribute (%s) is not a variable attribute of (%s->%s)",NrmQuarkToString(theatt),NrmQuarkToString(rec->file_path_q),NrmQuarkToString(thevar));
	return(NULL);
}
static NhlErrorTypes HDFWriteVar
#if	NhlNeedProto
(void * therec, NclQuark thevar, void *data, long* start, long *finish,long *stride)
#else
(therec, thevar, data, start, finish,stride)
void * therec;
NclQuark thevar;
void *data;
long *start;
long *finish;
long *stride;
#endif
{
	HDFFileRecord *rec = (HDFFileRecord*)therec;
	int cdfid;
	HDFVarInqRecList *stepvl; 
	long count[MAX_NC_DIMS];
	ng_size_t n_elem = 1;
	int i,no_stride = 1;
	int ret;

	if(rec->wr_status <= 0) {
		stepvl = rec->vars;
		while(stepvl != NULL) {
			if(stepvl->var_inq->name == thevar) {
				for(i= 0; i< stepvl->var_inq->n_dims; i++) {
					count[i] = (long)((finish[i] - start[i])/stride[i]) + 1;
					n_elem *= count[i];
					if(stride[i] != 1) {
						no_stride = 0;
					}
				}
				cdfid = sd_ncopen(NrmQuarkToString(rec->file_path_q),NC_WRITE);
				
				if(cdfid == -1) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"HDF: Could not reopen the file (%s) for writing",NrmQuarkToString(rec->file_path_q));
					return(NhlFATAL);
				}

	
				if(no_stride) {
					ret = sd_ncvarputg(cdfid,
						stepvl->var_inq->varid,
						start,
						count,
						NULL,
						NULL,
						data);
				} else {
					ret = sd_ncvarputg(cdfid,
						stepvl->var_inq->varid,
						start,
						count,
						stride,
						NULL,
						data);
				}
	
				sd_ncclose(cdfid);
				if(ret == -1) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"HDF: An error occurred while attempting to write variable (%s) from file (%s)",NrmQuarkToString(thevar),NrmQuarkToString(rec->file_path_q));
					return(NhlFATAL);
				} else {
					return(NhlNOERROR);
				}
			} else {
				stepvl = stepvl->next;
			}
		}
		
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"File (%s) was opened as a read only file, can not write to it",NrmQuarkToString(rec->file_path_q));
	}
	return(NhlFATAL);

	
}
static NhlErrorTypes HDFWriteCoord
#if	NhlNeedProto
(void *therec, NclQuark thevar, void* data, long* start, long* finish,long* stride)
#else
(therec, thevar, data, start, finish,stride)
void *therec;
NclQuark thevar;
void* data;
long* start;
long* finish;
long* stride;
#endif
{
	return(HDFWriteVar(therec,thevar,data,start,finish,stride));
}


static NhlErrorTypes HDFWriteAtt
#if	NhlNeedProto
(void *therec, NclQuark theatt, void *data )
#else
(therec, theatt, data )
void *therec;
NclQuark theatt;
void *data;
#endif
{
	HDFFileRecord* rec = (HDFFileRecord*)therec;
	HDFAttInqRecList *stepal;
	int cdfid;
	int ret = -1;
	int redef;
	char *buffer=NULL;

	if(rec->wr_status <= 0) {
		stepal = rec->file_atts;
		while(stepal != NULL) {
			if(stepal->att_inq->name == theatt) {
				cdfid = sd_ncopen(NrmQuarkToString(rec->file_path_q),NC_WRITE);
				if(cdfid == -1) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"HDF: Could not reopen the file (%s) for writing",NrmQuarkToString(rec->file_path_q));
					return(NhlFATAL);
				}
				if(stepal->att_inq->data_type == NC_CHAR  && !(theatt == Qfill_val || theatt == Qmissing_val)) {
					buffer = NrmQuarkToString(*(NclQuark*)data);
					if (strlen(buffer) == 0) {
						NhlPError(NhlWARNING,NhlEUNKNOWN,
                                      "HDF: The HDF library does not currently allow empty strings as attribute values; attribute (%s) in file (%s) not modified",
							  NrmQuarkToString(theatt),NrmQuarkToString(rec->file_path_q));
						sd_ncclose(cdfid);
						return (NhlWARNING);
					}
					redef = 0;
					if(strlen(buffer)+1 > stepal->att_inq->len) {
						sd_ncredef(cdfid);
						redef = 1;
					}
					ret = sd_ncattput(cdfid,NC_GLOBAL,NrmQuarkToString(theatt),stepal->att_inq->data_type,strlen(buffer)+1,(void*)buffer);
					if (redef)
						sd_ncendef(cdfid);
					if (stepal->att_inq->value != NULL)
						memcpy(stepal->att_inq->value,data,sizeof(NclQuark));
					
				} else {
					ret = sd_ncattput(cdfid,NC_GLOBAL,NrmQuarkToString(theatt),stepal->att_inq->data_type,stepal->att_inq->len,data);
					memcpy(stepal->att_inq->value,data,
					       sd_nctypelen(stepal->att_inq->data_type)*stepal->att_inq->len);
				}
	
	
				sd_ncclose(cdfid);
				if(ret == -1) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"HDF: An error occurred while attempting to write the attribute (%s) to file (%s)",NrmQuarkToString(theatt),NrmQuarkToString(rec->file_path_q));
					return(NhlFATAL);
				}
				return(NhlNOERROR);
			} else {
				stepal = stepal->next;
			}
		}	
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"File (%s) was opened as a read only file, can not write to it",NrmQuarkToString(rec->file_path_q));
	}
	return(NhlFATAL);
}

static NhlErrorTypes HDFDelAtt
#if 	NhlNeedProto
(void *therec, NclQuark theatt)
#else 
(therec, theatt)
void *therec;
NclQuark thevar;
NclQuark theatt;
#endif
{
	HDFFileRecord* rec = (HDFFileRecord*)therec;
	HDFAttInqRecList *stepal,*tmpal;
	int cdfid;
	int ret;

	if(rec->wr_status <= 0) {
		stepal = rec->file_atts;
		if((stepal != NULL) && (stepal->att_inq->name == theatt)) {
			cdfid = sd_ncopen(NrmQuarkToString(rec->file_path_q),NC_WRITE);
			if(cdfid == -1) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"HDF: Could not reopen the file (%s) for writing",NrmQuarkToString(rec->file_path_q));
				return(NhlFATAL);
			}
			sd_ncredef(cdfid);
			ret = sd_ncattdel(cdfid,NC_GLOBAL,(const char*)NrmQuarkToString(theatt));
			sd_ncendef(cdfid);
	
			tmpal = stepal;
			rec->file_atts = stepal->next;
			if(tmpal->att_inq->value) 
				NclFree(tmpal->att_inq->value);
			NclFree(tmpal->att_inq);
			NclFree(tmpal);
			sd_ncclose(cdfid);
			if(ret == -1) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"HDF: An error occurred while attempting to delete the attribute (%s) from file (%s)",NrmQuarkToString(theatt),NrmQuarkToString(rec->file_path_q));
				return(NhlFATAL);
			}
			return(NhlNOERROR);
		} else {
			while(stepal->next != NULL) {
				if(stepal->next->att_inq->name == theatt) {
					cdfid = sd_ncopen(NrmQuarkToString(rec->file_path_q),NC_WRITE);
					if(cdfid == -1) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"HDF: Could not reopen the file (%s) for writing",NrmQuarkToString(rec->file_path_q));
						return(NhlFATAL);
					}

					sd_ncredef(cdfid);
					ret = sd_ncattdel(cdfid,NC_GLOBAL,(const char*)NrmQuarkToString(theatt));
					sd_ncendef(cdfid);
					tmpal = stepal->next;
					stepal->next = stepal->next->next;
					if(tmpal->att_inq->value) 
						NclFree(tmpal->att_inq->value);
					NclFree(tmpal->att_inq);
					NclFree(tmpal);
					sd_ncclose(cdfid);
					if(ret == -1) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"HDF: An error occurred while attempting to delete the attribute (%s) from file (%s)",NrmQuarkToString(theatt),NrmQuarkToString(rec->file_path_q));
						return(NhlFATAL);
					}
					return(NhlNOERROR);
				} else {	
					stepal = stepal->next;
				}
			}	
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"File (%s) was opened as a read only file, can not write to it",NrmQuarkToString(rec->file_path_q));
	}
	return(NhlFATAL);
}

static NhlErrorTypes HDFDelVarAtt
#if 	NhlNeedProto
(void *therec, NclQuark thevar, NclQuark theatt)
#else 
(therec, thevar, theatt)
void *therec;
NclQuark thevar;
NclQuark theatt;
#endif
{
	HDFFileRecord* rec = (HDFFileRecord*)therec;
	HDFAttInqRecList *stepal,*tmpal;
	HDFVarInqRecList *stepvl;
	int cdfid;
	int ret;

	if(rec->wr_status <= 0) {
		stepvl = rec->vars;
		while(stepvl != NULL) {
			if(stepvl->var_inq->name == thevar) {
				stepal = stepvl->var_inq->att_list;
				if((stepal != NULL) && (stepal->att_inq->name == theatt)) {
					cdfid = sd_ncopen(NrmQuarkToString(rec->file_path_q),NC_WRITE);
					if(cdfid == -1) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"HDF: Could not reopen the file (%s) for writing",NrmQuarkToString(rec->file_path_q));
						return(NhlFATAL);
					}
					sd_ncredef(cdfid);
					ret = sd_ncattdel(cdfid,stepvl->var_inq->varid,(const char*)NrmQuarkToString(theatt));
					sd_ncendef(cdfid);
			
					tmpal = stepal;
					stepvl->var_inq->att_list = stepal->next;
					if(tmpal->att_inq->value) 
						NclFree(tmpal->att_inq->value);
					NclFree(tmpal->att_inq);
					NclFree(tmpal);
					sd_ncclose(cdfid);
					if(ret == -1) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"HDF: An error occurred while attempting to delete the attribute (%s) from variable (%s) in file (%s)",NrmQuarkToString(theatt),NrmQuarkToString(thevar),NrmQuarkToString(rec->file_path_q));
						return(NhlFATAL);
					}
					return(NhlNOERROR);
				} else {
					while(stepal->next != NULL) {
						if(stepal->next->att_inq->name == theatt) {
							cdfid = sd_ncopen(NrmQuarkToString(rec->file_path_q),NC_WRITE);
							if(cdfid == -1) {
								NhlPError(NhlFATAL,NhlEUNKNOWN,"HDF: Could not reopen the file (%s) for writing",NrmQuarkToString(rec->file_path_q));
								return(NhlFATAL);
							}

							sd_ncredef(cdfid);
							ret = sd_ncattdel(cdfid,stepvl->var_inq->varid,(const char*)NrmQuarkToString(theatt));
							sd_ncendef(cdfid);
							tmpal = stepal->next;
							stepal->next = stepal->next->next;
							if(tmpal->att_inq->value) 
								NclFree(tmpal->att_inq->value);
							NclFree(tmpal->att_inq);
							NclFree(tmpal);
							sd_ncclose(cdfid);
							if(ret == -1) {
								NhlPError(NhlFATAL,NhlEUNKNOWN,"HDF: An error occurred while attempting to delete the attribute (%s) from variable (%s) in file (%s)",NrmQuarkToString(theatt),NrmQuarkToString(thevar),NrmQuarkToString(rec->file_path_q));
								return(NhlFATAL);
							}
							return(NhlNOERROR);
						} else {	
							stepal = stepal->next;
						}
					}	
				}
			} else {
				stepvl = stepvl->next;
			}
		} 
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"File (%s) was opened as a read only file, can not write to it",NrmQuarkToString(rec->file_path_q));
	}
	return(NhlFATAL);
}
static NhlErrorTypes HDFWriteVarAtt 
#if	NhlNeedProto
(void *therec, NclQuark thevar, NclQuark theatt, void* data)
#else
(therec,thevar, theatt,  data )
void *therec;
NclQuark thevar;
NclQuark theatt;
void* data;
#endif
{
	HDFFileRecord* rec = (HDFFileRecord*)therec;
	HDFAttInqRecList *stepal;
	HDFVarInqRecList *stepvl;
	int cdfid;
	int ret;
	char * buffer = NULL;
	
	
	/*
	 * The "hdf_name" attribute does not exist for writing
	 */
	if (theatt == NrmStringToQuark("hdf_name"))
		return(NhlNOERROR);
	else if (theatt == NrmStringToQuark("hdf_group"))
		return(NhlNOERROR);
	else if (theatt == NrmStringToQuark("hdf_group_id"))
		return(NhlNOERROR);
	if(rec->wr_status <= 0) {
		stepvl = rec->vars;
		while(stepvl != NULL) {
			if(stepvl->var_inq->name == thevar) {
				stepal = stepvl->var_inq->att_list;
				while(stepal != NULL) {
					if(stepal->att_inq->name == theatt) {
						cdfid = sd_ncopen(NrmQuarkToString(rec->file_path_q),NC_WRITE);
						if(cdfid == -1) {
							NhlPError(NhlFATAL,NhlEUNKNOWN,"HDF: Could not reopen the file (%s) for writing",NrmQuarkToString(rec->file_path_q));
							return(NhlFATAL);
						}
						if(stepal->att_inq->data_type == NC_CHAR && !(theatt == Qfill_val || theatt == Qmissing_val)) {	
							int redef = 0;
							buffer = NrmQuarkToString(*(NclQuark*)data);
							if (strlen(buffer) == 0) {
								NhlPError(NhlWARNING,NhlEUNKNOWN,
                                      "HDF: The HDF library does not currently allow empty strings as attribute values; attribute (%s) of variable (%s) in file (%s) not modified",
									  NrmQuarkToString(theatt),NrmQuarkToString(thevar),NrmQuarkToString(rec->file_path_q));
								sd_ncclose(cdfid);
								return (NhlWARNING);
							}
							redef = 0;

							if(strlen(buffer)  > stepal->att_inq->len) {
								sd_ncredef(cdfid);
								redef = 1;
							}
							ret = sd_ncattput(cdfid,stepvl->var_inq->varid,NrmQuarkToString(theatt),stepal->att_inq->data_type,strlen(buffer),buffer);
							if (redef)
								sd_ncendef(cdfid);

							if (stepal->att_inq->value != NULL)
								memcpy(stepal->att_inq->value,data,sizeof(NclQuark));
						} else {
							sd_ncredef(cdfid);
							ret = sd_ncattput(cdfid,stepvl->var_inq->varid,NrmQuarkToString(theatt),stepal->att_inq->data_type,stepal->att_inq->len,data);
							if (stepal->att_inq->value != NULL) {
								memcpy(stepal->att_inq->value,data,
								       sd_nctypelen(stepal->att_inq->data_type)*stepal->att_inq->len);
							}
							sd_ncendef(cdfid);
						}
		
						sd_ncclose(cdfid);
						if(ret == -1) {
							NhlPError(NhlFATAL,NhlEUNKNOWN,"HDF: An error occurred while attempting to write the attribute (%s) to variable (%s) in file (%s)",NrmQuarkToString(theatt),NrmQuarkToString(thevar),NrmQuarkToString(rec->file_path_q));
							return(NhlFATAL);
						}
						return(NhlNOERROR);
					} else {	
						stepal = stepal->next;
					}
				}	
			} else {
				stepvl = stepvl->next;
			}
		} 
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"File (%s) was opened as a read only file, can not write to it",NrmQuarkToString(rec->file_path_q));
	}
	return(NhlFATAL);
}

static NhlErrorTypes HDFAddDim
#if	NhlNeedProto
(void* therec, NclQuark thedim, ng_size_t size,int is_unlimited)
#else
(therec, thedim, size)
void* therec;
NclQuark thedim;
ng_size_t size;
int is_unlimited;
#endif
{
	HDFFileRecord *rec = (HDFFileRecord*) therec;
	int cdfid;
	HDFDimInqRecList *stepdl;
	int ret = -1;
	int add_scalar = 0;

	if(rec->wr_status <=  0) {
		
		if(thedim == NrmStringToQuark("ncl_scalar")) {
			if (size != 1) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  "HDF: \"ncl_scalar\" is a reserved file dimension name in NCL, this name can only represent dimensions of size 1");
				return(NhlFATAL);
			}
			add_scalar = 1;
		}
		else {
			cdfid = sd_ncopen(NrmQuarkToString(rec->file_path_q),NC_WRITE);
			if(cdfid == -1) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"HDF: Could not reopen the file (%s) for writing",NrmQuarkToString(rec->file_path_q));
				return(NhlFATAL);
			}
			sd_ncredef(cdfid);
			if (is_unlimited) {
				ret = sd_ncdimdef(cdfid,NrmQuarkToString(thedim),(long)0);
			}
			else {
				ret = sd_ncdimdef(cdfid,NrmQuarkToString(thedim),(long)size);
			}
			sd_ncendef(cdfid);
			sd_ncclose(cdfid);
			if(ret == -1) {
				return(NhlFATAL);
			}

		}
		stepdl = rec->dims;

		if (add_scalar) {
			rec->has_scalar_dim = 1;
			rec->dims = (HDFDimInqRecList*)NclMalloc(
				(unsigned) sizeof(HDFDimInqRecList));
			rec->dims->dim_inq = (HDFDimInqRec*)NclMalloc(
				(unsigned)sizeof(HDFDimInqRec));
			rec->dims->next = stepdl;
			rec->dims->dim_inq->dimid = -5;
			rec->dims->dim_inq->size = 1;
			rec->dims->dim_inq->is_unlimited = 0;
			rec->dims->dim_inq->name = NrmStringToQuark("ncl_scalar");
			rec->n_dims++;
		}
		else if(stepdl == NULL) {
			rec->dims = (HDFDimInqRecList*)NclMalloc((unsigned)sizeof(HDFDimInqRecList));
			rec->dims->dim_inq = (HDFDimInqRec*)NclMalloc((unsigned)sizeof(HDFDimInqRec));
			rec->dims->dim_inq->dimid = ret;
			rec->dims->dim_inq->name = thedim;
			rec->dims->dim_inq->size = (long)size;
			rec->dims->dim_inq->is_unlimited = is_unlimited;
			rec->dims->next = NULL;
			rec->n_dims = 1;
		} else {
			while(stepdl->next != NULL) {
				stepdl = stepdl->next;
			}
			stepdl->next = (HDFDimInqRecList*)NclMalloc((unsigned)sizeof(HDFDimInqRecList));
			stepdl->next->dim_inq = (HDFDimInqRec*)NclMalloc((unsigned)sizeof(HDFDimInqRec));
			stepdl->next->dim_inq->dimid = ret;
			stepdl->next->dim_inq->name = thedim;
			stepdl->next->dim_inq->size = (long)size;
			stepdl->next->dim_inq->is_unlimited = is_unlimited;
			stepdl->next->next = NULL;
			rec->n_dims++;
		}	
		return(NhlNOERROR);
	} else {	
		NhlPError(NhlFATAL,NhlEUNKNOWN,"File (%s) was opened as a read only file, can not write to it",NrmQuarkToString(rec->file_path_q));
	}
	return(NhlFATAL);
}
/*ARGSUSED*/
static NhlErrorTypes HDFAddVar
#if	NhlNeedProto
(void* therec, NclQuark thevar, NclBasicDataTypes data_type, int n_dims,NclQuark *dim_names, ng_size_t* dim_sizes)
#else
(therec,thevar,data_type,n_dims,dim_names,dim_sizes)
void* therec;
NclQuark thevar;
NclBasicDataTypes data_type;
int n_dims;
NclQuark *dim_names;
ng_size_t* dim_sizes;
#endif
{
	HDFFileRecord* rec = (HDFFileRecord*)therec;
	HDFVarInqRecList *stepvl = NULL;
	int cdfid,i,ret;
	nc_type *the_data_type;
	int dim_ids[MAX_NC_DIMS];
	HDFDimInqRecList* stepdl = NULL;
	int add_scalar_dim = 0;
	int is_unsigned;

	if(rec->wr_status <= 0) {
		the_data_type = HDFMapFromNcl(data_type);
		if(the_data_type == NULL) {
			return NhlFATAL;
		}
		is_unsigned =  ((int) *the_data_type < NC_UOFFSET) ? 0 : 1;


/*
* All dimensions are correct dimensions for the file
*/
		dim_ids[0] = -999;
		for(i = 0; i < n_dims; i++) {
			stepdl = rec->dims;
			while(stepdl != NULL) {
				if(stepdl->dim_inq->name == dim_names[i]){
					if((n_dims > 1)&&(dim_names[i] == NrmStringToQuark("ncl_scalar"))) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"HDF: the reserved dimension name \"ncl_scalar\" was used in a value with more than one dimension, can not add variable");
						return(NhlFATAL);
					}
					dim_ids[i] = stepdl->dim_inq->dimid;
					break;
				} else {
					stepdl = stepdl->next;
				}
			}
		} 
		if (dim_ids[0] == -999) {
			if (n_dims == 1 && dim_sizes[0] == 1 && dim_names[0] == NrmStringToQuark("ncl_scalar")) {
				dim_ids[0] = -5;
				add_scalar_dim = 1;
			}
			else {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"HDF: internal error adding variable");
				return(NhlFATAL);
			}
		}

		if (! is_unsigned) {
			cdfid = sd_ncopen(NrmQuarkToString(rec->file_path_q),NC_WRITE);
			if(cdfid == -1) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"HDF: Could not reopen the file (%s) for writing",NrmQuarkToString(rec->file_path_q));
				return(NhlFATAL);
			}
			sd_ncredef(cdfid);
			if((n_dims == 1)&&(dim_ids[0] == -5)) {
				ret = sd_ncvardef(cdfid,NrmQuarkToString(thevar),*the_data_type, 0, NULL);
			} else {
				ret = sd_ncvardef(cdfid,NrmQuarkToString(thevar),*the_data_type, n_dims, dim_ids);
			}
			sd_ncendef(cdfid);
			sd_ncclose(cdfid);
			if(ret == -1) {
				NclFree(the_data_type);
				return(NhlFATAL);
			} 

		}
		else {
			int32 sd_id, sds_id;
			int32 hdf_dim_sizes[MAX_NC_DIMS];

			for (i = 0; i < n_dims; i++) {
				hdf_dim_sizes[i] = (int) dim_sizes[i];
			}
			sd_id = SDstart(NrmQuarkToString(rec->file_path_q),DFACC_WRITE);
			sds_id = SDcreate(sd_id,NrmQuarkToString(thevar),(int)*the_data_type - NC_UOFFSET,n_dims,hdf_dim_sizes);
			for (i = 0; i < n_dims; i++) {
				int32 dim_id = SDgetdimid(sds_id,i);
				if (dim_names[i] > NrmNULLQUARK) {
					ret = SDsetdimname(dim_id,NrmQuarkToString(dim_names[i]));
				}
			}
			ret = SDendaccess(sds_id);
			ret = SDend(sd_id);
			ret = sds_id;
		}
		stepvl = rec->vars;
		if(stepvl == NULL) {
			rec->vars = (HDFVarInqRecList*)NclMalloc(
				(unsigned)sizeof(HDFVarInqRecList));
			rec->vars->next = NULL;
			rec->vars->var_inq = (HDFVarInqRec*)NclMalloc(
				(unsigned)sizeof(HDFVarInqRec));
			rec->vars->var_inq->varid = 0;
			rec->vars->var_inq->name = thevar;
			if (! is_unsigned) {
				rec->vars->var_inq->data_type = *the_data_type;
				rec->vars->var_inq->hdf_type = *the_data_type;
			}
			else {
				
				rec->vars->var_inq->data_type = HDFMapToNC(*the_data_type - NC_UOFFSET);
				rec->vars->var_inq->hdf_type = NCMapToHDF(*the_data_type);
			}
			rec->vars->var_inq->n_dims = n_dims;
			rec->vars->var_inq->natts = 0;
			rec->vars->var_inq->att_list = NULL;
			for(i = 0 ; i< n_dims; i++) {
				rec->vars->var_inq->dim[i] = dim_ids[i];
			}
			rec->n_vars = 1;
		} else {
			while(stepvl->next != NULL) {
				stepvl= stepvl->next;
			}
			stepvl->next = (HDFVarInqRecList*)NclMalloc(
				(unsigned)sizeof(HDFVarInqRecList));
			stepvl->next->var_inq = (HDFVarInqRec*)NclMalloc(
				(unsigned)sizeof(HDFVarInqRec));
			stepvl->next->next = NULL;
			stepvl->next->var_inq->varid = rec->n_vars;
			stepvl->next->var_inq->name = thevar;
			stepvl->next->var_inq->data_type = *the_data_type;
			if (! is_unsigned) {
				stepvl->next->var_inq->data_type = *the_data_type;
				stepvl->next->var_inq->hdf_type = *the_data_type;
			}
			else {
				
				stepvl->next->var_inq->data_type = HDFMapToNC(*the_data_type - NC_UOFFSET);
				stepvl->next->var_inq->hdf_type = NCMapToHDF(*the_data_type);
			}
			stepvl->next->var_inq->n_dims = n_dims;
			stepvl->next->var_inq->natts = 0;
			stepvl->next->var_inq->att_list = NULL;
			for(i = 0 ; i< n_dims; i++) {
				stepvl->next->var_inq->dim[i] = dim_ids[i];
			}
			rec->n_vars++;
		}
		if (add_scalar_dim) {
			rec->has_scalar_dim = 1;
			stepdl = rec->dims;
			rec->dims = (HDFDimInqRecList*)NclMalloc(
				(unsigned) sizeof(HDFDimInqRecList));
			rec->dims->dim_inq = (HDFDimInqRec*)NclMalloc(
				(unsigned)sizeof(HDFDimInqRec));
			rec->dims->next = stepdl;
			rec->dims->dim_inq->dimid = -5;
			rec->dims->dim_inq->size = 1;
			rec->dims->dim_inq->is_unlimited = 0;
			rec->dims->dim_inq->name = NrmStringToQuark("ncl_scalar");
			rec->n_dims++;
		}
		NclFree(the_data_type);
		return(NhlNOERROR);
	} else {	
		NhlPError(NhlFATAL,NhlEUNKNOWN,"File (%s) was opened as a read only file, can not write to it",NrmQuarkToString(rec->file_path_q));
	}
	return(NhlFATAL);
}

static NhlErrorTypes HDFAddCoordVar
#if	NhlNeedProto
(void *therec, NclQuark thevar,NclBasicDataTypes data_type)
#else
(therec,thevar,data_type)
void *therec;
NclQuark thevar;
NclBasicDataTypes data_type;
#endif
{
	HDFFileRecord *rec = (HDFFileRecord*)therec;
	HDFDimInqRecList *stepdl = NULL;
	HDFVarInqRecList *stepvl = NULL;
	int cdfid;
	int size,ret = -1;
	nc_type *the_data_type;
	int is_unsigned;
	
	if(rec->wr_status <= 0) {
		the_data_type = HDFMapFromNcl(data_type);
		if (the_data_type == NULL) {
			return NhlFATAL;
		}
		is_unsigned =  ((int) *the_data_type < NC_UOFFSET) ? 0 : 1;

		if (! is_unsigned) {
			cdfid = sd_ncopen(NrmQuarkToString(rec->file_path_q),NC_WRITE);
			if(cdfid == -1) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"HDF: Could not reopen the file (%s) for writing",NrmQuarkToString(rec->file_path_q));
				return(NhlFATAL);
			}
			stepdl = rec->dims;
			while(stepdl != NULL ) {
				if(stepdl->dim_inq->name == thevar){
					sd_ncredef(cdfid);
					size = stepdl->dim_inq->size;
					ret = sd_ncvardef(cdfid,NrmQuarkToString(thevar),*the_data_type,1,&stepdl->dim_inq->dimid);
					if(ret == -1) {
						sd_ncabort(cdfid);
						sd_ncclose(cdfid);
						NclFree(the_data_type);
						return(NhlFATAL);
					} 
				}
			} 
		}
		else {
			int32 sd_id, sds_id;
			int32 dim_id;

			sd_id = SDstart(NrmQuarkToString(rec->file_path_q),DFACC_WRITE);
			stepdl = rec->dims;
			while(stepdl != NULL ) {
				if (stepdl->dim_inq->name == thevar) {
					size = stepdl->dim_inq->size;
					sds_id = SDcreate(sd_id,NrmQuarkToString(thevar),(int)*the_data_type - NC_UOFFSET,1,&size);
				}
			}
			ret = SDendaccess(sds_id);
			ret = SDend(sd_id);
		}
		stepvl = rec->vars;
		if(stepvl == NULL) {
			rec->vars = (HDFVarInqRecList*)NclMalloc(
				(unsigned)sizeof(HDFVarInqRecList));
			rec->vars->next = NULL;
			rec->vars->var_inq = (HDFVarInqRec*)NclMalloc(
				(unsigned)sizeof(HDFVarInqRec*));
			rec->vars->var_inq->varid = 0;
			rec->vars->var_inq->name = thevar;
			if (! is_unsigned) {
				rec->vars->var_inq->data_type = *the_data_type;
				rec->vars->var_inq->hdf_type = *the_data_type;
			}
			else {
				
				rec->vars->var_inq->data_type = HDFMapToNC(*the_data_type - NC_UOFFSET);
				rec->vars->var_inq->hdf_type = NCMapToHDF(*the_data_type);
			}
			rec->vars->var_inq->n_dims = 1;
			rec->vars->var_inq->dim[0] = stepdl->dim_inq->dimid;
			rec->vars->var_inq->natts = 0;
			rec->vars->var_inq->att_list = NULL;
			rec->n_vars++;
		} else {
			while(stepvl->next != NULL) {
				stepvl = stepvl->next;
			}
			stepvl->next = (HDFVarInqRecList*)NclMalloc(
				(unsigned)sizeof(HDFVarInqRecList));
			stepvl->next->next = NULL;
			stepvl->next->var_inq = (HDFVarInqRec*)NclMalloc(
				(unsigned)sizeof(HDFVarInqRec*));
			stepvl->next->var_inq->varid = rec->n_vars;
			stepvl->next->var_inq->name = thevar;
			if (! is_unsigned) {
				stepvl->next->var_inq->data_type = *the_data_type;
				stepvl->next->var_inq->hdf_type = *the_data_type;
			}
			else {
				
				stepvl->next->var_inq->data_type = HDFMapToNC(*the_data_type - NC_UOFFSET);
				stepvl->next->var_inq->hdf_type = NCMapToHDF(*the_data_type);
			}
			stepvl->next->var_inq->n_dims = 1;
			stepvl->next->var_inq->dim[0] = stepdl->dim_inq->dimid;
			stepvl->next->var_inq->natts = 0;
			stepvl->next->var_inq->att_list = NULL;
			rec->n_vars++;
		}
		NclFree(the_data_type);
		return NhlNOERROR;
	}
	else {	
		NhlPError(NhlFATAL,NhlEUNKNOWN,"File (%s) was opened as a read only file, can not write to it",NrmQuarkToString(rec->file_path_q));
	}
	return(NhlFATAL);
}

static NhlErrorTypes HDFRenameDim
#if	NhlNeedProto
(void* therec, NclQuark from, NclQuark to)
#else
(therec, from, to)
void* therec;
NclQuark from;
NclQuark to;
#endif
{
	HDFFileRecord *rec = (HDFFileRecord*)therec;
	HDFDimInqRecList *stepdl;
	int cdfid,ret;

	if(to == NrmStringToQuark("ncl_scalar")) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"HDF : \"ncl_scalar\" is a reserved file dimension name in NCL: other dimensions can not be changed to it");
                return(NhlFATAL);
	}
	stepdl = rec->dims;
	while(stepdl != NULL) {
		if(stepdl->dim_inq->name == from) {
			if(stepdl->dim_inq->dimid == -5) {
				stepdl->dim_inq->name = to;
				return(NhlNOERROR);
			}
			cdfid = sd_ncopen(NrmQuarkToString(rec->file_path_q),NC_WRITE);
			if(cdfid == -1) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"HDF: Could not reopen the file (%s) for writing",NrmQuarkToString(rec->file_path_q));
				return(NhlFATAL);
			}
			sd_ncredef(cdfid);
			ret = sd_ncdimrename(cdfid,stepdl->dim_inq->dimid,NrmQuarkToString(to));
			sd_ncclose(cdfid);
			if(ret == -1) {
				return(NhlFATAL);
			} else {
				stepdl->dim_inq->name = to;
				return(NhlNOERROR);
			}
		} else {
			stepdl = stepdl->next;
		}
	}
	return(NhlFATAL);
}

static char *MissingAtt = "missing";

static NhlErrorTypes HDFAddAtt
#if	NhlNeedProto
(void *therec,NclQuark theatt, NclBasicDataTypes data_type, int n_items, void * values)
#else
(therec,theatt,data_type,n_items,values)
	void *therec;
	NclQuark theatt;
	NclBasicDataTypes data_type;
	int n_items;
	void * values;
#endif
{
	HDFFileRecord *rec = (HDFFileRecord*)therec;
	HDFAttInqRecList* stepal;
	nc_type *the_data_type;
	int i,ret;
	int cdfid;
	void *lvalues = values;
	NhlErrorTypes eret = NhlNOERROR;
	int is_unsigned;

	if(rec->wr_status <= 0) {
		the_data_type = (nc_type*)HDFMapFromNcl(data_type);
		if (the_data_type == NULL) {
			return NhlFATAL;
		}
		is_unsigned =  ((int) *the_data_type < NC_UOFFSET) ? 0 : 1;
		if (*the_data_type == NC_CHAR) {
			if (! lvalues || (strlen((char *) lvalues) == 0)) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,
					  "HDF: The HDF library does not currently allow empty strings as attribute values; not adding attribute (%s) to file (%s)",
					  NrmQuarkToString(theatt),NrmQuarkToString(rec->file_path_q));
				eret = NhlWARNING;
				/* still necessary to add the attribute to avoid an error when it is later deleted */
				lvalues = (void*) MissingAtt;
				n_items = strlen (MissingAtt);
			}
		}
		if (! is_unsigned) {
			cdfid = sd_ncopen(NrmQuarkToString(rec->file_path_q),NC_WRITE);
			if(cdfid == -1) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"HDF: Could not reopen the file (%s) for writing",NrmQuarkToString(rec->file_path_q));
				NclFree(the_data_type);
				return(NhlFATAL);
			}
			sd_ncredef(cdfid);
			ret = sd_ncattput(cdfid,NC_GLOBAL,NrmQuarkToString(theatt),*the_data_type,n_items,lvalues);
			sd_ncendef(cdfid);
			sd_ncclose(cdfid);
		}
		else {
			int32 sd_id, sds_id;
			int32 dim_id;
			int32 ix, type, nvalues;

			sd_id = SDstart(NrmQuarkToString(rec->file_path_q),DFACC_WRITE);
			ret = SDsetattr(sd_id,NrmQuarkToString(theatt),(int)*the_data_type - NC_UOFFSET,n_items,lvalues);
#if 0
			ix = SDfindattr(sd_id,NrmQuarkToString(theatt));
			SDattrinfo(sd_id,ix,NrmQuarkToString(theatt),&type,&nvalues);
#endif
			ret = SDend(sd_id);
		}
		if(ret != -1 ) {
			stepal = rec->file_atts;
			if(stepal == NULL) {
				rec->file_atts = (HDFAttInqRecList*)NclMalloc((unsigned)
									      sizeof(HDFAttInqRecList));
				rec->file_atts->att_inq= (HDFAttInqRec*)NclMalloc((unsigned)sizeof(HDFAttInqRec));
				rec->file_atts->next = NULL;
				rec->file_atts->att_inq->att_num = 1;
				rec->file_atts->att_inq->name = theatt;
				rec->file_atts->att_inq->len = n_items;
				if (! is_unsigned) {
					rec->file_atts->att_inq->data_type = *the_data_type;
					rec->file_atts->att_inq->hdf_type = *the_data_type;
				}
				else {
					rec->file_atts->att_inq->data_type = HDFMapToNC(*the_data_type - NC_UOFFSET);
					rec->file_atts->att_inq->hdf_type = NCMapToHDF(*the_data_type);
				}
				HDFCacheAttValue(rec->file_atts->att_inq,lvalues);
			} else {	
				i = 0;
				while(stepal->next != NULL) {
					stepal = stepal->next; 
					i++;
				}
				stepal->next = (HDFAttInqRecList*)NclMalloc((unsigned)sizeof(HDFAttInqRecList));
				stepal->next->att_inq = (HDFAttInqRec*)NclMalloc((unsigned)sizeof(HDFAttInqRec));
				stepal->next->att_inq->att_num = i;
				stepal->next->att_inq->name = theatt;
				stepal->next->att_inq->len = n_items;
				stepal->next->next = NULL;
				if (! is_unsigned) {
					stepal->next->att_inq->data_type = *the_data_type;
					stepal->next->att_inq->hdf_type = *the_data_type;
				}
				else {
					stepal->next->att_inq->data_type = HDFMapToNC(*the_data_type - NC_UOFFSET);
					stepal->next->att_inq->hdf_type = NCMapToHDF(*the_data_type);
				}
				HDFCacheAttValue(stepal->next->att_inq,lvalues);
			}
			rec->n_file_atts++;
			NclFree(the_data_type);
			return(eret);
		} 
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"File (%s) was opened as a read only file, can not write to it",NrmQuarkToString(rec->file_path_q));
	}
	return(NhlFATAL);
}

static NhlErrorTypes HDFAddVarAtt
#if	NhlNeedProto
(void *therec,NclQuark thevar, NclQuark theatt, NclBasicDataTypes data_type, int n_items, void * values)
#else
(therec,thevar,theatt,data_type,n_items,values)
	void *therec;
	NclQuark thevar;
	NclQuark theatt;
	NclBasicDataTypes data_type;
	int n_items;
	void * values;
#endif
{
	HDFFileRecord *rec = (HDFFileRecord*)therec;
	HDFAttInqRecList* stepal;
	HDFVarInqRecList* stepvl;
	nc_type *the_data_type;
	int i;
	int cdfid,ret;
	void *lvalues = values;
	NhlErrorTypes eret = NhlNOERROR;
	int is_unsigned;
	
	if(rec->wr_status <= 0) {
		the_data_type = (nc_type*)HDFMapFromNcl(data_type);
		if (the_data_type == NULL) {
			return NhlFATAL;
		}
		is_unsigned =  ((int) *the_data_type < NC_UOFFSET) ? 0 : 1;

		if (*the_data_type == NC_CHAR) {
			if (! lvalues || (strlen((char *) lvalues) == 0)) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,
					  "HDF: The HDF library does not currently allow empty strings as attribute values; not adding attribute (%s) to variable (%s) in file (%s)",
					  NrmQuarkToString(theatt),NrmQuarkToString(thevar), NrmQuarkToString(rec->file_path_q));
				eret = NhlWARNING;
				/* still necessary to add the attribute to avoid an error when it is later deleted */
				lvalues = (void*) MissingAtt;
				n_items = strlen(MissingAtt);
			}
		}
		if (! is_unsigned) {
			cdfid = sd_ncopen(NrmQuarkToString(rec->file_path_q),NC_WRITE);
			if(cdfid == -1) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"HDF: Could not reopen the file (%s) for writing",NrmQuarkToString(rec->file_path_q));
				NclFree(the_data_type);
				return(NhlFATAL);
			}
			stepvl = rec->vars;	
			while(stepvl != NULL) {
				if(stepvl->var_inq->name == thevar) {
					break;
				} else {
					stepvl = stepvl->next;
				}
			}
			if (stepvl == NULL) {
				NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Invalid state"));
				return NhlFATAL;
			}
			sd_ncredef(cdfid);
			ret = sd_ncattput(cdfid,stepvl->var_inq->varid,NrmQuarkToString(theatt),*the_data_type,n_items,lvalues);
			sd_ncendef(cdfid);
			sd_ncclose(cdfid);
		}
		else {
			int32 sd_id, sds_id;
			int32 dim_id;

			stepvl = rec->vars;	
			while(stepvl != NULL) {
				if(stepvl->var_inq->name == thevar) {
					break;
				} else {
					stepvl = stepvl->next;
				}
			}
			if (stepvl == NULL) {
				NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Invalid state"));
				return NhlFATAL;
			}
			sd_id = SDstart(NrmQuarkToString(rec->file_path_q),DFACC_WRITE);
			sds_id = SDselect(sd_id, stepvl->var_inq->varid);
			ret = SDsetattr(sds_id,NrmQuarkToString(theatt),(int)*the_data_type - NC_UOFFSET,n_items,lvalues);
			ret = SDendaccess(sds_id);
			ret = SDend(sd_id);
		}
		if(ret != -1 ) {
			stepal = stepvl->var_inq->att_list;
			if(stepal == NULL) {
				stepvl->var_inq->att_list = (HDFAttInqRecList*)NclMalloc((unsigned)
											sizeof(HDFAttInqRecList));
				stepvl->var_inq->att_list->att_inq = (HDFAttInqRec*)NclMalloc((unsigned)sizeof(HDFAttInqRec));
				stepvl->var_inq->att_list->next = NULL;
				stepvl->var_inq->att_list->att_inq->att_num = 0;
				stepvl->var_inq->att_list->att_inq->name = theatt;
				stepvl->var_inq->att_list->att_inq->len = n_items;
				if (! is_unsigned) {
					stepvl->var_inq->att_list->att_inq->data_type = *the_data_type;
					stepvl->var_inq->att_list->att_inq->hdf_type = *the_data_type;
				}
				else {
					stepvl->var_inq->att_list->att_inq->data_type = HDFMapToNC(*the_data_type - NC_UOFFSET);
					stepvl->var_inq->att_list->att_inq->hdf_type = NCMapToHDF(*the_data_type);
				}
				HDFCacheAttValue(stepvl->var_inq->att_list->att_inq,lvalues);
				stepvl->var_inq->natts = 1;
			} else {	
				i = 0;
				while(stepal->next != NULL) {
					stepal = stepal->next; 
					i++;
				}
				stepal->next = (HDFAttInqRecList*)NclMalloc((unsigned)sizeof(HDFAttInqRecList));
				stepal->next->att_inq = (HDFAttInqRec*)NclMalloc((unsigned)sizeof(HDFAttInqRec));
				stepal->next->next = NULL;
				stepal->next->att_inq->att_num = i;
				stepal->next->att_inq->name = theatt;
				stepal->next->att_inq->len = n_items;
				if (! is_unsigned) {
					stepal->next->att_inq->data_type = *the_data_type;
					stepal->next->att_inq->hdf_type = *the_data_type;
				}
				else {
					stepal->next->att_inq->data_type = HDFMapToNC(*the_data_type - NC_UOFFSET);
					stepal->next->att_inq->hdf_type = NCMapToHDF(*the_data_type);
				}
				HDFCacheAttValue(stepal->next->att_inq,lvalues);
				stepvl->var_inq->natts++ ;
			}
			NclFree(the_data_type);
			return(eret);
		} 
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"File (%s) was opened as a read only file, can not write to it",NrmQuarkToString(rec->file_path_q));
	}
	return(NhlFATAL);
}


NclFormatFunctionRec HDFRec = {
/* NclInitializeFileRecFunc initialize_file_rec */      HDFInitializeFileRec,
/* NclCreateFileFunc	   create_file; */		HDFCreateFile,
/* NclOpenFileFunc         open_file; */		HDFOpenFile,
/* NclFreeFileRecFunc      free_file_rec; */		HDFFreeFileRec,
/* NclGetVarNamesFunc      get_var_names; */		HDFGetVarNames,
/* NclGetVarInfoFunc       get_var_info; */		HDFGetVarInfo,
/* NclGetDimNamesFunc      get_dim_names; */		HDFGetDimNames,
/* NclGetDimInfoFunc       get_dim_info; */		HDFGetDimInfo,
/* NclGetAttNamesFunc      get_att_names; */		HDFGetAttNames,
/* NclGetAttInfoFunc       get_att_info; */		HDFGetAttInfo,
/* NclGetVarAttNamesFunc   get_var_att_names; */	HDFGetVarAttNames,
/* NclGetVarAttInfoFunc    get_var_att_info; */		HDFGetVarAttInfo,
/* NclGetCoordInfoFunc     get_coord_info; */		HDFGetCoordInfo,
/* NclReadCoordFunc        read_coord; */		HDFReadCoord,
/* NclReadCoordFunc        read_coord; */		NULL,
/* NclReadVarFunc          read_var; */			HDFReadVar,
/* NclReadVarFunc          read_var; */			NULL,
/* NclReadAttFunc          read_att; */			HDFReadAtt,
/* NclReadVarAttFunc       read_var_att; */		HDFReadVarAtt,
/* NclWriteCoordFunc       write_coord; */		HDFWriteCoord,
/* NclWriteCoordFunc       write_coord; */		NULL,
/* NclWriteVarFunc         write_var; */		HDFWriteVar,
/* NclWriteVarFunc         write_var; */		NULL,
/* NclWriteAttFunc         write_att; */		HDFWriteAtt,
/* NclWriteVarAttFunc      write_var_att; */		HDFWriteVarAtt,
/* NclAddDimFunc           add_dim; */			HDFAddDim,
/* NclAddChunkDimFunc      add_chunk_dim; */		NULL,
/* NclRenameDimFunc        rename_dim; */		HDFRenameDim,
/* NclAddVarFunc           add_var; */			HDFAddVar,
/* NclAddVarChunkFunc      add_var_chunk; */		NULL,
/* NclAddVarChunkCacheFunc add_var_chunk_cache; */	NULL,
/* NclSetVarCompressLevelFunc set_var_compress_level; */ NULL,
/* NclAddVarFunc           add_coord_var; */		HDFAddCoordVar,
/* NclAddAttFunc           add_att; */			HDFAddAtt,
/* NclAddVarAttFunc        add_var_att; */		HDFAddVarAtt,
/* NclMapFormatTypeToNcl   map_format_type_to_ncl; */	HDFMapToNcl,
/* NclMapNclTypeToFormat   map_ncl_type_to_format; */	HDFMapFromNcl,
/* NclDelAttFunc           del_att; */			HDFDelAtt,
/* NclDelVarAttFunc        del_var_att; */		HDFDelVarAtt,
#include "NclGrpFuncs.null"
/* NclSetOptionFunc        set_option;  */		NULL
};
NclFormatFunctionRecPtr HDFAddFileFormat 
#if	NhlNeedProto
(void)
#else 
()
#endif
{
	
	return(&HDFRec);
}

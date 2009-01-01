
#ifdef NIO_LIB_ONLY
#include "niohlu.h"
#include "nioNresDB.h"
#include "nioCallbacks.h"
#else
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/hlu/Callbacks.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>
#include "defs.h"
#include "Symbol.h"
#include <math.h>
#include "NclVar.h"
#include "NclFile.h"
#include "NclFileInterfaces.h"
#include "DataSupport.h"
#include "VarSupport.h"
#include "NclMultiDValData.h"
#include "NclAtt.h"
#include "AttSupport.h"
#include "NclType.h"
#include "TypeSupport.h"
#include "FileSupport.h"
#include "NclMdInc.h"
#include "NclCoordVar.h"
#include "NclCallBacksI.h"

extern int grib_version;

#define NCLFILE_INC -1
#define NCLFILE_DEC -2
#define NCLFILE_VEC 0
static NclQuark FileGetDimName(
#if	NhlNeedProto
NclFile /* thefile */,
int /*num*/
#endif
);

static NclObjTypes FileVarRepValue(
#if	NhlNeedProto
NclFile /* thefile */,
NclQuark /* var */
#endif
);

#define FILE_COORD_VAR_ACCESS 0
#define FILE_VAR_ACCESS 1

static int FileIsVar(
#if	NhlNeedProto
NclFile /*thefile */,
NclQuark /* name */
#endif
);

static NhlErrorTypes FileWriteVarVar(
#if	NhlNeedProto
NclFile /*thefile */,
NclQuark /*lhs_var*/,
struct _NclSelectionRecord * /* lhs_sel_ptr */,
struct _NclVarRec* /* rhs_var*/,
struct _NclSelectionRecord * /* rhs_sel_ptr */
#endif
);

static NhlErrorTypes FileWriteVar(
#if	NhlNeedProto
NclFile /*thefile */,
NclQuark /*var_name*/,
struct _NclMultiDValDataRec * /* value */,
struct _NclSelectionRecord * /* sel_ptr */
#endif
);

static struct _NclVarRec *FileReadVar(
#if	NhlNeedProto
NclFile /*thefile*/,
NclQuark /* var_name */,
struct _NclSelectionRecord * /*sel_ptr*/
#endif
);

static struct _NclMultiDValDataRec * FileReadVarValue(
#if	NhlNeedProto
NclFile /*thefile*/,
NclQuark /*var_name */,
struct _NclSelectionRecord * /*sel_ptr*/
#endif
);

static int FileIsAtt(
#if	NhlNeedProto
NclFile /*thefile */,
NclQuark /* name */
#endif
);

static struct _NclMultiDValDataRec* FileReadAtt(
#if	NhlNeedProto
NclFile /* thefile */,
NclQuark /* attname */,
struct _NclSelectionRecord* /* sel_ptr*/
#endif
);

static NhlErrorTypes FileWriteAtt(
#if	NhlNeedProto
NclFile /*thefile */,
NclQuark /*attname */,
struct _NclMultiDValDataRec* /*value*/,
struct _NclSelectionRecord * /*sel_ptr*/
#endif
);

static int FileIsVarAtt(
#if	NhlNeedProto
NclFile /*file*/,
NclQuark /* var */,
NclQuark /* attname*/
#endif
);

static struct _NclMultiDValDataRec *FileReadVarAtt(
#if	NhlNeedProto
NclFile /* thefile */,
NclQuark /* var */,
NclQuark /* attname */,
struct _NclSelectionRecord* /*sel_ptr*/
#endif
);

static NhlErrorTypes FileWriteVarAtt(
#if	NhlNeedProto
NclFile /* thefile */,
NclQuark /* var */,
NclQuark /* attname */,
struct _NclMultiDValDataRec* /*value*/,
struct _NclSelectionRecord* /* sel_ptr*/
#endif
);

static int FileIsDim(
#if	NhlNeedProto
NclFile /*thefile */,
NclQuark /* dimname */
#endif
);
static int FileVarIsDim(
#if	NhlNeedProto
NclFile /*thefile */,
NclQuark /* var */,
NclQuark /* dimname */
#endif
);

static struct _NclMultiDValDataRec *FileVarReadDim(
#if	NhlNeedProto
NclFile /*thefile*/,
NclQuark /* var */,
NclQuark /*dim_name*/,
long /*dim_num*/
#endif
);

static NhlErrorTypes FileVarWriteDim(
#if	NhlNeedProto
NclFile /*thefile*/,
NclQuark /* var */,
NclQuark /*dim_name*/,
long /*dim_num */
#endif
);
static struct _NclMultiDValDataRec *FileReadDim(
#if	NhlNeedProto
NclFile /*thefile*/,
NclQuark /*dim_name*/,
long /*dim_num*/
#endif
);

static NhlErrorTypes FileWriteDim(
#if	NhlNeedProto
NclFile /*thefile*/,
NclQuark /*dim_name*/,
long /*dim_num */
#endif
);

static int FileIsCoord(
#if	NhlNeedProto
NclFile /*thefile */,
NclQuark /*coord_name */
#endif
);

static struct _NclVarRec *FileReadCoord(
#if	NhlNeedProto
NclFile /* thefile */,
NclQuark /* coord_name */,
struct _NclSelectionRecord* /* sel_ptr */
#endif
);

static NhlErrorTypes FileWriteCoord(
#if	NhlNeedProto
NclFile /* thefile */,
NclQuark /* coord_name */,
struct _NclMultiDValDataRec* /*value*/,
struct _NclSelectionRecord* /* sel_ptr */
#endif
);

/*
 * Updates the dimension info
 */

static NhlErrorTypes UpdateDims 
#if	NhlNeedProto
(
	NclFile  thefile
)
#else
(thefile)
NclFile thefile;
#endif
{
	NclQuark *name_list;
	int n_names;
	int i;
	int index;

	name_list = (*thefile->file.format_funcs->get_dim_names)(thefile->file.private_rec,&n_names);
	thefile->file.n_file_dims = n_names;
	for(i = 0; i < n_names; i++){
		if (thefile->file.file_dim_info[i])
			NclFree(thefile->file.file_dim_info[i]);
		thefile->file.file_dim_info[i] = (thefile->file.format_funcs->get_dim_info)
			(thefile->file.private_rec,name_list[i]);
		index = FileIsVar(thefile,name_list[i]);
		if(index > -1 && thefile->file.var_info[index]->num_dimensions == 1) {
			thefile->file.coord_vars[i] = thefile->file.var_info[index];
		}
	}
	NclFree((void*)name_list);
}

/*
 * Updates the coord info
 */

static NhlErrorTypes UpdateCoordInfo 
#if	NhlNeedProto
(
	NclFile  thefile,
	NrmQuark varname
)
#else
(thefile)
NclFile thefile;
#endif
{
	int dimid;
	int index;

	dimid = FileIsDim(thefile,varname);
	if (dimid >  -1) {
		index = FileIsVar(thefile,varname);
		if(index > -1 && thefile->file.var_info[index]->num_dimensions == 1) {
			thefile->file.coord_vars[dimid] = thefile->file.var_info[index];
		}
	}
	return NhlNOERROR;
}

static void AdjustForScalarDim
#if	NhlNeedProto
(NclFile thefile)
#else
(thefile)
NclFile thefile;
#endif
{
	int i, j;

	/* since the scalar dim is always first, all the other dims and coord vars need to shift down one element */

	for (i = thefile->file.n_file_dims; i > 0; i--) {
		thefile->file.file_dim_info[i] = thefile->file.file_dim_info[i-1];
		thefile->file.coord_vars[i] = thefile->file.coord_vars[i-1];
	}

	/* also all existing var dim numbers need to be incremented by 1 */
	/* note that the coord_vars field is just a pointer to the matching var_info field, so don't adjust them separately */ 

	for (i = 0; i < thefile->file.n_vars; i++) {
		for (j = 0; j < thefile->file.var_info[i]->num_dimensions; j++) {
			thefile->file.var_info[i]->file_dim_num[j]++;
		}
	}
	thefile->file.file_dim_info[0] = (*thefile->file.format_funcs->get_dim_info)(thefile->file.private_rec,NrmStringToQuark("ncl_scalar"));
	thefile->file.coord_vars[0] = NULL; /* no coord var for a scalar obviously */
	thefile->file.n_file_dims++;
}



static NhlErrorTypes FileAddDim
#if  NhlNeedProto
(NclFile thefile, NclQuark dimname, int dimsize, int is_unlimited)
#else
(thefile, dimname, dimsize, is_unlimited)
NclFile thefile;
NclQuark dimname;
int dimsize;
int is_unlimited;
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	
	if((thefile->file.wr_status <= 0)&&(thefile->file.format_funcs->add_dim != NULL)) {
		if (dimname == NrmStringToQuark("ncl_scalar")) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"FileAddDim:\"ncl_scalar\" is a reserved file dimension name in NCL; it cannot be defined by the user");
			return(NhlWARNING);
		}
		if((FileIsDim(thefile,dimname)) == -1) {
			ret = (*thefile->file.format_funcs->add_dim)(
				thefile->file.private_rec,
				dimname,
				dimsize,
				is_unlimited);
			if(ret < NhlWARNING) 
				return(ret);
			thefile->file.file_dim_info[thefile->file.n_file_dims] = (*thefile->file.format_funcs->get_dim_info)(thefile->file.private_rec,dimname);
			thefile->file.n_file_dims++;
			return(NhlNOERROR);
		} else {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"FileAddDim: Dimension %s is already defined",NrmQuarkToString(dimname));
			return(NhlWARNING);
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"FileAddDim: file (%s) was opened for reading only, can not write",NrmQuarkToString(thefile->file.fname));
	}
	return(NhlFATAL);
}

static NhlErrorTypes FileAddVar
#if	NhlNeedProto
(NclFile thefile, NclQuark varname, NclQuark type, int n_dims, NclQuark *dimnames)
#else
(thefile, varname, type, n_dims, dimnames)
NclFile thefile;
NclQuark varname;
NclQuark type;
int n_dims;
NclQuark *dimnames;
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	long dim_sizes[NCL_MAX_DIMENSIONS];
	int i,j;
	NclTypeClass typec;
	int dindex;
	int add_scalar_dim = 0;
	
	if((thefile->file.wr_status <= 0)&&(thefile->file.format_funcs->add_var != NULL)) {
		if((FileIsVar(thefile,varname)) == -1) {
			for(i = 0; i < n_dims; i++) {
				dindex = FileIsDim(thefile,dimnames[i]);
				if(dindex == -1) {
					if (n_dims == 1 && dimnames[0] == NrmStringToQuark("ncl_scalar")) {
						add_scalar_dim = 1;
						dim_sizes[i] = 1;
					}
					else {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"FileAddVar: Dimension (%s) is not currently defined, can't add variable",NrmQuarkToString(dimnames[i]));
						return(NhlFATAL);
					}
				} else {
					dim_sizes[i] = thefile->file.file_dim_info[i]->dim_size;
				}
					
			}
			typec = _NclNameToTypeClass(type); 
			if(typec != NULL) {
				ret = (*thefile->file.format_funcs->add_var)(
					thefile->file.private_rec,
					varname,
					typec->type_class.data_type,	
					n_dims,
					dimnames,
					dim_sizes
					);
				if(ret == NhlFATAL) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"FileAddVar: an error occurred while adding a variable to a file, check to make sure data type is supported by the output format");
				}
			} else {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"FileAddVar Incorrect type specified, can't add variable (%s)",NrmQuarkToString(varname));
				ret = NhlFATAL;
			}
			if(ret < NhlWARNING) 
				return(ret);
			if (add_scalar_dim) {
				AdjustForScalarDim(thefile);
			}
			thefile->file.var_info[thefile->file.n_vars] = (*thefile->file.format_funcs->get_var_info)(thefile->file.private_rec,varname);
			thefile->file.var_att_info[thefile->file.n_vars] = NULL;
			thefile->file.var_att_ids[thefile->file.n_vars] = -1;
			
			thefile->file.n_vars++;
			UpdateCoordInfo(thefile,varname); 
			return(NhlNOERROR);
		} else {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"FileAddVar: Variable %s is already defined, can not redefine",NrmQuarkToString(varname));
			return(NhlWARNING);
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"FileAddVar: file (%s) was opened for reading only, can not write",NrmQuarkToString(thefile->file.fname));
	}
	return(NhlFATAL);
}
static void FileAttIsBeingDestroyedNotify
#if     NhlNeedProto
(NhlArgVal cbdata, NhlArgVal udata)
#else
(cbdata, udata)
NhlArgVal cbdata;
NhlArgVal udata;
#endif
{
        NclAtt theattobj;
	NclFile thefile;
	NclQuark attname;
	NclQuark thevar;
	int index;
	NclFileAttInfoList *thelist;
	NclMultiDValData tmp_md;
	void *val;

	theattobj = (NclAtt)_NclGetObj(((FileCallBackRec*)udata.ptrval)->theattid);
	thefile = (NclFile)_NclGetObj(((FileCallBackRec*)udata.ptrval)->thefileid);
	thevar = ((FileCallBackRec*)udata.ptrval)->thevar;
	attname = cbdata.lngval;

	if((theattobj == NULL) ||(thefile == NULL)) return;

	if(thevar != -1) {
		if(_NclFileDeleteVarAtt(thefile,thevar,attname) < NhlNOERROR) {

/*
* The premis here is that the above only fails when deleting attributes is not
* supported or when the permissions were wrong. Therefore the following is
* valid
*/
			index = _NclFileIsVar(thefile,thevar);
			thelist = thefile->file.var_att_info[index] ;
			while(thelist != NULL) {
				if(thelist->the_att->att_name_quark ==  attname) {
					if(thefile->file.format_funcs->read_var_att != NULL) {
                                		val = NclMalloc(_NclSizeOf(thelist->the_att->data_type)* thelist->the_att->num_elements );
                                		(void)(*thefile->file.format_funcs->read_var_att)(
                                        		thefile->file.private_rec,
                                        		thevar,
                                        		thelist->the_att->att_name_quark,
                                        		val
                                        		);
                                		tmp_md = _NclCreateMultiDVal(
                                                		NULL,
                                                		NULL,
                                                		Ncl_MultiDValData,
                                                		0,
                                                		val,
                                                		NULL,
                                                		1,
                                                		&thelist->the_att->num_elements,
                                                		TEMPORARY,
                                                		NULL,
                                                		_NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(thelist->the_att->data_type))
                                                	);
                                			if(tmp_md != NULL) {
                                        			_NclAddAtt(thefile->file.var_att_ids[index],NrmQuarkToString(thelist->the_att->att_name_quark),tmp_md,NULL);
                                			}
					}
					return;
				} else {
					thelist = thelist->next;
				}
			}
		}
	} else {
		if(_NclFileDeleteAtt(thefile,attname)<NhlNOERROR) {
			index = _NclFileIsAtt(thefile,attname);
                	if(thefile->file.format_funcs->read_att != NULL) {
                                val = NclMalloc(_NclSizeOf(thefile->file.file_atts[index]->data_type)* thefile->file.file_atts[index]->num_elements );
                                (void)(*thefile->file.format_funcs->read_att)(
                                        thefile->file.private_rec,
                                        thefile->file.file_atts[index]->att_name_quark,
                                        val
                                        );
                                tmp_md = _NclCreateMultiDVal(
                                                NULL,
                                                NULL,
                                                Ncl_MultiDValData,
                                                0,
                                                val,
                                                NULL,
                                                1,
                                                &thefile->file.file_atts[index]->num_elements,
                                                TEMPORARY,
                                                NULL,
                                                _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(thefile->file.file_atts[index]->data_type)));
                                if(tmp_md != NULL) {
                                        _NclAddAtt(thefile->file.file_atts_id,NrmQuarkToString(thefile->file.file_atts[index]->att_name_quark),tmp_md,NULL);
				 }
			}
		}
	}
}

static void LoadVarAtts
#if     NhlNeedProto
(NclFile thefile, NclQuark var)
#else
(NclFile thefile, NclQuark var)
NclFile thefile;
NclQuark var;
#endif
{
	int index;
        NclFileAttInfoList *step;
        int att_id = -1;
        void *val;
        NclMultiDValData tmp_md;
        NhlArgVal udata;
	

	index = FileIsVar(thefile,var);
        if(index > -1) {
		if(thefile->file.var_att_ids[index] == -1) {
			step = thefile->file.var_att_info[index];
			att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,(NclObj)thefile);
			while(step != NULL) {
				if (step->the_att->data_type == NCL_none)
					val = NULL;
				else {
					val = NclMalloc(_NclSizeOf(step->the_att->data_type)* step->the_att->num_elements );
					(void)(*thefile->file.format_funcs->read_var_att)(
						thefile->file.private_rec,
						var,
						step->the_att->att_name_quark,
						val
						);
				}
				tmp_md = _NclCreateMultiDVal(
						NULL,
						NULL,
						Ncl_MultiDValData,
						0,
						val,
						NULL,
						1,
						&step->the_att->num_elements,
						TEMPORARY,
						NULL,
						_NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(step->the_att->data_type))
						);
				if(tmp_md != NULL) {
					_NclAddAtt(att_id,NrmQuarkToString(step->the_att->att_name_quark),tmp_md,NULL);
				}
				step = step->next;
			}
			udata.ptrval = (void*)NclMalloc(sizeof(FileCallBackRec));
			((FileCallBackRec*)udata.ptrval)->thefileid = thefile->obj.id;
			((FileCallBackRec*)udata.ptrval)->theattid = att_id;
			((FileCallBackRec*)udata.ptrval)->thevar = var;
			thefile->file.var_att_cb[index] = _NclAddCallback((NclObj)_NclGetObj(att_id),NULL,FileAttIsBeingDestroyedNotify,ATTDESTROYED,&udata);
			thefile->file.var_att_udata[index] = (FileCallBackRec*)udata.ptrval;
			thefile->file.var_att_ids[index] = att_id;
			return;
		}
	}
}

NhlErrorTypes FilePrint
#if	NhlNeedProto
(NclObj self, FILE    *fp)
#else
(self, fp)
NclObj self;
FILE    *fp;
#endif
{
	NclFile thefile = (NclFile)self;
	int i,j;
	NclFileAttInfoList* step;
	int ret = 0;
	NclMultiDValData tmp_md;
	NhlErrorTypes ret1 = NhlNOERROR;

	

	ret = nclfprintf(fp,"\nfilename:\t%s\n",NrmQuarkToString(thefile->file.fname));
	if(ret < 0) {	
		return(NhlWARNING);
	}
	ret = nclfprintf(fp,"path:\t%s\n",NrmQuarkToString(thefile->file.fpath));
	if(ret < 0) {	
		return(NhlWARNING);
	}
	ret = nclfprintf(fp,"   file global attributes:\n");
	if(ret < 0) {	
		return(NhlWARNING);
	}
	for(i = 0; i < thefile->file.n_file_atts; i++) {
		if(thefile->file.file_atts[i] != NULL) {
			ret = nclfprintf(fp,"      %s : ",NrmQuarkToString(thefile->file.file_atts[i]->att_name_quark));
			if(ret < 0) {	
				return(NhlWARNING);
			}
			if(thefile->file.file_atts[i]->num_elements == 1) {
				tmp_md = _NclFileReadAtt(thefile,thefile->file.file_atts[i]->att_name_quark,NULL);
				ret1 = _Nclprint(tmp_md->multidval.type,fp,tmp_md->multidval.val);
				if(ret < NhlINFO) {	
					return(NhlWARNING);
				}
				ret = nclfprintf(fp,"\n");
				if(ret < 0) {	
					return(NhlWARNING);
				}
			} else if (thefile->file.file_atts[i]->num_elements > 1 &&
				   thefile->file.file_atts[i]->num_elements < 11) {
				tmp_md = _NclFileReadAtt(thefile,thefile->file.file_atts[i]->att_name_quark,NULL);
				ret = nclfprintf(fp,"( ");
				if(ret < 0) {
					return(NhlWARNING);
				}
				for (j = 0; j < tmp_md->multidval.totalelements; j++) {
					char *val = (char*)tmp_md->multidval.val + 
						j * tmp_md->multidval.type->type_class.size; 
					ret1 = _Nclprint(tmp_md->multidval.type,fp,val);
					if(ret1 < NhlINFO) {
						return(ret1);
					}
					if (j < tmp_md->multidval.totalelements - 1) {
						ret = nclfprintf(fp,", ");
						if(ret < 0) {
							return(NhlWARNING);
						}
					}
				}
				ret = nclfprintf(fp," )\n");
				if(ret < 0) {
					return(NhlWARNING);
				}
			}
			else {
				ret = nclfprintf(fp,"<ARRAY of %d elements>\n",thefile->file.file_atts[i]->num_elements);
				if(ret < 0) {	
					return(NhlWARNING);
				}
			}
		}
	}
	ret = nclfprintf(fp,"   dimensions:\n");
	if(ret < 0) {	
		return(NhlWARNING);
	}
	for(i = 0; i< thefile->file.n_file_dims; i++) {
		if(thefile->file.file_dim_info[i]->is_unlimited) {
		  	ret = nclfprintf(fp,"      %s = %ld  // unlimited\n",NrmQuarkToString(thefile->file.file_dim_info[i]->dim_name_quark), thefile->file.file_dim_info[i]->dim_size);
		} else {
		 	ret = nclfprintf(fp,"      %s = %ld\n",NrmQuarkToString(thefile->file.file_dim_info[i]->dim_name_quark),thefile->file.file_dim_info[i]->dim_size);
		}
		if(ret < 0) {	
			return(NhlWARNING);
		}
	}
	ret = nclfprintf(fp,"   variables:\n");
	if(ret < 0) {	
		return(NhlWARNING);
	}
	for(i = 0; i < thefile->file.n_vars; i++) {
		if(thefile->file.var_info[i] != NULL) {
			ret = nclfprintf(fp,"      %s %s ( ",_NclBasicDataTypeToName(thefile->file.var_info[i]->data_type),NrmQuarkToString(thefile->file.var_info[i]->var_name_quark));
			if(ret < 0) {	
				return(NhlWARNING);
			}
			for(j=0; j< thefile->file.var_info[i]->num_dimensions - 1; j++) {
				ret = nclfprintf(fp,"%s, ",NrmQuarkToString(FileGetDimName(thefile,thefile->file.var_info[i]->file_dim_num[j])));
				if(ret < 0) {	
					return(NhlWARNING);
				}
			}
			ret = nclfprintf(fp,"%s )\n",NrmQuarkToString(FileGetDimName(thefile,thefile->file.var_info[i]->file_dim_num[thefile->file.var_info[i]->num_dimensions - 1])));
			if(ret < 0) {	
				return(NhlWARNING);
			}
			step = thefile->file.var_att_info[i];
			while(step != NULL) {
				ret = nclfprintf(fp,"         %s :\t", NrmQuarkToString(step->the_att->att_name_quark));
				if(ret < 0) {	
					return(NhlWARNING);
				}
				if(step->the_att->num_elements == 1) {
					tmp_md = _NclFileReadVarAtt(thefile,thefile->file.var_info[i]->var_name_quark,step->the_att->att_name_quark,NULL);
					ret1 = _Nclprint(tmp_md->multidval.type,fp,tmp_md->multidval.val);
					if(ret1 < NhlINFO) {	
						return(NhlWARNING);
					}
					ret = nclfprintf(fp,"\n");
					if(ret < 0) {	
						return(NhlWARNING);
					}
				} else if (step->the_att->num_elements > 1 &&
					   step->the_att->num_elements < 11) {
					tmp_md = _NclFileReadVarAtt(thefile,thefile->file.var_info[i]->var_name_quark,step->the_att->att_name_quark,NULL);
					ret = nclfprintf(fp,"( ");
					if(ret < 0) {
						return(NhlWARNING);
					}
					for (j = 0; j < tmp_md->multidval.totalelements; j++) {
						char *val = (char*)tmp_md->multidval.val + 
							j * tmp_md->multidval.type->type_class.size; 
						ret1 = _Nclprint(tmp_md->multidval.type,fp,val);
						if(ret1 < NhlINFO) {
							return(ret1);
						}
						if (j < tmp_md->multidval.totalelements - 1) {
							ret = nclfprintf(fp,", ");
							if(ret < 0) {
								return(NhlWARNING);
							}
						}
					}
					ret = nclfprintf(fp," )\n");
					if(ret < 0) {
						return(NhlWARNING);
					}
				} else {
					ret = nclfprintf(fp,"<ARRAY of %d elements>\n",step->the_att->num_elements);
					if(ret < 0) {	
						return(NhlWARNING);
					}
				}
				step = step->next;
			}
			ret = nclfprintf(fp,"\n");
			if(ret < 0) {	
				return(NhlWARNING);
			}
		}
	}
	
	return(NhlNOERROR);;
}

void FileDestroy
#if	NhlNeedProto
(NclObj self)
#else
(self)
NclObj self;
#endif
{
	NclFile thefile = (NclFile) self;
	int i;
	NclFileAttInfoList *step,*tmp;
	NclRefList *p, *pt;

	_NclUnRegisterObj((NclObj)self);
	if(thefile->file.format_funcs->free_file_rec != NULL) {
		(*thefile->file.format_funcs->free_file_rec)(thefile->file.private_rec);
	}
	for(i =0 ; i < thefile->file.n_vars; i++) {
		NclFree(thefile->file.var_info[i]);
		if(thefile->file.var_att_cb[i] != NULL) {
			NclFree(thefile->file.var_att_udata[i]);
			_NhlCBDelete(thefile->file.var_att_cb[i]);
		}
		if(thefile->file.var_att_ids[i]!= -1) {
			_NclDelParent(_NclGetObj(thefile->file.var_att_ids[i]),self);
		}
		step = thefile->file.var_att_info[i];	
		while(step != NULL) {
			NclFree(step->the_att);
			tmp = step;
			step = step->next;
			NclFree(tmp);
		}
	}
	for(i =0 ; i < thefile->file.n_file_dims; i++) {
		NclFree(thefile->file.file_dim_info[i]);
	}
	if(thefile->file.file_atts_id != -1) {
		NclFree(thefile->file.file_att_udata);
		_NhlCBDelete(thefile->file.file_att_cb);
		_NclDelParent(_NclGetObj(thefile->file.file_atts_id),self);
	}
	for(i =0 ; i < thefile->file.n_file_atts; i++) {
		NclFree(thefile->file.file_atts[i]);
	}
	if(thefile->obj.cblist != NULL) {
		_NhlCBDestroy(thefile->obj.cblist);
	}
	p = thefile->obj.parents;
	while (p) {
		pt = p;
		p = p->next;
		NclFree(pt);
	}
		
		
	NclFree(thefile);
	return;
}

NhlErrorTypes FileAddParent
#if	NhlNeedProto
(struct _NclObjRec *theobj, struct _NclObjRec *parent)
#else
(theobj, parent)
struct _NclObjRec *theobj;
struct _NclObjRec *parent;
#endif
{
	NclRefList * tmp = NULL;

        tmp = theobj->obj.parents;
        theobj->obj.parents = NclMalloc((unsigned)sizeof(NclRefList));
        theobj->obj.parents->next = tmp;
        theobj->obj.parents->pid = parent->obj.id;
        theobj->obj.ref_count++;
        return(NhlNOERROR);

}

NhlErrorTypes FileDelParent
#if	NhlNeedProto
(struct _NclObjRec *theobj, struct _NclObjRec *parent)
#else
(theobj, parent)
struct _NclObjRec *theobj;
struct _NclObjRec *parent;
#endif
{
	NclRefList *tmp,*tmp1;
        int found = 0;

        if(theobj->obj.parents == NULL) {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"FileDelParent: Attempt to delete parent from empty list");
                return(NhlFATAL);
        }

        tmp = theobj->obj.parents;
        if((tmp!=NULL)&&(tmp->pid == parent->obj.id)) {
                theobj->obj.parents = theobj->obj.parents->next;
                NclFree(tmp);
                tmp = theobj->obj.parents;
                found = 1;
                theobj->obj.ref_count--;
        }
        if((tmp == NULL)&&(found)) {
                _NclDestroyObj(theobj);
                return(NhlNOERROR);
        }
        while(tmp->next != NULL) {
                if(tmp->next->pid == parent->obj.id) {
                        found = 1;
                        tmp1 = tmp->next;
                        tmp->next = tmp->next->next;
                        NclFree(tmp1);
                        theobj->obj.ref_count--;
                        if(theobj->obj.ref_count <= 0)
                                _NclDestroyObj(theobj);
                        return(NhlNOERROR);
                } else {
                        tmp = tmp->next;
                }
        }
        return(NhlWARNING);

}
static NhlErrorTypes InitializeFileClass(
#if NhlNeedProto
void
#endif
);

static void * FileObtainCallData
#if NhlNeedProto
(NclObj obj, unsigned int type)
#else
(obj, type)
NclObj obj;
unsigned int type;
#endif
{
	NclFileClassInfo *tmp = NclMalloc(sizeof(NclFileClassInfo));
	NclFile file = (NclFile)obj;
	
	tmp->obj.obj_id = obj->obj.id;
	tmp->obj.obj_type = NCLFile;
	tmp->file.fname = file->file.fname;
	tmp->file.fpath = file->file.fpath;
	tmp->file.wr_status = file->file.wr_status;
	return((void*)tmp);

}

static NhlErrorTypes FileDelAtt
#if     NhlNeedProto
(NclFile thefile, NclQuark attname)
#else
(thefile, attname)
NclFile thefile;
NclQuark attname;
#endif
{
	int index;
	NhlErrorTypes ret = NhlNOERROR;
	NclFAttRec *tmpal;

	if(thefile->file.wr_status <= 0) {
		index = _NclFileIsAtt(thefile,attname);
		if(index != -1) {
			if(thefile->file.format_funcs->del_att != NULL) {
				ret = (*thefile->file.format_funcs->del_att)(thefile->file.private_rec,attname);
				if(ret < NhlNOERROR) {
					return(ret);
				}
				if(thefile->file.file_atts[index] != NULL) {
					tmpal = thefile->file.file_atts[index];
					for(;index < thefile->file.n_file_atts - 1;index++) {
						thefile->file.file_atts[index] = thefile->file.file_atts[index+1];
					}
					thefile->file.n_file_atts--;
					NclFree(tmpal);
					return(ret);
				}
			} else {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Attribute deletion not supported by format");
				return(NhlFATAL);
			}
		} else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Attempt to delete undefined attribute from file");
			return(NhlFATAL);
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"FileDelAtt: file (%s) is read only, can not delete attribute",NrmQuarkToString(thefile->file.fname));
		return(NhlFATAL);
	}
	return(NhlFATAL);
}

static NhlErrorTypes FileDelVarAtt
#if     NhlNeedProto
(NclFile thefile, NclQuark var, NclQuark attname)
#else
(thefile, var, attname)
NclFile thefile;
NclQuark var;
NclQuark attname;
#endif
{
	int index;
	int vindex;
	NhlErrorTypes ret = NhlNOERROR;
	NclFileAttInfoList  *stepal,*tmpal;

	if(thefile->file.wr_status <= 0) {
		vindex = _NclFileIsVar(thefile,var);
		index = _NclFileVarIsAtt(thefile,var,attname);
		if((index != -1)&&(vindex != -1)) {
			if(thefile->file.format_funcs->del_var_att != NULL) {
				ret = (*thefile->file.format_funcs->del_var_att)(thefile->file.private_rec,var,attname);
				if(ret < NhlNOERROR) {
					return(ret);
				}
				if(thefile->file.var_att_info[vindex] != NULL) {
					stepal = thefile->file.var_att_info[vindex];
					if(stepal->the_att->att_name_quark == attname) {
						tmpal = stepal;
						thefile->file.var_att_info[vindex] = stepal->next;
						NclFree(tmpal);
						return(NhlNOERROR);
					} else {
						while(stepal->next != NULL) {
							if(stepal->next->the_att->att_name_quark == attname) {
								tmpal = stepal->next;
								stepal->next= stepal->next->next;
								NclFree(tmpal);
							} else {
								stepal = stepal->next;
							}
						}
						return(NhlNOERROR);
					}
				}
			} else {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Attribute deletion not supported by format");
				return(NhlFATAL);
			}
		} else if(vindex == -1) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Attempt to delete attribute from undefined variable");
			return(NhlFATAL);
		} else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Attempt to delete undefined attribute from variable");
			return(NhlFATAL);
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"FileDelVarAtt: file (%s) is read only, can not delete attribute",NrmQuarkToString(thefile->file.fname));
		return(NhlFATAL);
	}
	return(NhlFATAL);
}

	

static NhlErrorTypes FileSetFileOption
#if	NhlNeedProto
(
	NclFile  thefile,
	NclQuark format,
	NclQuark option,
	NclMultiDValData value
)
#else
(thefile,format,option,value)
NclFile thefile;
NclQuark format;
NclQuark option;
NclMultiDValData value;
#endif
{
	int i;
	NclMultiDValData tmp_md;
	NclQuark loption;
	NclQuark *lvalue = NULL;
	NclFileClassPart *fcp = &(nclFileClassRec.file_class);
	
	loption = _NclGetLower(option);
	if (thefile) {
		if (thefile->file.format_funcs->set_option == NULL) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,
				  "FileSetFileOption: file does not support any options");
			return(NhlWARNING);
		}
		for (i = 0; i < fcp->num_options; i++) {
			if (fcp->options[i].name != loption)
				continue;
			if (thefile->file.format_funcs != _NclGetFormatFuncs(fcp->options[i].format)) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,
				    "FileSetFileOption: %s is not a recognized option for format %s",
					  NrmQuarkToString(option),NrmQuarkToString(format));
				return(NhlWARNING);
			}
			if (fcp->options[i].access == 1 && thefile->file.wr_status != 1) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,
				    "FileSetFileOption: option %s is invalid unless file is opened for reading only",
					  NrmQuarkToString(option));
				return(NhlWARNING);
			}
			else if (fcp->options[i].access == 2 && thefile->file.wr_status > 0) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,
				    "FileSetFileOption: option %s is invalid unless file is open for writing",
					  NrmQuarkToString(option));
				return(NhlWARNING);
			}
			else if (fcp->options[i].access == 3 && thefile->file.wr_status != -1) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,
				    "FileSetFileOption: option %s is can only be set prior to file creation",
					  NrmQuarkToString(option));
				return(NhlWARNING);
			}
			if (! value) {
				/* if no value specified restore default for this file only - it's not an error */
				tmp_md = fcp->options[i].def_value;
				thefile->file.format_funcs->set_option(thefile->file.private_rec,loption,
									tmp_md->multidval.data_type,
									tmp_md->multidval.totalelements,
									tmp_md->multidval.val);
				return NhlNOERROR;
			}
			tmp_md = _NclCoerceData(value,fcp->options[i].value->multidval.type->type_class.type,NULL);
			if (tmp_md == NULL) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,
				    "FileSetFileOption: invalid type for %s option value; value must be coercible to %s",
					  NrmQuarkToString(option), 
					  NrmQuarkToString(_NclObjTypeToName(fcp->options[i].value->multidval.type->type_class.type)));
				return(NhlWARNING);
			}
			if (fcp->options[i].valid_values) {
				int ok;
				int j,k;
				if (fcp->options[i].value->multidval.data_type == NCL_string) {
					lvalue = NclMalloc(tmp_md->multidval.totalelements * sizeof(NclQuark));
					ok = 0;
					for (k = 0; k < tmp_md->multidval.totalelements; k++) {
						lvalue[k] = _NclGetLower(*(NclQuark*)(((char *)tmp_md->multidval.val)+ k * sizeof(NclQuark)));
						for (j = 0; j < fcp->options[i].valid_values->multidval.totalelements; j++) {
							NclQuark valid_val = ((string *)fcp->options[i].valid_values->multidval.val)[j];
							if (lvalue[k] != valid_val)
								continue;
							ok = 1;
							break;
						}
					}
					if (! ok) {
						NclFree(lvalue);
						NhlPError(NhlWARNING,NhlEUNKNOWN,
							  "FileSetFileOption: invalid value supplied for option %s",
							  NrmQuarkToString(option));
						return(NhlWARNING);
					}
				}
				else {
					/* doesn't handle array valued options */
					for (j = 0; j < fcp->options[i].valid_values->multidval.totalelements; j++) {
						if (memcmp(tmp_md->multidval.val,
							   (char*)fcp->options[i].valid_values->multidval.val +
							   j * tmp_md->multidval.type->type_class.size,
							   tmp_md->multidval.type->type_class.size)) {
							continue;
						}
						ok = 1;
						break;
					}
				}
				if (! ok) {
					NhlPError(NhlWARNING,NhlEUNKNOWN,
						  "FileSetFileOption: invalid value supplied for option %s",
							  NrmQuarkToString(option));
					return(NhlWARNING);
				}
			}
			if (lvalue) {
				thefile->file.format_funcs->set_option(thefile->file.private_rec,loption,
									tmp_md->multidval.data_type,
									tmp_md->multidval.totalelements,
									(void *) lvalue);
				NclFree(lvalue);
			}
			else {
				thefile->file.format_funcs->set_option(thefile->file.private_rec,loption,
									tmp_md->multidval.data_type,
									tmp_md->multidval.totalelements,
									tmp_md->multidval.val);
			}
			if (tmp_md != value)
				_NclDestroyObj((NclObj)tmp_md);
			if (fcp->options[i].post_set_option) {
				return (*fcp->options[i].post_set_option)(thefile);
			}
			return NhlNOERROR;
		}
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  "FileSetFileOption: %s is not a recognized file option for format %s",
			  NrmQuarkToString(option),NrmQuarkToString(format));
		return(NhlWARNING);
	}
	else if (format != NrmNULLQUARK) {
		for (i = 0; i < fcp->num_options; i++) {
			if (fcp->options[i].name != loption)
				continue;
			if (! (_NclGetFormatFuncs(format) &&
			       _NclGetFormatFuncs(format) == _NclGetFormatFuncs(fcp->options[i].format)) ) {
				if (! (_NclGetLower(format) == NrmStringToQuark("bin") &&
				       fcp->options[i].format == _NclGetLower(format)) ) {
					NhlPError(NhlWARNING,NhlEUNKNOWN,
						  "FileSetFileOption: %s is not a recognized option for format %s",
						  NrmQuarkToString(option),NrmQuarkToString(format));
					return(NhlWARNING);
				}
			}
			if (! value) {
				/* if no value specified restore default - it's not an error */
				tmp_md = fcp->options[i].def_value;
				memcpy(fcp->options[i].value->multidval.val,tmp_md->multidval.val,
				       tmp_md->multidval.type->type_class.size);
				return NhlNOERROR;
			}
			tmp_md = _NclCoerceData(value,fcp->options[i].value->multidval.type->type_class.type,NULL);
			if (tmp_md == NULL) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,
				    "FileSetFileOption: invalid type for %s option value; value must be coercible to %s",
					  NrmQuarkToString(option), 
					  NrmQuarkToString(_NclObjTypeToName(fcp->options[i].value->multidval.type->type_class.type)));
				return(NhlWARNING);
			}
			if (fcp->options[i].valid_values) {
				int ok;
				int j,k;
				if (fcp->options[i].value->multidval.data_type == NCL_string) {
					lvalue = NclMalloc(tmp_md->multidval.totalelements * sizeof(NclQuark));
					ok = 0;
					for (k = 0; k < tmp_md->multidval.totalelements; k++) {
						lvalue[k] = _NclGetLower(*(NclQuark*)(((char *)tmp_md->multidval.val)+ k * sizeof(NclQuark)));
						for (j = 0; j < fcp->options[i].valid_values->multidval.totalelements; j++) {
							NclQuark valid_val = ((string *)fcp->options[i].valid_values->multidval.val)[j];
							if (lvalue[k] != valid_val)
								continue;
							ok = 1;
							break;
						}
					}
					if (! ok) {
						NclFree(lvalue);
						NhlPError(NhlWARNING,NhlEUNKNOWN,
							  "FileSetFileOption: invalid value supplied for option %s",
							  NrmQuarkToString(option));
						return(NhlWARNING);
					}
				}
				else {
					/* doesn't handle array valued options yet -- see the string handling */
					for (j = 0; j < fcp->options[i].valid_values->multidval.totalelements; j++) {
						if (memcmp(tmp_md->multidval.val,
							   (char*)fcp->options[i].valid_values->multidval.val +
							   j * tmp_md->multidval.type->type_class.size,
							   tmp_md->multidval.type->type_class.size)) {
							continue;
						}
						ok = 1;
						break;
					}
				}
				if (! ok) {
					NhlPError(NhlWARNING,NhlEUNKNOWN,
						  "FileSetFileOption: invalid value supplied for option %s",
							  NrmQuarkToString(option));
					return(NhlWARNING);
				}
			}
			if (lvalue) {
				/* store the lower-cased name */
				NclFree(fcp->options[i].value->multidval.val);
				fcp->options[i].value->multidval.val = (void *)lvalue;
				fcp->options[i].value->multidval.totalelements = tmp_md->multidval.totalelements;
			}
			else {
				/* doesn't handle array valued options yet -- see the string handling */
				memcpy(fcp->options[i].value->multidval.val,tmp_md->multidval.val,
				       tmp_md->multidval.type->type_class.size);
			}
			if (tmp_md != value)
				_NclDestroyObj((NclObj)tmp_md);
			return NhlNOERROR;
		}
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  "FileSetFileOption: %s is not a recognized file option for format %s",
			  NrmQuarkToString(option),NrmQuarkToString(format));
		return(NhlWARNING);
	}
	else {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  "FileSetFileOption: invalid file or format");
		return(NhlWARNING);
	}					    
		
	return NhlNOERROR;
}

static NhlErrorTypes UpdateGridTypeAtt 
#if	NhlNeedProto
(
	NclFile  thefile
)
#else
(thefile)
NclFile thefile;
#endif
{
	NclQuark *vnames;
	int n_vnames;
	int i;
	int vindex,att_id;
	NrmQuark grid_type_att_name;
	void *val;
	NclMultiDValData tmp_md;

	if (thefile->file.file_format != _NclGRIB2)
		return NhlNOERROR;

	grid_type_att_name = NrmStringToQuark("grid_type");
	vnames = (*thefile->file.format_funcs->get_var_names)(thefile->file.private_rec,&n_vnames);
	for(i = 0; i < n_vnames; i++){
		vindex = FileIsVar(thefile,vnames[i]);
		if(thefile->file.var_att_ids[vindex] == -1) {
			LoadVarAtts(thefile,vnames[i]);
			continue;
		}
		att_id = thefile->file.var_att_ids[vindex];
		if (! _NclIsAtt(att_id,NrmQuarkToString(grid_type_att_name)))
			continue;

		tmp_md = _NclGetAtt(att_id,NrmQuarkToString(grid_type_att_name),NULL);
		(*thefile->file.format_funcs->read_var_att)(thefile->file.private_rec,vnames[i],grid_type_att_name,&val);

		*((NrmQuark*)tmp_md->multidval.val) = (NrmQuark)val;
		_NclAddAtt(att_id,NrmQuarkToString(grid_type_att_name),tmp_md,NULL);
	}
	NclFree((void*)vnames);
	return NhlNOERROR;
}


NclFileOption file_options[] = {
	{ NrmNULLQUARK, NrmNULLQUARK, NULL, NULL, NULL, 2, NULL },  /* NetCDF PreFill */
	{ NrmNULLQUARK, NrmNULLQUARK, NULL, NULL, NULL, 2, NULL },  /* NetCDF define mode */
	{ NrmNULLQUARK, NrmNULLQUARK, NULL, NULL, NULL, 0, UpdateGridTypeAtt },  /* GRIB thinned grid interpolation method */
	{ NrmNULLQUARK, NrmNULLQUARK, NULL, NULL, NULL, 2, NULL },  /* NetCDF header reserve space */
	{ NrmNULLQUARK, NrmNULLQUARK, NULL, NULL, NULL, 0, NULL },  /* NetCDF suppress close option */
	{ NrmNULLQUARK, NrmNULLQUARK, NULL, NULL, NULL, 3, NULL },  /* NetCDF file format option */
	{ NrmNULLQUARK, NrmNULLQUARK, NULL, NULL, NULL, 0, NULL },  /* Binary file read byte order */
	{ NrmNULLQUARK, NrmNULLQUARK, NULL, NULL, NULL, 0, NULL },  /* Binary file write byte order */
	{ NrmNULLQUARK, NrmNULLQUARK, NULL, NULL, NULL, 0, UpdateDims },   /* GRIB initial time coordinate type */
	{ NrmNULLQUARK, NrmNULLQUARK, NULL, NULL, NULL, 0, NULL },  /* NetCDF missing to fill value option */
#ifdef USE_NETCDF4
	{ NrmNULLQUARK, NrmNULLQUARK, NULL, NULL, NULL, 2, NULL },   /* NetCDF 4 compression option level */
#endif
	{ NrmNULLQUARK, NrmNULLQUARK, NULL, NULL, NULL, 0, NULL }, /* GRIB default NCEP parameter table */
	{ NrmNULLQUARK, NrmNULLQUARK, NULL, NULL, NULL, 0, NULL },  /* GRIB print record info */
	{ NrmNULLQUARK, NrmNULLQUARK, NULL, NULL, NULL, 0, NULL },  /* GRIB single element dimensions */
	{ NrmNULLQUARK, NrmNULLQUARK, NULL, NULL, NULL, 0, NULL }  /* GRIB time period suffix */
};

NclFileClassRec nclFileClassRec = {
	{	
		"NclFileClass",
		sizeof(NclFileRec),
		(NclObjClass)&nclObjClassRec,
		0,
		(NclGenericFunction)FileDestroy,
		(NclSetStatusFunction)NULL,
		(NclInitPartFunction)NULL,
		(NclInitClassFunction)InitializeFileClass,
		(NclAddParentFunction)FileAddParent,
		(NclDelParentFunction)FileDelParent,
		(NclPrintFunction) FilePrint,
/* NclCallBackList* create_callback*/   NULL,
/* NclCallBackList* delete_callback*/   NULL,
/* NclCallBackList* modify_callback*/   NULL,
/* NclObtainCall obtain_calldata*/   FileObtainCallData
	},
	{
		FileVarRepValue,
		FileIsVar,
		FileWriteVar,
		FileWriteVarVar,
		FileReadVar,
		FileReadVarValue,
		FileIsAtt,
		FileReadAtt,
		FileWriteAtt,
		FileDelAtt,
		FileIsVarAtt,
		FileReadVarAtt,
		FileWriteVarAtt,
		FileDelVarAtt,
		FileIsDim,
		FileVarIsDim,
		FileVarReadDim,
		FileVarWriteDim,
		FileReadDim,
		FileWriteDim,
		FileIsCoord,
		FileReadCoord,
		FileWriteCoord,
		FileAddDim,
		FileAddVar,
		NULL,
		NULL,
		FileSetFileOption,
		file_options,
		sizeof(file_options) / sizeof(file_options[0])
	}
};

NclObjClass nclFileClass = (NclObjClass)&nclFileClassRec;


static NhlErrorTypes InitializeFileOptions
#if NhlNeedProto
(void)
#else
()
#endif
{
	NclFileClassPart *fcp = &(nclFileClassRec.file_class);
	logical *lval;
	string *sval;
	int *ival;
	int len_dims;
	NhlErrorTypes ret = NhlNOERROR;
	NclMultiDValData tmp_md;
	
	
	/* option names are case insensitive and so are string-type 
	 * option values
	 */

	/* NetCDF option PreFill */
	fcp->options[Ncl_PREFILL].format = NrmStringToQuark("nc");
	fcp->options[Ncl_PREFILL].name = NrmStringToQuark("prefill");
	len_dims = 1;
	lval = (logical*) NclMalloc(sizeof(logical));
	*lval = True;
	fcp->options[Ncl_PREFILL].value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)lval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypelogicalClass);
	lval = (logical*) NclMalloc(sizeof(logical));
	*lval = True;
	fcp->options[Ncl_PREFILL].def_value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)lval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypelogicalClass);
	fcp->options[Ncl_PREFILL].valid_values = NULL;

	/* NetCDF option DefineMode */
	fcp->options[Ncl_DEFINE_MODE].format = NrmStringToQuark("nc");
	fcp->options[Ncl_DEFINE_MODE].name = NrmStringToQuark("definemode");
	len_dims = 1;
	lval = (logical*) NclMalloc(sizeof(logical));
	*lval = True;
	fcp->options[Ncl_DEFINE_MODE].value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)lval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypelogicalClass);
	lval = (logical*) NclMalloc(sizeof(logical));
	*lval = True;
	fcp->options[Ncl_DEFINE_MODE].def_value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)lval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypelogicalClass);
	fcp->options[Ncl_DEFINE_MODE].valid_values = NULL;
	
	/* GRIB option ThinnedGridInterpolation */

	fcp->options[Ncl_THINNED_GRID_INTERPOLATION].format = NrmStringToQuark("grb");
	fcp->options[Ncl_THINNED_GRID_INTERPOLATION].name = NrmStringToQuark("thinnedgridinterpolation");
	len_dims = 1;
	sval = (string*) NclMalloc(sizeof(string));
	*sval = NrmStringToQuark("cubic");
	fcp->options[Ncl_THINNED_GRID_INTERPOLATION].value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)sval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypestringClass);
	sval = (string*) NclMalloc(sizeof(string));
	*sval = NrmStringToQuark("cubic");
	fcp->options[Ncl_THINNED_GRID_INTERPOLATION].def_value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)sval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypestringClass);
	sval = (string*) NclMalloc(2 * sizeof(string));
	sval[0] = NrmStringToQuark("linear");
	sval[1] = NrmStringToQuark("cubic");
	len_dims = 2;
	fcp->options[Ncl_THINNED_GRID_INTERPOLATION].valid_values = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)sval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypestringClass);

	/* NetCDF option HeaderReserveSpace */
	fcp->options[Ncl_HEADER_RESERVE_SPACE].format = NrmStringToQuark("nc");
	fcp->options[Ncl_HEADER_RESERVE_SPACE].name = NrmStringToQuark("headerreservespace");
	len_dims = 1;
	ival = (int*) NclMalloc(sizeof(int));
	*ival = 0;
	fcp->options[Ncl_HEADER_RESERVE_SPACE].value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)ival,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypeintClass);
	ival = (int*) NclMalloc(sizeof(int));
	*ival = 0;
	fcp->options[Ncl_HEADER_RESERVE_SPACE].def_value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)ival,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypeintClass);
	fcp->options[Ncl_HEADER_RESERVE_SPACE].valid_values = NULL;

	/* NetCDF option SuppressClose */
	fcp->options[Ncl_SUPPRESS_CLOSE].format = NrmStringToQuark("nc");
	fcp->options[Ncl_SUPPRESS_CLOSE].name = NrmStringToQuark("suppressclose");
	len_dims = 1;
	lval = (logical*) NclMalloc(sizeof(logical));
	*lval = True;
	fcp->options[Ncl_SUPPRESS_CLOSE].value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)lval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypelogicalClass);
	lval = (logical*) NclMalloc(sizeof(logical));
	*lval = True;
	fcp->options[Ncl_SUPPRESS_CLOSE].def_value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)lval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypelogicalClass);
	fcp->options[Ncl_SUPPRESS_CLOSE].valid_values = NULL;


	/* NetCDF option Format */

	fcp->options[Ncl_FORMAT].format = NrmStringToQuark("nc");
	fcp->options[Ncl_FORMAT].name = NrmStringToQuark("format");
	len_dims = 1;
	sval = (string*) NclMalloc(sizeof(string));
	*sval = NrmStringToQuark("classic");
	fcp->options[Ncl_FORMAT].value = _NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)sval,
						    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypestringClass);
	sval = (string*) NclMalloc(sizeof(string));
	*sval = NrmStringToQuark("classic");
	fcp->options[Ncl_FORMAT].def_value = _NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)sval,
						    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypestringClass);
#ifdef USE_NETCDF4
	len_dims = 4;
#else
	len_dims = 3;
#endif
	sval = (string*) NclMalloc(len_dims * sizeof(string));
	sval[0] = NrmStringToQuark("classic");
	sval[1] = NrmStringToQuark("64bitoffset");
	sval[2] = NrmStringToQuark("largefile");

#ifdef USE_NETCDF4
	sval[3] = NrmStringToQuark("netcdf4classic");
#endif

	fcp->options[Ncl_FORMAT].valid_values = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)sval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypestringClass);

	/* Binary option ReadByteOrder */

	fcp->options[Ncl_READ_BYTE_ORDER].format = NrmStringToQuark("bin");
	fcp->options[Ncl_READ_BYTE_ORDER].name = NrmStringToQuark("readbyteorder");
	len_dims = 1;
	sval = (string*) NclMalloc(sizeof(string));
	*sval = NrmStringToQuark("native");
	fcp->options[Ncl_READ_BYTE_ORDER].value = _NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)sval,
						    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypestringClass);
	sval = (string*) NclMalloc(sizeof(string));
	*sval = NrmStringToQuark("native");
	fcp->options[Ncl_READ_BYTE_ORDER].def_value = _NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)sval,
						    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypestringClass);
	sval = (string*) NclMalloc(3 * sizeof(string));
	sval[0] = NrmStringToQuark("native");
	sval[1] = NrmStringToQuark("bigendian");
	sval[2] = NrmStringToQuark("littleendian");
	len_dims = 3;
	fcp->options[Ncl_READ_BYTE_ORDER].valid_values = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)sval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypestringClass);

	/* Binary option WriteByteOrder */

	fcp->options[Ncl_WRITE_BYTE_ORDER].format = NrmStringToQuark("bin");
	fcp->options[Ncl_WRITE_BYTE_ORDER].name = NrmStringToQuark("writebyteorder");
	len_dims = 1;
	sval = (string*) NclMalloc(sizeof(string));
	*sval = NrmStringToQuark("native");
	fcp->options[Ncl_WRITE_BYTE_ORDER].value = _NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)sval,
						    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypestringClass);
	sval = (string*) NclMalloc(sizeof(string));
	*sval = NrmStringToQuark("native");
	fcp->options[Ncl_WRITE_BYTE_ORDER].def_value = _NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)sval,
						    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypestringClass);
	sval = (string*) NclMalloc(3 * sizeof(string));
	sval[0] = NrmStringToQuark("native");
	sval[1] = NrmStringToQuark("bigendian");
	sval[2] = NrmStringToQuark("littleendian");
	len_dims = 3;
	fcp->options[Ncl_WRITE_BYTE_ORDER].valid_values = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)sval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypestringClass);


	/* Grib option NumericIniitialTimeCoordinates */
	fcp->options[Ncl_INITIAL_TIME_COORDINATE_TYPE].format = NrmStringToQuark("grb");
	fcp->options[Ncl_INITIAL_TIME_COORDINATE_TYPE].name = NrmStringToQuark("initialtimecoordinatetype");
	len_dims = 1;
	sval = (string*) NclMalloc(sizeof(string));
	*sval = NrmStringToQuark("numeric");
	fcp->options[Ncl_INITIAL_TIME_COORDINATE_TYPE].value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)sval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypestringClass);
	sval = (string*) NclMalloc(sizeof(string));
	*sval = NrmStringToQuark("numeric");
	fcp->options[Ncl_INITIAL_TIME_COORDINATE_TYPE].def_value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)sval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypestringClass);
	sval = (string*) NclMalloc(2 * sizeof(string));
	sval[0] = NrmStringToQuark("string");
	sval[1] = NrmStringToQuark("numeric");
	len_dims = 2;
	fcp->options[Ncl_INITIAL_TIME_COORDINATE_TYPE].valid_values = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)sval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypestringClass);

	/* NetCDF option MissingToFillValue */
	fcp->options[Ncl_MISSING_TO_FILL_VALUE].format = NrmStringToQuark("nc");
	fcp->options[Ncl_MISSING_TO_FILL_VALUE].name = NrmStringToQuark("missingtofillvalue");
	len_dims = 1;
	lval = (logical*) NclMalloc(sizeof(logical));
	*lval = True;
	fcp->options[Ncl_MISSING_TO_FILL_VALUE].value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)lval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypelogicalClass);
	lval = (logical*) NclMalloc(sizeof(logical));
	*lval = True;
	fcp->options[Ncl_MISSING_TO_FILL_VALUE].def_value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)lval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypelogicalClass);
	fcp->options[Ncl_MISSING_TO_FILL_VALUE].valid_values = NULL;


#ifdef USE_NETCDF4
	/* NetCDF 4 option compression level */
	fcp->options[Ncl_COMPRESSION_LEVEL].format = NrmStringToQuark("nc");
	fcp->options[Ncl_COMPRESSION_LEVEL].name = NrmStringToQuark("compressionlevel");
	len_dims = 1;
	ival = (int*) NclMalloc(sizeof(int));
	*ival = -1;
	fcp->options[Ncl_COMPRESSION_LEVEL].value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)ival,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypeintClass);
	ival = (int*) NclMalloc(sizeof(int));
	*ival = -1;
	fcp->options[Ncl_COMPRESSION_LEVEL].def_value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)ival,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypeintClass);
	fcp->options[Ncl_COMPRESSION_LEVEL].valid_values = NULL;
#endif

	/* Grib option Default_NCEP_Ptable */
	fcp->options[Ncl_DEFAULT_NCEP_PTABLE].format = NrmStringToQuark("grb");
	fcp->options[Ncl_DEFAULT_NCEP_PTABLE].name = NrmStringToQuark("defaultncepptable");
	len_dims = 1;
	sval = (string*) NclMalloc(sizeof(string));
	*sval = NrmStringToQuark("operational");
	fcp->options[Ncl_DEFAULT_NCEP_PTABLE].value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)sval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypestringClass);
	sval = (string*) NclMalloc(sizeof(string));
	*sval = NrmStringToQuark("operational");
	fcp->options[Ncl_DEFAULT_NCEP_PTABLE].def_value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)sval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypestringClass);
	sval = (string*) NclMalloc(2 * sizeof(string));
	sval[0] = NrmStringToQuark("operational");
	sval[1] = NrmStringToQuark("reanalysis");
	len_dims = 2;
	fcp->options[Ncl_DEFAULT_NCEP_PTABLE].valid_values = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)sval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypestringClass);


	/* GRIB (2) option PrintRecordInfo */
	fcp->options[Ncl_PRINT_RECORD_INFO].format = NrmStringToQuark("grb");
	fcp->options[Ncl_PRINT_RECORD_INFO].name = NrmStringToQuark("printrecordinfo");
	len_dims = 1;
	lval = (logical*) NclMalloc(sizeof(logical));
	*lval = False;
	fcp->options[Ncl_PRINT_RECORD_INFO].value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)lval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypelogicalClass);
	lval = (logical*) NclMalloc(sizeof(logical));
	*lval = False;
	fcp->options[Ncl_PRINT_RECORD_INFO].def_value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)lval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypelogicalClass);
	fcp->options[Ncl_PRINT_RECORD_INFO].valid_values = NULL;

	/* Grib option Single element dimensions */
	fcp->options[Ncl_SINGLE_ELEMENT_DIMENSIONS].format = NrmStringToQuark("grb");
	fcp->options[Ncl_SINGLE_ELEMENT_DIMENSIONS].name = NrmStringToQuark("singleelementdimensions");
	len_dims = 1;
	sval = (string*) NclMalloc(sizeof(string));
	*sval = NrmStringToQuark("none");
	fcp->options[Ncl_SINGLE_ELEMENT_DIMENSIONS].value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)sval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypestringClass);
	sval = (string*) NclMalloc(sizeof(string));
	*sval = NrmStringToQuark("none");
	fcp->options[Ncl_SINGLE_ELEMENT_DIMENSIONS].def_value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)sval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypestringClass);
	sval = (string*) NclMalloc(7 * sizeof(string));
	sval[0] = NrmStringToQuark("none");
	sval[1] = NrmStringToQuark("all");
	sval[2] = NrmStringToQuark("ensemble");
	sval[3] = NrmStringToQuark("initial_time");
	sval[4] = NrmStringToQuark("forecast_time");
	sval[5] = NrmStringToQuark("level");
	sval[6] = NrmStringToQuark("probability");
	len_dims = 7;
	fcp->options[Ncl_SINGLE_ELEMENT_DIMENSIONS].valid_values = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)sval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypestringClass);

	/* GRIB option TimePeriodSuffix */

	fcp->options[Ncl_TIME_PERIOD_SUFFIX].format = NrmStringToQuark("grb");
	fcp->options[Ncl_TIME_PERIOD_SUFFIX].name = NrmStringToQuark("timeperiodsuffix");
	len_dims = 1;
	lval = (logical*) NclMalloc(sizeof(logical));
	*lval = True;
	fcp->options[Ncl_TIME_PERIOD_SUFFIX].value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)lval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypelogicalClass);
	lval = (logical*) NclMalloc(sizeof(logical));
	*lval = True;
	fcp->options[Ncl_TIME_PERIOD_SUFFIX].def_value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)lval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypelogicalClass);
	fcp->options[Ncl_TIME_PERIOD_SUFFIX].valid_values = NULL;

	/* End of options */

	return ret;
}

static NhlErrorTypes InitializeFileClass
#if NhlNeedProto
(void)
#else
()
#endif
{

	InitializeFileOptions();

	_NclRegisterClassPointer(
		Ncl_File,
		(NclObjClass)&nclFileClassRec
	);
	
	return(NhlNOERROR);
}

static void AddAttInfoToList
#if	NhlNeedProto
(NclFileAttInfoList **list_handle,struct _NclFAttRec*     the_att) 
#else 
(list_handle,the_att) 
NclFileAttInfoList **list_handle;
struct _NclFAttRec*     the_att; 
#endif
{
	NclFileAttInfoList *step;
	if(*list_handle == NULL) {
		*list_handle = (NclFileAttInfoList*)NclMalloc(sizeof(NclFileAttInfoList));
		(*list_handle)->next = NULL;
		(*list_handle)->the_att = the_att;
	} else {
		step = *list_handle;
		while(step->next != NULL) {
			step = step->next;
		}
		step->next = (NclFileAttInfoList*)NclMalloc(sizeof(NclFileAttInfoList));
		step->next->next = NULL;
		step->next->the_att = the_att;
	}
	return;
}
static int FileIsVar
#if	NhlNeedProto
(NclFile thefile,NclQuark var)
#else 
(thefile,var)
	NclFile thefile;
	NclQuark var;
#endif
{
	int i;

	for(i = 0; i < thefile->file.n_vars; i++) {
		if(thefile->file.var_info[i]->var_name_quark == var) {
			return(i);
		}
	}
	return(-1);
}

static NclObjTypes FileVarRepValue
#if	NhlNeedProto
(NclFile thefile, NclQuark var)
#else 
(thefile, var)
NclFile thefile;
NclQuark var;
#endif
{
	int index; 

	index = FileIsVar(thefile,var);

	if(index > -1) {	
		return(_NclBasicDataTypeToObjType(thefile->file.var_info[index]->data_type));
	} else {
		return(Ncl_None);
	}
}


static void ReverseIt
#if	NhlNeedProto
(void *val,void* swap_space,int ndims,int *compare_sel,int *dim_sizes,int el_size)
#else
(val,swap_space,ndims,compare_sel,dim_sizes,el_size)
void *val;
void* swap_space;
int ndims;
int *compare_sel;
int *dim_sizes;
int el_size;
#endif
{
	int i,j;
	char *tmp;
	int block_size = el_size;

	for(i = 1; i < ndims; i++) {
		block_size *= dim_sizes[i];
	}
	tmp = (char*)val;
	if(ndims != 1) {
		for(i = 0; i < *dim_sizes; i++) {
			ReverseIt((void*)(tmp+(i*block_size)),swap_space,ndims-1,&(compare_sel[1]),&(dim_sizes[1]),el_size);
		}
	}
	if(*compare_sel == NCLFILE_DEC) {
		j = *dim_sizes -1;
		for(i = 0; i < (*dim_sizes)/2;i++) {
			memcpy(swap_space,(void*)&(tmp[i*block_size]),block_size);
			memcpy((void*)&(tmp[i*block_size]),(void*)&(tmp[j*block_size]),block_size);
			memcpy((void*)&(tmp[j*block_size]),swap_space,block_size);
			j--;
		}
		
	} 	
	return;
}

static struct _NclMultiDValDataRec* MyFileReadVarValue
#if	NhlNeedProto
(NclFile thefile, NclQuark var_name, struct _NclSelectionRecord* sel_ptr,NclDimRec* dim_info,int vtype)
#else 
(thefile, var_name, sel_ptr,dim_info,vtype)
NclFile thefile;
NclQuark var_name;
struct _NclSelectionRecord* sel_ptr;
NclDimRec* dim_info;
int vtype;
#endif
{
	NclMultiDValData tmp_md = NULL;
	NclMultiDValData mis_md = NULL;
	NclScalar missing_value;
	int has_missing = 0;
	void *val;
	int index;
	long start[NCL_MAX_DIMENSIONS];
	long finish[NCL_MAX_DIMENSIONS];
	long stride[NCL_MAX_DIMENSIONS];
	long real_stride[NCL_MAX_DIMENSIONS];
	int i,j,k,done = 0,inc_done = 0;
	int n_dims_input,n_elem;
	int n_dims_output;
	long total_elements = 1;
	int has_vectors = 0;
	int has_stride = 0;
	int has_reverse = 0;
	int has_reorder = 0;
	int to = 0,block_read_limit,n_elem_block;
	
	long multiplier_input[NCL_MAX_DIMENSIONS];
	int compare_sel[NCL_MAX_DIMENSIONS];
	long current_index[NCL_MAX_DIMENSIONS];
	long current_finish[NCL_MAX_DIMENSIONS];
	int index_map[NCL_MAX_DIMENSIONS];
	int output_dim_sizes[NCL_MAX_DIMENSIONS];
	int keeper[NCL_MAX_DIMENSIONS];
	NclSelection *sel;
	float tmpf;
	int tmpi;
	int swap_size;
	void *swap_space = NULL;
/*
* By the the time it gets here the file suport routines in that build the selection
* record have made sure var_name is valid and all the demensions in sel_ptr
* are valid. However, the values have not been checked for out_of_ranges
* subscripts
*/
	index = FileIsVar(thefile,var_name);
	n_dims_input = thefile->file.var_info[index]->num_dimensions;
	if(sel_ptr != NULL) {
		sel = sel_ptr->selection;
		for(i = 0; i < n_dims_input; i++) {
			switch(sel->sel_type) {
			case Ncl_SUB_ALL:
				start[sel->dim_num] = 0;
			case Ncl_SUB_VAL_DEF:
				if(Ncl_SUB_VAL_DEF == sel->sel_type) {
					start[sel->dim_num] = sel->u.sub.start;
				}
				finish[sel->dim_num] = thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[sel->dim_num]]->dim_size-1;
			case Ncl_SUB_DEF_VAL:
				if(sel->sel_type == Ncl_SUB_DEF_VAL) {
					finish[sel->dim_num] = sel->u.sub.finish;
					start[sel->dim_num] = 0;
				} 
			case Ncl_SUBSCR:
				if(sel->u.sub.is_single) {
					keeper[i] = 0;
				} else {
					keeper[i] = 1;
				}
				if(sel->sel_type == Ncl_SUBSCR) {
					start[sel->dim_num] = sel->u.sub.start;
					finish[sel->dim_num] = sel->u.sub.finish;
					stride[sel->dim_num] = sel->u.sub.stride;

				} else {
					stride[sel->dim_num] = sel->u.sub.stride;
				}
				if(finish[sel->dim_num] < start[sel->dim_num]) {
					if(stride[sel->dim_num] < 0) {
						tmpi = finish[sel->dim_num] + (start[sel->dim_num] - finish[sel->dim_num]) % abs(stride[sel->dim_num]);
						finish[sel->dim_num] = start[sel->dim_num];
						start[sel->dim_num] = tmpi;
						compare_sel[sel->dim_num] = NCLFILE_INC;
						stride[sel->dim_num] = -(stride[sel->dim_num]); 
					} else {
						compare_sel[sel->dim_num] = NCLFILE_DEC;
						stride[sel->dim_num] = -(stride[sel->dim_num]); 
						has_reverse = 1;
					}
				} else {
					if(stride[sel->dim_num] < 0) {
						has_reverse = 1;
                                                tmpi = finish[sel->dim_num] - (finish[sel->dim_num] - start[sel->dim_num]) % abs(stride[sel->dim_num]);
                                                finish[sel->dim_num] = start[sel->dim_num];
                                                start[sel->dim_num] = tmpi;
                                                compare_sel[sel->dim_num] = NCLFILE_DEC;
                                                stride[sel->dim_num] = (stride[sel->dim_num]);
                                        } else {
                                                compare_sel[sel->dim_num] = NCLFILE_INC;
                                                stride[sel->dim_num] = (stride[sel->dim_num]);
                                        }

				}
				if(abs(stride[sel->dim_num]) > 1) 
					has_stride = 1;
				if(stride[sel->dim_num] != 0)  {
					tmpf = (float)fabs(((float)sel->u.sub.stride));
				} else {
					NhlPError(NhlWARNING,NhlEUNKNOWN,"Invalid stride: stride must be positive non-zero integer");

					stride[sel->dim_num] = 1;
					tmpf = 1;
				}
				n_elem =(int)(fabs(((double)(finish[sel->dim_num] - start[sel->dim_num]))) /tmpf) + 1;
				if((sel->u.sub.start > thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[sel->dim_num]]->dim_size-1)||(sel->u.sub.start < 0)) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
					return(NULL);
				}
				if((sel->u.sub.finish> thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[sel->dim_num]]->dim_size-1)||(sel->u.sub.finish< 0)) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
					return(NULL);
				}
				

/*
* set when normal subscript
*/
				if(sel->dim_num != i) {
					has_reorder = 1;
				}
				index_map[i] = sel->dim_num;
				break;
			case Ncl_VECSUBSCR:
				keeper[i] = 1;
				if((sel->u.vec.min < 0)||(sel->u.vec.min >= thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[sel->dim_num]]->dim_size)) {	
					NhlPError(NhlFATAL,NhlEUNKNOWN, "Subscript out of range, error in subscript #%d",i);
					return(NULL);
				}
				if((sel->u.vec.max < 0)||(sel->u.vec.max >= thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[sel->dim_num]]->dim_size)) {	
					NhlPError(NhlFATAL,NhlEUNKNOWN, "Subscript out of range, error in subscript #%d",i);
					return(NULL);
				}
				n_elem = sel->u.vec.n_ind;
			
				stride[sel->dim_num] = 0;
				start[sel->dim_num] = finish[sel->dim_num] = sel->u.vec.ind[0];
				has_vectors = 1;
				if(sel->dim_num != i) {
					has_reorder = 1;
				}
				index_map[i] = sel->dim_num;

/*
* 0 when vector subscript
*/
				compare_sel[sel->dim_num] = NCLFILE_VEC;
				break;
			}
			if(sel->dim_num != n_dims_input -1) {
				multiplier_input[sel->dim_num] = 1;
				for(k = sel->dim_num + 1; k < n_dims_input; k++) {
					multiplier_input[sel->dim_num] *= (long)thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[k]]->dim_size;
				}
			}
			output_dim_sizes[i] = n_elem;
			(dim_info)[i].dim_num = i;
			(dim_info)[i].dim_size = n_elem;
			(dim_info)[i].dim_quark = FileGetDimName(thefile,thefile->file.var_info[index]->file_dim_num[sel->dim_num]);
			total_elements = total_elements * n_elem;
			sel++;
		}
		sel = sel_ptr->selection;
	} else {
		for(i = 0; i< n_dims_input; i++) {
			start[i] = 0;
			finish[i] = thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[i]]->dim_size - 1;
			stride[i] = 1;
			total_elements *= (finish[i] + 1);
			output_dim_sizes[i] = finish[i] + 1;
			(dim_info)[i].dim_num = i;
			(dim_info)[i].dim_size = output_dim_sizes[i];
			(dim_info)[i].dim_quark = FileGetDimName(thefile,thefile->file.var_info[index]->file_dim_num[i]);
			compare_sel[i] = NCLFILE_INC;
			multiplier_input[i] = 1;
			for(k = i + 1; k < n_dims_input; k++) {
				multiplier_input[i] *= (long)thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[k]]->dim_size;
			}

		}
		sel = NULL;
	}

	if (total_elements == 0) {
		/* can't return any data because there is a 0-length dimension but nevertheless return what is possible */
		NhlPError(NhlWARNING,NhlEUNKNOWN,"FileReadVar: %s contains a 0 length dimension", 
			  NrmQuarkToString(var_name));
		n_dims_output = n_dims_input;
		val = NULL;

		if(sel_ptr != NULL) {
			i = 0;
			while((i <  n_dims_output)&&(n_dims_output > 1)) {
				if((output_dim_sizes[i] == 1)&&!(keeper[i])) {
					for(j = i; j < n_dims_output-1; j++) {
						output_dim_sizes[j] = output_dim_sizes[j+1];
						keeper[j] = keeper[j+1];
						(dim_info)[j] = (dim_info)[j+1];
					}
					n_dims_output--;
				} else {
					i++;
				}
			}
		}

		if(FileIsVarAtt(thefile,var_name,NrmStringToQuark(NCL_MISSING_VALUE_ATT))!=-1){
			mis_md = FileReadVarAtt(thefile,var_name,NrmStringToQuark(NCL_MISSING_VALUE_ATT),NULL);
			if(mis_md != NULL) {
				memcpy((void*)&missing_value,mis_md->multidval.val,_NclSizeOf(mis_md->multidval.data_type));
				has_missing = 1;
			}
		} 
		if(vtype == FILE_COORD_VAR_ACCESS) {
			tmp_md = _NclOneDValCoordDataCreate(
				NULL,
				NULL,
				Ncl_OneDValCoordData,
				0,
				val,
				(has_missing ? &missing_value:NULL),
				n_dims_output,
				output_dim_sizes,
				TEMPORARY,
				sel_ptr,
				_NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(thefile->file.var_info[index]->data_type))
				);
		} else {
			tmp_md = _NclCreateMultiDVal(
				NULL,
				NULL,
				Ncl_MultiDValData,
				0,
				val,
				(has_missing ? &missing_value:NULL),
				n_dims_output,
				output_dim_sizes,
				TEMPORARY,
				sel_ptr,
				_NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(thefile->file.var_info[index]->data_type))
				);
		}
		return(tmp_md);
	}
		

/*
* When ncl gets here all strides are positive and finishs are greater than starts
* and stride and finishes all corespond to their *CORRECT* dimension numbers not
* and reordered dimensions. Any reordering has to occur afterwards.
*/

/*
* Basically everything is different depending on which format_func field contains
* the read function.
*/

/* 
* Take care of simplest case here
*/
	if((vtype == FILE_VAR_ACCESS? thefile->file.format_funcs->read_var != NULL:thefile->file.format_funcs->read_coord != NULL)) {
		if((!has_vectors)&&(!has_reverse)&&(!has_reorder)) {
			val = (void*)NclMalloc(total_elements*_NclSizeOf(thefile->file.var_info[index]->data_type));
			if(vtype == FILE_VAR_ACCESS) {
				(*thefile->file.format_funcs->read_var)(
					thefile->file.private_rec,
					thefile->file.var_info[index]->var_name_quark,
					start,
					finish,
					stride,
					val);
			} else {
				(*thefile->file.format_funcs->read_coord)(
					thefile->file.private_rec,
					thefile->file.var_info[index]->var_name_quark,
					start,
					finish,
					stride,
					val);
			}
		
			n_dims_output = n_dims_input;
			if(sel_ptr != NULL) {
				i = 0;
				while((i <  n_dims_output)&&(n_dims_output > 1)) {
					if((output_dim_sizes[i] == 1)&&!(keeper[i])) {
						for(j = i; j < n_dims_output-1; j++) {
							output_dim_sizes[j] = output_dim_sizes[j+1];
							keeper[j] = keeper[j+1];
							(dim_info)[j] = (dim_info)[j+1];
						}
						n_dims_output--;
					} else {
						i++;
					}
				}
			}
		} else if((has_reverse)&&(!has_vectors)&&(!has_reorder)){
/*
* If a reverse is detected it is quicker to read everything in inorder and
* perform swapping in memory. This is easiest done if selections containing
* vectors and dimension reordering are excluded. Unfortunately swap space
* is needed and this could be as large as the product of the sizes of all 
* dimensions with the exception of dimension 0.
*/
			val = (void*)NclMalloc(total_elements*_NclSizeOf(thefile->file.var_info[index]->data_type));
			i = 0;
			while((i<n_dims_input)&&(compare_sel[i] != NCLFILE_DEC)){
				i++;
			}
			swap_size = 1;
			for(j = i + 1; j < n_dims_input; j++) {	
				swap_size *= output_dim_sizes[j];
			}
			swap_space = (void*)NclMalloc(swap_size * _NclSizeOf(thefile->file.var_info[index]->data_type));
			for(i = 0;i < n_dims_input; i++) {
				switch(compare_sel[i]) {
				case NCLFILE_INC:
					current_index[i] = start[i];
					current_finish[i] = finish[i];
					real_stride[i] = abs(stride[i]);
					break;
				case NCLFILE_DEC:
/*
* Problem here is that selecting in reverse order could
* alter selection when (finish - start )%stride != 0
* Therefore a new start and finish must be computed to
* produce desired selection
*/
					real_stride[i] = abs(stride[i]);
					current_finish[i] = start[i];
					if(( start[i] - finish[i])%abs(stride[i]) == 0) {
						current_index[i] = finish[i] ;
					} else {
						current_index[i] = finish[i]+ (start[i] - finish[i])%abs(stride[i]);
					}
					break;
				}
			}
			if(vtype == FILE_VAR_ACCESS) {
				(*thefile->file.format_funcs->read_var)(
					thefile->file.private_rec,
					thefile->file.var_info[index]->var_name_quark,
					current_index,
					current_finish,
					real_stride,
					(void*)val);
			} else {
				(*thefile->file.format_funcs->read_coord)(
					thefile->file.private_rec,
					thefile->file.var_info[index]->var_name_quark,
					current_index,
					current_finish,
					real_stride,
					(void*)val);
			}
                        n_dims_output = n_dims_input;
			if(sel_ptr != NULL) {
                        	i = 0;
                        	while((i <  n_dims_output)&&(n_dims_output > 1)) {
                                	if((output_dim_sizes[i] == 1)&&!(keeper[i])) {
                                        	for(j = i; j < n_dims_output-1; j++) {
                                                	output_dim_sizes[j] = output_dim_sizes[j+1];
                                                	keeper[j] = keeper[j+1];
							compare_sel[j] = compare_sel[j+1];
                                                	(dim_info)[j] = (dim_info)[j+1];
                                        	}
                                        	n_dims_output--;
                                	} else {
                                        	i++;
                                	}
                        	}
			}
			ReverseIt(val,swap_space,n_dims_output,compare_sel,output_dim_sizes,_NclSizeOf(thefile->file.var_info[index]->data_type));
			NclFree(swap_space);
		} else {
			val = (void*)NclMalloc(total_elements*_NclSizeOf(thefile->file.var_info[index]->data_type));
			to = 0;
			block_read_limit = n_dims_input - 1 ;
/*
* Find out what size chunks can be read in at once
*/
			for(i = n_dims_input-1 ; i>= 0; i--) {
				if((compare_sel[index_map[i]] != NCLFILE_INC)||(index_map[i] != i)) {
					block_read_limit = i;
					break;
				}
			}
/*
* Initialize starting index, finish and stride values for first read
*/
                        n_elem_block = 1;
                        for(i = 0; i < n_dims_input ; i++) {
                                current_index[index_map[i]] = start[index_map[i]];
                                if(i > block_read_limit) {
/*
* OK to use i here since these indices are in order
*/
                                        n_elem_block *= output_dim_sizes[index_map[i]];
                                        current_finish[index_map[i]] = finish[index_map[i]];
                                        real_stride[index_map[i]] = abs(stride[index_map[i]]);
                                } else {
                                        switch(compare_sel[index_map[i]]) {
                                        case NCLFILE_INC:
                                                current_finish[index_map[i]] = current_index[index_map[i]] ;
                                                real_stride[index_map[i]] = 1;
                                                break;
                                        case NCLFILE_DEC:
                                                current_finish[index_map[i]] = current_index[index_map[i]] ;
                                                real_stride[index_map[i]] = 1;
                                                break;
                                        default:         /* vectors */
                                                current_finish[index_map[i]]  = current_index[index_map[i]];
                                                real_stride[index_map[i]] = 1;
                                                break;
                                        }
                                }
                        }
			while(!done) {
				if(vtype == FILE_VAR_ACCESS) {
					(*thefile->file.format_funcs->read_var)(
						thefile->file.private_rec,
						thefile->file.var_info[index]->var_name_quark,
						current_index,
						current_finish,
						real_stride,
						(void*)&(((char*)val)[to]));
				} else {
					(*thefile->file.format_funcs->read_coord)(
						thefile->file.private_rec,
						thefile->file.var_info[index]->var_name_quark,
						current_index,
						current_finish,
						real_stride,
						(void*)&(((char*)val)[to]));
				}
				to += n_elem_block * _NclSizeOf(thefile->file.var_info[index]->data_type);
				if(compare_sel[index_map[block_read_limit]] < 0) {
					current_index[index_map[block_read_limit]] += stride[index_map[block_read_limit]];
					current_finish[index_map[block_read_limit]] = current_index[index_map[block_read_limit]];
				} else {
					compare_sel[index_map[block_read_limit]]++;
				}
				for(i = block_read_limit; i > 0; i--) {
					switch(compare_sel[index_map[i]]) {
					case NCLFILE_INC:
						if(current_index[index_map[i]] > finish[index_map[i]]) {
							current_index[index_map[i]] = start[index_map[i]];
							if(compare_sel[index_map[i-1]] < 0 ) {
								current_index[index_map[i-1]] += stride[index_map[i-1]];
							} else {
								compare_sel[index_map[i-1]]++;
							}

						} else {
							inc_done = 1;
						}	
						current_finish[index_map[i]] = current_index[index_map[i]] ;
						break;
					case NCLFILE_DEC:
						if(current_index[index_map[i]] < finish[index_map[i]]) {
							current_index[index_map[i]] = start[index_map[i]];
							if(compare_sel[index_map[i-1]] < 0) {
								current_index[index_map[i-1]] += stride[index_map[i-1]];
							} else {
								compare_sel[index_map[i-1]]++;
							}
						} else {	
							inc_done = 1;
						}
						current_finish[index_map[i]] = current_index[index_map[i]];
						break;
					default:
						if(compare_sel[index_map[i]] >= sel[i].u.vec.n_ind) {
							compare_sel[index_map[i]] = 0;
							current_index[index_map[i]] = sel[i].u.vec.ind[0];
							if(compare_sel[index_map[i-1]] < 0 ) {
								current_index[index_map[i-1]] += stride[index_map[i-1]];
							} else {
								compare_sel[index_map[i-1]]++;
							}
						} else {
							current_index[index_map[i]] = sel[i].u.vec.ind[compare_sel[index_map[i]]];
							inc_done = 1;
						}
						current_finish[index_map[i]] = current_index[index_map[i]];
						break;
					} 
					if(inc_done) {
						inc_done = 0;
						break;
					}
				}
				switch(compare_sel[index_map[0]]) {
				case NCLFILE_DEC:
					if(current_index[index_map[0]] < finish[index_map[0]]) 
							done = 1;
					current_finish[index_map[0]] = current_index[index_map[0]]; 
					break;
				case NCLFILE_INC:
					if(current_index[index_map[0]] > finish[index_map[0]]) 
							done = 1;
					current_finish[index_map[0]] = current_index[index_map[0]]; 
					break;
				default:
					if(compare_sel[index_map[0]] >= sel[0].u.vec.n_ind) {
							done = 1;
					}  else {
						current_index[index_map[0]] = sel[0].u.vec.ind[compare_sel[index_map[0]]];
					}
					current_finish[index_map[0]] = current_index[index_map[0]]; 
				}
			}
			n_dims_output = n_dims_input;
			if(sel_ptr != NULL) {
				i = 0;
				while((i <  n_dims_output)&&(n_dims_output > 1)) {
					if((output_dim_sizes[i] == 1)&&!(keeper[i])) {
						for(j = i; j < n_dims_output-1; j++) {
							output_dim_sizes[j] = output_dim_sizes[j+1];
							keeper[j] = keeper[j+1];
							(dim_info)[j] = (dim_info)[j+1];
						}
						n_dims_output--;
					} else {
						i++;
					}
				}
			}
		}
	} else if((vtype == FILE_VAR_ACCESS ? thefile->file.format_funcs->read_var_ns != NULL : thefile->file.format_funcs->read_coord_ns!= NULL)){
		if(!has_stride) {
			if((!has_vectors)&&(!has_reverse)&&(!has_reorder)) {
				val = (void*)NclMalloc(total_elements*_NclSizeOf(thefile->file.var_info[index]->data_type));
				if(vtype == FILE_VAR_ACCESS) {
					(*thefile->file.format_funcs->read_var_ns)(
						thefile->file.private_rec,
						thefile->file.var_info[index]->var_name_quark,
						start,
						finish,
						val);
				} else {
					(*thefile->file.format_funcs->read_coord_ns)(
						thefile->file.private_rec,
						thefile->file.var_info[index]->var_name_quark,
						start,
						finish,
						val);
				}
			
				n_dims_output = n_dims_input;
				if(sel_ptr != NULL) {
					i = 0;
					while((i <  n_dims_output)&&(n_dims_output > 1)) {
						if((output_dim_sizes[i] == 1)&&!(keeper[i])) {
							for(j = i; j < n_dims_output-1; j++) {
								output_dim_sizes[j] = output_dim_sizes[j+1];
								keeper[j] = keeper[j+1];
								(dim_info)[j] = (dim_info)[j+1];
							}
							n_dims_output--;
						} else {
							i++;
						}
					}
				}
			} else if((has_reverse)&&(!has_vectors)&&(!has_reorder)){
	/*
	* If a reverse is detected it is quicker to read everything in inorder and
	* perform swapping in memory. This is easiest done if selections containing
	* vectors and dimension reordering are excluded. Unfortunately swap space
	* is needed and this could be as large as the product of the sizes of all 
	* dimensions with the exception of dimension 0.
	*/
				val = (void*)NclMalloc(total_elements*_NclSizeOf(thefile->file.var_info[index]->data_type));
				i = 0;
				while((i<n_dims_input)&&(compare_sel[i] != NCLFILE_DEC)){
					i++;
				}
				swap_size = 1;
				for(j = i + 1; j < n_dims_input; j++) {	
					swap_size *= output_dim_sizes[j];
				}
				swap_space = (void*)NclMalloc(swap_size * _NclSizeOf(thefile->file.var_info[index]->data_type));
				for(i = 0;i < n_dims_input; i++) {
					switch(compare_sel[i]) {
					case NCLFILE_INC:
						current_index[i] = start[i];
						current_finish[i] = finish[i];
						break;
					case NCLFILE_DEC:
	/*
	* Problem here is that selecting in reverse order could
	* alter selection when (finish - start )%stride != 0
	* Therefore a new start and finish must be computed to
	* produce desired selection
	*/
						current_finish[i] = start[i];
						current_index[i] = finish[i] ;
						break;
					}
				}
				if(vtype == FILE_VAR_ACCESS) {
					(*thefile->file.format_funcs->read_var_ns)(
						thefile->file.private_rec,
						thefile->file.var_info[index]->var_name_quark,
						current_index,
						current_finish,
						(void*)val);
				} else {
					(*thefile->file.format_funcs->read_coord_ns)(
						thefile->file.private_rec,
						thefile->file.var_info[index]->var_name_quark,
						current_index,
						current_finish,
						(void*)val);
				}
                        	n_dims_output = n_dims_input;
				if(sel_ptr != NULL) {
                        		i = 0;
                        		while((i <  n_dims_output)&&(n_dims_output > 1)) {
                                		if((output_dim_sizes[i] == 1)&&!(keeper[i])) {
                                        		for(j = i; j < n_dims_output-1; j++) {
                                                		output_dim_sizes[j] = output_dim_sizes[j+1];
                                                		keeper[j] = keeper[j+1];
								compare_sel[j] = compare_sel[j+1];
                                                		(dim_info)[j] = (dim_info)[j+1];
                                        		}
                                        		n_dims_output--;
                                		} else {
                                        		i++;
                                		}
                        		}
				}
				ReverseIt(val,swap_space,n_dims_output,compare_sel,output_dim_sizes,_NclSizeOf(thefile->file.var_info[index]->data_type));
				NclFree(swap_space);
			} else {
				val = (void*)NclMalloc(total_elements*_NclSizeOf(thefile->file.var_info[index]->data_type));
				to = 0;
				block_read_limit = n_dims_input - 1 ;
	/*
	* Find out what size chunks can be read in at once
	*/
				for(i = n_dims_input-1 ; i>= 0; i--) {
					if((compare_sel[index_map[i]] != NCLFILE_INC)||(index_map[i] != i)) {
						block_read_limit = i;
						break;
					}
				}
	/*
	* Initialize starting index, finish and stride values for first read
	*/
                        	n_elem_block = 1;
                        	for(i = 0; i < n_dims_input ; i++) {
                                	current_index[index_map[i]] = start[index_map[i]];
                                	if(i > block_read_limit) {
	/*
	* OK to use i here since these indices are in order
	*/
                                        	n_elem_block *= output_dim_sizes[i];
                                        	current_finish[index_map[i]] = finish[index_map[i]];
                                	} else {
                                        	switch(compare_sel[index_map[i]]) {
                                        	case NCLFILE_INC:
                                                	current_finish[index_map[i]] = current_index[index_map[i]] ;
                                                	break;
                                        	case NCLFILE_DEC:
                                                	current_finish[index_map[i]] = current_index[index_map[i]] ;
                                                	break;
                                        	default:         /* vectors */
                                                	current_finish[index_map[i]]  = current_index[index_map[i]];
                                                	break;
                                        	}
                                	}
                        	}
				while(!done) {
					if(vtype == FILE_VAR_ACCESS) {
						(*thefile->file.format_funcs->read_var_ns)(
							thefile->file.private_rec,
							thefile->file.var_info[index]->var_name_quark,
							current_index,
							current_finish,
							(void*)&(((char*)val)[to]));
					} else {
						(*thefile->file.format_funcs->read_coord_ns)(
							thefile->file.private_rec,
							thefile->file.var_info[index]->var_name_quark,
							current_index,
							current_finish,
							(void*)&(((char*)val)[to]));
					}
					to += n_elem_block * _NclSizeOf(thefile->file.var_info[index]->data_type);
					if(compare_sel[index_map[block_read_limit]] < 0) {
						current_index[index_map[block_read_limit]] += 1;
						current_finish[index_map[block_read_limit]] = current_index[index_map[block_read_limit]];
					} else {
						compare_sel[index_map[block_read_limit]]++;
					}
					for(i = block_read_limit; i > 0; i--) {
						switch(compare_sel[index_map[i]]) {
						case NCLFILE_INC:
							if(current_index[index_map[i]] > finish[index_map[i]]) {
								current_index[index_map[i]] = start[index_map[i]];
								if(compare_sel[index_map[i-1]] < 0 ) {
									current_index[index_map[i-1]] += 1; 
								} else {
									compare_sel[index_map[i-1]]++;
								}
	
							} else {
								inc_done = 1;
							}	
							current_finish[index_map[i]] = current_index[index_map[i]] ;
							break;
						case NCLFILE_DEC:
							if(current_index[index_map[i]] < finish[index_map[i]]) {
								current_index[index_map[i]] = start[index_map[i]];
								if(compare_sel[index_map[i-1]] < 0) {
									current_index[index_map[i-1]] += 1; 
								} else {
									compare_sel[index_map[i-1]]++;
								}
							} else {	
								inc_done = 1;
							}
							current_finish[index_map[i]] = current_index[index_map[i]];
							break;
						default:
							if(compare_sel[index_map[i]] >= sel[index_map[i]].u.vec.n_ind) {
								compare_sel[index_map[i]] = 0;
								current_index[index_map[i]] = sel[index_map[i]].u.vec.ind[0];
								if(compare_sel[index_map[i-1]] < 0 ) {
									current_index[index_map[i-1]] += 1; 
								} else {
									compare_sel[index_map[i-1]]++;
								}
							} else {
								current_index[index_map[i]] = sel[index_map[i]].u.vec.ind[compare_sel[index_map[i]]];
								inc_done = 1;
							}
							current_finish[index_map[i]] = current_index[index_map[i]];
							break;
						} 
						if(inc_done) {
							inc_done = 0;
							break;
						}
					}
					switch(compare_sel[index_map[0]]) {
					case NCLFILE_DEC:
						if(current_index[index_map[0]] < finish[index_map[0]]) 
								done = 1;
						current_finish[index_map[0]] = current_index[index_map[0]]; 
						break;
					case NCLFILE_INC:
						if(current_index[index_map[0]] > finish[index_map[0]]) 
								done = 1;
						current_finish[index_map[0]] = current_index[index_map[0]]; 
						break;
					default:
						if(compare_sel[index_map[0]] >= sel[0].u.vec.n_ind) {
								done = 1;
						}  else {
							current_index[index_map[0]] = sel[0].u.vec.ind[compare_sel[index_map[0]]];
						}
						current_finish[index_map[0]] = current_index[index_map[0]]; 
					}
				}
				n_dims_output = n_dims_input;
				if(sel_ptr != NULL ) {
					i = 0;
					while((i <  n_dims_output)&&(n_dims_output > 1)) {
						if((output_dim_sizes[i] == 1)&&!(keeper[i])) {
							for(j = i; j < n_dims_output-1; j++) {
								output_dim_sizes[j] = output_dim_sizes[j+1];
								(dim_info)[j] = (dim_info)[j+1];
							}
							n_dims_output--;
						} else {
							i++;
						}
					}
				}
			}
		} else {
			if((!has_vectors)&&(!has_reverse)&&(!has_reorder)) {
/*
* Loop through and find block size. Then implement read like it is vectors
*/
				for(i = n_dims_input-1; i >= 0; i--) {
					if(stride[i] != 1) {
						block_read_limit = i;
						break;
					}
				}
				n_elem_block = 1;
				for(i = 0; i < n_dims_input; i++) {
					current_index[i] = start[i];
					if(i > block_read_limit) {
						n_elem_block *= output_dim_sizes[i];
						current_finish[i] = finish[i];
					} else {
						current_finish[i] = current_index[i];
					}
				}
				val = (void*)NclMalloc(total_elements*_NclSizeOf(thefile->file.var_info[index]->data_type));
				to = 0;
				while(!done) {
					if(vtype == FILE_VAR_ACCESS) {
						(*thefile->file.format_funcs->read_var_ns)(
							thefile->file.private_rec,
							thefile->file.var_info[index]->var_name_quark,
							current_index,
							current_finish,
							(void*)&(((char*)val)[to]));
					} else {
						(*thefile->file.format_funcs->read_coord_ns)(
							thefile->file.private_rec,
							thefile->file.var_info[index]->var_name_quark,
							current_index,
							current_finish,
							val);
					}
					to += n_elem_block * _NclSizeOf(thefile->file.var_info[index]->data_type);
/*
* No reverse here so all is just added
*/
					for(i = block_read_limit; i >= 0 ; i--) {
						if((current_index[i] + stride[i] > finish[i])&&(i != 0)) {
							current_index[i] = start[i];
							current_finish[i] = current_index[i];
						} else {
							current_index[i] = current_index[i] + stride[i];
							current_finish[i] = current_index[i];
							break;
						}
					}
					if(current_index[0]  > finish[0]) {
						done = 1;
					}
					current_finish[0] = current_index[0];
				}
				n_dims_output = n_dims_input;
				if(sel_ptr != NULL) {
					i = 0;
					while((i <  n_dims_output)&&(n_dims_output > 1)) {
						if((output_dim_sizes[i] == 1)&&!(keeper[i])) {
							for(j = i; j < n_dims_output-1; j++) {
								output_dim_sizes[j] = output_dim_sizes[j+1];
								(dim_info)[j] = (dim_info)[j+1];
							}
							n_dims_output--;
						} else {
							i++;
						}
					}
				}
			} else if((has_reverse)&&(!has_vectors)&&(!has_reorder)){
/*
* Loop through and file block size. Then implment read like it contains vectors. Finnally call ReverseIt.
*/
				for(i = n_dims_input-1; i >= 0; i--) {
					if(stride[i] != 1) {
						block_read_limit = i;
						break;
					}
				}
				n_elem_block = 1;
				for(i = 0; i < n_dims_input; i++) {
					if(i > block_read_limit) {
/*
* To be in this range stride is 1
* Still could be reverse.
*/
						n_elem_block *= output_dim_sizes[i];
						switch(compare_sel[i]) {
						case NCLFILE_DEC:
							real_stride[i] = (stride[i]);
							current_finish[i] = start[i];
							current_index[i] = finish[i];
							break;
						case NCLFILE_INC:
							real_stride[i] = (stride[i]);
							current_finish[i] = finish[i];
							current_index[i] = start[i];
							break;
						}
					} else {
						switch(compare_sel[i]) {
						case NCLFILE_DEC:
							real_stride[i] = (stride[i]);
							break;
						case NCLFILE_INC:
							real_stride[i] = (stride[i]);
							break;
						}
						current_index[i] = start[i];
						current_finish[i] = current_index[i];
					}
				}
				val = (void*)NclMalloc(total_elements*_NclSizeOf(thefile->file.var_info[index]->data_type));
				swap_space = NclMalloc(n_elem_block * _NclSizeOf(thefile->file.var_info[index]->data_type));
				to = 0;
				while(!done) {
					if(vtype == FILE_VAR_ACCESS) {
						(*thefile->file.format_funcs->read_var_ns)(
							thefile->file.private_rec,
							thefile->file.var_info[index]->var_name_quark,
							current_index,
							current_finish,
							(void*)&(((char*)val)[to]));
					} else {
						(*thefile->file.format_funcs->read_coord_ns)(
							thefile->file.private_rec,
							thefile->file.var_info[index]->var_name_quark,
							current_index,
							current_finish,
							val);
					}
					if((n_dims_input - (block_read_limit + 1))>=1) {
						ReverseIt((void*)&(((char*)val)[to]),swap_space,n_dims_input - (block_read_limit + 1),&(compare_sel[block_read_limit+1]),&(output_dim_sizes[block_read_limit+1]),_NclSizeOf(thefile->file.var_info[index]->data_type));
					}
					to += n_elem_block * _NclSizeOf(thefile->file.var_info[index]->data_type);
/*
* No reverse here so all is just added
*/
					for(i = block_read_limit; i >= 0 ; i--) {
						switch(compare_sel[i]) {
						case NCLFILE_DEC:
							if((current_index[i] + real_stride[i] < finish[i])&&(i != 0)) {
								current_index[i] = start[i];
								current_finish[i] = current_index[i];
							} else {
								current_index[i] +=  real_stride[i];
								current_finish[i] = current_index[i];
								inc_done = 1;
							}
							break;
						case NCLFILE_INC:
							if((current_index[i] + real_stride[i]> finish[i])&&(i != 0)) {
								current_index[i] = start[i];
								current_finish[i] = current_index[i];
							} else {
								current_index[i] +=  real_stride[i];
								current_finish[i] = current_index[i];
								inc_done = 1;
							}
							break;
						}
						if(inc_done) {
							inc_done = 0;
							break;
						}
					}
					if(compare_sel[0] == NCLFILE_INC) {
						if(current_index[0] > finish[0]) {
							done = 1;
						}
					} else {
						if(current_index[0] < start[0]) {
							done = 1;
						}
					}
					current_finish[0] = current_index[0];
				}
				NclFree(swap_space);
				n_dims_output = n_dims_input;

				if(sel_ptr != NULL)  {
					i = 0;
					while((i <  n_dims_output)&&(n_dims_output > 1)) {
						if((output_dim_sizes[i] == 1)&&!(keeper[i])) {
							for(j = i; j < n_dims_output-1; j++) {
								output_dim_sizes[j] = output_dim_sizes[j+1];
								(dim_info)[j] = (dim_info)[j+1];
							}
							n_dims_output--;
						} else {
							i++;
						}
					}
				}
			} else {
/*
* has vectors or reorder or both
*/
				val = (void*)NclMalloc(total_elements*_NclSizeOf(thefile->file.var_info[index]->data_type));
				to = 0;
				block_read_limit = n_dims_input - 1 ;
/*
* Find out what size chunks can be read in at once
*/
				for(i = n_dims_input-1 ; i>= 0; i--) {
					if((compare_sel[index_map[i]] != NCLFILE_INC)||(index_map[i] != i)||(stride[index_map[i]] != 1)) {
						block_read_limit = i;
						break;
					}
				}
/*
* Initialize starting index, finish and stride values for first read
*/
                        	n_elem_block = 1;
                        	for(i = 0; i < n_dims_input ; i++) {
                                	current_index[index_map[i]] = start[index_map[i]];
                                	if(i > block_read_limit) {
/*
* OK to use i here since these indices are in order also above loop filter strides so stride 
* = 1 ###
*/
                                        	n_elem_block *= output_dim_sizes[index_map[i]];
                                        	current_finish[index_map[i]] = finish[index_map[i]];
                                        	real_stride[index_map[i]] = stride[index_map[i]];
                                	} else {
                                        	switch(compare_sel[index_map[i]]) {
                                        	case NCLFILE_INC:
                                                	current_finish[index_map[i]] = current_index[index_map[i]] ;
                                                	real_stride[index_map[i]] = stride[index_map[i]];
                                                	break;
                                        	case NCLFILE_DEC:
                                                	current_finish[index_map[i]] = current_index[index_map[i]] ;
                                                	real_stride[index_map[i]] = stride[index_map[i]];
                                                	break;
                                        	default:         /* vectors */
                                                	current_finish[index_map[i]]  = current_index[index_map[i]];
                                                	real_stride[index_map[i]] = stride[index_map[i]];
                                                	break;
                                        	}
                                	}
                        	}
				while(!done) {
					if(vtype == FILE_VAR_ACCESS) {
						(*thefile->file.format_funcs->read_var_ns)(
							thefile->file.private_rec,
							thefile->file.var_info[index]->var_name_quark,
							current_index,
							current_finish,
							(void*)&(((char*)val)[to]));
					} else {
						(*thefile->file.format_funcs->read_coord_ns)(
							thefile->file.private_rec,
							thefile->file.var_info[index]->var_name_quark,
							current_index,
							current_finish,
							(void*)&(((char*)val)[to]));
					}
					to += n_elem_block * _NclSizeOf(thefile->file.var_info[index]->data_type);
					if(compare_sel[index_map[block_read_limit]] < 0) {
						current_index[index_map[block_read_limit]] += real_stride[index_map[block_read_limit]];
						current_finish[index_map[block_read_limit]] = current_index[index_map[block_read_limit]];
					} else {
						compare_sel[index_map[block_read_limit]]++;
					}
					for(i = block_read_limit; i > 0; i--) {
						switch(compare_sel[index_map[i]]) {
						case NCLFILE_INC:
							if(current_index[index_map[i]] > finish[index_map[i]]) {
								current_index[index_map[i]] = start[index_map[i]];
								if(compare_sel[index_map[i-1]] < 0 ) {
									current_index[index_map[i-1]] += real_stride[index_map[i-1]];
								} else {
									compare_sel[index_map[i-1]]++;
								}
	
							} else {
								inc_done = 1;
							}	
							current_finish[index_map[i]] = current_index[index_map[i]] ;
							break;
						case NCLFILE_DEC:
							if(current_index[index_map[i]] < finish[index_map[i]]) {
								current_index[index_map[i]] = start[index_map[i]];
								if(compare_sel[index_map[i-1]] < 0) {
									current_index[index_map[i-1]] += real_stride[index_map[i-1]];
								} else {
									compare_sel[index_map[i-1]]++;
								}
							} else {	
								inc_done = 1;
							}
							current_finish[index_map[i]] = current_index[index_map[i]];
							break;
						default:
							if(compare_sel[index_map[i]] >= sel[index_map[i]].u.vec.n_ind) {
								compare_sel[index_map[i]] = 0;
								current_index[index_map[i]] = sel[index_map[i]].u.vec.ind[0];
								if(compare_sel[index_map[i-1]] < 0 ) {
									current_index[index_map[i-1]] += real_stride[index_map[i-1]];
								} else {
									compare_sel[index_map[i-1]]++;
								}
							} else {
								current_index[index_map[i]] = sel[index_map[i]].u.vec.ind[compare_sel[index_map[i]]];
								inc_done = 1;
							}
							current_finish[index_map[i]] = current_index[index_map[i]];
							break;
						} 
						if(inc_done) {
							inc_done = 0;
							break;
						}
					}
					switch(compare_sel[index_map[0]]) {
					case NCLFILE_DEC:
						if(current_index[index_map[0]] < finish[index_map[0]]) 
								done = 1;
						current_finish[index_map[0]] = current_index[index_map[0]]; 
						break;
					case NCLFILE_INC:
						if(current_index[index_map[0]] > finish[index_map[0]]) 
								done = 1;
						current_finish[index_map[0]] = current_index[index_map[0]]; 
						break;
					default:
						if(compare_sel[index_map[0]] >= sel[0].u.vec.n_ind) {
								done = 1;
						}  else {
							current_index[index_map[0]] = sel[0].u.vec.ind[compare_sel[index_map[0]]];
						}
						current_finish[index_map[0]] = current_index[index_map[0]]; 
					}
				}
				n_dims_output = n_dims_input;
				fprintf(stdout,"Temporary comment 9\n");
				if(sel_ptr != NULL) {
					i = 0;
					while((i <  n_dims_output)&&(n_dims_output > 1)) {
						if((output_dim_sizes[i] == 1)&&!(keeper[i])) {
							for(j = i; j < n_dims_output-1; j++) {
								output_dim_sizes[j] = output_dim_sizes[j+1];
								(dim_info)[j] = (dim_info)[j+1];
							}
							n_dims_output--;
						} else {
							i++;
						}
					}
				}
			}
		} 
	} 
	if(FileIsVarAtt(thefile,var_name,NrmStringToQuark(NCL_MISSING_VALUE_ATT))!=-1){
		mis_md = FileReadVarAtt(thefile,var_name,NrmStringToQuark(NCL_MISSING_VALUE_ATT),NULL);
		if(mis_md != NULL) {
			memcpy((void*)&missing_value,mis_md->multidval.val,_NclSizeOf(mis_md->multidval.data_type));
			has_missing = 1;
		}
	} 
	if(vtype == FILE_COORD_VAR_ACCESS) {
		tmp_md = _NclOneDValCoordDataCreate(
			NULL,
			NULL,
			Ncl_OneDValCoordData,
			0,
			val,
			(has_missing ? &missing_value:NULL),
			n_dims_output,
			output_dim_sizes,
			TEMPORARY,
			sel_ptr,
			_NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(thefile->file.var_info[index]->data_type))
			);
	} else {
		tmp_md = _NclCreateMultiDVal(
			NULL,
			NULL,
			Ncl_MultiDValData,
			0,
			val,
			(has_missing ? &missing_value:NULL),
			n_dims_output,
			output_dim_sizes,
			TEMPORARY,
			sel_ptr,
			_NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(thefile->file.var_info[index]->data_type))
			);
	}
	return(tmp_md);
}
static struct _NclMultiDValDataRec* FileReadVarValue
#if	NhlNeedProto
(NclFile thefile, NclQuark var_name, struct _NclSelectionRecord* sel_ptr)
#else 
(thefile, var_name, sel_ptr)
NclFile thefile;
NclQuark var_name;
struct _NclSelectionRecord* sel_ptr;
#endif
{
	NclDimRec dim_info[NCL_MAX_DIMENSIONS];

	return(MyFileReadVarValue(thefile, var_name, sel_ptr,dim_info,FILE_VAR_ACCESS));
}


static struct _NclVarRec *FileReadVar
#if	NhlNeedProto
(NclFile thefile, NclQuark var_name, struct _NclSelectionRecord* sel_ptr)
#else 
(thefile, var_name, sel_ptr)
NclFile thefile;
NclQuark var_name;
struct _NclSelectionRecord* sel_ptr;
#endif
{
	NclMultiDValData tmp_md = NULL;
	NclMultiDValData tmp_att_md = NULL;
	NclFileAttInfoList *step = NULL;
	NclVar tmp_var = NULL;
	int index;
	int att_id,i,j=0;
	NclSelectionRecord tmp_sel;
	NclDimRec dim_info[NCL_MAX_DIMENSIONS];
	int coords[NCL_MAX_DIMENSIONS];
	NclSelection *sel = NULL;
	NclObj  att_obj = NULL;
	int single = 0;
	NhlArgVal udata;
/*
* By the the time it gets here the file suport routines in that build the selection
* record have made sure var_name is valid and all the demensions in sel_ptr
* are valid. However, the values have not been checked for out_of_ranges
* subscripts
*/
	index = FileIsVar(thefile,var_name);
	if(index > -1) {
		tmp_md = MyFileReadVarValue(thefile,var_name,sel_ptr,dim_info,FILE_VAR_ACCESS);
		if(tmp_md == NULL) {
			return(NULL);
		}

		if(thefile->file.var_att_ids[index] == -1)
			LoadVarAtts(thefile,var_name);
		att_id = thefile->file.var_att_ids[index];
		att_obj = (NclObj)_NclCopyAtt((NclAtt)_NclGetObj(att_id),NULL);
		if(att_obj != NULL) {
			att_id = att_obj->obj.id;
		} else {
			att_id = -1;
		}
		if(sel_ptr == NULL) {
/*
* Because some file may allow dimensions of size 1 special care must be taken here
*/
			for(i = 0 ; i < tmp_md->multidval.n_dims; i++){
				if(_NclFileVarIsCoord(thefile,
					dim_info[i].dim_quark)!= -1) {
					tmp_var = _NclFileReadCoord(thefile,dim_info[i].dim_quark,NULL);
					if(tmp_var != NULL) {
                                		coords[i] = tmp_var->obj.id;
					} else {
						coords[i] = -1;
					}
				} else {
					coords[i] = -1;
				}
			}
			sel = NULL;
		} else {
			sel = sel_ptr->selection;
			tmp_sel.n_entries = 1;
			tmp_sel.selected_from_sym = NULL;
			tmp_sel.selected_from_var = NULL;
			tmp_sel.selection[0].dim_num = 0;
			j = 0;
			for(i = 0 ; i < thefile->file.var_info[index]->num_dimensions; i++){
				if(_NclFileVarIsCoord(thefile,
					thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[sel[i].dim_num]]->dim_name_quark)!= -1) {
					tmp_sel.selection[0] = sel[i];
					tmp_sel.selection[0].dim_num = 0;
					tmp_var = _NclFileReadCoord(thefile,thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[sel[i].dim_num]]->dim_name_quark,&tmp_sel);
					if(tmp_var != NULL) {
						if(sel[i].sel_type == Ncl_VECSUBSCR) {
							if((tmp_var->var.n_dims == 1)&&(tmp_var->var.dim_info[0].dim_size == 1)) {
								single = 1;
							}
				
						} else {
							if(sel[i].u.sub.start == sel[i].u.sub.finish) {
                                                	        single = sel[i].u.sub.is_single;
                                                	}
						}
						coords[j] = tmp_var->obj.id;
					} else {
						return(NULL);
					}
				} else {
					switch(sel[i].sel_type) {
					case Ncl_VECSUBSCR:
						if(sel[i].u.vec.n_ind == 1) {
							single = 1;
						}
						break;
					case Ncl_SUB_ALL:
						if(thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[sel[i].dim_num]]->dim_size == 1) {
							single = 0;
						}
						break;
					case Ncl_SUB_VAL_DEF:
						if(sel[i].u.sub.start == thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[sel[i].dim_num]]->dim_size -1) {
							single = 0;
						}
						break;
					case Ncl_SUB_DEF_VAL:
						if(sel[i].u.sub.finish== 0) {
							single = 0;
						}
						break;
					case Ncl_SUBSCR:
						if(sel[i].u.sub.start == sel[i].u.sub.finish) {
							single = sel[i].u.sub.is_single;
						}
						break;
					}
					coords[j] = -1;
				}
				if(single) {
					if(coords[j] != -1) {
						if(att_id == -1) {
							att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);
						} 
						_NclAddAtt(att_id,NrmQuarkToString(thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[sel[i].dim_num]]->dim_name_quark),_NclVarValueRead(tmp_var,NULL,NULL),&tmp_sel);
						coords[j] = -1;
						if(tmp_var->obj.status != PERMANENT) {
							_NclDestroyObj((NclObj)tmp_var);
						}
					}
					single = 0;
				} else {
					j++;
				}
			}
		}
		tmp_var = NULL;
		
		
	
		if(tmp_md != NULL) {
			tmp_var = _NclVarCreate(
				NULL,
				NULL,
				Ncl_Var,
				0,
				NULL,
				tmp_md,
				dim_info,
				att_id,
				coords,
				(sel == NULL ? FILEVAR : FILEVARSUBSEL),
				NrmQuarkToString(var_name),TEMPORARY);
			if(tmp_var == NULL) {
				_NclDestroyObj((NclObj)tmp_md);
				if(att_id != -1) {
					_NclDestroyObj((NclObj)_NclGetObj(att_id));
				}
			}
		}
	}
	return(tmp_var);
}


static int FileIsVarAtt
#if	NhlNeedProto
(NclFile thefile,NclQuark var,NclQuark theatt)
#else 
(thefile,var,theatt)
NclFile thefile;
NclQuark var;
NclQuark theatt;
#endif
{
	int index,i;
	NclFileAttInfoList *step_att;

	index = FileIsVar(thefile,var);
	i = 0;
	if(index > -1) {
		step_att = thefile->file.var_att_info[index];
		while(step_att != NULL) {
			if(step_att->the_att->att_name_quark == theatt) {
				return(i);
			} else {
				i++;
				step_att = step_att->next;
			}
		}
	}
	return(-1);
}
static struct _NclMultiDValDataRec *FileReadVarAtt
#if	NhlNeedProto
(NclFile thefile, NclQuark var, NclQuark attname, struct _NclSelectionRecord *sel_ptr)
#else 
(thefile, var, attname, sel_ptr)
NclFile thefile;
NclQuark var;
NclQuark attname;
struct _NclSelectionRecord *sel_ptr;
#endif
{
	int aindex,index;
	NclFileAttInfoList *step;
	int att_id = -1;
	void *val;
	NclMultiDValData tmp_md;
	NhlArgVal udata;

	aindex = FileIsVarAtt(thefile,var,attname);
	if(aindex > -1) {
		index = FileIsVar(thefile,var);
		if(thefile->file.var_att_ids[index] == -1) 
			LoadVarAtts(thefile,var);
		return(_NclGetAtt(thefile->file.var_att_ids[index],NrmQuarkToString(attname),sel_ptr));
	}
	NhlPError(NhlWARNING,NhlEUNKNOWN,"FileReadVarAtt: (%s) is not an attribute of (%s)",NrmQuarkToString(attname),NrmQuarkToString(var));
	return(_NclCreateMissing());
}

static int FileIsAtt
#if	NhlNeedProto
(NclFile thefile,NclQuark theatt)
#else 
(thefile,theatt)
NclFile thefile;
NclQuark theatt;
#endif
{
	int i;

	for( i = 0; i < thefile->file.n_file_atts; i++) {
		if(thefile->file.file_atts[i]->att_name_quark == theatt) {
			return(i);
		}
	}
	return(-1);
}


static int FileIsDim
#if	NhlNeedProto
(NclFile thefile, NclQuark dim_name)
#else 
(thefile, dim_name)
NclFile thefile;
NclQuark dim_name;
#endif
{
	int i;
	
	for(i =0; i < thefile->file.n_file_dims; i++) {
		if(thefile->file.file_dim_info[i]->dim_name_quark == dim_name) {
			return(i);
		}
	} 
	return (-1);
}

static int FileVarIsDim
#if	NhlNeedProto
(NclFile thefile,NclQuark var, NclQuark dim_name)
#else 
(thefile,var,dim_name)
NclFile thefile;
NclQuark var;
NclQuark dim_name;
#endif
{
	int i,j;

	for(i = 0 ; i < thefile->file.n_vars; i++) {
		if(thefile->file.var_info[i]->var_name_quark == var) {
			for(j = 0; j < thefile->file.var_info[i]->num_dimensions; j++){
				if(FileGetDimName(thefile,thefile->file.var_info[i]->file_dim_num[j]) == dim_name) {
					return(j);
				}
			}
			return(-1);
		}
	}
	return(-1);
}


static int FileIsCoord
#if	NhlNeedProto
(NclFile thefile, NclQuark coord_name)
#else 
(thefile,coord_name)
NclFile thefile;
NclQuark coord_name;
#endif
{
	int index;

	index = FileIsDim(thefile,coord_name);
	if(index > -1) {
		if(thefile->file.coord_vars[index] != NULL) {
			return(index);
		} 
	}
	return(-1);
}


NclFile _NclCreateFile
#if	NhlNeedProto
(NclObj  inst, NclObjClass theclass, NclObjTypes obj_type, unsigned int obj_type_mask, NclStatus status, NclQuark path,int rw_status)
#else
(inst, theclass, obj_type, obj_type_mask, status, path, rw_status)
NclObj  inst;
NclObjClass theclass;
NclObjTypes obj_type;
unsigned int obj_type_mask;
NclStatus status; 
NclQuark path;
int rw_status;
#endif
{
	char *the_path = NrmQuarkToString(path);
	NclQuark the_real_path = -1;
	char *last_slash = NULL;
	char *end_of_name = NULL;
	char *tmp_path = NULL;
	int len_path = 0;
	char buffer[NCL_MAX_STRING];
	NclQuark fname_q;
	int i,j;
	NclFile file_out = NULL;
	int file_out_free = 0;
	NhlErrorTypes ret= NhlNOERROR;
	NclObjClass class_ptr;
	NclQuark file_ext_q;
	NclQuark *name_list;
	int n_names;
	NclQuark *name_list2;
	int n_names2;
	int index;
	struct stat buf;
	NhlBoolean is_http = False;

	if (! strncmp(the_path,"http://",7))
		is_http = True;

	ret = _NclInitClass(nclFileClass);
	if(ret < NhlWARNING) 
		return(NULL);
	if(theclass == NULL) {
		class_ptr = nclFileClass;
	} else {
		class_ptr = theclass;
	}
	last_slash = strrchr(the_path,'/');
	if(last_slash == NULL) {
		last_slash = the_path;
		len_path = 0;
	}  else {
/*
* skip over '/'
*/
		last_slash++;
	}
	end_of_name = strrchr(last_slash,'.');
	if (is_http) {
		if (end_of_name == NULL) {
			end_of_name = &last_slash[strlen(last_slash)];
		}
		len_path = end_of_name - the_path;
		i = 0;
		while(last_slash != end_of_name) {
			buffer[i] = *last_slash;
			i++;
			last_slash++;
		}
		buffer[i] = '\0';
		fname_q = NrmStringToQuark(buffer);
		file_ext_q = NrmStringToQuark("nc");
	}
	else if(end_of_name == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"(%s) has no file extension, can't determine type of file to open",NrmQuarkToString(path));
		return(NULL);
	} else {
		len_path = end_of_name - the_path;
		i = 0;
		while(last_slash != end_of_name) {
			buffer[i] = *last_slash;
			i++;
			last_slash++;
		}
		buffer[i] = '\0';
		fname_q = NrmStringToQuark(buffer);
/*
* skip over '.'
*/
		end_of_name++;
		file_ext_q = NrmStringToQuark(end_of_name);
		
	}

 	/*
     	 * If a GRIB file, check version.  First verify that the file exists
     	 * and is accessible (path to it must be searchable). _NclFormatEqual handles the 
	 * case-less comparison of all possible variants of the the extension.
	 * Note we also need to check here for extensions added to the real path.
     	 */
	if (_NclFormatEqual(NrmStringToQuark("grb"),NrmStringToQuark(end_of_name))) {
		the_real_path = path;
		if(stat(_NGResolvePath(NrmQuarkToString(path)),&buf) == -1) {
			tmp_path = NclMalloc(len_path+1);
			strncpy(tmp_path,the_path,len_path);
			tmp_path[len_path] = '\0';
			if(stat(_NGResolvePath(tmp_path),&buf) == -1) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  "_NclCreateFile: Requested file does not exist as (%s) or as (%s), can't add file",
					  the_path,tmp_path);
				NclFree(tmp_path);
				return(NULL);
			} else {
				the_real_path = NrmStringToQuark(tmp_path);
				NclFree(tmp_path);
			}
		}
                grib_version = _NclGribVersion(NrmStringToQuark(_NGResolvePath(NrmQuarkToString(the_real_path))));
        }

	if(inst == NULL) {
		file_out = (NclFile)NclMalloc(sizeof(NclFileRec));
		file_out_free = 1;
	} else {
		file_out = (NclFile)inst;
	}
	file_out->file.fname = fname_q;
	file_out->file.file_format = 0;
	file_out->file.n_vars = 0;
	file_out->file.file_atts_id = -1;
	for(i = 0; i < NCL_MAX_FVARS; i++) {
		file_out->file.var_info[i] = NULL;
		file_out->file.file_atts[i] = NULL;
		file_out->file.var_att_info[i] = NULL;
		file_out->file.var_att_udata[i] = NULL;
		file_out->file.var_att_cb[i] = NULL;
		file_out->file.var_att_ids[i] = -1;
		file_out->file.file_dim_info[i] = NULL;
		file_out->file.coord_vars[i] = NULL;
	}
	file_out->file.format_funcs = _NclGetFormatFuncs(file_ext_q);
	if (! file_out->file.format_funcs) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"An internal error has occurred. The file format requested does not appear to be supported, could not open (%s)",NrmQuarkToString(path));
		if(file_out_free) 
			NclFree((void*)file_out);
		return(NULL);
	}
	file_out->file.private_rec = (*file_out->file.format_funcs->initialize_file_rec)(&file_out->file.file_format);
	if(file_out->file.private_rec == NULL) {
		NhlPError(NhlFATAL,ENOMEM,NULL);
		if(file_out_free) 
			NclFree((void*)file_out);
		return(NULL);
	}
	if (file_out->file.format_funcs->set_option != NULL) {
		NclFileClassPart *fcp = &(nclFileClassRec.file_class);
		for (i = 0; i < fcp->num_options; i++) {
			if (file_out->file.format_funcs != _NclGetFormatFuncs(fcp->options[i].format)) {
				continue;
			}
			if (fcp->options[i].access == 1 && rw_status != 1)
				continue;
			else if (fcp->options[i].access == 2 && rw_status > 0)
				continue;
			else if (fcp->options[i].access == 3 && rw_status != -1)
				continue;
			file_out->file.format_funcs->set_option(file_out->file.private_rec,fcp->options[i].name,
								fcp->options[i].value->multidval.data_type,
								fcp->options[i].value->multidval.totalelements,
								fcp->options[i].value->multidval.val);
		}
	}					
	if (is_http) {
		file_out->file.fpath = the_real_path = path;
		file_out->file.wr_status = rw_status;
		file_out->file.private_rec = (*file_out->file.format_funcs->open_file)(file_out->file.private_rec,
								the_real_path,rw_status);
		if(! file_out->file.private_rec) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not open (%s)",NrmQuarkToString(the_real_path));
			if(file_out_free) 
				NclFree((void*)file_out);
			return(NULL);
		}
	}
	else {
		if((file_out->file.format_funcs->open_file != NULL)&&((rw_status != -1)||
								      (file_out->file.format_funcs->create_file != NULL))) {
			
			if(rw_status == -1) {
				file_out->file.fpath = the_real_path = path;
				file_out->file.wr_status = rw_status;
				file_out->file.private_rec = (*file_out->file.format_funcs->create_file)
					(file_out->file.private_rec,
					 NrmStringToQuark(_NGResolvePath(NrmQuarkToString(the_real_path))));
				if(! file_out->file.private_rec) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not create (%s)",NrmQuarkToString(the_real_path));
					if(file_out_free) 
						NclFree((void*)file_out);
					return(NULL);
				}
			} else {
				if(stat(_NGResolvePath(NrmQuarkToString(path)),&buf) == -1) {
					tmp_path = NclMalloc(len_path+1);
					strncpy(tmp_path,the_path,len_path);
					tmp_path[len_path] = '\0';
					if(stat(_NGResolvePath(tmp_path),&buf) == -1) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclCreateFile: Requested file does not exist as (%s) or as (%s), can't add file",the_path,tmp_path);
						NclFree(tmp_path);
						return(NULL);
					} else {
						the_real_path = NrmStringToQuark(tmp_path);
						file_out->file.fpath = the_real_path;
						NclFree(tmp_path);
					}
				} else {
					the_real_path = path;
					file_out->file.fpath = the_real_path;
				}
				file_out->file.wr_status = rw_status;
				file_out->file.private_rec = (*file_out->file.format_funcs->open_file)
					(file_out->file.private_rec,
					 NrmStringToQuark(_NGResolvePath(NrmQuarkToString(the_real_path))),rw_status);	
				if(! file_out->file.private_rec) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not open (%s)",NrmQuarkToString(the_real_path));
					if(file_out_free) 
						NclFree((void*)file_out);
					return(NULL);
				}
			}
		} else  {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"An internal error in the extension code for the requested file format has occurred, could not open (%s)",NrmQuarkToString(the_real_path));
		if(file_out_free) 
			NclFree((void*)file_out);
			return(NULL);
		}
	}
	if(file_out->file.format_funcs->get_var_names != NULL) {
		name_list = (*file_out->file.format_funcs->get_var_names)(file_out->file.private_rec,&n_names);
		file_out->file.n_vars = n_names;
		if(n_names > NCL_MAX_FVARS) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"The file (%s) contains (%d) variable which  exceeds the number of allowable variables (%d), ",NrmQuarkToString(path),n_names,NCL_MAX_FVARS);
			NclFree((void*)name_list);
			if(file_out_free) 
				NclFree((void*)file_out);
			return(NULL);
		}
		for(i = 0; i < n_names; i++){
			file_out->file.var_info[i] = (*file_out->file.format_funcs->get_var_info)(file_out->file.private_rec,name_list[i]);
			if(file_out->file.format_funcs->get_var_att_names != NULL) {
				name_list2 = (*file_out->file.format_funcs->get_var_att_names)(file_out->file.private_rec,name_list[i],&n_names2);
				for(j = 0; j<n_names2; j++) {
					AddAttInfoToList(&(file_out->file.var_att_info[i]),
						(*file_out->file.format_funcs->get_var_att_info)(file_out->file.private_rec,name_list[i],name_list2[j]));
				}
				NclFree((void*)name_list2);
			} else {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"Can not access variable attributes for the file format");
			}
		}
		NclFree((void*)name_list);
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not get variable names for file (%s), can't add file",NrmQuarkToString(the_real_path));
		if(file_out_free) 
			NclFree((void*)file_out);
		return(NULL);
	}
	if(file_out->file.format_funcs->get_dim_names!= NULL) {
		name_list = (*file_out->file.format_funcs->get_dim_names)(file_out->file.private_rec,&n_names);
		file_out->file.n_file_dims = n_names;
		for(i = 0; i < n_names; i++){
			file_out->file.file_dim_info[i] = (*file_out->file.format_funcs->get_dim_info)(file_out->file.private_rec,name_list[i]);
			index = FileIsVar(file_out,name_list[i]);
			if(index > -1 && file_out->file.var_info[index]->num_dimensions == 1) {
				file_out->file.coord_vars[i] = file_out->file.var_info[index];
			}
		}
		NclFree((void*)name_list);
	} else {
/*
* Need code to free already allocated information
*/
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not get dimension names for file (%s), can't add file",NrmQuarkToString(the_real_path));
		if(file_out_free) 
			NclFree((void*)file_out);
	
		return(NULL);
	}
	if(file_out->file.format_funcs->get_att_names != NULL) {
		name_list = (*file_out->file.format_funcs->get_att_names)(file_out->file.private_rec,&n_names);
		file_out->file.n_file_atts = n_names;
		if(n_names > NCL_MAX_FVARS) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"Maximum number of file attributes (%d) exceeded, must ignore some attributes",NCL_MAX_FVARS);
			n_names = NCL_MAX_FVARS;
		}
		for(i = 0; i < n_names; i++) {
			file_out->file.file_atts[i] = (*file_out->file.format_funcs->get_att_info)(file_out->file.private_rec,name_list[i]);
		}
		NclFree((void*)name_list);
	} else {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"Could not get attribute names for file (%s), no attributes added ",NrmQuarkToString(the_real_path));
	}
	(void)_NclObjCreate((NclObj)file_out,class_ptr,obj_type,(obj_type_mask | Ncl_File),status);
	if(class_ptr == nclFileClass){
		_NclCallCallBacks((NclObj)file_out,CREATED);
	}
	return(file_out);
}

static NhlErrorTypes MyFileWriteVar
#if	NhlNeedProto
(NclFile thefile, NclQuark var, struct _NclMultiDValDataRec *value,struct _NclSelectionRecord * sel_ptr,NclQuark *dim_names,int type)
#else 
(thefile, var, value, sel_ptr,dim_names,type)
NclFile thefile;
NclQuark var;
struct _NclMultiDValDataRec *value;
struct _NclSelectionRecord * sel_ptr;
NclQuark *dim_names;
int type;
#endif
{
	NclObjTypes lhs_type,rhs_type;
	NclMultiDValData tmp_md = NULL;
	NclMultiDValData mis_md = NULL;
	NclQuark new_dim_quarks[NCL_MAX_DIMENSIONS];
	long 	new_dim_sizes[NCL_MAX_DIMENSIONS];
	
	int has_missing = 0;
	int update_unlimited = 0;
	char buffer[8];
	void *val;
	NhlErrorTypes ret = NhlNOERROR;
	int index,dindex;
	long start[NCL_MAX_DIMENSIONS];
	long finish[NCL_MAX_DIMENSIONS];
	long stride[NCL_MAX_DIMENSIONS];
	long real_stride[NCL_MAX_DIMENSIONS];
	int i,j,k,done = 0,inc_done = 0;
	int n_dims_target,n_elem = 1;
	int n_dims_selection;
	int total_elements = 1;
	int has_vectors = 0;
	int has_stride = 0;
	int has_reverse = 0;
	int has_reorder = 0;
	int from = 0,block_write_limit,n_elem_block;
	NclFDimRec *tmpfdim = NULL;
	
	int multiplier_target[NCL_MAX_DIMENSIONS];
	int compare_sel[NCL_MAX_DIMENSIONS];
	long current_index[NCL_MAX_DIMENSIONS];
	long current_finish[NCL_MAX_DIMENSIONS];
	int keeper[NCL_MAX_DIMENSIONS];
	int index_map[NCL_MAX_DIMENSIONS];
	int selection_dim_sizes[NCL_MAX_DIMENSIONS];
	NclSelection *sel;
	float tmpf;
	NclScalar *tmp_mis;
	NclScalar tmp_scalar;
	NclScalar tmp_scalar0;
	int tmp_size = 1,tmpi;
	void *data_type;
	NclBasicDataTypes from_type,to_type;
	NclObjTypes obj_type;
	int result = 0;
	int free_tmp_md = 0;

	if(thefile->file.wr_status <= 0) {
		index = FileIsVar(thefile,var);
		if(index > -1) {
			n_dims_target = thefile->file.var_info[index]->num_dimensions;
			if(sel_ptr != NULL) {
				sel = sel_ptr->selection;
				for(i = 0; i < n_dims_target; i++) {
					switch(sel->sel_type) {
					case Ncl_SUB_ALL:
						start[sel->dim_num] = 0;
					case Ncl_SUB_VAL_DEF:
						if(sel->sel_type == Ncl_SUB_VAL_DEF) {
							start[sel->dim_num] = sel->u.sub.start;
						}
						finish[sel->dim_num] = thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[sel->dim_num]]->dim_size-1;
					case Ncl_SUB_DEF_VAL:
						if(sel->sel_type == Ncl_SUB_DEF_VAL) {
							start[sel->dim_num] = 0;
							finish[sel->dim_num] = sel->u.sub.finish;
						} 
					case Ncl_SUBSCR:
						if(sel->u.sub.is_single) {
							keeper[sel->dim_num] = 0;
						} else {
							keeper[sel->dim_num] = 1;
						}
						if(sel->sel_type == Ncl_SUBSCR) {
							start[sel->dim_num] = sel->u.sub.start;
							finish[sel->dim_num] = sel->u.sub.finish;
							stride[sel->dim_num] = sel->u.sub.stride;
						} else {
							stride[sel->dim_num] = sel->u.sub.stride;
						}
						if(finish[sel->dim_num] < start[sel->dim_num]) {
							if(stride[sel->dim_num] < 0) {
								tmpi = finish[sel->dim_num] + (start[sel->dim_num] - finish[sel->dim_num]) % abs(stride[sel->dim_num]);
								finish[sel->dim_num] = start[sel->dim_num];
								start[sel->dim_num] = tmpi;
								compare_sel[sel->dim_num] = NCLFILE_INC;
								stride[sel->dim_num] = -(stride[sel->dim_num]); 
							} else {
								compare_sel[sel->dim_num] = NCLFILE_DEC;
								stride[sel->dim_num] = -(stride[sel->dim_num]); 
								has_reverse = 1;
							}
						} else {
							if(stride[sel->dim_num] < 0) {
								has_reverse = 1;
								tmpi = finish[sel->dim_num] - (finish[sel->dim_num] - start[sel->dim_num]) % abs(stride[sel->dim_num]);
								finish[sel->dim_num] = start[sel->dim_num];
								start[sel->dim_num] = tmpi;
								compare_sel[sel->dim_num] = NCLFILE_DEC;
								stride[sel->dim_num] = (stride[sel->dim_num]);
							} else {
								compare_sel[sel->dim_num] = NCLFILE_INC;
								stride[sel->dim_num] = (stride[sel->dim_num]);
							}
						}

						if(abs(stride[sel->dim_num]) > 1) {
							has_stride = 1;
						}
						if(stride[sel->dim_num] != 0)  {
							tmpf = (float)fabs(((float)sel->u.sub.stride));
						} else {
							NhlPError(NhlWARNING,NhlEUNKNOWN,"Invalid stride: stride must be positive non-zero integer");
							stride[sel->dim_num] = 1;
							tmpf = 1;
						}
						n_elem = (int)(fabs(((double)(finish[sel->dim_num] -start[sel->dim_num])))/tmpf) + 1;

						if((sel->u.sub.start > thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[sel->dim_num]]->dim_size-1 )||(sel->u.sub.start < 0)) {
							if(!( thefile->file.file_dim_info[ thefile->file.var_info[index]->file_dim_num[sel->dim_num]]->is_unlimited)||(sel->u.sub.start < 0)) {
								NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
                                        			return(NhlFATAL);
							} else if(sel->u.sub.start >= thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[sel->dim_num]]->dim_size){
								update_unlimited = 1;
							}
                                		}
                                		if((sel->u.sub.finish> thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[sel->dim_num]]->dim_size-1)||(sel->u.sub.finish < 0)) {
							if(!( thefile->file.file_dim_info[ thefile->file.var_info[index]->file_dim_num[sel->dim_num]]->is_unlimited)||(sel->u.sub.finish < 0)) {
								NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
                                        			return(NhlFATAL);
							} else if(sel->u.sub.finish >= thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[sel->dim_num]]->dim_size){
								update_unlimited = 1;
							}
                                		}
						if(sel->dim_num != i) {
							has_reorder = 1;
						}
						index_map[i] = sel->dim_num;
						break;
					case Ncl_VECSUBSCR:
						keeper[sel->dim_num] = 0;
						if((sel->u.vec.min < 0 ) || (sel->u.vec.min >= thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[sel->dim_num]]->dim_size)){
							if(!( thefile->file.file_dim_info[ thefile->file.var_info[index]->file_dim_num[sel->dim_num]]->is_unlimited)||(sel->u.vec.min < 0)) {
								NhlPError(NhlFATAL,NhlEUNKNOWN,"Vector subscript out of range, error in subscript #%d",i);
                                        			return(NhlFATAL);
							} else if(sel->u.vec.min >= thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[sel->dim_num]]->dim_size){
								update_unlimited = 1;
							}
						}
						if((sel->u.vec.max < 0)||(sel->u.vec.max >= thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[sel->dim_num]]->dim_size)) {
							if(!( thefile->file.file_dim_info[ thefile->file.var_info[index]->file_dim_num[sel->dim_num]]->is_unlimited)||(sel->u.vec.max < 0)) {
								NhlPError(NhlFATAL,NhlEUNKNOWN,"Vector subscript out of range, error in subscript #%d",i);
                                        			return(NhlFATAL);
							} else if(sel->u.vec.max >= thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[sel->dim_num]]->dim_size){
								update_unlimited = 1;
							}
						}
						n_elem = sel->u.vec.n_ind;
						stride[sel->dim_num] = 0;
						start[sel->dim_num] = finish[sel->dim_num] = sel->u.vec.ind[0];
						has_vectors = 1;
						index_map[i] = sel->dim_num;
						if(sel->dim_num != i) {
							has_reorder = 1;
						}
						compare_sel[sel->dim_num] = NCLFILE_VEC;
						break;
					}
					multiplier_target[sel->dim_num] = 1;
					if(sel->dim_num != n_dims_target - 1) {
						for(k = sel->dim_num +1 ; k< n_dims_target; k++) {
							multiplier_target[sel->dim_num] *= (long)thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[k]]->dim_size;
						}
					}
					selection_dim_sizes[i] =n_elem;
					total_elements = total_elements * n_elem;
					sel++;
				}
				sel = sel_ptr->selection;
			} else {
				for(i = 0 ; i < n_dims_target; i++) {
					keeper[i] = 1;
					start[i] = 0;
					if(thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[i]]->is_unlimited) {
						update_unlimited = 1;
						if(value->multidval.dim_sizes[i]> thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[i]]->dim_size) {
							finish[i] = value->multidval.dim_sizes[i] -1;
						} else {
							finish[i] = thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[i]]->dim_size -1;
						
						}
					} else {
						finish[i] = thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[i]]->dim_size -1;
					}
					stride[i] = 1;
					index_map[i] = i;
					total_elements *= (finish[i] + 1);
					selection_dim_sizes[i] = (finish[i]+ 1);
					compare_sel[i] = NCLFILE_INC;
					multiplier_target[i] = 1;
					for(k = i + 1; k < n_dims_target; k++) {
						multiplier_target[i] *= (long)thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[k]]->dim_size;
					}
				}
				sel = NULL;
			}
			n_dims_selection = n_dims_target;		
			i = 0;
			while((i < n_dims_selection)&&(n_dims_selection > 1)) {
				if((selection_dim_sizes[i] == 1)&&!(keeper[i])) {
					for(j = i ; j < n_dims_selection -1;j++) {
						selection_dim_sizes[j] = selection_dim_sizes[j+1];
						keeper[j] = keeper[j+1];
					}
					n_dims_selection--;
				} else {
					i++;
				}
			}
			if(value->multidval.kind != SCALAR) {
				for(i = 0, j = 0; i< n_dims_selection; i++) {
					if (selection_dim_sizes[i] == 1 && value->multidval.dim_sizes[j] != 1)
						continue;
					else if (selection_dim_sizes[i] != 1 && value->multidval.dim_sizes[j] == 1) {
						while (value->multidval.dim_sizes[j] == 1) 
							j++;
					}
					if(selection_dim_sizes[i] != value->multidval.dim_sizes[j]) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Dimension sizes of left hand side do not match right hand side");
						return(NhlFATAL);
					}
					j++;
				}
			} 
			lhs_type = _NclBasicDataTypeToObjType(thefile->file.var_info[index]->data_type);
	
			rhs_type = value->multidval.type->type_class.type ;

			has_missing = (FileIsVarAtt(thefile,var,NrmStringToQuark(NCL_MISSING_VALUE_ATT)) > -1 ? 1 :  0);

			if(lhs_type != rhs_type) {
				if(has_missing) {
					mis_md = FileReadVarAtt(thefile,var,NrmStringToQuark(NCL_MISSING_VALUE_ATT),NULL);
					tmp_md = _NclCoerceData(value,lhs_type,(NclScalar*)mis_md->multidval.val);
				} else {
					tmp_md = _NclCoerceData(value,lhs_type,NULL);
				}
				if(tmp_md == NULL) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"FileWriteVar: Type mismatch, can't perform assignment");
				}
			} else {
				if((has_missing)&&(value->multidval.missing_value.has_missing)) {
					mis_md = FileReadVarAtt(thefile,var,NrmStringToQuark(NCL_MISSING_VALUE_ATT),NULL);
					_Ncleq(value->multidval.type,(void*)&(result),(void*)&(value->multidval.missing_value.value),(void*)(mis_md->multidval.val),NULL,NULL,1,1);
					if (result) {
						tmp_md = value;
					}
					else if(value->obj.status != PERMANENT) {
						tmp_md = value;
						memcpy(&tmp_scalar,mis_md->multidval.val,mis_md->multidval.totalsize);
						_NclResetMissingValue(tmp_md,(NclScalar*) &tmp_scalar);
					} else {

/* Situation where missing values are not equal and can't just overwrite input's*/

						memcpy(&tmp_scalar,mis_md->multidval.val,mis_md->multidval.totalsize);
						tmp_md = _NclCopyVal(value,&tmp_scalar);
						free_tmp_md = 1;
					}
				} else if(value->multidval.missing_value.has_missing) {
					tmp_mis = (NclScalar*)NclMalloc((unsigned)sizeof(NclScalar));
					*tmp_mis = value->multidval.missing_value.value;
					mis_md = _NclCreateMultiDVal(
						NULL,
						NULL,
						Ncl_MultiDValData,
						0,
						(void*)tmp_mis,
						NULL,
						1,
						&tmp_size,
						TEMPORARY,
						NULL,
						_NclTypeEnumToTypeClass(lhs_type));
					FileWriteVarAtt(thefile,var,NrmStringToQuark(NCL_MISSING_VALUE_ATT),mis_md,NULL);
					tmp_md = value;
				} else {
					tmp_md = value;
				}
			}
			if(tmp_md == NULL) {
				return(NhlFATAL);
			}
			if((type == FILE_VAR_ACCESS) ? thefile->file.format_funcs->write_var != NULL:thefile->file.format_funcs->write_coord != NULL ) {
				if((!has_vectors)&&(!has_reverse)&&(!has_reorder)&&(value->multidval.kind != SCALAR)) {
					if(type == FILE_VAR_ACCESS) {
						ret = (*thefile->file.format_funcs->write_var)(
							thefile->file.private_rec,
							var,
							tmp_md->multidval.val,
							start,
							finish,	
							stride);
					} else {
						ret = (*thefile->file.format_funcs->write_coord)(
							thefile->file.private_rec,
							var,
							tmp_md->multidval.val,
							start,
							finish,	
							stride);
					}
/*
* Add unlimited update
*/
					if(update_unlimited) {
						for(i = 0; i < thefile->file.n_file_dims;i++) {
							if(thefile->file.file_dim_info[i]->is_unlimited) {
								tmpfdim= thefile->file.file_dim_info[i];
								thefile->file.file_dim_info[i] = (*thefile->file.format_funcs->get_dim_info)(thefile->file.private_rec,tmpfdim->dim_name_quark);
								NclFree(tmpfdim);
							}
						}
					}
					if(free_tmp_md) {
						_NclDestroyObj((NclObj)tmp_md);
					}

						return(ret);
				} else {
					if(value->multidval.kind != SCALAR) {
						val = tmp_md->multidval.val;
						from = 0;
						block_write_limit = n_dims_target -1;
						for(i = n_dims_target - 1; i >= 0; i--) {
							if((compare_sel[index_map[i]] != NCLFILE_INC)||(index_map[i] != i)) {
								block_write_limit = i;
								break;
							}
						}
					} else {
						block_write_limit = n_dims_target -1;
						val = tmp_md->multidval.val;
						from = 0;
					}
					n_elem_block = 1;
					for(i = 0; i < n_dims_target; i++) {
						current_index[index_map[i]] = start[index_map[i]];
						if(i > block_write_limit) {
							n_elem_block *= selection_dim_sizes[i];
							current_finish[i] = finish[i];
							real_stride[i] = abs(stride[i]);
						} else {
							current_finish[index_map[i]] = current_index[index_map[i]];
							real_stride[index_map[i]] = 1;
						}
					}
					while(!done) {
						if(type == FILE_VAR_ACCESS) {
							ret = (*thefile->file.format_funcs->write_var) (
								thefile->file.private_rec,
								var,
								(void*)&(((char*)val)[from]),
								current_index,
								current_finish,
								real_stride);
						} else {
							ret = (*thefile->file.format_funcs->write_coord) (
								thefile->file.private_rec,
								var,
								(void*)&(((char*)val)[from]),
								current_index,
								current_finish,
								real_stride);
						}
						if(ret < NhlWARNING) {
							if(free_tmp_md) {
								_NclDestroyObj((NclObj)tmp_md);
							}
							return(ret);
						}
						if(value->multidval.kind != SCALAR) {
							from += n_elem_block * _NclSizeOf(thefile->file.var_info[index]->data_type);
						}
						if(compare_sel[index_map[block_write_limit]] < 0) {
							current_index[index_map[block_write_limit]] += stride[index_map[block_write_limit]];
							current_finish[index_map[block_write_limit]] = current_index[index_map[block_write_limit]];
						} else {
							compare_sel[index_map[block_write_limit]]++;
						}
						for( i = block_write_limit; i > 0 ; i--) {
							switch(compare_sel[index_map[i]]) {
							case NCLFILE_INC:
								if(current_index[index_map[i]] > finish[index_map[i]]) {
									current_index[index_map[i]] = start[index_map[i]];
									if(compare_sel[index_map[i-1]] < 0) {
										current_index[index_map[i-1]] += stride[index_map[i-1]];
									} else {
										current_index[index_map[i-1]]++;
									}
								} else {
									inc_done = 1;
								}
								current_finish[index_map[i]] = current_index[index_map[i]];
								break;
							case NCLFILE_DEC:
								if(current_index[index_map[i]] < finish[index_map[i]]) {
									current_index[index_map[i]] = start[index_map[i]];
									if(compare_sel[index_map[i-1]] < 0) {
										current_index[index_map[i-1]] += stride[index_map[i-1]];
									} else {
										compare_sel[index_map[i-1]]++;
									}
								} else {
									inc_done =1;
								}
								current_finish[index_map[i]] = current_index[index_map[i]];
								break;
							default:
								if(compare_sel[index_map[i]] >= sel[index_map[i]].u.vec.n_ind) {
									compare_sel[index_map[i]] = 0;
									current_index[index_map[i]] = sel[index_map[i]].u.vec.ind[0];
									if(compare_sel[index_map[i-1]] < 0 ) {
										current_index[index_map[i-1]] += stride[index_map[i-1]];
									} else {
										compare_sel[index_map[i-1]]++;
									}
								} else {
									current_index[index_map[i]] = sel[index_map[i]].u.vec.ind[compare_sel[index_map[i]]];
									inc_done = 1;
								}
								current_finish[index_map[i]] = current_index[index_map[i]];
								break;
							}
							if(inc_done) {
								inc_done = 0;
								break;
							}
						}
						switch(compare_sel[index_map[0]]) {
						case NCLFILE_DEC:
							if(current_index[index_map[0]] < finish[index_map[0]])
								done = 1;
							current_finish[index_map[0]] = current_index[index_map[0]];
							break;
						case NCLFILE_INC:
							if(current_index[index_map[0]] > finish[index_map[0]])
								done = 1;
							current_finish[index_map[0]] = current_index[index_map[0]];
							break;
						default:
							if(compare_sel[index_map[0]] >= sel[0].u.vec.n_ind) {
								done = 1;
							} else {
								current_index[index_map[0]] = sel[0].u.vec.ind[compare_sel[index_map[0]]];
							}
							current_finish[index_map[0]] = current_index[index_map[0]];
						}
					}	
/*
* Add unlimited update
*/
					if(update_unlimited) {
						for(i = 0; i < thefile->file.n_file_dims;i++) {
							if(thefile->file.file_dim_info[i]->is_unlimited) {
								tmpfdim= thefile->file.file_dim_info[i];
								thefile->file.file_dim_info[i] = (*thefile->file.format_funcs->get_dim_info)(thefile->file.private_rec,tmpfdim->dim_name_quark);
								NclFree(tmpfdim);
							}
						}
					}

					if(free_tmp_md) {
						_NclDestroyObj((NclObj)tmp_md);
					}
					return(ret);
				}
			} else if((type == FILE_VAR_ACCESS) ? thefile->file.format_funcs->write_var_ns != NULL : thefile->file.format_funcs->write_coord_ns != NULL) {
				if((!has_vectors)&&(!has_reverse)&&(!has_reorder)&&(!has_stride)) {	
					if(type == FILE_VAR_ACCESS) {
						ret = (*thefile->file.format_funcs->write_var_ns)(
							thefile->file.private_rec,
							var,
							tmp_md->multidval.val,
							start,
							finish
							);
					} else {
						ret = (*thefile->file.format_funcs->write_coord_ns)(
							thefile->file.private_rec,
							var,
							tmp_md->multidval.val,
							start,
							finish
							);
					}
/*
* Add unlimited update
*/
					if(update_unlimited) {
						for(i = 0; i < thefile->file.n_file_dims;i++) {
							if(thefile->file.file_dim_info[i]->is_unlimited) {
								tmpfdim= thefile->file.file_dim_info[i];
								thefile->file.file_dim_info[i] = (*thefile->file.format_funcs->get_dim_info)(thefile->file.private_rec,tmpfdim->dim_name_quark);
								NclFree(tmpfdim);
							}
						}
					}
					if(free_tmp_md) {
						_NclDestroyObj((NclObj)tmp_md);
					}

					return(ret);
				}else{
/*
* Need code here
*/
					if(value->multidval.kind != SCALAR) {
						val = tmp_md->multidval.val;
						from = 0;
						block_write_limit = n_dims_target -1;
						for(i = n_dims_target - 1; i >= 0; i--) {
							if((compare_sel[index_map[i]] != NCLFILE_INC)||(stride[index_map[i]] != 1)||(index_map[i] != i)) {
								block_write_limit = i;
								break;
							}
						}
					} else {
						block_write_limit = n_dims_target -1;
						val = tmp_md->multidval.val;
						from = 0;
					}
					n_elem_block = 1;
					for(i = 0; i < n_dims_target; i++) {
						current_index[index_map[i]] = start[index_map[i]];
						if(i > block_write_limit) {
							n_elem_block *= selection_dim_sizes[i];
							current_finish[i] = finish[i];
							real_stride[i] = stride[i];
						} else {
							current_finish[index_map[i]] = current_index[index_map[i]];
							real_stride[index_map[i]] = 1;
						}
					}
					while(!done) {
						if(type == FILE_VAR_ACCESS) {
							ret = (*thefile->file.format_funcs->write_var_ns) (
								thefile->file.private_rec,
								var,
								(void*)&(((char*)val)[from]),
								current_index,
								current_finish
								);
						} else {
							ret = (*thefile->file.format_funcs->write_coord_ns) (
								thefile->file.private_rec,
								var,
								(void*)&(((char*)val)[from]),
								current_index,
								current_finish
								);
						}
						if(ret < NhlWARNING) {
							if(free_tmp_md) {
								_NclDestroyObj((NclObj)tmp_md);
							}
							return(ret);
						}
						if(value->multidval.kind != SCALAR) {
							from += n_elem_block * _NclSizeOf(thefile->file.var_info[index]->data_type);
						}
						if(compare_sel[index_map[block_write_limit]] < 0) {
							current_index[index_map[block_write_limit]] += stride[index_map[block_write_limit]];
							current_finish[index_map[block_write_limit]] = current_index[index_map[block_write_limit]];
						} else {
							compare_sel[index_map[block_write_limit]]++;
						}
						for( i = block_write_limit; i > 0 ; i--) {
							switch(compare_sel[index_map[i]]) {
							case NCLFILE_INC:
								if(current_index[index_map[i]] > finish[index_map[i]]) {
									current_index[index_map[i]] = start[index_map[i]];
									if(compare_sel[index_map[i-1]] < 0) {
										current_index[index_map[i-1]] += stride[index_map[i-1]];
									} else {
										current_index[index_map[i-1]]++;
									}
								} else {
									inc_done = 1;
								}
								current_finish[index_map[i]] = current_index[index_map[i]];
								break;
							case NCLFILE_DEC:
								if(current_index[index_map[i]] < finish[index_map[i]]) {
									current_index[index_map[i]] = start[index_map[i]];
									if(compare_sel[index_map[i-1]] < 0) {
										current_index[index_map[i-1]] += stride[index_map[i-1]];
									} else {
										compare_sel[index_map[i-1]]++;
									}
								} else {
									inc_done =1;
								}
								current_finish[index_map[i]] = current_index[index_map[i]];
								break;
							default:
								if(compare_sel[index_map[i]] >= sel[index_map[i]].u.vec.n_ind) {
									compare_sel[index_map[i]] = 0;
									current_index[index_map[i]] = sel[index_map[i]].u.vec.ind[0];
									if(compare_sel[index_map[i-1]] < 0 ) {
										current_index[index_map[i-1]] += stride[index_map[i-1]];
									} else {
										compare_sel[index_map[i-1]]++;
									}
								} else {
									current_index[index_map[i]] = sel[index_map[i]].u.vec.ind[compare_sel[index_map[i]]];
									inc_done = 1;
								}
								current_finish[index_map[i]] = current_index[index_map[i]];
								break;
							}
							if(inc_done) {
								inc_done = 0;
								break;
							}
						}
						switch(compare_sel[index_map[0]]) {
						case NCLFILE_DEC:
							if(current_index[index_map[0]] < finish[index_map[0]])
								done = 1;
							current_finish[index_map[0]] = current_index[index_map[0]];
							break;
						case NCLFILE_INC:
							if(current_index[index_map[0]] > finish[index_map[0]])
								done = 1;
							current_finish[index_map[0]] = current_index[index_map[0]];
							break;
						default:
							if(compare_sel[index_map[0]] >= sel[0].u.vec.n_ind) {
								done = 1;
							} else {
								current_index[index_map[0]] = sel[0].u.vec.ind[compare_sel[index_map[0]]];
							}
							current_finish[index_map[0]] = current_index[index_map[0]];
						}
					}	
/*
* Add unlimited update
*/
					if(update_unlimited) {
						for(i = 0; i < thefile->file.n_file_dims;i++) {
							if(thefile->file.file_dim_info[i]->is_unlimited) {
								tmpfdim= thefile->file.file_dim_info[i];
								thefile->file.file_dim_info[i] = (*thefile->file.format_funcs->get_dim_info)(thefile->file.private_rec,tmpfdim->dim_name_quark);
								NclFree(tmpfdim);
							}
						}
					}
					if(free_tmp_md) {
						_NclDestroyObj((NclObj)tmp_md);
					}
					return(ret);
				}
			}
		} else {
/*
* Need to add variable to file situation
*/
			if(type == FILE_COORD_VAR_ACCESS) {
				if((dindex = FileIsDim(thefile,var)) == -1) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"(%s) is not a dimension in file (%s), can not add coordinate variable",NrmQuarkToString(var),NrmQuarkToString(thefile->file.fpath));
					return(NhlFATAL);
				} else if((thefile->file.file_dim_info[dindex]->dim_size == value->multidval.dim_sizes[0])||(thefile->file.file_dim_info[dindex]->is_unlimited)) {
					if(value->multidval.n_dims != 1) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate variables must be single dimension arrays, attempt to assign (%d) dimension value to coordinate variable",value->multidval.n_dims); 
						return(NhlFATAL);
					}
					new_dim_quarks[0] = var;
					new_dim_sizes[0] = value->multidval.dim_sizes[0];
					start[0] = 0;
					finish[0] = value->multidval.dim_sizes[0] -1 ;
					stride[0] = 1;
					if(thefile->file.file_dim_info[dindex]->is_unlimited) {
						update_unlimited = 1;
					}
				}
			} else {
/*
* Since it is imposible to guess names of dimensions they must
* blindly be added
*/
				if(dim_names == NULL) {
					for(i = 0; i < value->multidval.n_dims; i++) {
						sprintf(buffer,"ncl%d",thefile->file.n_file_dims);
						new_dim_quarks[i] = NrmStringToQuark(buffer);
						new_dim_sizes[i] = (long)value->multidval.dim_sizes[i];
						start[i] = 0;
						finish[i] = value->multidval.dim_sizes[i] -1;
						stride[i] = 1;

						ret = (*thefile->file.format_funcs->add_dim)(
							thefile->file.private_rec,
							new_dim_quarks[i],
							new_dim_sizes[i],
							0);
						if(ret < NhlWARNING) {
							return(ret);
						}
						thefile->file.file_dim_info[thefile->file.n_file_dims] = (*thefile->file.format_funcs->get_dim_info)(thefile->file.private_rec,new_dim_quarks[i]);
						thefile->file.n_file_dims++;
				
					}
				} else {
					for(i = 0 ; i < value->multidval.n_dims; i++) {
						if(dim_names[i] != -1) {
							new_dim_quarks[i] = dim_names[i];
						} else {
							sprintf(buffer,"ncl%d",thefile->file.n_file_dims);
							new_dim_quarks[i] = NrmStringToQuark(buffer);
						}
						new_dim_sizes[i] = value->multidval.dim_sizes[i];
						start[i] = 0;
						finish[i] = value->multidval.dim_sizes[i] - 1;
						stride[i] = 1;
						if((dindex = FileIsDim(thefile,dim_names[i])) == -1) {
							ret = (*thefile->file.format_funcs->add_dim)(
								thefile->file.private_rec,
								new_dim_quarks[i],
								new_dim_sizes[i],
								0);
							if(ret < NhlWARNING) 
								return(ret);
							if (value->multidval.n_dims == 1 && new_dim_quarks[i] == NrmStringToQuark("ncl_scalar")) {
								AdjustForScalarDim(thefile);
							}
							else {
								thefile->file.file_dim_info[thefile->file.n_file_dims] = (*thefile->file.format_funcs->get_dim_info)(thefile->file.private_rec,new_dim_quarks[i]);
								thefile->file.n_file_dims++;
							}
						} else {
							if((thefile->file.file_dim_info[dindex]->dim_size != value->multidval.dim_sizes[i])&&(!(thefile->file.file_dim_info[dindex]->is_unlimited))) {
								NhlPError(NhlFATAL,NhlEUNKNOWN,"File dimension conflict, dimension (%s) has a size of (%d) can not set it to requested size (%d)",NrmQuarkToString(dim_names[i]),thefile->file.file_dim_info[dindex]->dim_size,value->multidval.dim_sizes[i]);
								return(NhlFATAL);
			
							}
							if(thefile->file.file_dim_info[dindex]->is_unlimited) {
								update_unlimited = 1;
							}
						}
					}
				}
			}
/*
* Make sure data can be written
*/
			data_type = (*thefile->file.format_funcs->map_ncl_type_to_format)(value->multidval.data_type);
			if(data_type == NULL) {
				from_type = value->multidval.data_type;
				to_type = _NclPromoteType(from_type);
				while((from_type != to_type )&&((data_type = (*thefile->file.format_funcs->map_ncl_type_to_format)(to_type))==NULL)) {
					from_type = to_type;
					to_type = _NclPromoteType(from_type);
				}
				if(data_type != NULL) {
					NclFree(data_type);
				}
				obj_type = _NclBasicDataTypeToObjType(to_type);
				tmp_md = _NclCoerceData(value,obj_type,NULL);
				if((tmp_md == NULL)||( to_type == value->multidval.data_type)) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Attempting to write variable (%s) of type (%s) which is not representable in the format of file (%s)",
						NrmQuarkToString(var),
						_NclBasicDataTypeToName(value->multidval.data_type),
						NrmQuarkToString(thefile->file.fpath));
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Trying using a type conversion function");
					return(NhlFATAL);
				} else {
					ret = (*thefile->file.format_funcs->add_var)(
						thefile->file.private_rec,
						var,
						tmp_md->multidval.data_type,
						tmp_md->multidval.n_dims,
						new_dim_quarks,
						new_dim_sizes
					);
					if(update_unlimited) {
						for(i = 0; i < thefile->file.n_file_dims;i++) {
							if(thefile->file.file_dim_info[i]->is_unlimited) {
								tmpfdim= thefile->file.file_dim_info[i];
								thefile->file.file_dim_info[i] = (*thefile->file.format_funcs->get_dim_info)(thefile->file.private_rec,tmpfdim->dim_name_quark);
								NclFree(tmpfdim);
							}
						}
					}
				}
			} else {

				ret = (*thefile->file.format_funcs->add_var)(
					thefile->file.private_rec,
					var,
					value->multidval.data_type,
					value->multidval.n_dims,
					new_dim_quarks,
					new_dim_sizes
				);
					if(update_unlimited) {
						for(i = 0; i < thefile->file.n_file_dims;i++) {
							if(thefile->file.file_dim_info[i]->is_unlimited) {
								tmpfdim= thefile->file.file_dim_info[i];
								thefile->file.file_dim_info[i] = (*thefile->file.format_funcs->get_dim_info)(thefile->file.private_rec,tmpfdim->dim_name_quark);
								NclFree(tmpfdim);
							}
						}
					}
				tmp_md = value;
				NclFree(data_type);
			}
			if(ret < NhlWARNING) {
				return(ret);
			}
			if((type == FILE_VAR_ACCESS) ? thefile->file.format_funcs->write_var != NULL : thefile->file.format_funcs->write_coord != NULL) {
				if(type == FILE_VAR_ACCESS) {
					ret = (*thefile->file.format_funcs->write_var)(
						thefile->file.private_rec,
						var,
						tmp_md->multidval.val,
						start,
						finish,
						stride);
				} else {
					ret = (*thefile->file.format_funcs->write_coord)(
						thefile->file.private_rec,
						var,
						tmp_md->multidval.val,
						start,
						finish,
						stride);
				}
				if((tmp_md!=value)&&(tmp_md->obj.status != PERMANENT))
					_NclDestroyObj((NclObj)tmp_md);
			} else {
				if(type == FILE_VAR_ACCESS) {
					ret = (*thefile->file.format_funcs->write_var_ns)(
						thefile->file.private_rec,
						var,
						tmp_md->multidval.val,
						start,
						finish);
				} else {
					ret = (*thefile->file.format_funcs->write_coord_ns)(
						thefile->file.private_rec,
						var,
						tmp_md->multidval.val,
						start,
						finish);
				}
				if((tmp_md!=value)&&(tmp_md->obj.status != PERMANENT))
					_NclDestroyObj((NclObj)tmp_md);
			}
			if(ret < NhlWARNING) {
				return(ret);
			}
			thefile->file.var_info[thefile->file.n_vars] = (*thefile->file.format_funcs->get_var_info)(thefile->file.private_rec,var);
			thefile->file.var_att_info[thefile->file.n_vars] = NULL;
			thefile->file.var_att_ids[thefile->file.n_vars] = -1;
			
			thefile->file.n_vars++;
/*
* Add update unlimited
*/
					if(update_unlimited) {
						for(i = 0; i < thefile->file.n_file_dims;i++) {
							if(thefile->file.file_dim_info[i]->is_unlimited) {
								tmpfdim= thefile->file.file_dim_info[i];
								thefile->file.file_dim_info[i] = (*thefile->file.format_funcs->get_dim_info)(thefile->file.private_rec,tmpfdim->dim_name_quark);
								NclFree(tmpfdim);
							}
						}
					}
			return(NhlNOERROR);
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"FileWriteVar: file (%s) was opened for reading only, can not write",NrmQuarkToString(thefile->file.fname));
	}
	return(NhlFATAL);
}
static NhlErrorTypes FileWriteCoord
#if	NhlNeedProto
(NclFile thefile, NclQuark coord_name, struct _NclMultiDValDataRec* value, struct _NclSelectionRecord* sel_ptr)
#else 
(thefile, coord_name, value, sel_ptr)
NclFile thefile;
NclQuark coord_name;
struct _NclMultiDValDataRec* value;
struct _NclSelectionRecord* sel_ptr;
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	int dindex;
	int index;
	
	if(thefile->file.wr_status<=0) {
		dindex = FileIsDim(thefile,coord_name);
		if(dindex > -1) {
			ret = MyFileWriteVar(thefile,coord_name,value,sel_ptr,NULL,FILE_COORD_VAR_ACCESS);
			if(thefile->file.coord_vars[dindex] == NULL) {
				index = FileIsVar(thefile,coord_name);
				if(index > -1 && thefile->file.var_info[index]->num_dimensions == 1) {
					thefile->file.coord_vars[dindex] = thefile->file.var_info[index];
				} 
			}	
			return(ret);
		} else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Dimension (%s) is not a valid dimension in file (%s), can't write coord_var",NrmQuarkToString(coord_name),NrmQuarkToString(thefile->file.fname));
			return(NhlFATAL);
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"FileWriteCoord: file (%s) was opened for reading only, can not write",NrmQuarkToString(thefile->file.fname));
		return(NhlFATAL);
	}
}

static NhlErrorTypes FileWriteVarVar
#if	NhlNeedProto
(NclFile thefile, NclQuark lhs_var, struct _NclSelectionRecord * lhs_sel_ptr, struct _NclVarRec* rhs_var, struct _NclSelectionRecord *rhs_sel_ptr)
#else
(thefile , lhs_var, lhs_sel_ptr, rhs_var, rhs_sel_ptr)
NclFile thefile;
NclQuark lhs_var;
struct _NclSelectionRecord *lhs_sel_ptr;
struct _NclVarRec* rhs_var;
struct _NclSelectionRecord *rhs_sel_ptr;
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	struct _NclVarRec* tmp_var;
	struct _NclVarRec* tmp_coord_var;
	int i,j,m;
	NclQuark dim_names[NCL_MAX_DIMENSIONS];
	NclAtt theatt;
	NclAttList *step;
	int index,cindex,lhs_n_elem;	
	NclSelectionRecord tmp_sel;
        void *tmp_coord;
        char *tmp_ptr;
	NclMultiDValData tmp_md;
	struct _NclVarRec* cvar;
	int dimsize = -1;

	tmp_sel.n_entries = 1;
	tmp_sel.selected_from_sym = NULL;
	tmp_sel.selected_from_var = NULL;


	
	if(thefile->file.wr_status<=0) {
		tmp_var = _NclVarRead(rhs_var,rhs_sel_ptr);
		for ( i = 0; i < tmp_var->var.n_dims; i++) {
			dim_names[i] = tmp_var->var.dim_info[i].dim_quark;
		}
		ret = MyFileWriteVar(thefile,lhs_var,(NclMultiDValData)_NclGetObj(tmp_var->var.thevalue_id),lhs_sel_ptr,dim_names,FILE_VAR_ACCESS);
		if(ret < NhlWARNING) {
			return(ret);
		}
		if(rhs_var->var.att_id != -1) {
			theatt = (NclAtt)_NclGetObj(rhs_var->var.att_id);
			step = theatt->att.att_list;
			while(step != NULL) {
				ret = FileWriteVarAtt(thefile,lhs_var,step->quark,step->attvalue,NULL);
				if(ret < NhlWARNING){
					NhlPError(NhlWARNING,NhlEUNKNOWN,"FileWriteVarVar: Could not attribute (%s) to file (%s), continuing anyway",NrmQuarkToString(step->quark),NrmQuarkToString(thefile->file.fname));
					ret = NhlWARNING;
				}
				step = step->next;
			}
		}
		index = FileIsVar(thefile,lhs_var);
		if(lhs_sel_ptr != NULL) {
			j = 0;
			for(i = 0; i < lhs_sel_ptr->n_entries; i++) {
				if(!lhs_sel_ptr->selection[i].u.sub.is_single ) {
					switch(lhs_sel_ptr->selection[i].sel_type) {
					case Ncl_VECSUBSCR:
						lhs_n_elem = lhs_sel_ptr->selection[i].u.vec.n_ind;
						break;
					default:
						if(lhs_sel_ptr->selection[i].u.sub.finish < lhs_sel_ptr->selection[i].u.sub.start) {
							lhs_n_elem = (int)(((double)(lhs_sel_ptr->selection[i].u.sub.start - lhs_sel_ptr->selection[i].u.sub.finish))/(double)fabs(((double)lhs_sel_ptr->selection[i].u.sub.stride))) + 1;
						} else {
							lhs_n_elem = (int)(((double)(lhs_sel_ptr->selection[i].u.sub.finish - lhs_sel_ptr->selection[i].u.sub.start))/(double)fabs(((double)lhs_sel_ptr->selection[i].u.sub.stride))) + 1;
						}
						break;
					}
					if(tmp_var->var.dim_info[j].dim_quark > 0) {
						if(thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[lhs_sel_ptr->selection[i].dim_num]]->dim_name_quark != tmp_var->var.dim_info[j].dim_quark) {
/*
* Dimnames are unequal give warning then overwrite
*/
							NhlPError(NhlWARNING,NhlEUNKNOWN,"Dimension names of left hand side and right hand side do not match, overwriting dimension (%s), use (/ .. /) if this is not the desired result",NrmQuarkToString(thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[lhs_sel_ptr->selection[i].dim_num]]->dim_name_quark) );
							_NclFileWriteDim(thefile,thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[lhs_sel_ptr->selection[i].dim_num]]->dim_name_quark,thefile->file.var_info[index]->file_dim_num[lhs_sel_ptr->selection[i].dim_num]);

						} 
/*
* Now dimension names are equal, proceed to write coordinate variable
*/
						if(tmp_var->var.coord_vars[j] != -1) {
							cindex = FileIsCoord(thefile,tmp_var->var.dim_info[j].dim_quark);
							if(cindex != -1) {
/*
* Simply write coordinate using sel_ptr
*/
								tmp_sel.selection[0] = lhs_sel_ptr->selection[i];
								tmp_sel.selection[0].dim_num = 0;
								ret = _NclFileWriteCoord(thefile,tmp_var->var.dim_info[j].dim_quark,_NclVarValueRead((NclVar)_NclGetObj(tmp_var->var.coord_vars[j]),NULL,NULL),&tmp_sel);
								cvar = (NclVar)_NclGetObj(tmp_var->var.coord_vars[j]);

							} else {
/*
* Need to create a temporary missing value filled array and write it then make and assigment using 
* sel_ptr
*/
								dimsize = (int)thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[lhs_sel_ptr->selection[i].dim_num]]->dim_size;
								cvar = (NclVar)_NclGetObj(tmp_var->var.coord_vars[j]);
								tmp_md = (NclMultiDValData)_NclGetObj(cvar->var.thevalue_id);
								tmp_coord = NclMalloc(dimsize*tmp_md->multidval.type->type_class.size);
                                                                tmp_ptr = (char*)tmp_coord;
                                                                for(m = 0; m < dimsize; m++) {
                                                                        memcpy((void*)tmp_ptr,(void*)&(tmp_md->multidval.type->type_class.default_mis),tmp_md->multidval.type->type_class.size);
                                                                        tmp_ptr = tmp_ptr + tmp_md->multidval.type->type_class.size;

                                                                }

								ret = _NclFileWriteCoord(
									thefile,	
									tmp_var->var.dim_info[j].dim_quark,
									_NclCreateMultiDVal( 
										NULL, 
										NULL, 
										Ncl_MultiDValData, 
										0, 
										tmp_coord, 
										&tmp_md->multidval.type->type_class.default_mis, 
										1, 
										&dimsize, 
										TEMPORARY, 
										NULL,
										tmp_md->multidval.type),
									NULL);

								tmp_sel.selection[0] = lhs_sel_ptr->selection[i];
								tmp_sel.selection[0].dim_num = 0;
								ret = _NclFileWriteCoord(thefile,tmp_var->var.dim_info[j].dim_quark,tmp_md,&tmp_sel);
							}
							if(ret < NhlWARNING) {
								NhlPError(NhlWARNING,NhlEUNKNOWN,"FileWriteVarVar: Could not write coordinate variable (%s) to file (%s), continuing anyway",NrmQuarkToString(tmp_var->var.dim_info[i].dim_quark),NrmQuarkToString(thefile->file.fname));
								ret = NhlWARNING;
							} else {
								if(cvar->var.att_id != -1) {
									theatt = (NclAtt)_NclGetObj(cvar->var.att_id);
									step = theatt->att.att_list;
									while(step != NULL) {
										ret = FileWriteVarAtt(thefile,tmp_var->var.dim_info[i].dim_quark,step->quark,step->attvalue,NULL);
										if(ret < NhlWARNING){
											NhlPError(NhlWARNING,NhlEUNKNOWN,"FileWriteVarVar: Could not attribute (%s) to file (%s), continuing anyway",NrmQuarkToString(step->quark),NrmQuarkToString(thefile->file.fname));
											ret = NhlWARNING;
										}
										step = step->next;
				
									}
								}
							}
						}
				
					} else if(thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[lhs_sel_ptr->selection[i].dim_num]]->dim_name_quark > 0) {

/*
* right hand side has no dimension name and hence no coordinate variable so give warning and proceed
*/
						NhlPError(NhlWARNING,NhlEUNKNOWN,"Right hand side has no dimension name can not delete dimension of a file, use (/ .. /) to avoid this message");
					}
					j++;
				} 
			} 
		} else {
			for(i = 0, j = 0; i < thefile->file.var_info[index]->num_dimensions; i++) {
				int file_dim_num = thefile->file.var_info[index]->file_dim_num[i];

				if (thefile->file.file_dim_info[file_dim_num]->dim_size  == 1 && tmp_var->var.dim_info[j].dim_size != 1) 
					continue;
				else if (thefile->file.file_dim_info[file_dim_num]->dim_size != 1 && tmp_var->var.dim_info[j].dim_size == 1) {
					while (tmp_var->var.dim_info[j].dim_size == 1)
						j++;
				}

				if(tmp_var->var.dim_info[j].dim_quark > 0) {
					if(thefile->file.file_dim_info[file_dim_num]->dim_name_quark != tmp_var->var.dim_info[j].dim_quark) {
/*
* Dimnames are unequal give warning then overwrite
*/
						NhlPError(NhlWARNING,NhlEUNKNOWN,"Dimension names of left hand side and right hand side do not match, overwriting dimension (%s), use (/ .. /) if this is not the desired result",NrmQuarkToString(thefile->file.file_dim_info[file_dim_num]->dim_name_quark) );
						_NclFileWriteDim(thefile,tmp_var->var.dim_info[j].dim_quark,file_dim_num);

					} 
					if(tmp_var->var.coord_vars[j] != -1) {
						if (tmp_var->var.dim_info[j].dim_quark == NrmStringToQuark("ncl_scalar")) {
							NhlPError(NhlWARNING,NhlEUNKNOWN,"FileWriteVarVar: Variable (%s) has coordinate variable named \"ncl_scalar\"; not writing coodinate variable to file (%s)",
								  NrmQuarkToString(tmp_var->var.var_quark),NrmQuarkToString(thefile->file.fname));
								  ret = NhlWARNING;
								  continue;
						}
						tmp_coord_var = (NclVar)_NclGetObj(tmp_var->var.coord_vars[j]);
						ret = FileWriteCoord(thefile,tmp_var->var.dim_info[j].dim_quark,_NclVarValueRead(tmp_coord_var,NULL,NULL),NULL);
						if(ret < NhlWARNING) {
							NhlPError(NhlWARNING,NhlEUNKNOWN,"FileWriteVarVar: Could not write coordinate variable (%s) to file (%s), continuing anyway",
								  NrmQuarkToString(tmp_var->var.dim_info[j].dim_quark),NrmQuarkToString(thefile->file.fname));
							ret = NhlWARNING;
						} else {
							if(tmp_coord_var->var.att_id != -1) {
								theatt = (NclAtt)_NclGetObj(tmp_coord_var->var.att_id);
								step = theatt->att.att_list;
								while(step != NULL) {
									ret = FileWriteVarAtt(thefile,tmp_var->var.dim_info[j].dim_quark,step->quark,step->attvalue,NULL);
									if(ret < NhlWARNING){
										NhlPError(NhlWARNING,NhlEUNKNOWN,"FileWriteVarVar: Could not attribute (%s) to file (%s), continuing anyway",NrmQuarkToString(step->quark),NrmQuarkToString(thefile->file.fname));
										ret = NhlWARNING;
									}
									step = step->next;
			
								}
							}
						}
					} else if(thefile->file.coord_vars[file_dim_num] != NULL) {

/*
* right hand side has no dimension name and hence no coordinate variable so give warning and proceed
*/
						NhlPError(NhlWARNING,NhlEUNKNOWN,"Right hand side has no coordinate variable can not delete coordinate variable of a file, use (/ .. /) to avoid this message");
						ret = NhlWARNING;
					}
				}
				j++;
			}
		}
		return(ret);
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"FileWriteVarVar: file (%s) was opened for reading only, can not write",NrmQuarkToString(thefile->file.fname));
		return(NhlFATAL);
	}
}


static NhlErrorTypes FileWriteVar
#if	NhlNeedProto
(NclFile thefile, NclQuark var, struct _NclMultiDValDataRec *value,struct _NclSelectionRecord * sel_ptr)
#else 
(thefile, var, value, sel_ptr)
NclFile thefile;
NclQuark var;
struct _NclMultiDValDataRec *value;
struct _NclSelectionRecord * sel_ptr;
#endif
{
	return(MyFileWriteVar(thefile,var,value,sel_ptr,NULL,FILE_VAR_ACCESS));
}


static NhlErrorTypes FileWriteVarAtt
#if	NhlNeedProto
(NclFile thefile, NclQuark var, NclQuark attname,struct _NclMultiDValDataRec* value, struct _NclSelectionRecord * sel_ptr)
#else 
(thefile, var, attname,value, sel_ptr)
NclFile thefile;
NclQuark var;
NclQuark attname;
struct _NclMultiDValDataRec* value;
struct _NclSelectionRecord * sel_ptr;
#endif
{
	int exists;
	NclMultiDValData tmp_att_md,tmp_md;
	int att_id;
	NhlErrorTypes ret = NhlNOERROR;
	int index = -1;
	NclFileAttInfoList *step;
	NclBasicDataTypes from_type,to_type;
	NclObjTypes obj_type;
	void *data_type;
	NhlArgVal udata;

	if(thefile->file.wr_status<=0) {
		index = FileIsVar(thefile,var);
		if(index > -1) {
			if(thefile->file.var_att_ids[index] == -1) 
				LoadVarAtts(thefile,var);

			att_id = thefile->file.var_att_ids[index];
/*
* Hereis the trick. It is easier to let the _NclAddAtt... functions deal
* with the coercion than to figure out what it should be 
*/
			exists = _NclIsAtt(att_id,NrmQuarkToString(attname));
			if((exists)&&(thefile->file.format_funcs->write_att != NULL))  {
				ret = _NclAddAtt(att_id,NrmQuarkToString(attname),value,sel_ptr);
				if(ret < NhlWARNING) {
                                	NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not write attribute (%s) to attribute list", NrmQuarkToString( attname));
                                	return(NhlFATAL);
                        	}
				tmp_att_md = _NclGetAtt(att_id,NrmQuarkToString(attname),NULL);
				ret = (*thefile->file.format_funcs->write_var_att)(
					thefile->file.private_rec,
					var,
					attname,
					tmp_att_md->multidval.val
				);
				return(ret);
			} else if((!exists)&&(thefile->file.format_funcs->add_att != NULL)){
				if(value->multidval.data_type == NCL_char) {	
					tmp_md = _NclCharMdToStringMd(value);
					ret = _NclAddAtt(att_id,NrmQuarkToString(attname),tmp_md,sel_ptr);
					if(ret < NhlWARNING) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not write attribute (%s) to attribute list",NrmQuarkToString(attname));
						return(NhlFATAL);
					}
					ret = (*thefile->file.format_funcs->add_var_att)(
						thefile->file.private_rec,
						var,
						attname,
						value->multidval.data_type,
						value->multidval.totalelements,
						value->multidval.val
					);
					if(ret > NhlWARNING) {
						AddAttInfoToList(&(thefile->file.var_att_info[index]), (*thefile->file.format_funcs->get_var_att_info)(thefile->file.private_rec,var,attname));
						return(ret);
					} else {
						_NclDeleteAtt(att_id,NrmQuarkToString(attname));
					}
				} else {
					if((data_type = (*thefile->file.format_funcs->map_ncl_type_to_format)(value->multidval.data_type)) == NULL)  {
						if(value->multidval.data_type == NCL_string) {
							tmp_md = _NclStringMdToCharMd(value);
							/* 
							 * simple hack to get rid of the null terminator, which should not be written to the output file
							 */
							tmp_md->multidval.totalelements--;
							tmp_md->multidval.totalsize--;
							tmp_md->multidval.dim_sizes[0]--;
							/*
							 * end hack
							 */
							ret = _NclFileWriteVarAtt(thefile,var,attname,tmp_md,sel_ptr);
							_NclDestroyObj((NclObj)tmp_md);
							return(ret);
						} else {
							from_type = value->multidval.data_type;
							to_type = _NclPromoteType(from_type);
							while((from_type != to_type)&&((data_type =(*thefile->file.format_funcs->map_ncl_type_to_format)(to_type))==NULL)) {
								from_type = to_type;
								to_type = _NclPromoteType(from_type);
							}
							if(data_type != NULL) {
								NclFree(data_type);
							}
							if((data_type = (*thefile->file.format_funcs->map_ncl_type_to_format)(to_type))==NULL) {
								NhlPError(NhlFATAL,NhlEUNKNOWN,"The type (%s) is not representable as an attribute in the file (%s)",_NclBasicDataTypeToName(to_type),NrmQuarkToString(thefile->file.fpath));
								return(NhlFATAL);
							} else {
								NclFree(data_type);
								obj_type = _NclBasicDataTypeToObjType(to_type);
								tmp_md = _NclCoerceData(value,obj_type,NULL);
							}
						}
						
					} else {
						NclFree(data_type);
						tmp_md = value;
					}
					ret = _NclAddAtt(att_id,NrmQuarkToString(attname),tmp_md,sel_ptr);
					if(ret < NhlWARNING) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not write attribute (%s) to attribute list",NrmQuarkToString(attname));
						return(NhlFATAL);
					}
					ret = (*thefile->file.format_funcs->add_var_att)(
						thefile->file.private_rec,
						var,
						attname,
						tmp_md->multidval.data_type,
						tmp_md->multidval.totalelements,
						tmp_md->multidval.val
					);
					if(ret > NhlWARNING) {
						AddAttInfoToList(&(thefile->file.var_att_info[index]), (*thefile->file.format_funcs->get_var_att_info)(thefile->file.private_rec,var,attname));
						return(ret);
					} else {
						_NclDeleteAtt(att_id,NrmQuarkToString(attname));
					}
				}
				return(ret);
			} else {
				return(NhlFATAL);
			}
		} else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"(%s) is not a variable in file (%s)",NrmQuarkToString(var),NrmQuarkToString(thefile->file.fname));
		}
		return(NhlFATAL);
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"FileWriteVarAtt: file (%s) was opened for reading only, can not write",NrmQuarkToString(thefile->file.fname));
		return(NhlFATAL);
	}
}
static struct _NclMultiDValDataRec *FileReadAtt
#if	NhlNeedProto
(NclFile thefile, NclQuark attname, struct _NclSelectionRecord *sel_ptr)
#else 
(thefile, attname, sel_ptr)
NclFile thefile;
NclQuark attname;
struct _NclSelectionRecord *sel_ptr;
#endif
{
	int aindex;
	int att_id = -1,i;
	void *val;
	NclMultiDValData tmp_md;
	NhlArgVal udata;
	aindex = FileIsAtt(thefile,attname);
	if(aindex > -1) {
		if(thefile->file.file_atts_id != -1) {
			return(_NclGetAtt(thefile->file.file_atts_id,NrmQuarkToString(attname),sel_ptr));
		}
		if(thefile->file.format_funcs->read_att != NULL) {
			att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,(NclObj)thefile);
			for(i = 0; i < thefile->file.n_file_atts; i++){
				val = NclMalloc(_NclSizeOf(thefile->file.file_atts[i]->data_type)* thefile->file.file_atts[i]->num_elements );
				(void)(*thefile->file.format_funcs->read_att)(
					thefile->file.private_rec,
					thefile->file.file_atts[i]->att_name_quark,
					val
					);
				tmp_md = _NclCreateMultiDVal(
						NULL,
						NULL,
						Ncl_MultiDValData,
						0,
						val,
						NULL,
						1,
						&thefile->file.file_atts[i]->num_elements,
						TEMPORARY,
						NULL,
						_NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(thefile->file.file_atts[i]->data_type)));
				if(tmp_md != NULL) {
					_NclAddAtt(att_id,NrmQuarkToString(thefile->file.file_atts[i]->att_name_quark),tmp_md,NULL);
				}
			}
			udata.ptrval = (void*)NclMalloc(sizeof(FileCallBackRec));
			((FileCallBackRec*)udata.ptrval)->thefileid = thefile->obj.id;
			((FileCallBackRec*)udata.ptrval)->theattid = att_id;
			((FileCallBackRec*)udata.ptrval)->thevar = -1;
			thefile->file.file_att_cb = _NclAddCallback((NclObj)_NclGetObj(att_id),NULL,FileAttIsBeingDestroyedNotify,ATTDESTROYED,&udata);
			thefile->file.file_att_udata = (FileCallBackRec*)udata.ptrval;
			if(att_id != -1) {	
				thefile->file.file_atts_id = att_id;
				return(_NclGetAtt(thefile->file.file_atts_id,NrmQuarkToString(attname),sel_ptr));
			}
		}
	}
	NhlPError(NhlWARNING,NhlEUNKNOWN,"FileReadVarAtt: (%s) is not an attribute of (%s)",NrmQuarkToString(attname),NrmQuarkToString(thefile->file.fname));
	return(_NclCreateMissing());
}

static NhlErrorTypes FileWriteAtt
#if	NhlNeedProto
(NclFile thefile, NclQuark attname, struct _NclMultiDValDataRec* value, struct _NclSelectionRecord *sel_ptr)
#else 
(thefile, attname, value, sel_ptr)
NclFile thefile;
NclQuark attname;
struct _NclMultiDValDataRec* value;
struct _NclSelectionRecord *sel_ptr;
#endif
{
	int i,exists;
	NclMultiDValData tmp_att_md,tmp_md;
	int att_id;
	NhlErrorTypes ret = NhlNOERROR;
	NclBasicDataTypes from_type,to_type;
	NclObjTypes obj_type;
	void *data_type;
	NhlArgVal udata;

	if(thefile->file.wr_status<=0) {
		if(thefile->file.file_atts_id == -1) {
			att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,(NclObj)thefile);
			for(i= 0; i < thefile->file.n_file_atts; i++) {
				tmp_att_md = FileReadAtt(thefile,thefile->file.file_atts[i]->att_name_quark,NULL);
				if(tmp_att_md != NULL) {
					if(tmp_att_md->obj.status == TEMPORARY){
						_NclAddAtt(att_id,NrmQuarkToString(thefile->file.file_atts[i]->att_name_quark),tmp_att_md,NULL);
					} else {
						tmp_att_md = _NclCopyVal(tmp_att_md, NULL);
						_NclAddAtt(att_id,NrmQuarkToString(thefile->file.file_atts[i]->att_name_quark),tmp_att_md,NULL);
					}
				}
			}
			thefile->file.file_atts_id = att_id;
			udata.ptrval = (void*)NclMalloc(sizeof(FileCallBackRec));
			((FileCallBackRec*)udata.ptrval)->thefileid = thefile->obj.id;
			((FileCallBackRec*)udata.ptrval)->theattid = att_id;
			((FileCallBackRec*)udata.ptrval)->thevar = -1;
			thefile->file.file_att_cb = _NclAddCallback((NclObj)_NclGetObj(att_id),NULL,FileAttIsBeingDestroyedNotify,ATTDESTROYED,&udata);
			thefile->file.file_att_udata = (FileCallBackRec*)udata.ptrval;
		}  else {
			att_id = thefile->file.file_atts_id;
		}
		exists = _NclIsAtt(att_id,NrmQuarkToString(attname));
		if((exists)&&(thefile->file.format_funcs->write_att != NULL))  {
/*
* Hereis the trick. It is easier to let the _NclAddAtt... functions deal
* with the coercion than to figure out what it should be 
*/
			ret = _NclAddAtt(att_id,NrmQuarkToString(attname),value,sel_ptr);
			if(ret < NhlWARNING) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not write attribute (%s) to attribute list", NrmQuarkToString(attname));
				return(NhlFATAL);
			}
			tmp_att_md = _NclGetAtt(att_id,NrmQuarkToString(attname),NULL);
			ret = (*thefile->file.format_funcs->write_att)(
				thefile->file.private_rec,
				attname,
				tmp_att_md->multidval.val
			);
			return(ret);
		} else if((!exists)&&(thefile->file.format_funcs->add_att != NULL)){
			if(value->multidval.data_type == NCL_char) {
				tmp_md = _NclCharMdToStringMd(value);
				ret = _NclAddAtt(att_id,NrmQuarkToString(attname),tmp_md,sel_ptr);
       	        		if(ret < NhlWARNING) {
       	                 	NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not write attribute (%s) to attribute list", NrmQuarkToString(attname));
       	                 	return(NhlFATAL);
       	         	}
				ret = (*thefile->file.format_funcs->add_att)(
					thefile->file.private_rec,
					attname,
					value->multidval.data_type,
					value->multidval.totalelements,
					value->multidval.val
				);
				if(ret > NhlWARNING) {
					thefile->file.file_atts[thefile->file.n_file_atts] = (*thefile->file.format_funcs->get_att_info)(thefile->file.private_rec,attname);
					if(thefile->file.file_atts[thefile->file.n_file_atts] != NULL) {
						thefile->file.n_file_atts++;
					}
					return(ret);
				} else {
					_NclDeleteAtt(att_id,NrmQuarkToString(attname));
				}
			} else {
				if((data_type = (*thefile->file.format_funcs->map_ncl_type_to_format)(value->multidval.data_type)) == NULL)  {
					if(value->multidval.data_type == NCL_string) {
						tmp_md = _NclStringMdToCharMd(value);
						/* 
						 * simple hack to get rid of the null terminator, which should not be written to the output file
						 */
						tmp_md->multidval.totalelements--;
						tmp_md->multidval.totalsize--;
						tmp_md->multidval.dim_sizes[0]--;
						/*
						 * end hack
						 */
						ret = _NclFileWriteAtt(thefile,attname,tmp_md,sel_ptr);
						_NclDestroyObj((NclObj)tmp_md);
						return(ret);
					} else {
						from_type = value->multidval.data_type;
						to_type = _NclPromoteType(from_type);
						while((from_type != to_type )&&((data_type = (*thefile->file.format_funcs->map_ncl_type_to_format)(to_type))==NULL)) {
							from_type = to_type;
							to_type = _NclPromoteType(from_type);
						}
						if(data_type != NULL) {
							NclFree(data_type);
						}
						if((data_type = (*thefile->file.format_funcs->map_ncl_type_to_format)(to_type))==NULL)  {
							NhlPError(NhlFATAL,NhlEUNKNOWN,"The type (%s) is not representable as an attribute in the file (%s)",_NclBasicDataTypeToName(to_type),NrmQuarkToString(thefile->file.fpath));
       	                                          return(NhlFATAL);
	
						} else {
							NclFree(data_type);
							obj_type = _NclBasicDataTypeToObjType(to_type);
							tmp_md = _NclCoerceData(value,obj_type,NULL);
						}
					}
				} else {
					NclFree(data_type);
					tmp_md= value;
				}
				ret = _NclAddAtt(att_id,NrmQuarkToString(attname),tmp_md,sel_ptr);
       	        		if(ret < NhlWARNING) {
       		                 	NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not write attribute (%s) to attribute list", NrmQuarkToString(attname));
       		                 	return(NhlFATAL);
       		         	}
				ret = (*thefile->file.format_funcs->add_att)(
					thefile->file.private_rec,
					attname,
					tmp_md->multidval.data_type,
					tmp_md->multidval.totalelements,
					tmp_md->multidval.val
				);
				if((tmp_md != value)&&(tmp_md->obj.status != PERMANENT)) {
					_NclDestroyObj((NclObj)tmp_md);
				}
				if(ret > NhlWARNING) {
					thefile->file.file_atts[thefile->file.n_file_atts] = (*thefile->file.format_funcs->get_att_info)(thefile->file.private_rec,attname);
					if(thefile->file.file_atts[thefile->file.n_file_atts] != NULL) {
						thefile->file.n_file_atts++;
					}
					return(ret);
				} else {
					_NclDeleteAtt(att_id,NrmQuarkToString(attname));
				}
			}
			return(ret);
		} 
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"FileWriteAtt: file (%s) was opened for reading only, can not write",NrmQuarkToString(thefile->file.fname));
	}
	return(NhlFATAL);
}


static struct _NclMultiDValDataRec* FileVarReadDim
#if	NhlNeedProto
(NclFile thefile, NclQuark var, NclQuark dim_name, long dim_num)
#else 
(thefile, var, dim_name, dim_num)
NclFile thefile;
NclQuark var;
NclQuark dim_name;
long dim_num;
#endif
{
	int index;
	int i;
	int *tmpi;
	NclQuark *tmpq;
	int output_dim_sizes = 1;
	
	
	index = FileIsVar(thefile,var);
	if(index > -1) {
		if(dim_name > -1) {
			for( i=0; i < thefile->file.var_info[index]->num_dimensions; i++) {
				if(FileGetDimName(thefile,thefile->file.var_info[index]->file_dim_num[i]) == dim_name) {
					tmpi = (int*)NclMalloc(sizeof(int));
					*tmpi = i;
					return( _NclCreateMultiDVal(
						NULL,
						NULL,
						Ncl_MultiDValData,
						0,
						(void*)tmpi,
						NULL,
						1,
						&output_dim_sizes,
						TEMPORARY,
						NULL,
						(NclTypeClass)nclTypeintClass));
				}
			}
		} else if ( dim_num > -1) {
			if(dim_num < thefile->file.var_info[index]->num_dimensions) {
				tmpq = (NclQuark*)NclMalloc(sizeof(NclQuark));
				*tmpq = FileGetDimName(thefile,thefile->file.var_info[index]->file_dim_num[dim_num]);	
				return( _NclCreateMultiDVal(
					NULL,
					NULL,
					Ncl_MultiDValData,
					0,
					(void*)tmpq,
					NULL,
					1,
					&output_dim_sizes,
					TEMPORARY,
					NULL,
					(NclTypeClass)nclTypestringClass));
			}
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Dimension number (%d) is out of range for variable (%s->%s)",dim_num,NrmQuarkToString(thefile->file.fname),NrmQuarkToString(var));
			return(NULL);
		} 
	}
	return(NULL);
}
/*
* dim_num is dim number of variable and dim_name is new name
*/
static NhlErrorTypes FileVarWriteDim
#if	NhlNeedProto
(NclFile thefile, NclQuark var, NclQuark dim_name, long dim_num)
#else 
(thefile,var, dim_name, dim_num)
NclFile thefile;
NclQuark var;
NclQuark dim_name;
long dim_num;
#endif
{
	int index;
	NclQuark old_name;

	if(thefile->file.wr_status <= 0) {
		index = FileIsVar(thefile,var);
		if(index > -1) {
			if((dim_num > -1)&&(dim_num < thefile->file.var_info[index]->num_dimensions)) {
				old_name = FileGetDimName(thefile,thefile->file.var_info[index]->file_dim_num[dim_num]);
				if(thefile->file.format_funcs->rename_dim != NULL) {
					if((*thefile->file.format_funcs->rename_dim)(
						thefile->file.private_rec,
						old_name,
						dim_name) < NhlWARNING) {
						
						NhlPError(NhlFATAL,NhlEUNKNOWN,
							"Cannot rename dimension (%d) in variable (%s)",
							dim_num,
							NrmQuarkToString(var));
						return(NhlFATAL);
					} else {
						thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[dim_num]]->dim_name_quark = dim_name;
						return(NhlNOERROR);
					}
				}
			}
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"FileVarWriteDim: file (%s) was opened for reading only, can not write",NrmQuarkToString(thefile->file.fname));
	}
	return(NhlFATAL);
}

static struct _NclMultiDValDataRec* FileReadDim
#if	NhlNeedProto
(NclFile thefile, NclQuark dim_name, long dim_num)
#else 
(thefile, dim_name, dim_num)
NclFile thefile;
NclQuark dim_name;
long dim_num;
#endif
{
	int i = 0;
	NclQuark *tmps;
	int *tmpl;
	int output_dim_sizes = 1;
	if(dim_name != -1) {
		for(i =0; i< thefile->file.n_file_dims; i++) {
			if(thefile->file.file_dim_info[i]->dim_name_quark) {
				tmpl = (int*)NclMalloc(sizeof(int));
				*tmpl = i;
				return( _NclCreateMultiDVal(
					NULL,
					NULL,
					Ncl_MultiDValData,
					0,
					(void*)tmpl,
					NULL,
					1,
					&output_dim_sizes,
					TEMPORARY,
					NULL,
					(NclTypeClass)nclTypeintClass));
			}
		}
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Dimension (%s) is not a defined dimension in file (%s)",NrmQuarkToString(dim_name),NrmQuarkToString(thefile->file.fname));
		return(NULL);
	} else if(dim_num > -1){
		if(dim_num < thefile->file.n_file_dims) {
			tmps = (NclQuark*)NclMalloc(sizeof(NclQuark));
			*tmps = thefile->file.file_dim_info[dim_num]->dim_name_quark;
			return( _NclCreateMultiDVal(
				NULL,
				NULL,
				Ncl_MultiDValData,
				0,
				(void*)tmps,
				NULL,
				1,
				&output_dim_sizes,
				TEMPORARY,
				NULL,
				(NclTypeClass)nclTypestringClass));
		}
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Dimension #%ld is out of range",dim_num);
		return(NULL);

	} else {
		return(NULL);
	}
}

/*
* dim_num is the number of the dimension dim_name is the name to change it to
*/
static NhlErrorTypes FileWriteDim
#if	NhlNeedProto
(NclFile thefile, NclQuark dim_name, long dim_num)
#else 
(thefile, dim_name, dim_num)
NclFile thefile;
NclQuark dim_name;
long dim_num;
#endif
{
	if(thefile->file.wr_status <= 0) {
		if((dim_num > -1)&&(dim_num < thefile->file.n_file_dims)) {
			if(thefile->file.format_funcs->rename_dim != NULL) {
				if((*thefile->file.format_funcs->rename_dim)(
					thefile->file.private_rec,
					thefile->file.file_dim_info[dim_num]->dim_name_quark,
					dim_name)  < NhlWARNING) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not change dimension (%d) to (%s) for file (%s)",dim_num,NrmQuarkToString(dim_name),NrmQuarkToString(thefile->file.fname));
					return(NhlFATAL);
				} else {
					thefile->file.file_dim_info[dim_num]->dim_name_quark = dim_name;
					return(NhlNOERROR);
				}
			}
		} else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Dimension (%d) is out of range for file (%s)",dim_num,NrmQuarkToString(thefile->file.fname));
			return(NhlFATAL);
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"FileWriteDim: file (%s) was opened for reading only, can not write",NrmQuarkToString(thefile->file.fname));
	}
	return(NhlFATAL);
}
	
static struct _NclVarRec* FileReadCoord
#if	NhlNeedProto
(NclFile thefile, NclQuark coord_name, struct _NclSelectionRecord* sel_ptr)
#else 
(thefile, coord_name, sel_ptr)
NclFile thefile;
NclQuark coord_name;
struct _NclSelectionRecord* sel_ptr;
#endif
{
	NclSelection *sel;
	int index;
	NclMultiDValData tmp_md,tmp_att_md;
	NclDimRec dim_info[NCL_MAX_DIMENSIONS];
	int att_id = -1;
	NclObj att_obj = NULL;
	NclVar tmp_var = NULL;
	NclFileAttInfoList *step;
	NhlArgVal udata;

	if(FileIsCoord(thefile,coord_name) > -1){
		index = FileIsVar(thefile,coord_name);
		tmp_md = MyFileReadVarValue(thefile,coord_name,sel_ptr,dim_info,
			FILE_COORD_VAR_ACCESS);
		if(tmp_md == NULL) 
			return(NULL);
		if(thefile->file.var_att_ids[index] == -1) 
			LoadVarAtts(thefile,coord_name);
		att_id = thefile->file.var_att_ids[index];
		att_obj = (NclObj)_NclCopyAtt((NclAtt)_NclGetObj(att_id),NULL);
		if(att_obj != NULL) {
			att_id = att_obj->obj.id;
		} else {
			att_id = -1;
		}
		if(sel_ptr != NULL) {
			sel = sel_ptr->selection;
		} else {
			sel = NULL;
		}
		if(tmp_md != NULL) {
			tmp_var = _NclCoordVarCreate(
					NULL,
					NULL,
					Ncl_CoordVar,
					0,
					NULL,
					tmp_md,
					dim_info,
					att_id,
					NULL,
					((sel== NULL)? COORD:COORDSUBSEL),
					NrmQuarkToString(coord_name),
					TEMPORARY);
			if(tmp_var == NULL) {
				_NclDestroyObj((NclObj)tmp_md);
				if(att_obj != NULL) {
					_NclDestroyObj((NclObj)att_obj);
				}
			}
		}
		return(tmp_var);
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"(%s) is not a coordinate variable for file (%s)",NrmQuarkToString(coord_name),NrmQuarkToString(thefile->file.fname));
	}
	return(NULL);
}

	

static NclQuark FileGetDimName
#if	NhlNeedProto
(NclFile thefile, int num)
#else
(thefile, num)
NclFile thefile;
int num;
#endif
{
	if((num > -1)&&(num < thefile->file.n_file_dims)) {
		return(thefile->file.file_dim_info[num]->dim_name_quark);
	} else {
		return(-1);
	}
}


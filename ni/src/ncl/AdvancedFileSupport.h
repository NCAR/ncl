/*
 *      $Id: FileSupport.h 12753 2011-12-20 21:43:15Z huangwei $
 */
/************************************************************************
*									*
*			     Copyright (C)  2012			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		AdvancedFileSupport.h
 *
 *	Author:		Wei Huang
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri May 11 08:30:53 MDT 2012
 *
 *	Description:	
 */
#ifndef _AdvancedFileSupport_h
#define _AdvancedFileSupport_h

#ifdef NIO_LIB_ONLY
#include "niohlu.h"
#include "nioNresDB.h"
#include "nioError.h"
#else
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include "ncarg/hlu/Error.h"
#endif

#include <netcdf.h>

#ifdef BuildHDF5
#include <hdf5.h>

#ifdef BuildHDFEOS5
#include <HE5_HdfEosDef.h>
#endif
#endif

/*
#ifdef BuildHDF4
#include <mfhdf.h>

#ifdef BuildHDFEOS
#include <HdfEosDef.h>
#endif
#endif
*/

#include "defs.h"
#include "NclMultiDValData.h"
#include "NclFile.h"
#include "NclAdvancedFile.h"
#include "NclList.h"
#include "NclGroup.h"
#include "NclAdvancedGroup.h"
#include "NclFileInterfaces.h"
#include "DataSupport.h"
#include "TypeSupport.h"
#include "Symbol.h"
#include "NclCoordVar.h"
#include "FileSupport.h"
#include "VarSupport.h"
#include "ApiRecords.h"
#include "NclAtt.h"
#include "NclAdvancedFileStructure.h"
#include "NclOptions.h"

#include <sys/stat.h>

NclQuark *GetGrpVarNames(void *therec, int *num_vars);
NclQuark *GetGrpDimNames(void *therec, int *num_dims);
NclQuark *GetGrpAttNames(void* therec, int *num_atts);
NclQuark *GetVarAttNamesFromGrp(void *therec, NclQuark thevar, int *num_atts);

NclFileVarNode *GetVarNodeFromGrpNode(void *therec, NclQuark var_name);
NclFileAttNode *GetAttInfoFromGrpNode(NclFileGrpNode *grpnode, NclQuark att_name);
NclFileAttNode *GetAttInfoFromVarNode(NclFileVarNode *varnode, NclQuark att_name);
NclFileAttNode *GetVarAttInfoFromGrpNode(NclFileGrpNode *grpnode,
                                         NclQuark var_name, NclQuark att_name);

NclFVarRec *GetVarInfo(void *therec, NclQuark var_name);
NclFAttRec *GetAttInfo(void* therec, NclQuark att_name);
NclFDimRec *GetDimInfo(void* therec, NclQuark dim_name);

NclFAttRec *GetVarAttInfo(void *therec, NclQuark thevar, NclQuark theatt);

NhlErrorTypes AddNewGrp(void *rec, NclQuark grpname, size_t id);

NclQuark *splitString(NclQuark inq, int *num);

int get_sizeof(int nv, int ts);

void _Ncl_add_udt(NclFileUDTRecord **rootudtrec,
                  int gid, int uid, NclQuark name,
                  int ncl_class, int type,
                  size_t size, size_t nfields,
                  NclQuark *mem_name, NclBasicDataTypes *mem_type);

void *GetCachedValue(NclFileVarNode *varnode,
                     long start, long finish, long stride, void *storage);
void _NclCopyGroupOptions(NclFileGrpNode *grpnode, NclFileGrpNode *rootgrpnode);
void _NclCopyOption(NCLOptions *option, NclQuark name,
                    NclBasicDataTypes data_type, int n_items, void *values);
#endif


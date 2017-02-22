/************************************************************************
*                                                                       *
*                 Copyright (C)  1994                                   *
*         University Corporation for Atmospheric Research               *
*                 All Rights Reserved                                   *
*                                                                       *
************************************************************************/
/*
 *      $Id$
 */

#include "NclAdvancedFile.h"
#include "AdvancedFileSupport.h"

static char blank_space[MAX_BLANK_SPACE_LENGTH];
static int indentation_level;
static int indentation_length;

static struct _NclMultiDValDataRec *AdvancedFileReadVarAtt(NclFile infile,
                                                      NclQuark var,
                                                      NclQuark attname,
                                                      struct _NclSelectionRecord *sel_ptr);
static struct _NclVarRec* AdvancedFileReadCoord(NclFile infile, NclQuark coord_name,
                                           struct _NclSelectionRecord* sel_ptr);
static NhlErrorTypes AdvancedFileWriteAtt(NclFile infile, NclQuark attname,
                                     struct _NclMultiDValDataRec* value,
                                     struct _NclSelectionRecord *sel_ptr);
static NhlErrorTypes AdvancedFileSetFileOption(NclFile  thefile,
                                          NclQuark format,
                                          NclQuark option,
                                          NclMultiDValData value);
static NhlErrorTypes AdvancedFileAddDim(NclFile thefile, NclQuark dimname,
                                   ng_size_t dimsize, int is_unlimited);
static NhlErrorTypes AdvancedFileAddChunkDim(NclFile infile, NclQuark dimname,
                                        ng_size_t dimsize, int is_unlimited);
static NhlErrorTypes AdvancedFileAddVarChunk(NclFile infile, NclQuark varname,
                                        int n_dims, ng_size_t *dims);
static NhlErrorTypes AdvancedFileAddVarChunkCache(NclFile thefile, NclQuark varname,
                                             ng_size_t cache_size, ng_size_t cache_nelems, 
                                             float cache_preemption);
static NhlErrorTypes AdvancedFileSetVarCompressLevel(NclFile infile, NclQuark varname,
                                                int compress_level);
/*
static NhlErrorTypes AdvancedFileAddGrp(NclFile thefile, NclQuark grpname);
*/
static NhlErrorTypes AdvancedFileAddVar(NclFile thefile, NclQuark varname,
                                   NclQuark type, int n_dims, NclQuark *dimnames);
static NhlErrorTypes AdvancedFileWriteVarAtt(NclFile thefile, NclQuark var, NclQuark attname,
                                        struct _NclMultiDValDataRec* value,
                                        struct _NclSelectionRecord * sel_ptr);
static NhlErrorTypes MyAdvancedFileWriteVar(NclFile thefile, NclQuark var,
                                       struct _NclMultiDValDataRec *value,
                                       struct _NclSelectionRecord * sel_ptr,
                                       NclQuark *dim_names, int type);
static NhlErrorTypes AdvancedFileWriteGrp(NclFile thefile, NclQuark grpname);
static NhlErrorTypes AdvancedFileCreateVlenType(NclFile thefile, NclQuark vlen_name, NclQuark var_name,
                                           NclQuark type, NclQuark *dim_names, ng_size_t ndims);
static NhlErrorTypes AdvancedFileWriteVar(NclFile thefile, NclQuark var,
                                     struct _NclMultiDValDataRec *value,
                                     struct _NclSelectionRecord *sel_ptr);
static NhlErrorTypes AdvancedFileWriteVarVar(NclFile infile, NclQuark lhs_var,
                                        struct _NclSelectionRecord *lhs_sel_ptr,
                                        struct _NclVarRec *rhs_var,
                                        struct _NclSelectionRecord *rhs_sel_ptr);
static NhlErrorTypes AdvancedFileWriteCoord(NclFile infile, NclQuark coord_name,
                                       struct _NclMultiDValDataRec *value,
                                       struct _NclSelectionRecord *sel_ptr);
static NhlErrorTypes AdvancedFileVarWriteDim(NclFile infile, NclQuark var,
                                        NclQuark dim_name, long dim_num);
static struct _NclMultiDValDataRec* AdvancedFileReadVarValue(NclFile thefile, NclQuark var_name,
                                                        struct _NclSelectionRecord* sel_ptr);
static struct _NclMultiDValDataRec* MyAdvancedFileReadVarValue(NclFile thefile, NclQuark var_name,
                                                          struct _NclSelectionRecord *sel_ptr,
                                                          NclDimRec *dim_info, int vtype);
static struct _NclMultiDValDataRec* AdvancedFileReadVarValue(NclFile thefile, NclQuark var_name,
                                                        struct _NclSelectionRecord* sel_ptr);
static struct _NclMultiDValDataRec *AdvancedFileReadAtt(NclFile infile, NclQuark attname,
                                                   struct _NclSelectionRecord *sel_ptr);
static struct _NclMultiDValDataRec *AdvancedFileVarReadDim(NclFile infile, NclQuark var,
                                                      NclQuark dim_name, long dim_num);
static struct _NclMultiDValDataRec* AdvancedFileReadDim(NclFile infile,
                                                   NclQuark dim_name, 
                                                   long dim_num);
static NclObjTypes AdvancedFileVarRepValue(NclFile thefile, NclQuark var);
static NhlErrorTypes AdvancedUpdateCoordInfo(NclAdvancedFile thefile, NrmQuark varname);
static NhlErrorTypes AdvancedFileWriteDim(NclFile infile, NclQuark dim_name, long dimid);
static NhlErrorTypes AdvancedFileDelAtt(NclFile infile, NclQuark attname);
static NhlErrorTypes AdvancedFileDelVarAtt(NclFile infile, NclQuark var, NclQuark attname);
static void AdvancedAdjustForScalarDim(NclAdvancedFile thefile);
static int AdvancedFileIsCoord(NclFile infile, NclQuark coord_name);
static int AdvancedFileIsDim(NclFile infile, NclQuark dim_name);
static int AdvancedFileIsVarAtt(NclFile infile, NclQuark var, NclQuark theatt);
static int AdvancedVarAttIndex(NclFileVarNode *varnode, NclQuark theatt);
static int AdvancedFileIsAtt(NclFile infile,NclQuark theatt);
static int myVarIsDimInGrpNode(NclFileGrpNode *grpnode, NclQuark var, NclQuark dim_name);
static int AdvancedFileVarIsDim(NclFile infile, NclQuark var, NclQuark dim_name);
static int _getGroupIdFromGrpNode(NclFileGrpNode *grpnode, NclQuark group);
static int AdvancedFileIsGroup(NclFile infile, NclQuark group);
static int isUnlimitedDimension(NclFileGrpNode *grpnode, NclQuark dimname);

static NclGroup *AdvancedFileReadGroup(NclFile infile, NclQuark group_name);

NhlErrorTypes _addNclEnumNode(NclFileEnumRecord **enumrec,
                             NclQuark name, long long value)
{
    int i, n = 0;

    if(NULL == *enumrec)
    {
        *enumrec = _NclFileEnumAlloc(10);
        (*enumrec)->n_enums = 0;
    }
    else if((*enumrec)->n_enums >= (*enumrec)->max_enums)
    {
        _NclFileEnumRealloc(enumrec);
    }

    n = (*enumrec)->n_enums;

    for(i = 0; i < n; i++)
    {
        if(name == (*enumrec)->enum_node[i].name)
            return (NhlNOERROR);
    }

    (*enumrec)->enum_node[n].name = name;
    (*enumrec)->enum_node[n].value = value;
    (*enumrec)->n_enums++;

    return (NhlNOERROR);
}

NclFileEnumRecord *_NclFileEnumAlloc(int n_enums)
{
    NclFileEnumRecord *enum_rec = NULL;

    if(n_enums < 1)
    {
        return enum_rec;
    }
   
    enum_rec = (NclFileEnumRecord *)NclCalloc(1, sizeof(NclFileEnumRecord));
    assert(enum_rec);

    enum_rec->n_enums = n_enums;
    enum_rec->max_enums = n_enums;

    enum_rec->enum_node = (NclFileEnumNode *)NclCalloc(enum_rec->max_enums,
                           sizeof(NclFileEnumNode));
    assert(enum_rec->enum_node);

    return enum_rec;
}

void _NclFileEnumRealloc(NclFileEnumRecord **enum_rec)
{
    if((*enum_rec)->n_enums >= (*enum_rec)->max_enums)
    {
        NclFileEnumNode *enumnodeptr;

        (*enum_rec)->max_enums *= 2;

        enumnodeptr = (NclFileEnumNode *)NclRealloc((*enum_rec)->enum_node,
                       (*enum_rec)->max_enums * sizeof(NclFileEnumNode));
        assert(enumnodeptr);
        (*enum_rec)->enum_node = enumnodeptr;
    }
}

NclQuark* _NclSplitString(NclQuark str, int *ns)
{
    int n;

    char delim[2] = "/";
    char *tmp_str;
    char *result = NULL;
    NclQuark *arraySubString;

    int max_length = 4;
    
  /*
   *fprintf(stderr, "\nEnter _Nclstr_split, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tstr: <%s>\n", NrmQuarkToString(str));
   */

    arraySubString = (NclQuark *) NclMalloc(max_length * sizeof(NclQuark));
    if(NULL == arraySubString)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return 0;
    }

    n = strlen((char *) NrmQuarkToString(str)) + 2;
    tmp_str = (char *) NclMalloc(n);
    if(NULL == tmp_str)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return 0;
    }

    strcpy(tmp_str, (char *) NrmQuarkToString(str));
    result = strtok(tmp_str, delim);
    n = 0;
    while(result != NULL)
    {
        arraySubString[n] = NrmStringToQuark(result);

      /*
       *fprintf(stderr, "\tsubString[%d] = <%s>\n", n, result);
       */
        ++n;
        if(n >= max_length)
        {
            max_length *= 2;
            arraySubString = (NclQuark *) NclRealloc(arraySubString, max_length*sizeof(NclQuark));
            if(NULL == arraySubString)
            {
                NHLPERROR((NhlFATAL,ENOMEM,NULL));
                return 0;
            }
        }
        result = strtok(NULL, delim);
    }

    if(n)
        arraySubString = (NclQuark *) NclRealloc(arraySubString, n*sizeof(NclQuark));
    else
    {
        NclFree(arraySubString);
        arraySubString = NULL;
    }

    *ns = n;

    return arraySubString;
}

char *_getComponentName(const char *fullname, char **structname)
{
    size_t ns = 1;
    size_t nc = 1;
    char *dot_ptr;
    char *cname = NULL;
    char *sname = NULL;

  /*
   *fprintf(stderr, "\nEnter _getComponentName, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tfullname: <%s>\n", fullname);
   */

    dot_ptr = strchr(fullname, '.');
    if(dot_ptr && (NULL == strchr(dot_ptr, '/')))
    {
        ns = dot_ptr - fullname;
        nc = strlen(dot_ptr);
        sname = (char *) NclCalloc(ns + 1, sizeof(char));
        cname = (char *) NclCalloc(nc, sizeof(char));

        memcpy(sname, fullname, ns);
        sname[ns] = '\0';

        memcpy(cname, dot_ptr + 1, nc - 1);
        cname[nc-1] = '\0';

      /*
       *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\tcname: <%s>\n", cname);
       *fprintf(stderr, "\tsname: <%s>\n", sname);
       */
    }
    else
    {
        sname = (char *) NclCalloc(strlen(fullname) + 1, sizeof(char));
        strcpy(sname, fullname);
    }

    *structname = sname;

  /*
   *fprintf(stderr, "Leave _getComponentName, file: %s, line: %d\n\n", __FILE__, __LINE__);
   */

    return cname;
}

NclFileCompoundNode *_getComponentNodeFromVarNode(NclFileVarNode *varnode,
                                                  const char *component_name)
{
    NclFileCompoundNode *compnode = NULL;
    int n;
    NclQuark qcn = NrmStringToQuark(component_name);

    if(NCL_compound == varnode->type)
    {
      /*
       *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\tcomponent_name: <%s>\n", component_name);
       *fprintf(stderr, "\tvarnode->name: <%s>\n", NrmQuarkToString(varnode->name));
       */

        for(n = 0; n < varnode->comprec->n_comps; n++)
        {
            compnode = &(varnode->comprec->compnode[n]);
          /*
           *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
           *fprintf(stderr, "\tcomponent[%d]: <%s>\n", n, component_name);
           */
            if(qcn == compnode->name)
            {
                return (compnode);
            }
        }
    }

    return compnode;
}


NhlErrorTypes _NclAdvancedFilePrintSummary(NclObj self, FILE *fp)
{
    NclAdvancedFile thefile = (NclAdvancedFile)self;
    int ret = 0;

    if(Ncl_FileVar == thefile->advancedfile.type) {
        ret = nclfprintf(fp,"Type: file\n");
	nclfprintf(fp, "filename:\t%s\n",NrmQuarkToString(thefile->advancedfile.fname));
    }
   else if(Ncl_FileGroup == thefile->advancedfile.type)
    {
        ret = nclfprintf(fp,"Type: group\n");
        nclfprintf(fp, "groupname:\t%s\n",NrmQuarkToString(thefile->advancedfile.gname));
    }
    ret = nclfprintf(fp,"File path\t:\t%s\n\n",NrmQuarkToString(thefile->advancedfile.fpath));
    if(ret < 0)
        return(NhlWARNING);
    
    if(NULL != thefile->advancedfile.grpnode->udt_rec)
    {
        nclfprintf(fp,"Number of user defined types\t:\t %d\n",
                thefile->advancedfile.grpnode->udt_rec->n_udts);
    }
    
    if(NULL != thefile->advancedfile.grpnode->att_rec)
    {
        nclfprintf(fp,"Number of global attributes\t:\t %d\n",
                thefile->advancedfile.grpnode->att_rec->n_atts);
    }

    if(NULL != thefile->advancedfile.grpnode->dim_rec)
    {
        nclfprintf(fp,"Number of dimensions\t:\t %d\n",
                   thefile->advancedfile.grpnode->dim_rec->n_dims);
    }

    if(NULL != thefile->advancedfile.grpnode->chunk_dim_rec)
    {
        nclfprintf(fp,"Number of chunk_dimensions\t:\t %d\n",
                   thefile->advancedfile.grpnode->chunk_dim_rec->n_dims);
    }

    if(NULL != thefile->advancedfile.grpnode->grp_rec)
    {
	    int n_grps;
	    NrmQuark *grp_names;
	    grp_names = _NclGetGrpNames((void *) thefile->advancedfile.grpnode,&n_grps);
	    NclFree(grp_names);
	    nclfprintf(fp,"Number of groups\t:\t %d (in this group only) %d (including all descendent groups)\n",
		       thefile->advancedfile.grpnode->grp_rec->n_grps, n_grps);
    }

    {
	    int n_vars;
	    NrmQuark *var_names;
	    var_names = GetGrpVarNames((void *)thefile->advancedfile.grpnode, &n_vars);
	    NclFree(var_names);
	    if(n_vars)
	    {
		    nclfprintf(fp,"Number of variables\t:\t %d (in this group only) %d (including all descendent groups)\n",
			       thefile->advancedfile.grpnode->var_rec ? thefile->advancedfile.grpnode->var_rec->n_vars : 0, n_vars);
	    }
    }

    return ret;
}

void _clearNclPrintIndentation()
{
    blank_space[0] = '\0';
    indentation_level = 0;
    indentation_length = 0;
}

void _increaseNclPrintIndentation()
{
     indentation_level ++;
     indentation_length += 4;
     strcat(blank_space, "    ");
}

void _decreaseNclPrintIndentation()
{
     if(indentation_level)
         indentation_level --;
     if(indentation_length)
         indentation_length -= 4;
     blank_space[indentation_length] = '\0';
}

void _printNclTypeUtil(FILE *fp, NclBasicDataTypes type, void *val, size_t index, int align, int newline)
{
    if (align) {
        nclfprintf(fp, "%s", blank_space);
    }
    
    switch(type)
    {
        case NCL_string:
        {
             NclQuark *v = (NclQuark*)val;
             nclfprintf(fp, "%s", NrmQuarkToString(v[index]));
             break;
        }
        case NCL_float:
        {
             float *v = (float *)val;
             nclfprintf(fp, "%2.7g", v[index]);
             break;
        }
        case NCL_double:
        {
             double *v = (double *)val;
             nclfprintf(fp, "%4.16lg", v[index]);
             break;
        }
        case NCL_int:
        {
             int *v = (int *)val;
             nclfprintf(fp, "%d", v[index]);
             break;
        }
        case NCL_uint:
        {
             int *v = (int *)val;
             nclfprintf(fp, "%u", v[index]);
             break;
        }
        case NCL_byte:
        {
             char *v = (char *)val;
             nclfprintf(fp, "%hhd", v[index]);
             break;
        }
        case NCL_ubyte:
        {
             char *v = (char *)val;
             nclfprintf(fp, "%hhu", v[index]);
             break;
        }
        case NCL_short:
        {
             short *v = (short *)val;
             nclfprintf(fp, "%hd", v[index]);
             break;
        }
        case NCL_ushort:
        {
             short *v = (short *)val;
             nclfprintf(fp, "%hu", v[index]);
             break;
        }
        case NCL_long:
        {
             long *v = (long *)val;
             nclfprintf(fp, "%ld", v[index]);
             break;
        }
        case NCL_ulong:
        {
             long *v = (long *)val;
             nclfprintf(fp, "%lu", v[index]);
             break;
        }
        case NCL_int64:
        {
             long long *v = (long long *)val;
             nclfprintf(fp, "%lld", v[index]);
             break;
        }
        case NCL_uint64:
        {
             long long *v = (long long *)val;
             nclfprintf(fp, "%llu", v[index]);
             break;
        }
        case NCL_char:
        {
             char *v = (char *)val;
             nclfprintf(fp, "%c", v[index]);
             break;
        }
        case NCL_reference:
        {
             NclFileReferenceNode *v = (NclFileReferenceNode *)val;
             nclfprintf(fp, "reference to <%s>", NrmQuarkToString(v[index].obj_name));
             break;
        }
        default:
            fprintf(stderr, "\nIn file: %s, line: %d\n", __FILE__, __LINE__);
            fprintf(stderr, "\tUNKNOWN type: 0%o, val (in char): <%s>", type, (char *)val);
            break;
    }

    if(newline)
        nclfprintf(fp, "\n");
}

void _printNclTypeValAligned(FILE *fp, NclBasicDataTypes type, void *val, int newline)
{
    _printNclTypeUtil(fp, type, val, 0, TRUE, newline);
}

void _printNclTypeVal(FILE *fp, NclBasicDataTypes type, void *val, int newline)
{
    _printNclTypeUtil(fp, type, val, 0, FALSE, newline);
}

void _printNclTypeValIndexed(FILE *fp, NclBasicDataTypes type, void *val, size_t np, int newline)
{
    _printNclTypeUtil(fp, type, val, np, FALSE, newline);
}

void _printStringConstUtil(FILE *fp, char* str, int align, int newline)
{
    if (align)
        nclfprintf(fp, "%s", blank_space);

    nclfprintf(fp, "%s", str);    
    
    if(newline)
        nclfprintf(fp, "\n");    
}

void _printStringConstAligned(FILE *fp, char* str, int newline)
{
    _printStringConstUtil(fp, str, TRUE, newline);
}

void _printStringConst(FILE *fp, char* str, int newline) {
    _printStringConstUtil(fp, str, FALSE, newline);
}

void _printNclFileAttRecord(FILE *fp, NclAdvancedFile thefile, NclFileAttRecord *attrec)
{
    NclFileAttNode   *attnode;
    NclMultiDValData tmp_md;
    int i, j;
    int max_print_att = 10;
    
    if(NULL == attrec)
        return;

  /*
   *_justPrintTypeVal(fp, NCL_char, "\n", 0);
   */
    _printStringConstAligned(fp, "Number of Attributes:", FALSE);
    _printNclTypeValAligned(fp, NCL_int, &attrec->n_atts, TRUE);

    _increaseNclPrintIndentation();

  /*
   *fprintf(stderr, "\n_printNclFileAttRecord, in file: %s, line: %d\n", __FILE__, __LINE__);
   */

    for(i = 0; i < attrec->n_atts; i++)
    {
        attnode = &(attrec->att_node[i]);
        _printNclTypeValAligned(fp, NCL_string, &(attnode->name), FALSE);
        _printStringConst(fp, "\t: ", FALSE);

        if(attnode->is_compound)
        {
            NclFileCompoundRecord *comprec = (NclFileCompoundRecord *) attnode->value;
            NclFileCompoundNode *compnode;
          /*
           *fprintf(stderr, "\nIn file: %s, line: %d\n", __FILE__, __LINE__);
           *fprintf(stderr, "\tAtt No. %d: name: <%s>, nelem: %d, type: 0%o, type-name: %s\n",
           *                 i, NrmQuarkToString(attnode->name), attnode->n_elem,
           *                 attnode->type, NrmQuarkToString(comprec->name));
           */

            _printStringConst(fp, "\t", FALSE);
            _printNclTypeVal(fp, NCL_string, &comprec->name, FALSE);
            _printStringConst(fp, " (\"", FALSE);

            for(j = 0; j < comprec->n_comps; j++)
            {
                compnode = &(comprec->compnode[j]);
                if(j)
                {
                    _printStringConst(fp, "\", \"", FALSE);
                }
                if(NULL != compnode->value)
                    _printNclTypeVal(fp, NCL_string, compnode->value, FALSE);
              /*
               *_justPrintTypeVal(fp, compnode->type, compnode->value, 0);
               */

              /*
               *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
               *fprintf(stderr, "\tcompnode->value: <%s>\n", NrmQuarkToString((NclQuark)compnode->value));
               *fprintf(stderr, "\tcompnode->the_nc_type: 0%o, compnode->type: 0%o, NCL_string: 0%o\n",
               *                   compnode->the_nc_type, compnode->type, NCL_string);
               */
            }

            _printStringConst(fp, "\")", TRUE);

            continue;
        }
        else if(attnode->is_vlen)
        {
            size_t n;
            NclFileVlenRecord *vlenrec = (NclFileVlenRecord *) attnode->value;
          /*
           *fprintf(stderr, "\nIn file: %s, line: %d\n", __FILE__, __LINE__);
           *fprintf(stderr, "\tAtt No. %d: name: <%s>, n_vlens: %d, type: 0%o, type-name: %s\n",
           *                 i, NrmQuarkToString(attnode->name), vlenrec->n_vlens,
           *                 vlenrec->type, NrmQuarkToString(vlenrec->name));
           */

            _printStringConst(fp, "\t", FALSE);
	    if (vlenrec->name > NrmNULLQUARK)
		    _printNclTypeVal(fp, NCL_string, &vlenrec->name, FALSE);
            _printStringConst(fp, " {{", FALSE);

            for(j = 0; j < vlenrec->n_vlens; j++)
            {
                if(j)
                {
                    _printStringConst(fp, "}, {", FALSE);
                }

              /*
               *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
               *fprintf(stderr, "\tvlenrec->vs[%d]: %d, vlenrec->ve[%d]: %d\n",
               *                j, vlenrec->vs[j], j, vlenrec->ve[j]);
               */

                for(n = vlenrec->vs[j]; n < vlenrec->ve[j]; n++)
                {
                  /*
                   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
                   *fprintf(stderr, "\tn = %d\n", n);
                   */

                    if(n > vlenrec->vs[j])
                        _printStringConst(fp, ", ", FALSE);
                    _printNclTypeValIndexed(fp, vlenrec->type, vlenrec->values, n, FALSE);
                }

              /*
               *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
               *fprintf(stderr, "\tvlenrec->the_nc_type: 0%o, vlenrec->type: 0%o, NCL_string: 0%o\n",
               *                   vlenrec->the_nc_type, vlenrec->type, NCL_string);
               */
            }

            _printStringConst(fp, "}}", TRUE);

            continue;
        }
        else if(attnode->is_opaque)
        {
            int k;
            size_t n = 0;
            NclFileOpaqueRecord *opaquerec = (NclFileOpaqueRecord *) attnode->value;
          /*
           *fprintf(stderr, "\nIn file: %s, line: %d\n", __FILE__, __LINE__);
           *fprintf(stderr, "\tAtt No. %d: name: <%s>, n_opaques: %d, type: 0%o, type-name: %s\n",
           *                 i, NrmQuarkToString(attnode->name), opaquerec->n_opaques,
           *                 opaquerec->type, NrmQuarkToString(opaquerec->name));
           *fprintf(stderr, "\topaquerec->n_opaques = %d, opaquerec->size = %d\n",
           *                   opaquerec->n_opaques, opaquerec->size);
           */

            _printStringConst(fp, "\t", FALSE);
            _printNclTypeVal(fp, NCL_string, &opaquerec->name, FALSE);
            _printStringConst(fp, " {{", FALSE);

            for(j = 0; j < opaquerec->n_opaques; j++)
            {
                if(j)
                {
                    _printStringConst(fp, "}, {", FALSE);
                }

                for(k = 0; k < opaquerec->size; k++)
                {
                    if(k) _printStringConst(fp, ", ", FALSE);
                    _printNclTypeValIndexed(fp, opaquerec->type, opaquerec->values, n, FALSE);
                    n++;
                }
            }

            _printStringConst(fp, "}}", TRUE);

            continue;
        }

        if(NULL == attnode->value)
        {
            tmp_md = _NclFileReadAtt((NclFile)thefile,attnode->name,NULL);
            attnode->value = NclCalloc(attnode->n_elem, _NclSizeOf(attnode->type));
            memcpy(attnode->value, tmp_md->multidval.val,
                   attnode->n_elem * _NclSizeOf(attnode->type));
        }

        if(1 == attnode->n_elem)
        {
            _printStringConst(fp, "\t", FALSE);
            _printNclTypeVal(fp, attnode->type, attnode->value, TRUE);
        }
        else if (attnode->n_elem > 1)
        {
            max_print_att = attnode->n_elem;
            if(max_print_att > 10)
                max_print_att = 10;
            _printStringConst(fp, "\t( ", FALSE);
            for (j = 0; j < max_print_att; j++)
            {
                if(j)
                    _printStringConst(fp, ", ", FALSE);
                _printNclTypeValIndexed(fp, attnode->type, attnode->value, j, FALSE);
            }

            if(max_print_att != attnode->n_elem)
            {
                _printStringConst(fp, ", ... [Total of ", FALSE);
                _printNclTypeVal(fp, NCL_int, &(attnode->n_elem), FALSE);
                _printStringConst(fp, " values] )", TRUE);
            }
            else
                _printStringConst(fp, " )", TRUE);
        }
        else
        {
            _printStringConst(fp, "<ARRAY of ", FALSE);
            _printNclTypeVal(fp, NCL_int, &(attnode->n_elem), FALSE);
            _printStringConst(fp, " elements>", TRUE);
        }
    }

    _decreaseNclPrintIndentation();
}

void _printNclFileUDTRecord(FILE *fp, NclAdvancedFile thefile, NclFileUDTRecord *udtrec)
{
    NclFileUDTNode   *udtnode;
    int i, n;
    
    if(NULL == udtrec)
        return;

  /*
   *_justPrintTypeVal(fp, NCL_char, "\n", 0);
   */

    _printStringConstAligned(fp, "User Defined Types:", TRUE);
    _increaseNclPrintIndentation();

  /*
   *fprintf(stderr, "\nEnter _printNclFileUDTRecord, in file: %s, line: %d\n", __FILE__, __LINE__);
   */

    for(i = 0; i < udtrec->n_udts; i++)
    {
        udtnode = &(udtrec->udt_node[i]);
      /*
       *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\tUDT No. %d: name: <%s>\n", i, 
       *                 NrmQuarkToString(udtnode->name));
       */

        _printNclTypeValAligned(fp, NCL_string, &(udtnode->name), TRUE);

        _printStringConstAligned(fp, "{", TRUE);

        _increaseNclPrintIndentation();

        for(n = 0; n < udtnode->n_fields; n++)
        {
		ng_size_t i;
		/*
		 *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
		 *fprintf(stderr, "\tUDT Comp No. %d: name: <%s>\n", n,
		 *                 NrmQuarkToString(udtnode->mem_name[n]));
		 */
		NclFileUDTField   *field;
		NrmQuark type_name;
		
		field = &(udtnode->fields[n]);
		type_name = NrmStringToQuark(_NclBasicDataTypeToName(field->field_type));
		
		_printNclTypeValAligned(fp, NCL_string, &type_name, FALSE);
		_printNclTypeValAligned(fp, NCL_string, &(field->field_name), FALSE);
		if (field->n_dims > 0) {
			_printStringConst(fp, " (", FALSE);
			for (i = 0; i < field->n_dims; i++) {
				_printNclTypeVal(fp, NCL_int64, &field->dim_sizes[i], FALSE);
				if (i < field->n_dims -1)
					_printStringConst(fp, ",", FALSE);
				else
					_printStringConst(fp, ")", FALSE);
			}
		}
		_printStringConst(fp, ";", TRUE);
        }

        _decreaseNclPrintIndentation();

        _printStringConstAligned(fp, "}\n\n", FALSE);
    }

    _decreaseNclPrintIndentation();

  /*
   *fprintf(stderr, "Leave _printNclFileUDTRecord, in file: %s, line: %d\n\n", __FILE__, __LINE__);
   */
}

void _printNclFileDimRecord(FILE *fp, NclAdvancedFile thefile, NclFileDimRecord *dimrec)
{
    NclFileDimNode   *dimnode;
    long long llv;
    int i;

    if(NULL == dimrec)
        return;

    _printStringConst(fp, "\n", FALSE);
    _printStringConstAligned(fp, "dimensions:", TRUE);
    _increaseNclPrintIndentation();

    for(i = 0; i < dimrec->n_dims; i++)
    {
        dimnode = &(dimrec->dim_node[i]);
        llv = dimnode->size;
        _printNclTypeValAligned(fp, NCL_string, &(dimnode->name), FALSE);
        _printStringConst(fp, "\t= ", FALSE);
        if(dimnode->is_unlimited)
        {
            _printNclTypeVal(fp, NCL_int64, &llv, FALSE);
            _printStringConst(fp, " // unlimited", TRUE);
        }
        else
        {
            _printNclTypeVal(fp, NCL_int64, &llv, TRUE);
        }
    }

    _decreaseNclPrintIndentation();
}

void _printNclFileChunkDimRecord(FILE *fp, NclAdvancedFile thefile, NclFileDimRecord *dimrec)
{
    NclFileDimNode   *dimnode;
    long long llv;
    int i;

    if(NULL == dimrec)
        return;

    _printStringConst(fp, "\n", FALSE);
    _printStringConstAligned(fp, "chunk dimensions:", TRUE);
    _increaseNclPrintIndentation();

    for(i = 0; i < dimrec->n_dims; i++)
    {
        dimnode = &(dimrec->dim_node[i]);
        llv = dimnode->size;
        _printNclTypeValAligned(fp, NCL_string, &(dimnode->name), FALSE);
        _printStringConst(fp, "\t= ", FALSE);
        if(dimnode->is_unlimited)
        {
            _printNclTypeVal(fp, NCL_int64, &llv, FALSE);
            _printStringConst(fp, " // unlimited", TRUE);
        }
        else
        {
            _printNclTypeVal(fp, NCL_int64, &llv, TRUE);
        }
    }

    _decreaseNclPrintIndentation();
}

void _printNclFileVarDimRecord(FILE *fp, NclFileDimRecord *dim_rec)
{
    NclFileDimNode   *dimnode;
    long long llv;
    int i;
   
    _printStringConst(fp, "\t[ ", FALSE);

    for(i = 0; i < dim_rec->n_dims; i++)
    {
        dimnode = &(dim_rec->dim_node[i]);

        if(i)
            _printStringConst(fp, " x ", FALSE);

        llv = dimnode->size;
        _printNclTypeVal(fp, NCL_int64, &llv, FALSE);
        _printStringConst(fp, " <", FALSE);
        _printNclTypeVal(fp, NCL_string, &(dimnode->name), FALSE);
        if(dimnode->is_unlimited)
            _printStringConst(fp, " | unlimited", FALSE);

        _printStringConst(fp, ">", FALSE);
    }

    _printStringConst(fp, " ]", TRUE);
}

void _printNclFileVarNode(FILE *fp, NclAdvancedFile thefile, NclFileVarNode *varnode)
{
    NclFileDimRecord* dim_rec;
    NclFileDimNode* dimnode;
    NclFileVarNode* dimvarnode;
    long long total_size = 1;
    float sval = 0.0;
    float eval = 0.0;
    float* fptr;
    double* dptr;
    int* iptr;
    char type_str[1024];
    int i;
    
    if(NULL == varnode)
        return;


    if (varnode->udt_type_node) {
	    strcpy(type_str, NrmQuarkToString(varnode->udt_type_node->name));
    }
    else {
	    switch (varnode->udt_type) {
	    case NCL_UDT_compound:
		    strcpy(type_str, NrmQuarkToString(varnode->comprec->name));
		    break;
	    default:
		    strcpy(type_str, _NclBasicDataTypeToName(varnode->type));
		    break;
	    }
    }
	    

  /*
   *if(0 == strcmp("compound", type_str))
   *{
   *    NclFileCompoundRecord *comprec = (NclFileCompoundRecord *)varnode->comprec;

   *    strcpy(type_str, NrmQuarkToString(comprec->name));

   *   *fprintf(stderr, "\nin _printNclFileVarRecord, file: %s, line: %d\n", __FILE__, __LINE__);
   *   *fprintf(stderr, "\tNEED TO HANDLE _printNCLVarRecord of compound.\n\n");
   *}
  */

    _printStringConstAligned(fp, "Variable: ", FALSE);
    _printNclTypeVal(fp, NCL_string, &(varnode->name), TRUE);

    _printStringConstAligned(fp, "Type: ", FALSE);
    _printStringConst(fp, type_str, TRUE);
  
    dim_rec = varnode->dim_rec;

    if(NULL != dim_rec)
    {
        for(i = 0; i < dim_rec->n_dims; i++)
        {
            dimnode = &(dim_rec->dim_node[i]);
            total_size *= dimnode->size;
        }

        _printStringConstAligned(fp, "Total Size: ", FALSE);
        _printNclTypeVal(fp, NCL_int64, &total_size, FALSE);
        _printStringConst(fp, " values", TRUE);

	if (varnode->udt_type == NCL_UDT_compound)
		total_size *= varnode->comprec->size;
	else
		total_size *= _NclSizeOf(varnode->type);

        _printStringConstAligned(fp, "            ", FALSE);
        _printNclTypeVal(fp, NCL_int64, &total_size, FALSE);
        _printStringConst(fp, " bytes", TRUE);

        _printStringConstAligned(fp, "Number of Dimensions: ", FALSE);
        _printNclTypeVal(fp, NCL_int, &dim_rec->n_dims, TRUE);

        _printStringConstAligned(fp, "Dimensions and sizes:", FALSE);
        _printNclFileVarDimRecord(fp, varnode->dim_rec);

        if(0 < varnode->is_chunked)
        {
            _printStringConstAligned(fp, "Chunking Info:", FALSE);
            _printNclFileVarDimRecord(fp, varnode->chunk_dim_rec);
        }

        _printStringConstAligned(fp, "Coordinates:", TRUE);
        for(i = 0; i < dim_rec->n_dims; i++)
        {
            dimnode = &(dim_rec->dim_node[i]);
            if(0 < dimnode->name)
            {
            dimvarnode = _getVarNodeFromNclFileGrpNode(thefile->advancedfile.grpnode, dimnode->name);

            if(NULL != dimvarnode)
            {
                if(NULL != dimvarnode->value)
                {
                    if(NCL_float == dimvarnode->type)
                    {
                        fptr = (float *) dimvarnode->value;
                        sval = fptr[0];
                        eval = fptr[dimnode->size - 1];
                    }
                    else if(NCL_double == dimvarnode->type)
                    {
                        dptr = (double *) dimvarnode->value;
                        sval = (float) dptr[0];
                        eval = (float) dptr[dimnode->size - 1];
                    }
                    else if(NCL_int == dimvarnode->type)
                    {
                        iptr = (int *) dimvarnode->value;
                        sval = (float) iptr[0];
                        eval = (float) iptr[dimnode->size - 1];
                    }
                }
                else
                {
                    NclMultiDValData tmp_md = NULL;
                    NclDimRec dim_info[NCL_MAX_DIMENSIONS];
                    tmp_md = MyAdvancedFileReadVarValue((NclFile) thefile, dimnode->name, NULL, dim_info, FILE_VAR_ACCESS);

                    if(NULL != tmp_md)
                    {
                        dimvarnode->value = (void *)NclMalloc(tmp_md->multidval.totalsize);
                        assert(dimvarnode->value);
                        memcpy(dimvarnode->value, tmp_md->multidval.val, tmp_md->multidval.totalsize);
                        if(NCL_float == tmp_md->multidval.data_type)
                        {
                            fptr = (float *) tmp_md->multidval.val;
                            sval = fptr[0];
                            eval = fptr[dimnode->size - 1];
                        }
                        else if(NCL_double == tmp_md->multidval.data_type)
                        {
                            dptr = (double *) tmp_md->multidval.val;
                            sval = (float) dptr[0];
                            eval = (float) dptr[dimnode->size - 1];
                        }
                        else if(NCL_int == tmp_md->multidval.data_type)
                        {
                            iptr = (int *) tmp_md->multidval.val;
                            sval = (float) iptr[0];
                            eval = (float) iptr[dimnode->size - 1];
                        }

                        _NclDestroyObj((NclObj)tmp_md);
                    }
                }

                _printStringConstAligned(fp, "            ", FALSE);
                _printNclTypeVal(fp, NCL_string, &dimnode->name, FALSE);
                _printStringConst(fp, ": [", FALSE);
                _printNclTypeVal(fp, NCL_float, &sval, FALSE);
                _printStringConst(fp, "..", FALSE);
                _printNclTypeVal(fp, NCL_float, &eval, FALSE);
                _printStringConst(fp, "]", TRUE);
		sval = eval = 0.0;
            }
            }
        }
    }

    _increaseNclPrintIndentation();
    if(NULL != varnode->att_rec)
    {
        if(NULL == varnode->att_rec->att_node[0].value)
            AdvancedLoadVarAtts(thefile,varnode->name);
    }
    if((NCL_enum == varnode->type) && (NULL != varnode->att_rec))
    {
        short *typechanged = (short *)NclCalloc(varnode->att_rec->n_atts, sizeof(short));
	for(i = 0; i < varnode->att_rec->n_atts; ++i)
	{
	    if(NCL_enum == varnode->att_rec->att_node[i].type)
	    {
		typechanged[i] = 1;
		varnode->att_rec->att_node[i].type = varnode->base_type;
	    }
	}
	
        _printNclFileAttRecord(fp, thefile, varnode->att_rec);

	for(i = 0; i < varnode->att_rec->n_atts; ++i)
	{
	    if(typechanged[i])
	    {
		varnode->att_rec->att_node[i].type = varnode->type;
	    }
	}
	NclFree(typechanged);
    }
    else
        _printNclFileAttRecord(fp, thefile, varnode->att_rec);
    _decreaseNclPrintIndentation();

    nclfprintf(fp, "\n");
}

void _printNclFileVarRecord(FILE *fp, NclAdvancedFile thefile, NclFileVarRecord *varrec)
{
    NclFileVarNode *varnode;
    int i;
    
    if(NULL == varrec)
        return;

    _printStringConst(fp, "\n", FALSE);
    _printStringConstAligned(fp, "variables:", TRUE);
    _increaseNclPrintIndentation();

    for(i = 0; i < varrec->n_vars; i++)
    {
        varnode = &(varrec->var_node[i]);

        _printNclFileVarNode(fp, thefile, varnode);
    }

    _decreaseNclPrintIndentation();
}

void _printNclFileGrpRecord(FILE *fp, NclAdvancedFile thefile, NclFileGrpRecord *grprec)
{
    NclFileGrpNode *grpnode;
    int i;

    if(NULL == grprec)
        return;

    _increaseNclPrintIndentation();
    _printStringConst(fp, "\n", FALSE);
    _printStringConstAligned(fp, "groups:", TRUE);

    for(i = 0; i < grprec->n_grps; i++)
    {
        grpnode = grprec->grp_node[i];

        _printNclTypeValAligned(fp, NCL_string, &(grpnode->name), FALSE);
        _printStringConst(fp, "\t<group>", TRUE);

        _printStringConstAligned(fp, "{", TRUE);

        _increaseNclPrintIndentation();

        _printNclFileAttRecord(fp, thefile, grpnode->att_rec);

        _printNclFileDimRecord(fp, thefile, grpnode->dim_rec);

        _printNclFileVarRecord(fp, thefile, grpnode->var_rec);

        _printNclFileGrpRecord(fp, thefile, grpnode->grp_rec);

        _decreaseNclPrintIndentation();

        _printStringConstAligned(fp, "}  end of ", FALSE);
        _printNclTypeVal(fp, NCL_string, &(grpnode->name), TRUE);

        nclfprintf(fp, "\n");
    }

    _decreaseNclPrintIndentation();
}


NhlErrorTypes AdvancedFilePrint(NclObj self, FILE *fp)
{
    NclAdvancedFile thefile = (NclAdvancedFile)self;
    NhlErrorTypes ret = NhlNOERROR;

    if(Ncl_FileVar == thefile->advancedfile.type)
        ret = nclfprintf(fp,"Type: file\n");
    else if(Ncl_FileGroup == thefile->advancedfile.type)
    {
        ret = nclfprintf(fp,"Type: group\n");
        nclfprintf(fp, "groupname:\t%s\n",NrmQuarkToString(thefile->advancedfile.gname));
    }
    else
        ret = nclfprintf(fp,"Type: should be file or group, but not clear at this time.\n");
    nclfprintf(fp, "filename:\t%s\n",NrmQuarkToString(thefile->advancedfile.fname));
    nclfprintf(fp, "path:\t%s\n",NrmQuarkToString(thefile->advancedfile.fpath));

    if(NULL == thefile->advancedfile.grpnode)
    {
        nclfprintf(fp,"Empty file!\n");
        return (ret);
    }

    _clearNclPrintIndentation();

    _printNclFileUDTRecord(fp, thefile, thefile->advancedfile.grpnode->udt_rec);

    _printNclFileAttRecord(fp, thefile, thefile->advancedfile.grpnode->att_rec);

    _printNclFileDimRecord(fp, thefile, thefile->advancedfile.grpnode->dim_rec);

    _printNclFileChunkDimRecord(fp, thefile, thefile->advancedfile.grpnode->chunk_dim_rec);

    _printNclFileVarRecord(fp, thefile, thefile->advancedfile.grpnode->var_rec);

    _printNclFileGrpRecord(fp, thefile, thefile->advancedfile.grpnode->grp_rec);

    return ret;
}


NhlErrorTypes _delNclAttNode(NclFileAttRecord **theattrec, NclQuark name)
{
    NclFileAttRecord *attrec = *theattrec;
    NclFileAttNode *newnode = NULL;
    NclFileAttNode *oldnode = NULL;
    NhlErrorTypes ret = NhlNOERROR;
    int i, size;
    int idx = -1;

    for(i = 0; i < attrec->n_atts; i++)
    {
        newnode = &(attrec->att_node[i]);
        if(name == newnode->name)
        {
            idx = i;
            break;
        }
    }

    if(idx < 0)
        return (ret);
    
    attrec->n_atts--;

    for(i = idx; i < attrec->n_atts; i++)
    {
        newnode = &(attrec->att_node[i]);
        oldnode = &(attrec->att_node[i+1]);

        memcpy(newnode, oldnode, sizeof(NclFileAttNode));

        size = oldnode->n_elem * _NclSizeOf(oldnode->type);
        if(NCL_char == oldnode->type)
            size ++;
        newnode->value = (char *)NclRealloc(newnode->value, size);
        memcpy(newnode->value, oldnode->value, size);
    }

    if(NULL != oldnode)
    {
        if(NULL != oldnode->value)
            NclFree(oldnode->value);
    }
    return (ret);
}

NhlErrorTypes _addNclAttNode(NclFileAttRecord **rootattrec,
                             NclQuark name, NclBasicDataTypes type,
                             int n_elem, void *value)
{
    NclFileAttRecord *attrec;
    NclFileAttNode   *attnode;
    int i, n = 0;

    if(NULL == *rootattrec)
    {
        *rootattrec = _NclFileAttAlloc(NCL_MINIMUM_ATTS);
        (*rootattrec)->n_atts = 0;
    }
    else if((*rootattrec)->n_atts >= (*rootattrec)->max_atts)
    {
        _NclFileAttRealloc(rootattrec);
    }

    attrec = *rootattrec;

    n = attrec->n_atts;

    for(i = 0; i < n; i++)
    {
        if(name == attrec->att_node[i].name)
            return (NhlNOERROR);
    }

    attnode = &(attrec->att_node[n]);

    attnode->name = name;
    attnode->type = type;
    attnode->id   = -1;
    attnode->n_elem = n_elem;
    attnode->value = (void *)NclMalloc(n_elem * _NclSizeOf(type));
    assert(attnode->value);
    memcpy(attnode->value, value, n_elem * _NclSizeOf(type));
    attnode->the_nc_type = -1;
    attnode->is_virtual = 0;
    attnode->is_compound = 0;
    attnode->is_vlen = 0;
    attnode->is_opaque = 0;
    attnode->is_enum = 0;
    attrec->n_atts++;

    return (NhlNOERROR);
}

NhlErrorTypes _addNclDimNode(NclFileDimRecord **thedimrec, NclQuark name, int dimid,
			     ng_size_t size, int is_unlimited)
{
    NclFileDimRecord *dimrec = *thedimrec;
    int n = 0;

    if(NULL == dimrec)
    {
        dimrec = _NclFileDimAlloc(NCL_MINIMUM_DIMS);
        dimrec->n_dims = 0;
        *thedimrec = dimrec;
    }
    else if(dimrec->n_dims >= dimrec->max_dims)
    {
        _NclFileDimRealloc(dimrec);
        *thedimrec = dimrec;
    }

  /*
   *fprintf(stderr, "\nEnter _addNclDimNode, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tname: <%s>, size: %ld, is_unlimited: %d\n",
   *                  NrmQuarkToString(name), (long)size, is_unlimited);
   *fprintf(stderr, "\tstart with dimrec->n_dims = %d\n", dimrec->n_dims);
   *fprintf(stderr, "\tstart with dimrec->max_dims = %d\n", dimrec->max_dims);
   */

    for(n = 0; n < dimrec->n_dims; ++n)
    {
        if(dimrec->dim_node[n].name == name)
        {
            dimrec->dim_node[n].id   = dimid;
            dimrec->dim_node[n].size = size;
            dimrec->dim_node[n].is_unlimited = is_unlimited;
            return (NhlNOERROR);
        }
    }

    n = dimrec->n_dims;

    memset(&(dimrec->dim_node[n]), 0, sizeof(NclFileDimNode));

    dimrec->dim_node[n].name = name;
    dimrec->dim_node[n].id   = dimid;
    dimrec->dim_node[n].size = size;
    dimrec->dim_node[n].is_unlimited = is_unlimited;
    dimrec->n_dims++;

  /*
   *fprintf(stderr, "\tdim %d, name: %s, size: %ld\n", n, NrmQuarkToString(name), (long)size);
   *fprintf(stderr, "\tend with dimrec->n_dims = %d\n", dimrec->n_dims);
   *fprintf(stderr, "\tend with dimrec->max_dims = %d\n", dimrec->max_dims);
   *fprintf(stderr, "Leave _addNclDimNode, file: %s, line: %d\n\n", __FILE__, __LINE__);
   */

    return (NhlNOERROR);
}

NhlErrorTypes _addNclChunkDimNode(NclFileDimRecord **thedimrec, NclQuark name, int dimid,
			          ng_size_t size, int is_unlimited)
{
    NclFileDimRecord *dimrec = *thedimrec;
    int n = 0;
    int nfound = -1;

    if(NULL == dimrec)
    {
        dimrec = _NclFileDimAlloc(NCL_MINIMUM_DIMS);
        dimrec->n_dims = 0;
        *thedimrec = dimrec;
    }
    else if(dimrec->n_dims >= dimrec->max_dims)
    {
        _NclFileDimRealloc(dimrec);
        *thedimrec = dimrec;
    }

    for(n = 0; n < dimrec->n_dims; ++n)
    {
        if(dimrec->dim_node[n].name == name)
        {
            nfound = n;
            break;
        }
    }

    if(0 > nfound)
        return (NhlNOERROR);

    n = nfound;

  /*
   *fprintf(stderr, "\nEnter _addNclChunkDimNode, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tchunk dim %d, name: %s, size: %ld\n", n, NrmQuarkToString(name), (long)size);
   *fprintf(stderr, "\tend with dimrec->n_dims = %d\n", dimrec->n_dims);
   *fprintf(stderr, "\tend with dimrec->max_dims = %d\n", dimrec->max_dims);
   */

    memset(&(dimrec->dim_node[n]), 0, sizeof(NclFileDimNode));

    dimrec->dim_node[n].name = name;
    dimrec->dim_node[n].id   = dimid;
    dimrec->dim_node[n].size = size;
    dimrec->dim_node[n].is_unlimited = is_unlimited;

    return (NhlNOERROR);
}

NhlErrorTypes _addNclGrpNodeToGrpNode(NclFileGrpNode *rootgrpnode, NclQuark grpname)
{
    NclFileGrpNode   *grpnode;
    NclFileGrpRecord *grprec;
    int n;

    grprec = rootgrpnode->grp_rec;

    if(NULL == grprec)
    {
        rootgrpnode->grp_rec = _NclFileGrpAlloc(NCL_MINIMUM_GRPS);
        rootgrpnode->grp_rec->n_grps = 0;
        grprec = rootgrpnode->grp_rec;
    }
    else if(grprec->n_grps >= grprec->max_grps)
    {
        _NclFileGrpRealloc(rootgrpnode->grp_rec);
        grprec = rootgrpnode->grp_rec;
    }

    for(n = 0; n < grprec->n_grps; n++)
    {
        grpnode = grprec->grp_node[n];
        if(grpname == grpnode->name)
        {
            return (NhlNOERROR);
        }
    }

    n = grprec->n_grps;

    grpnode = grprec->grp_node[n];
    if(NULL == grpnode)
    {
        grpnode = (NclFileGrpNode *)NclCalloc(1, sizeof(NclFileGrpNode));
        assert(grpnode);
        grprec->grp_node[n] = grpnode;
    }

    grpnode->name = grpname;
    grpnode->att_rec = NULL;
    grpnode->dim_rec = NULL;

    grprec->n_grps++;

    return (NhlNOERROR);
}

NhlErrorTypes _addNclVarNodeToGrpNode(NclFileGrpNode *grpnode, NclQuark name,
			     int varid, NclBasicDataTypes type, int n_dims, NclQuark *dimnames,
                             ng_size_t *dimsizes)
{
    NclFileVarNode   *var_node;
    NclFileDimNode   *grp_dim_node;
    NclFileDimNode   *var_dim_node;
    NclFileDimRecord *dim_rec;
    NclFileVarRecord *var_rec;
    char buffer[NCL_MAX_NAME_LENGTH];
    int i, n = 0;

    var_rec = grpnode->var_rec;

    if(NULL == var_rec)
    {
        grpnode->var_rec = _NclFileVarAlloc(NCL_MINIMUM_VARS);
        grpnode->var_rec->n_vars = 0;
        var_rec = grpnode->var_rec;
    }
    else if(var_rec->n_vars >= var_rec->max_vars)
    {
        _NclFileVarRealloc(grpnode->var_rec);
        var_rec = grpnode->var_rec;
    }

    n = var_rec->n_vars;
    var_node = &(var_rec->var_node[n]);

    strcpy(buffer, NrmQuarkToString(grpnode->real_name));
  /*strcat(buffer, "/");*/
    strcat(buffer, NrmQuarkToString(name));
    var_node->name = name;
    var_node->real_name = NrmStringToQuark(buffer);
    var_node->id = varid;
    var_node->type = type;
    var_node->comprec = NULL;
    var_node->att_rec = NULL;
    var_node->dim_rec = _NclFileDimAlloc(n_dims);
    assert(var_node->dim_rec);

    dim_rec = grpnode->dim_rec;
    for(n = 0; n < n_dims; n++)
    {
            var_dim_node = &(var_node->dim_rec->dim_node[n]);
	    if (dim_rec != NULL) {
		    for(i = 0; i < dim_rec->n_dims; i++)
		    {
			    grp_dim_node = &(dim_rec->dim_node[i]);
			    if(grp_dim_node->name == dimnames[n])
			    {
				    memcpy(var_dim_node, grp_dim_node, sizeof(NclFileDimNode));
				    break;
			    }
		    }
	    }
            var_dim_node->name = dimnames[n];
            var_dim_node->size = dimsizes[n];
    }

    if(grpnode->is_chunked)
    {
        var_node->chunk_dim_rec = _NclFileDimAlloc(n_dims);
        assert(var_node->chunk_dim_rec);

        var_node->is_chunked = grpnode->is_chunked;

        dim_rec = grpnode->chunk_dim_rec;
        if(NULL != dim_rec)
        {
        for(n = 0; n < n_dims; n++)
        {
            var_dim_node = &(var_node->chunk_dim_rec->dim_node[n]);
            for(i = 0; i < dim_rec->n_dims; i++)
            {
                grp_dim_node = &(dim_rec->dim_node[i]);
                if(grp_dim_node->name == dimnames[n])
                {
                     memcpy(var_dim_node, grp_dim_node, sizeof(NclFileDimNode));

                     var_dim_node->name = dimnames[n];
                     var_dim_node->size = grp_dim_node->size;
                   /*
                    *var_dim_node->size = dimsizes[n];
                    */
                     break;
                }
            }
        }
        }
    }

    var_rec->n_vars++;

    return (NhlNOERROR);
}

NhlErrorTypes _addNclCoordVarNode(NclFileCoordVarRecord **root_coord_var_rec,
				  NclFileVarNode *var_node)
{
    NclFileCoordVarRecord *coord_var_rec = *root_coord_var_rec;
    int i, n, found = 0;
    NclFileVarNode *coord_var_node;

    if(NULL == coord_var_rec)
    {
        coord_var_rec = _NclFileCoordVarAlloc(NCL_MINIMUM_VARS);
        coord_var_rec->n_vars = 0;
        *root_coord_var_rec = coord_var_rec;
    }
    else if(coord_var_rec->n_vars >= coord_var_rec->max_vars)
    {
        coord_var_rec = _NclFileCoordVarRealloc(coord_var_rec);
        *root_coord_var_rec = coord_var_rec;
    }

    n = coord_var_rec->n_vars;

    for(i = 0; i < n; i++)
    {
        coord_var_node = coord_var_rec->var_node[i];
        if(coord_var_node->name == var_node->name)
        {
            found = 1;
            return (NhlNOERROR);
            /*break;*/
        }
    }
  
    if(!found)
    {
        coord_var_rec->var_node[n] = var_node;
        coord_var_rec->n_vars++;
    }

    return (NhlNOERROR);
}

static void _NclInitNclFileGrpRecord(NclFileGrpRecord *grp_rec, int start)
{
    NclFileGrpNode *grpnode;
    int i;

    if(NULL != grp_rec)
    {
        for(i = start; i < grp_rec->max_grps; i++)
        {
            grpnode = (NclFileGrpNode *)NclCalloc(1, sizeof(NclFileGrpNode));
            assert(grpnode);

            grpnode->pname = -1;
            grpnode->name  = -1;
            grpnode->pid   = -1;
            grpnode->fid   = -1;
            grpnode->gid   = -1;

            grpnode->unlimit_dim_rec = NULL;
            grpnode->chunk_dim_rec   = NULL;
            grpnode->coord_var_rec   = NULL;
            grpnode->options         = NULL;
            grpnode->dim_rec         = NULL;
            grpnode->att_rec         = NULL;
            grpnode->var_rec         = NULL;
            grpnode->grp_rec         = NULL;
            grpnode->udt_rec         = NULL;
            grpnode->parent          = NULL;

            grp_rec->grp_node[i] = grpnode;
        }
    }
}

static void _NclInitNclFileVarRecord(NclFileVarRecord *var_rec, int start)
{
    NclFileVarNode *varnode;
    int i;

    if(NULL != var_rec)
    {
        for(i = start; i < var_rec->max_vars; i++)
        {
            varnode = &(var_rec->var_node[i]);
            memset(varnode, 0, sizeof(NclFileVarNode));
            varnode->name = -1;
            varnode->real_name = -1;
            varnode->type = NCL_none;
            varnode->comprec = NULL;
            varnode->att_rec = NULL;
            varnode->dim_rec = NULL;
            varnode->chunk_dim_rec = NULL;
            varnode->value = NULL;
        }
    }
}

NclFileGrpRecord *_NclFileGrpAlloc(int n_grps)
{
    NclFileGrpRecord *grp_rec = NULL;

    if(n_grps < 1)
    {
        return grp_rec;
    }
   
    grp_rec = (NclFileGrpRecord *)NclCalloc(1, sizeof(NclFileGrpRecord));
    assert(grp_rec);

    grp_rec->max_grps = n_grps;

    grp_rec->grp_node = (NclFileGrpNode **)NclCalloc(grp_rec->max_grps, sizeof(NclFileGrpNode *));
    assert(grp_rec->grp_node);

    _NclInitNclFileGrpRecord(grp_rec, 0);
    grp_rec->n_grps = n_grps;

    return grp_rec;
}

NclFileVarRecord *_NclFileVarAlloc(int n_vars)
{
    NclFileVarRecord *var_rec = NULL;

    if(n_vars < 1)
    {
        return var_rec;
    }
   
    var_rec = (NclFileVarRecord *)NclCalloc(1, sizeof(NclFileVarRecord));
    assert(var_rec);

    var_rec->n_vars = n_vars;
    var_rec->max_vars = n_vars;

    var_rec->var_node = (NclFileVarNode *)NclCalloc(var_rec->max_vars, sizeof(NclFileVarNode));
    assert(var_rec->var_node);

    _NclInitNclFileVarRecord(var_rec, 0);
    return var_rec;
}

NclFileUDTRecord *_NclFileUDTAlloc(int n_udts)
{
    NclFileUDTRecord *udt_rec = NULL;

    if(n_udts < 1)
    {
        return udt_rec;
    }
   
    udt_rec = (NclFileUDTRecord *)NclCalloc(1, sizeof(NclFileUDTRecord));
    assert(udt_rec);

    udt_rec->n_udts = n_udts;
    udt_rec->max_udts = n_udts;

    udt_rec->udt_node = (NclFileUDTNode *)NclCalloc(udt_rec->max_udts, sizeof(NclFileUDTNode));
    assert(udt_rec->udt_node);

    return udt_rec;
}

void _NclFileGrpRealloc(NclFileGrpRecord *grp_rec)
{
   /*
    *fprintf(stderr, "\nEnter _NclFileGrpRealloc, file: %s, line: %d\n", __FILE__, __LINE__);
    *fprintf(stderr, "\tstart with grp_rec->n_grps = %d\n", grp_rec->n_grps);
    *fprintf(stderr, "\tstart with grp_rec->max_grps = %d\n", grp_rec->max_grps);
    */

    if(grp_rec->n_grps >= grp_rec->max_grps)
    {
        grp_rec->max_grps *= 2;

        grp_rec->grp_node = (NclFileGrpNode **)NclRealloc(grp_rec->grp_node,
                    grp_rec->max_grps * sizeof(NclFileGrpNode *));
        assert(grp_rec->grp_node);

        _NclInitNclFileGrpRecord(grp_rec, grp_rec->n_grps);
    }

   /*
    *fprintf(stderr, "\tend with grp_rec->n_grps = %d\n", grp_rec->n_grps);
    *fprintf(stderr, "\tend with grp_rec->max_grps = %d\n", grp_rec->max_grps);
    *fprintf(stderr, "Leave _NclFileGrpRealloc, file: %s, line: %d\n\n", __FILE__, __LINE__);
    */
}

void _NclFileVarRealloc(NclFileVarRecord *var_rec)
{
   /*
    *fprintf(stderr, "\nEnter _NclFileVarRealloc, file: %s, line: %d\n", __FILE__, __LINE__);
    *fprintf(stderr, "\tstart with var_rec->n_vars = %d\n", var_rec->n_vars);
    *fprintf(stderr, "\tstart with var_rec->max_vars = %d\n", var_rec->max_vars);
    */

    if(var_rec->n_vars >= var_rec->max_vars)
    {
        NclFileVarNode *varnodeptr;

        var_rec->max_vars *= 2;

        varnodeptr = (NclFileVarNode *)NclRealloc(var_rec->var_node,
                    var_rec->max_vars * sizeof(NclFileVarNode));
        assert(var_rec->var_node);
        var_rec->var_node = varnodeptr;

        _NclInitNclFileVarRecord(var_rec, var_rec->n_vars);
    }

    /*
    *fprintf(stderr, "\tend with var_rec->n_vars = %d\n", var_rec->n_vars);
    *fprintf(stderr, "\tend with var_rec->max_vars = %d\n", var_rec->max_vars);
    *fprintf(stderr, "Leave _NclFileVarRealloc, file: %s, line: %d\n\n", __FILE__, __LINE__);
    */
}

void _NclFileUDTRealloc(NclFileUDTRecord *udt_rec)
{
    /*
    *fprintf(stderr, "\nEnter _NclFileUDTRealloc, file: %s, line: %d\n", __FILE__, __LINE__);
    *fprintf(stderr, "\tstart with udt_rec->n_udts = %d\n", udt_rec->n_udts);
    *fprintf(stderr, "\tstart with udt_rec->max_udts = %d\n", udt_rec->max_udts);
    */

    if(udt_rec->n_udts >= udt_rec->max_udts)
    {
        NclFileUDTNode *udtnodeptr;

        if(0 >= udt_rec->max_udts)
            udt_rec->max_udts = 1;

	udt_rec->max_udts = udt_rec->n_udts + 1;

        udtnodeptr = (NclFileUDTNode *)NclRealloc(udt_rec->udt_node,
                    udt_rec->max_udts * sizeof(NclFileUDTNode));
        assert(udt_rec->udt_node);
        udt_rec->udt_node = udtnodeptr;
    }

    /*
    *fprintf(stderr, "\tend with udt_rec->n_udts = %d\n", udt_rec->n_udts);
    *fprintf(stderr, "\tend with udt_rec->max_udts = %d\n", udt_rec->max_udts);
    *fprintf(stderr, "Leave _NclFileUDTRealloc, file: %s, line: %d\n\n", __FILE__, __LINE__);
    */
}

NclFileCoordVarRecord *_NclFileCoordVarRealloc(NclFileCoordVarRecord *coord_var_rec)
{
    NclFileCoordVarRecord *new_coord_var_rec;
    int n;

    if(coord_var_rec->n_vars < coord_var_rec->max_vars)
    {
        return coord_var_rec;
    }

    coord_var_rec->max_vars *= 2;

    new_coord_var_rec = _NclFileCoordVarAlloc(coord_var_rec->max_vars);
    new_coord_var_rec->n_vars = coord_var_rec->n_vars;

    for(n = 0; n < new_coord_var_rec->n_vars; n++)
        new_coord_var_rec->var_node[n] = coord_var_rec->var_node[n];

    for(n = new_coord_var_rec->n_vars; n < coord_var_rec->max_vars; n++)
        new_coord_var_rec->var_node[n] = NULL;
    
    free(coord_var_rec->var_node);
    free(coord_var_rec);

    return new_coord_var_rec;
}

NclFileCompoundRecord *_NclFileCompoundAlloc(int n_comps)
{
    NclFileCompoundRecord *comp_rec = NULL;

    if(n_comps < 1)
    {
        return comp_rec;
    }
   
    comp_rec = (NclFileCompoundRecord *)NclCalloc(1, sizeof(NclFileCompoundRecord));
    assert(comp_rec);

    comp_rec->max_comps = n_comps;
    comp_rec->n_comps = n_comps;
    comp_rec->size = 0;
    comp_rec->type = -1;
    comp_rec->xtype = -1;
    comp_rec->base_nc_type = -1;

    comp_rec->compnode = (NclFileCompoundNode *)NclCalloc(comp_rec->max_comps,
                            sizeof(NclFileCompoundNode));
    assert(comp_rec->compnode);

    return comp_rec;
}

NclFileCoordVarRecord *_NclFileCoordVarAlloc(int n_vars)
{
    NclFileCoordVarRecord *coord_var_rec = NULL;

    if(n_vars < 1)
    {
        return coord_var_rec;
    }
   
    coord_var_rec = (NclFileCoordVarRecord *) NclCalloc(1, sizeof(NclFileCoordVarRecord));
    assert(coord_var_rec);

    coord_var_rec->n_vars = n_vars;
    coord_var_rec->max_vars = n_vars;

    coord_var_rec->var_node = (NclFileVarNode **)NclCalloc(coord_var_rec->max_vars, sizeof(NclFileVarNode *));
    assert(coord_var_rec->var_node);

    return coord_var_rec;
}

NclFileAttRecord *_NclFileAttAlloc(int n_atts)
{
    NclFileAttRecord *att_rec = NULL;

    if(n_atts < 1)
    {
        return att_rec;
    }
   
    att_rec = (NclFileAttRecord *) NclCalloc(1, sizeof(NclFileAttRecord));
    assert(att_rec);

    att_rec->n_atts = n_atts;
    att_rec->max_atts = n_atts;

    att_rec->att_node = (NclFileAttNode *)NclCalloc(att_rec->max_atts, sizeof(NclFileAttNode));
    assert(att_rec->att_node);

    att_rec->id = -1;
    att_rec->cb = NULL;
    att_rec->udata = NULL;

    return att_rec;
}

void _NclFileAttRealloc(NclFileAttRecord **att_rec)
{
    int n;
    NclFileAttNode *attnode;
    /*
    *fprintf(stderr, "\nEnter _NclFileAttRealloc, file: %s, line: %d\n", __FILE__, __LINE__);
    *fprintf(stderr, "\tstart with att_rec->n_atts = %d\n", att_rec->n_atts);
    *fprintf(stderr, "\tstart with att_rec->max_atts = %d\n", att_rec->max_atts);
    */

    if((*att_rec)->n_atts >= (*att_rec)->max_atts)
    {
        (*att_rec)->max_atts *= 2;

        (*att_rec)->att_node = (NclFileAttNode *)NclRealloc((*att_rec)->att_node,
                    (*att_rec)->max_atts * sizeof(NclFileAttNode));
        assert((*att_rec)->att_node);

        for(n = (*att_rec)->n_atts; n < (*att_rec)->max_atts; ++n)
        {
            attnode = &((*att_rec)->att_node[n]);
            memset(attnode, 0, sizeof(NclFileAttNode));
        }
    }

    /*
    *fprintf(stderr, "\tend with att_rec->n_atts = %d\n", (*att_rec)->n_atts);
    *fprintf(stderr, "\tend with att_rec->max_atts = %d\n", (*att_rec)->max_atts);
    *fprintf(stderr, "Leave _NclFileAttRealloc, file: %s, line: %d\n\n", __FILE__, __LINE__);
    */
}

NclFileDimRecord *_NclFileDimAlloc(int n_dims)
{
    NclFileDimRecord *dim_rec = NULL;

    if(n_dims < 1)
    {
        return dim_rec;
    }
   
    dim_rec = (NclFileDimRecord *) NclCalloc(1, sizeof(NclFileDimRecord));
    assert(dim_rec);

    dim_rec->n_dims = n_dims;
    dim_rec->max_dims = n_dims;

    dim_rec->dim_node = (NclFileDimNode *) NclCalloc(dim_rec->max_dims, sizeof(NclFileDimNode));
    assert(dim_rec->dim_node);

    return dim_rec;
}

void _NclFileDimRealloc(NclFileDimRecord *dim_rec)
{
    /*
    *fprintf(stderr, "\nEnter _NclFileDimRealloc, file: %s, line: %d\n", __FILE__, __LINE__);
    *fprintf(stderr, "\tstart with dim_rec->n_dims = %d\n", dim_rec->n_dims);
    *fprintf(stderr, "\tstart with dim_rec->max_dims = %d\n", dim_rec->max_dims);
    */

    if(dim_rec->n_dims >= dim_rec->max_dims)
    {
        NclFileDimNode *dimnodeptr;

        dim_rec->max_dims *= 2;

        dimnodeptr = (NclFileDimNode *)NclRealloc(dim_rec->dim_node,
                    dim_rec->max_dims * sizeof(NclFileDimNode));
        assert(dimnodeptr);
        dim_rec->dim_node = dimnodeptr;
    }

    /*
    *fprintf(stderr, "\tend with dim_rec->n_dims = %d\n", dim_rec->n_dims);
    *fprintf(stderr, "\tend with dim_rec->max_dims = %d\n", dim_rec->max_dims);
    *fprintf(stderr, "Leave _NclFileDimRealloc, file: %s, line: %d\n\n", __FILE__, __LINE__);
    */
}

NclFileAttNode *_getAttNodeFromNclFileGrpNode(NclFileGrpNode *grp_node,
                                                NclQuark att_name)
{
    NclFileAttNode *att_node = NULL;
    int n = 0;
    if(NULL == grp_node->att_rec)
            return NULL;

    for(n = 0; n < grp_node->att_rec->n_atts; n++)
    {
        att_node = &(grp_node->att_rec->att_node[n]);
        if(att_name == att_node->name)
            return att_node;
    }

    return NULL;
}

NclFileGrpNode *_getGrpNodeFromNclFileGrpNode(NclFileGrpNode *ingrpnode,
                        NclQuark grpname)
{
    int n;
    NclFileGrpNode *outgrpnode = NULL;
    NclQuark new_grpname = -1;
    NclQuark newroot_grpname = -1;

    char *full_str;
    char buffer[NCL_MAX_STRING];
    char newroot[NCL_MAX_STRING];
    int  gl;
    int  only_this_level = 1;

    if(NULL == ingrpnode)
    {
        NHLPERROR((NhlWARNING,NhlEUNKNOWN, "_getGrpNodeFromNclFileGrpNode: input grpnode is NULL.\n"));
        goto done_getGrpNodeFromNclFileGrpNode;
    }

  /*
   *fprintf(stderr, "\nEnter _getGrpNodeFromNclFileGrpNode, file: %s, line:%d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tingrpnode->name: <%s>\n", NrmQuarkToString(ingrpnode->name));
   *fprintf(stderr, "\tgrpname: <%s>\n", NrmQuarkToString(grpname));
   */

    if((grpname == ingrpnode->name) || (grpname == ingrpnode->name_an) || (grpname == ingrpnode->real_name))
    {
       outgrpnode =  ingrpnode;
       goto done_getGrpNodeFromNclFileGrpNode;
    }

    full_str = NrmQuarkToString(grpname);

  /*
   *fprintf(stderr, "\tfile: %s, line:%d\n", __FILE__, __LINE__);
   */

    if('/' == full_str[0])
    {
        strcpy(buffer, full_str + 1);
    }
    else
    {
        strcpy(buffer, full_str);
    }

    strcpy(newroot, buffer);
    full_str = strchr(buffer, '/');

    if(NULL != full_str)
    {
      /*
       *fprintf(stderr, "\tfile: %s, line:%d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\tnewroot: <%s>, full_str: <%s>\n", newroot, full_str);
       */

        gl = strlen(newroot) - strlen(full_str);
        newroot[gl] = '\0';
        full_str = full_str + 1;
        only_this_level = 0;

      /*
       *fprintf(stderr, "\tfile: %s, line:%d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\tnewroot: <%s>, full_str: <%s>\n", newroot, full_str);
       */

        new_grpname = NrmStringToQuark(full_str);

      /*
       *fprintf(stderr, "\tfile: %s, line:%d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\tnew_grpname: <%s>\n", NrmQuarkToString(new_grpname));
       */
    }

    newroot_grpname = NrmStringToQuark(newroot);

  /*
   *fprintf(stderr, "\tfile: %s, line:%d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tnewroot_grpname: <%s>\n", NrmQuarkToString(newroot_grpname));
   */

    if(NULL != ingrpnode->grp_rec)
    {
        if(only_this_level)
        {
            full_str = NrmQuarkToString(grpname);

            if('/' == full_str[0])
            {
              /*
               *fprintf(stderr, "\tfile: %s, line:%d\n", __FILE__, __LINE__);
               *fprintf(stderr, "\tfull_str: <%s>, grpname: <%s>\n", full_str, NrmQuarkToString(grpname));
               */

                strcpy(buffer, full_str + 1);
                
                new_grpname = NrmStringToQuark(buffer);
            }
            else
            {
                new_grpname = grpname;
            }

          /*
           *fprintf(stderr, "\tfile: %s, line:%d\n", __FILE__, __LINE__);
           *fprintf(stderr, "\tfull_str: <%s>, new_grpname: <%s>\n", full_str, NrmQuarkToString(new_grpname));
           */

            for(n = 0; n < ingrpnode->grp_rec->n_grps; n++)
            {
                outgrpnode = ingrpnode->grp_rec->grp_node[n];
              /*
               *fprintf(stderr, "\tfile: %s, line:%d\n", __FILE__, __LINE__);
               *fprintf(stderr, "\tCheck group %d, name: <%s>, new_grpname: <%s>\n", n, 
               *                 NrmQuarkToString(outgrpnode->name),
               *                 NrmQuarkToString(new_grpname));
               */
                if(new_grpname == outgrpnode->name || new_grpname == outgrpnode->name_an)
		{
                  /*
                   *fprintf(stderr, "\tfile: %s, line:%d\n", __FILE__, __LINE__);
                   *fprintf(stderr, "\tFound group %d, name: <%s>\n", n, 
                   *             NrmQuarkToString(outgrpnode->name));
                   */
                    goto done_getGrpNodeFromNclFileGrpNode;
		}
            }
        }
        else
        {
            for(n = 0; n < ingrpnode->grp_rec->n_grps; n++)
            {
                outgrpnode = ingrpnode->grp_rec->grp_node[n];
              /*
               *fprintf(stderr, "\tfile: %s, line:%d\n", __FILE__, __LINE__);
               *fprintf(stderr, "\tCheck group %d, name: <%s>, rootgrpname: <%s>\n", n, 
               *                 NrmQuarkToString(outgrpnode->name),
               *                 NrmQuarkToString(newroot_grpname));
               */
                if(newroot_grpname == outgrpnode->name || newroot_grpname == outgrpnode->name_an)
                {
                    outgrpnode = _getGrpNodeFromNclFileGrpNode(ingrpnode->grp_rec->grp_node[n], new_grpname);
                    if(NULL != outgrpnode)
                        goto done_getGrpNodeFromNclFileGrpNode;
                }
            }
        }
    }

    outgrpnode = NULL;

done_getGrpNodeFromNclFileGrpNode:
  /*
   *fprintf(stderr, "Leave _getGrpNodeFromNclFileGrpNode, file: %s, line:%d\n\n", __FILE__, __LINE__);
   */

    return outgrpnode;
}

NclFileVarNode *_getVarNodeFromNclFileVarRecord(NclFileVarRecord *var_rec,
                        NclQuark var_name)
{
    int n;
    NclFileVarNode *var_node;

    if(NULL != var_rec)
    {
        for(n = 0; n < var_rec->n_vars; n++)
        {
            var_node = &(var_rec->var_node[n]);
            if((var_name == var_node->name) || (var_name == var_node->real_name))
                return var_node;
        }
    }

    return NULL;
}

NclFileVarNode *_getVarNodeFromThisGrpNode(NclFileGrpNode *grpnode,
                        NclQuark varname)
{
    int n;
    NclFileVarNode *varnode = NULL;

    if(NULL != grpnode->var_rec)
    {
        for(n = 0; n < grpnode->var_rec->n_vars; n++)
        {
            varnode = &(grpnode->var_rec->var_node[n]);

            if((varname == varnode->name) || (varname == varnode->real_name))
            {
                return varnode;
            }
        }
    }

    return NULL;
}

NclFileVarNode *_getVarNodeFromNclFileGrpNode_asVar(NclFileGrpNode *grpnode,
                                                    NclQuark varname)
{
    int n;
    NclFileVarNode *varnode = NULL;
    NclFileGrpNode *tmpgrpnode = NULL;

    if(NULL != grpnode->var_rec)
    {
        for(n = 0; n < grpnode->var_rec->n_vars; n++)
        {
            varnode = &(grpnode->var_rec->var_node[n]);

            if(NULL == varnode)
                continue;

            if((varname == varnode->name) || (varname == varnode->short_name) || (varname == varnode->real_name))
                return varnode;
        }
    }

    if(NULL != grpnode->grp_rec)
    {
        for(n = 0; n < grpnode->grp_rec->n_grps; n++)
        {
            tmpgrpnode = grpnode->grp_rec->grp_node[n];
            varnode = _getVarNodeFromNclFileGrpNode_asVar(tmpgrpnode, varname);

            if(NULL == varnode)
                continue;

            if((varname == varnode->name) || (varname == varnode->short_name) || (varname == varnode->real_name))
                return varnode;
        }
    }

    return NULL;
}

NclFileVarNode *_getVarNodeFromNclFileGrpNode_asCompound(NclFileGrpNode *grpnode,
                                                         NclQuark varname)
{
    int n;
    NclFileGrpNode *tmpgrpnode;
    NclFileVarNode *varnode = NULL;
    NclQuark vn = varname;
    char *struct_name = NULL;
    char *component_name = NULL;

    component_name = _getComponentName(NrmQuarkToString(varname), &struct_name);
    if(NULL != component_name)
    {
        vn = NrmStringToQuark(struct_name);
        NclFree(component_name);
    }

    if(NULL != struct_name)
        NclFree(struct_name);

    if(NULL != grpnode->var_rec)
    {
        for(n = 0; n < grpnode->var_rec->n_vars; n++)
        {
            varnode = &(grpnode->var_rec->var_node[n]);

            if(NULL == varnode)
                continue;

            if((vn == varnode->name) || (vn == varnode->real_name))
                return varnode;
        }
    }

    if(NULL != grpnode->grp_rec)
    {
        for(n = 0; n < grpnode->grp_rec->n_grps; n++)
        {
            tmpgrpnode = grpnode->grp_rec->grp_node[n];
            varnode = _getVarNodeFromNclFileGrpNode_asVar(tmpgrpnode, vn);

            if(NULL == varnode)
                continue;

            if((vn == varnode->name) || (vn == varnode->real_name))
                return varnode;
        }
    }

    return NULL;
}

NclFileVarNode *_getVarNodeFromNclFileGrpNode(NclFileGrpNode *grpnode,
                                              NclQuark varname)
{
    NclFileVarNode *varnode;
    char *cp, *last_cp;
    int is_fully_qualified;
    NclFileGrpNode *lgrpnode = grpnode;
    char *vname = NrmQuarkToString(varname);
    char *buf = alloca(strlen(vname) + 1);
    NrmQuark lvarname = varname;
    
    buf[0] = '\0';
    strcat(buf,vname);
    last_cp = buf;
    is_fully_qualified =  (last_cp[0] == '/') ? 1 : 0;
    if (is_fully_qualified) last_cp++;
    while ((cp = strchr(last_cp, '/')) != NULL) {
	    *cp = '\0';
	    lgrpnode = _getGrpNodeFromNclFileGrpNode(lgrpnode,NrmStringToQuark(last_cp));
	    last_cp = cp + 1;
    }
    lvarname = NrmStringToQuark(last_cp);

    
    varnode = _getVarNodeFromNclFileGrpNode_asVar(lgrpnode, lvarname);
    if(NULL != varnode)
        return varnode;

    varnode = _getVarNodeFromNclFileGrpNode_asCompound(lgrpnode, lvarname);
    return varnode;
}

NclFileVarNode *_getCoordVarNodeFromNclFileGrpNode(NclFileGrpNode *grpnode,
                        NclQuark varname)
{
    int n;
    NclFileVarNode *varnode = NULL;
    NclQuark vn = varname;

  /*
   *fprintf(stderr, "\nEnter _getCoordVarNodeFromNclFileGrpNode, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tgrpname: <%s>\n", NrmQuarkToString(grpnode->name));
   *fprintf(stderr, "\tvarname: <%s>\n", NrmQuarkToString(varname));
   */

    if(NULL != grpnode->coord_var_rec)
    {
        for(n = 0; n < grpnode->coord_var_rec->n_vars; n++)
        {
            varnode = grpnode->coord_var_rec->var_node[n];

            if(NULL == varnode)
                continue;

            if((vn == varnode->name) || (vn == varnode->real_name))
                return varnode;
        }
    }
    else if(NULL != grpnode->var_rec)
    {
        for(n = 0; n < grpnode->var_rec->n_vars; n++)
        {
            varnode = &grpnode->var_rec->var_node[n];

            if(NULL == varnode)
                continue;

            if((vn == varnode->name) || (vn == varnode->real_name))
                return varnode;
        }
    }

#if 0
  /*Do we want to search all the groups below?*/
    {
        char *varstr;
        varstr = NrmQuarkToString(vn);
        if(NULL == strchr(varstr, "/"))
        {
            return NULL;
        }
    }
#endif

    if(NULL != grpnode->grp_rec)
    {
        for(n = 0; n < grpnode->grp_rec->n_grps; n++)
        {
            varnode = _getCoordVarNodeFromNclFileGrpNode(grpnode->grp_rec->grp_node[n], vn);

            if(NULL == varnode)
                continue;

            if((vn == varnode->name) || (vn == varnode->real_name))
                return varnode;
        }
    }

    return NULL;
}

NclFileDimNode *_getDimNodeFromNclFileGrpNode(NclFileGrpNode *grpnode,
                        NclQuark dim_name)
{
    int n;
    NclFileDimNode *dim_node;

    if(NULL != grpnode->dim_rec)
    {
        for(n = 0; n < grpnode->dim_rec->n_dims; n++)
        {
            dim_node = &(grpnode->dim_rec->dim_node[n]);
            if(dim_name == dim_node->name)
                return dim_node;
        }
    }

    if(NULL != grpnode->grp_rec)
    {
        for(n = 0; n < grpnode->grp_rec->n_grps; n++)
        {
            dim_node = _getDimNodeFromNclFileGrpNode(grpnode->grp_rec->grp_node[n], dim_name);
            if(NULL != dim_node)
                return dim_node;
        }
    }

    return NULL;
}

NclFileDimNode *_getDimNodeFromNclFileGrpNodeWithID(NclFileGrpNode *grpnode, int dimid)
{
    int n;
    NclFileDimNode *dim_node = NULL;

    if(NULL != grpnode->dim_rec)
    {
        for(n = 0; n < grpnode->dim_rec->n_dims; n++)
        {
            dim_node = &(grpnode->dim_rec->dim_node[n]);
            if(dimid == dim_node->id)
                return dim_node;
        }
    }

    if(NULL != grpnode->grp_rec)
    {
        for(n = 0; n < grpnode->grp_rec->n_grps; n++)
        {
            dim_node = _getDimNodeFromNclFileGrpNodeWithID(grpnode->grp_rec->grp_node[n], dimid);
            if(NULL != dim_node)
                return dim_node;
        }
    }

    return NULL;
}

NclFileDimNode *_getChunkDimNodeFromNclFileGrpNode(NclFileGrpNode *grpnode,
                        NclQuark dim_name)
{
    int n;
    NclFileDimNode *dim_node;

    if(NULL != grpnode->chunk_dim_rec)
    {
        for(n = 0; n < grpnode->chunk_dim_rec->n_dims; n++)
        {
            dim_node = &(grpnode->chunk_dim_rec->dim_node[n]);
            if(dim_name == dim_node->name)
                return dim_node;
        }
    }

    if(NULL != grpnode->grp_rec)
    {
        for(n = 0; n < grpnode->grp_rec->n_grps; n++)
        {
            dim_node = _getChunkDimNodeFromNclFileGrpNode(grpnode->grp_rec->grp_node[n], dim_name);
            if(NULL != dim_node)
                return dim_node;
        }
    }

    return NULL;
}

NclFileAttNode *_getAttNodeFromNclFileVarNode(NclFileVarNode *varnode,
                        NclQuark attname)
{
    int n;
    NclFileAttNode *attnode;

    if(NULL != varnode->att_rec)
    {
        for(n = 0; n < varnode->att_rec->n_atts; n++)
        {
            attnode = &(varnode->att_rec->att_node[n]);
            if(attname == attnode->name)
                return attnode;
        }
    }

    return NULL;
}

void FileDestroyAttRecord(NclFileAttRecord *att_rec)
{
    int n;
    NclFileAttNode *attnode;

    if(NULL != att_rec)
    {
        if(NULL != att_rec->att_node)
        {
          /*
           *fprintf(stderr, "file: %s, line: %d\n", __FILE__, __LINE__);
           *fprintf(stderr, "n_atts: %d, max_atts: %d\n", att_rec->n_atts, att_rec->max_atts);
           */
            for(n = 0; n < att_rec->max_atts; n++)
            {
                attnode = &(att_rec->att_node[n]);
                if(NULL != attnode->value)
                {
			if (attnode->is_opaque) {
				NclFileOpaqueRecord *opaquerec = (NclFileOpaqueRecord *) attnode->value;
				NclFree(opaquerec->values);
			}
			NclFree(attnode->value);
			attnode->value = NULL;
                }
            }
            NclFree(att_rec->att_node);
            att_rec->att_node = NULL;
        }
        if(NULL != att_rec->cb)
        {
           /*
            *_NhlCB tmpcb = att_rec->cb;
            *while(NULL != tmpcb)
            *{
            *    att_rec->cb = tmpcb->next;
            *    tmpcb->next = NULL;
            *    _NhlCBDestroy(tmpcb->cblist);
            *    NclFree(tmpcb);
            *    tmpcb = att_rec->cb;
            *}            */

	    if(NULL != att_rec->udata)
		NclFree(att_rec->udata);
	    att_rec->udata = NULL;
            NclFree(att_rec->cb);
            att_rec->cb = NULL;
        }
        NclFree(att_rec);
        att_rec = NULL;
    }
}

void FileDestroyDimRecord(NclFileDimRecord *dim_rec)
{
	if(NULL != dim_rec)
	{
		if(dim_rec->max_dims)
		{
			if(NULL != dim_rec->dim_node)
			{
				NclFree(dim_rec->dim_node);
				dim_rec->dim_node = NULL;
			}

			NclFree(dim_rec);
			dim_rec = NULL;
		}
	}
}

void FileDestroyCompoundRecord(NclFileCompoundRecord *comprec)
{
    int n;
    NclFileCompoundNode   *compnode;

    if(NULL != comprec)
    {
        if(NULL != comprec->compnode)
        {
            for(n = 0; n < comprec->max_comps; ++n)
            {
                compnode = &(comprec->compnode[n]);
                if(NULL != compnode->dimsizes)
                {
                    NclFree(compnode->dimsizes);
                    compnode->dimsizes = NULL;
                }
            }
            NclFree(comprec->compnode);
            comprec->compnode = NULL;
        }
        NclFree(comprec);
        comprec = NULL;
    }
}

void FileDestroyCoordVarRecord(NclFileCoordVarRecord *coord_rec)
{
    if(NULL != coord_rec)
    {
        if(NULL != coord_rec->var_node)
        {
            NclFree(coord_rec->var_node);
            coord_rec->var_node = NULL;
        }
        NclFree(coord_rec);
        coord_rec = NULL;
    }
}

void FileDestroyUDTRecord(NclFileUDTRecord *udt_rec)
{
    int n;
    NclFileUDTNode *udt_node;

    if(NULL != udt_rec)
    {
        if(NULL != udt_rec->udt_node)
        {
            for(n = 0; n < udt_rec->max_udts; ++n)
            {
		    NclFileUDTField *field;
		    int i;

		    udt_node = &(udt_rec->udt_node[n]);
		    if (udt_node->n_fields > 0) {
			    for (i = 0; i < udt_node->n_fields; i++) {
				    field = &(udt_node->fields[i]);
				    if (field->n_dims > 0) {
					    NclFree(field->dim_sizes);
				    }
			    }
			    NclFree(udt_node->fields);
		    }
            }
            NclFree(udt_rec->udt_node);
            udt_rec->udt_node = NULL;
        }
        NclFree(udt_rec);
        udt_rec = NULL;
    }
}

void FileDestroyVarRecord(NclFileVarRecord *var_rec)
{
    int n;
    NclFileVarNode *varnode;

    if(NULL != var_rec)
    {
        if(NULL != var_rec->var_node)
        {
          /*
           *fprintf(stderr, "file: %s, line: %d\n", __FILE__, __LINE__);
           *fprintf(stderr, "n_vars: %d, max_vars: %d\n", var_rec->n_vars, var_rec->max_vars);
           */
            for(n = 0; n < var_rec->max_vars; n++)
            {
                varnode = &(var_rec->var_node[n]);

                if(NULL != varnode->value)
                {
                    NclFree(varnode->value);
                    varnode->value = NULL;
                }

                FileDestroyDimRecord(varnode->dim_rec);
                FileDestroyAttRecord(varnode->att_rec);
                FileDestroyDimRecord(varnode->chunk_dim_rec);
                FileDestroyCompoundRecord(varnode->comprec);
            }
            NclFree(var_rec->var_node);
            var_rec->var_node = NULL;
        }
        NclFree(var_rec);
        var_rec = NULL;
    }
}

void FileDestroyGrpNode(NclFileGrpNode *grpnode)
{
    int n;

    if(NULL != grpnode)
    {
        if(NULL != grpnode->grp_rec)
        {
          /*
           *fprintf(stderr, "file: %s, line: %d\n", __FILE__, __LINE__);
           *fprintf(stderr, "n_grps: %d, max_grps: %d\n", grpnode->grp_rec->n_grps, grpnode->grp_rec->max_grps);
           */
          /*for(n = 0; n < grpnode->grp_rec->n_grps; n++)*/
            for(n = 0; n < grpnode->grp_rec->max_grps; n++)
                FileDestroyGrpNode(grpnode->grp_rec->grp_node[n]);
    
            NclFree(grpnode->grp_rec->grp_node);
            NclFree(grpnode->grp_rec);
            grpnode->grp_rec = NULL;
        }

        if(NULL != grpnode->options)
	{
            for(n = 0; n < grpnode->n_options; ++n)
                if(NULL != grpnode->options[n].values)
                    NclFree(grpnode->options[n].values);
            NclFree(grpnode->options);
	}

        grpnode->options = NULL;

        FileDestroyAttRecord(grpnode->att_rec);
        FileDestroyDimRecord(grpnode->dim_rec);
        FileDestroyDimRecord(grpnode->chunk_dim_rec);
        FileDestroyDimRecord(grpnode->unlimit_dim_rec);
        FileDestroyCoordVarRecord(grpnode->coord_var_rec);
        FileDestroyVarRecord(grpnode->var_rec);
        FileDestroyUDTRecord(grpnode->udt_rec);

        if(NULL != grpnode->parent)
            grpnode->parent = NULL;

        NclFree(grpnode);
        grpnode = NULL;
    }
}

void AdvancedFileDestroy(NclObj self)
{
    NclAdvancedFile thefile = (NclAdvancedFile) self;
    NclRefList *p, *pt;

    _NclUnRegisterObj((NclObj)self);
    if((NULL != thefile->advancedfile.format_funcs->free_file_rec) && (Ncl_FileVar == thefile->advancedfile.type))
    {
	    if(thefile->advancedfile.grpnode != NULL)
		    (*thefile->advancedfile.format_funcs->free_file_rec)(thefile->advancedfile.grpnode);
    }

    if(thefile->obj.cblist != NULL)
    {
        _NhlCBDestroy(thefile->obj.cblist);
        thefile->obj.cblist = NULL;
    }

    p = thefile->obj.parents;
    while (p)
    {
        pt = p;
        p = p->next;
        NclFree(pt);
        pt = NULL;
    }
        
    NclFree(thefile);
    thefile = NULL;
}

static NhlErrorTypes InitializeAdvancedFileClass
#if NhlNeedProto
(void)
#else
()
#endif
{
    NclFileClassPart *fcp = &(nclAdvancedFileClassRec.file_class);
    InitializeFileOptions(fcp->options);

    _NclRegisterClassPointer(Ncl_File, nclAdvancedFileClass);
    
    return(NhlNOERROR);
}

/* Fill the High_level-File-Structure */
static int _NclFileFillHLFS(NclAdvancedFile file_out, int is_http,
                            NclQuark path, int need_free_file,
                            int rw_status, int len_path)
{
    NclQuark the_real_path = -1;
    struct stat buf;

  /*
   *fprintf(stderr, "\nEnter _NclFileFillHLFS file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tpath: <%s>\n", NrmQuarkToString(path));
   */

    if(is_http)
    {
        the_real_path = path;
        file_out->advancedfile.fpath = the_real_path;
        file_out->advancedfile.wr_status = rw_status;

        file_out->advancedfile.grpnode = (NclFileGrpNode *)(*file_out->advancedfile.format_funcs->open_file)
                      (file_out->advancedfile.grpnode, the_real_path, rw_status);
    }
    else
    {
        if((file_out->advancedfile.format_funcs->open_file != NULL) && ((rw_status != -1) ||
           (file_out->advancedfile.format_funcs->create_file != NULL)))
        {
            if(rw_status == -1)
            {
                the_real_path = path;
                file_out->advancedfile.fpath = the_real_path;
                file_out->advancedfile.wr_status = rw_status;

                file_out->advancedfile.grpnode = (*file_out->advancedfile.format_funcs->create_file)
                              (file_out->advancedfile.grpnode,
                               NrmStringToQuark(_NGResolvePath(NrmQuarkToString(the_real_path))));
            }
            else
            {
                if(stat(_NGResolvePath(NrmQuarkToString(path)),&buf) == -1)
                {
                    char *the_path = NrmQuarkToString(path);
                    char *tmp_path = NULL;
                    tmp_path = NclMalloc(len_path+1);
                    strncpy(tmp_path,the_path,len_path);
                    tmp_path[len_path] = '\0';
                    if(stat(_NGResolvePath(tmp_path),&buf) == -1)
                    {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,
                            "_NclFileFillHLFS: Requested file (%s) does not exist or as (%s)",
                            the_path,tmp_path);
                        NclFree(tmp_path);
                        return (1);
                    }
                    else
                    {
                        the_real_path = NrmStringToQuark(tmp_path);
                        file_out->advancedfile.fpath = the_real_path;
                        NclFree(tmp_path);
                    }
                }
                else
                {
                    the_real_path = path;
                    file_out->advancedfile.fpath = the_real_path;
                }

                file_out->advancedfile.wr_status = rw_status;

                file_out->advancedfile.grpnode = (*file_out->advancedfile.format_funcs->open_file)
                               (file_out->advancedfile.grpnode,
                                NrmStringToQuark(_NGResolvePath(NrmQuarkToString(the_real_path))),
                                rw_status);

		if(NULL == file_out->advancedfile.grpnode)
                {
                    NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                              "_NclFileFillHLFS: Error opening file <%s>", NrmQuarkToString(path)));

                    if(need_free_file) 
                        NclFree((void*)file_out);
                    return (-1);
                }
            }
        }
        else
        {
            NhlPError(NhlFATAL,NhlEUNKNOWN,
                "An internal error in the extension code for the requested file format has occurred, could not open (%s)",
                NrmQuarkToString(the_real_path));

            if(need_free_file) 
                NclFree((void*)file_out);
            return (-1);
        }

    }

  /*
   *fprintf(stderr, "\tpath: <%s>\n", NrmQuarkToString(path));
   *fprintf(stderr, "Leave _NclFileFillHLFS file: %s, line: %d\n\n", __FILE__, __LINE__);
   */

    return (0);
}

NclFile _NclAdvancedFileCreate(NclObj inst, NclObjClass theclass, NclObjTypes obj_type,
                          unsigned int obj_type_mask, NclStatus status, NclQuark path,
                          int rw_status, NclQuark file_ext_q, NclQuark fname_q,
			  NhlBoolean is_http, char *end_of_name, int len_path)
{
    char *the_path = NrmQuarkToString(path);
    NclQuark the_real_path = -1;
    char *tmp_path = NULL;
    int i;
    NclAdvancedFile file_out = NULL;
    int file_out_free = 0;
    NclObjClass class_ptr;
    struct stat buf;
    NclFileClassPart *fcp = &(nclAdvancedFileClassRec.file_class);
    int ret_error = 0;

    NclFormatFunctionRecPtr topForFunRecPtr = NULL;
    NclFormatFunctionRecPtr locForFunRecPtr = NULL;

  /*
   *fprintf(stderr, "\nEnter _NclAdvancedFileCreate, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tpath: <%s>\n", NrmQuarkToString(path));
   */

    if(theclass == NULL)
        class_ptr = nclAdvancedFileClass;
    else
        class_ptr = theclass;

  /*
   * If a GRIB file, check version.  First verify that the file exists
   * and is accessible (path to it must be searchable). _NclFormatEqual handles the 
   * case-less comparison of all possible variants of the the extension.
   * Note we also need to check here for extensions added to the real path.
   */

    if(_NclFormatEqual(NrmStringToQuark("grb"),NrmStringToQuark(end_of_name)))
    {
      if(! is_http)
      {
        the_real_path = path;
        if(stat(_NGResolvePath(NrmQuarkToString(path)),&buf) == -1)
        {
            tmp_path = NclMalloc(len_path+1);
            strncpy(tmp_path,the_path,len_path);
            tmp_path[len_path] = '\0';
            if(stat(_NGResolvePath(tmp_path),&buf) == -1)
            {
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                      "_NclAdvancedFileCreate: Requested file does not exist as (%s) or as (%s)",
                      the_path,tmp_path);
                NclFree(tmp_path);
                return(NULL);
            }
            else
            {
                the_real_path = NrmStringToQuark(tmp_path);
                NclFree(tmp_path);
            }
        }
        grib_version = _NclGribVersion(NrmStringToQuark(_NGResolvePath(NrmQuarkToString(the_real_path))));
      }
    }

    if(inst == NULL)
    {
        file_out = (NclAdvancedFile)NclCalloc(1, sizeof(NclAdvancedFileRec));
        file_out_free = 1;
    }
    else
    {
        file_out = (NclAdvancedFile)inst;
    }

  /*
   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tfname_q: <%s>\n", NrmQuarkToString(fname_q));
   *fprintf(stderr, "\tpath: <%s>\n", NrmQuarkToString(path));
   *fprintf(stderr, "\tfile_ext_q: <%s>\n", end_of_name);
   */

    file_out->advancedfile.fname = fname_q;
    file_out->advancedfile.file_format = 0;
    file_out->advancedfile.file_ext_q = file_ext_q;
    file_out->file.advanced_file_structure = 1;

    topForFunRecPtr = _NclGetFormatFuncsWithAdvancedFileStructure(file_ext_q);
    file_out->advancedfile.format_funcs = topForFunRecPtr;

    if (! topForFunRecPtr)
    {
        NHLPERROR((NhlFATAL,NhlEUNKNOWN,
            "Requested file format is not supported, could not open (%s)",
            NrmQuarkToString(path)));
        if(file_out_free) 
            NclFree((void*)file_out);
        return(NULL);
    }

    file_out->advancedfile.grpnode = (NclFileGrpNode *)
                    (*file_out->advancedfile.format_funcs->initialize_file_rec)
                    (&file_out->advancedfile.file_format);
    if(NULL == file_out->advancedfile.grpnode)
    {
        NhlPError(NhlFATAL,ENOMEM,NULL);
        if(file_out_free) 
            NclFree((void*)file_out);
        return(NULL);
    }

    if(topForFunRecPtr->set_option != NULL)
    {
        for (i = 0; i < fcp->num_options; i++)
        {
            if (fcp->options[i].access == 1 && rw_status != 1)
                continue;
            else if (fcp->options[i].access == 2 && rw_status > 0)
                continue;
            else if (fcp->options[i].access == 3 && rw_status != -1)
                continue;

            locForFunRecPtr = _NclGetFormatFuncsWithAdvancedFileStructure(fcp->options[i].format);

            if (topForFunRecPtr != locForFunRecPtr)
                continue;

            topForFunRecPtr->set_option(file_out->advancedfile.grpnode,
                                        fcp->options[i].name,
                                        fcp->options[i].value->multidval.data_type,
                                        fcp->options[i].value->multidval.totalelements,
                                        fcp->options[i].value->multidval.val);
        }
    }                    

    /* Fill AdvancedFileStructure */
    ret_error = _NclFileFillHLFS(file_out, is_http, path,
                                 file_out_free, rw_status, len_path);

    if(ret_error)
        return (NULL);

  /*
   *file_out->advancedfile.grpnode->path = fname_q;
   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tfile_out->advancedfile.grpnode->path: <%s>\n",
   *                   NrmQuarkToString(file_out->advancedfile.grpnode->path));
   *fprintf(stderr, "\tpath: <%s>\n", NrmQuarkToString(path));
   */
    file_out->advancedfile.grpnode->path = path;
    file_out->advancedfile.grpnode->extension = file_ext_q;
    file_out->advancedfile.gname = -1;
    file_out->advancedfile.type = Ncl_FileVar;

    (void)_NclObjCreate((NclObj)file_out,class_ptr,obj_type,(obj_type_mask | Ncl_File),status);

    if(class_ptr == nclAdvancedFileClass)
    {
        _NclCallCallBacks((NclObj)file_out,CREATED);
    }

  /*
   *fprintf(stderr, "Leave _NclAdvancedFileCreate, file: %s, line: %d\n\n", __FILE__, __LINE__);
   */

    return((NclFile)file_out);
}

static int AdvancedFileIsVar(NclFile thefile, NclQuark var)
{
    NclAdvancedFile advancedfile = (NclAdvancedFile)thefile;
    NclFileVarNode *varnode;
    int ret = 1;
  /*
   *fprintf(stdout, "\nHit AdvancedFileIsVar. file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stdout, "\tvar: <%s>\n", NrmQuarkToString(var));
   */

    varnode = _getVarNodeFromNclFileGrpNode(advancedfile->advancedfile.grpnode, var);

    if(NULL == varnode)
        ret = -1;
  /*
   *fprintf(stdout, "\tCANNOT FIND var: <%s>\n", NrmQuarkToString(var));
   *fprintf(stdout, "\tindex: %d\n", ret);
   *fprintf(stdout, "End AdvancedFileIsVar. file: %s, line: %d\n\n", __FILE__, __LINE__);
   */
    return ret;
}

static struct _NclMultiDValDataRec* MyAdvancedFileReadVarValue(NclFile infile, NclQuark var_name,
                                                          struct _NclSelectionRecord *sel_ptr,
                                                          NclDimRec *dim_info, int vtype)
{
    NclAdvancedFile thefile = (NclAdvancedFile) infile;
    NclMultiDValData tmp_md = NULL;
    NclMultiDValData mis_md = NULL;
    NclScalar missing_value;
    int has_missing = 0;
    void *val = NULL;
    long start[NCL_MAX_DIMENSIONS];
    long finish[NCL_MAX_DIMENSIONS];
    long stride[NCL_MAX_DIMENSIONS];
    long real_stride[NCL_MAX_DIMENSIONS];
    int i,j,k,done = 0,inc_done = 0;
    int n_dims_input;
    long  n_elem = 1;
    int n_dims_output = 1;
    long total_elements = 1;
    int has_vectors = 0;
    int has_stride = 0;
    int has_reverse = 0;
    int has_reorder = 0;
    int to = 0,block_read_limit = 1,n_elem_block;
    
    long multiplier_input[NCL_MAX_DIMENSIONS];
    int compare_sel[NCL_MAX_DIMENSIONS];
    long current_index[NCL_MAX_DIMENSIONS];
    long current_finish[NCL_MAX_DIMENSIONS];
    int index_map[NCL_MAX_DIMENSIONS];
    ng_size_t output_dim_sizes[NCL_MAX_DIMENSIONS];
    int keeper[NCL_MAX_DIMENSIONS];
    NclSelection *sel;
    long tmpi = 0;
    int swap_size;
    void *swap_space = NULL;

    NclFileVarNode *varnode = NULL;
    NclFileDimNode *dimnode = NULL;
    NclFileCompoundNode *compnode = NULL;
    NclFileOpaqueRecord *opaquerec = NULL;
    NclFileEnumRecord   *enumrec = NULL;

 /*
  * By the the time it gets here the file suport routines in that build the selection
  * record have made sure var_name is valid and all the demensions in sel_ptr
  * are valid. However, the values have not been checked for out_of_ranges
  * subscripts
  */
    varnode = _getVarNodeFromNclFileGrpNode(thefile->advancedfile.grpnode, var_name);

    n_dims_input = varnode->dim_rec->n_dims;
    if(sel_ptr != NULL)
    {
        sel = sel_ptr->selection;
        for(i = 0; i < n_dims_input; i++)
        {
            dimnode = &(varnode->dim_rec->dim_node[i]);

            switch(sel->sel_type)
            {
            case Ncl_SUB_ALL:
                start[sel->dim_num] = 0;
            case Ncl_SUB_VAL_DEF:
                if(Ncl_SUB_VAL_DEF == sel->sel_type)
                {
                    start[sel->dim_num] = sel->u.sub.start;
                }
                finish[sel->dim_num] = dimnode->size - 1;
            case Ncl_SUB_DEF_VAL:
                if(sel->sel_type == Ncl_SUB_DEF_VAL)
                {
                    finish[sel->dim_num] = sel->u.sub.finish;
                    start[sel->dim_num] = 0;
                } 
            case Ncl_SUBSCR:
                if(sel->u.sub.is_single)
                    keeper[i] = 0;
                else
                    keeper[i] = 1;

                if(sel->sel_type == Ncl_SUBSCR)
                {
                    start[sel->dim_num] = sel->u.sub.start;
                    finish[sel->dim_num] = sel->u.sub.finish;
                    stride[sel->dim_num] = sel->u.sub.stride;
                }
                else
                    stride[sel->dim_num] = sel->u.sub.stride;

                if(finish[sel->dim_num] < start[sel->dim_num])
                {
                    if(stride[sel->dim_num] < 0)
                    {
                        tmpi = finish[sel->dim_num] + (start[sel->dim_num] - finish[sel->dim_num])
                                                    % labs(stride[sel->dim_num]);
                        finish[sel->dim_num] = start[sel->dim_num];
                        start[sel->dim_num] = tmpi;
                        compare_sel[sel->dim_num] = NCLFILE_INC;
                        stride[sel->dim_num] = -(stride[sel->dim_num]); 
                    }
                    else
                    {
                        compare_sel[sel->dim_num] = NCLFILE_DEC;
                        stride[sel->dim_num] = -(stride[sel->dim_num]); 
                        has_reverse = 1;
                    }
                }
                else
                {
                    if(stride[sel->dim_num] < 0)
                    {
                        has_reverse = 1;
                        tmpi = finish[sel->dim_num] - (finish[sel->dim_num] - start[sel->dim_num])
                                                    % labs(stride[sel->dim_num]);
                        finish[sel->dim_num] = start[sel->dim_num];
                        start[sel->dim_num] = tmpi;
                        compare_sel[sel->dim_num] = NCLFILE_DEC;
                        stride[sel->dim_num] = (stride[sel->dim_num]);
                    }
                    else
                    {
                        compare_sel[sel->dim_num] = NCLFILE_INC;
                        stride[sel->dim_num] = (stride[sel->dim_num]);
                    }
                }

                if(labs(stride[sel->dim_num]) > 1) 
                    has_stride = 1;

                if(stride[sel->dim_num] != 0) 
                {
                    tmpi = labs(sel->u.sub.stride);
                }
                else
                {
                    NHLPERROR((NhlWARNING,NhlEUNKNOWN,"Invalid stride: %ld. %s",
                              stride[sel->dim_num], "Stride must be positive non-zero integer"));

                    stride[sel->dim_num] = 1;
                }

                n_elem = labs((finish[sel->dim_num] - start[sel->dim_num]) /tmpi) + 1;
                if((sel->u.sub.start > dimnode->size - 1)||(sel->u.sub.start < 0))
                {
                    NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i));
                    return(NULL);
                }

                if((sel->u.sub.finish > dimnode->size - 1)||(sel->u.sub.finish< 0))
                {
                    NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i));
                    return(NULL);
                }
                
             /*
              * set when normal subscript
              */
                if(sel->dim_num != i)
                    has_reorder = 1;

                index_map[i] = sel->dim_num;
                break;
            case Ncl_VECSUBSCR:
                keeper[i] = 1;
                if((sel->u.vec.min < 0)||(sel->u.vec.min >= dimnode->size))
                {    
                    NHLPERROR((NhlFATAL,NhlEUNKNOWN, "Subscript out of range, error in subscript #%d",i));
                    return(NULL);
                }

                if((sel->u.vec.max < 0)||(sel->u.vec.max >= dimnode->size))
                {    
                    NHLPERROR((NhlFATAL,NhlEUNKNOWN, "Subscript out of range, error in subscript #%d",i));
                    return(NULL);
                }
                n_elem = sel->u.vec.n_ind;
            
                stride[sel->dim_num] = 0;
                start[sel->dim_num] = finish[sel->dim_num] = sel->u.vec.ind[0];
                has_vectors = 1;
                if(sel->dim_num != i)
                {
                    has_reorder = 1;
                }
                index_map[i] = sel->dim_num;

             /*
              * 0 when vector subscript
              */
                compare_sel[sel->dim_num] = NCLFILE_VEC;
                break;
            }

            output_dim_sizes[i] = n_elem;
            (dim_info)[i].dim_num = i;
            (dim_info)[i].dim_size = n_elem;
            (dim_info)[i].dim_quark = dimnode->name;
            total_elements = total_elements * n_elem;

            if(sel->dim_num != n_dims_input - 1)
            {
                multiplier_input[sel->dim_num] = 1;
                for(k = sel->dim_num + 1; k < n_dims_input; k++)
                {
                    dimnode = &(varnode->dim_rec->dim_node[k]);
                    multiplier_input[sel->dim_num] *= (long)dimnode->size;
                }
            }
            sel++;
        }

        sel = sel_ptr->selection;
    }
    else
    {
      /*
       *fprintf(stdout, "\n\n\nhit MyAdvancedFileReadVarValue. file: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stdout, "\tn_dims_input = %d\n", n_dims_input);
       */

        for(i = 0; i< n_dims_input; i++)
        {
            dimnode = &(varnode->dim_rec->dim_node[i]);
            start[i] = 0;
            finish[i] = dimnode->size - 1;

            if(0 > finish[i])
            {
                finish[i] = 0;
                dimnode->size = 1;
            }

            stride[i] = 1;
            total_elements *= (finish[i] + 1);
            output_dim_sizes[i] = finish[i] + 1;
            (dim_info)[i].dim_num = i;
            (dim_info)[i].dim_size = output_dim_sizes[i];
            (dim_info)[i].dim_quark = dimnode->name;
            compare_sel[i] = NCLFILE_INC;
            multiplier_input[i] = 1;

            for(k = i + 1; k < n_dims_input; k++)
            {
                dimnode = &(varnode->dim_rec->dim_node[k]);
                multiplier_input[i] *= (long) dimnode->size;
            }
        }
        sel = NULL;
    }

    if (total_elements == 0)
    {
     /* can't return any data because there is a 0-length dimension
      * but nevertheless return what is possible
      */
        NHLPERROR((NhlWARNING,NhlEUNKNOWN,"MyAdvancedFileReadVarValue: %s contains a 0 length dimension", 
            NrmQuarkToString(var_name)));
        n_dims_output = n_dims_input;
        val = NULL;

        if(sel_ptr != NULL)
        {
            i = 0;
            while((i <  n_dims_output)&&(n_dims_output > 1))
            {
                if((output_dim_sizes[i] == 1)&&!(keeper[i]))
                {
                    for(j = i; j < n_dims_output-1; j++)
                    {
                        output_dim_sizes[j] = output_dim_sizes[j+1];
                        keeper[j] = keeper[j+1];
                        (dim_info)[j] = (dim_info)[j+1];
                    }
                    n_dims_output--;
                }
                else
                {
                    i++;
                }
            }
        }

      /*
       *if(AdvancedFileIsVarAtt(infile,var_name,NrmStringToQuark(NCL_MISSING_VALUE_ATT)) != -1)
       */
        if(AdvancedVarAttIndex(varnode, NrmStringToQuark(NCL_MISSING_VALUE_ATT)) != -1)
        {
            mis_md = AdvancedFileReadVarAtt(infile,var_name,NrmStringToQuark(NCL_MISSING_VALUE_ATT),NULL);
            if(mis_md != NULL)
            {
                has_missing = 1;
                if (mis_md->multidval.val == NULL)
                {
                    NHLPERROR((NhlWARNING,NhlEUNKNOWN,
                        "MyAdvancedFileReadVarValue: _FillValue attribute for  variable (%s) in file (%s) has NULL value, %s\n",
                         NrmQuarkToString(var_name),NrmQuarkToString(thefile->advancedfile.fname),
                        "substituting default fill value of variable type"));
                    _NclGetDefaultFillValue(varnode->type, &missing_value);
                }
                else if (mis_md->multidval.data_type == varnode->type)
                {
                    memcpy((void*)&missing_value,mis_md->multidval.val,_NclSizeOf(mis_md->multidval.data_type));
                }
                else
                {
                    NHLPERROR((NhlWARNING,NhlEUNKNOWN,
                        "MyAdvancedFileReadVarValue: _FillValue attribute type differs from variable (%s) type in file (%s), %s\n",
                         NrmQuarkToString(var_name),NrmQuarkToString(thefile->advancedfile.fname),
                        "forcing type conversion; may result in overflow and/or loss of precision"));

                    _NclScalarForcedCoerce(mis_md->multidval.val,mis_md->multidval.data_type,
                                          (void*)&missing_value, varnode->type);
                }
            }
        } 

        if(vtype == FILE_COORD_VAR_ACCESS)
        {
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
                     _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(varnode->type))
                     );
        }
        else
        {
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
                     _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(varnode->type))
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
    if((vtype == FILE_VAR_ACCESS? thefile->advancedfile.format_funcs->read_var != NULL:thefile->advancedfile.format_funcs->read_coord != NULL))
    {
        if((!has_vectors)&&(!has_reverse)&&(!has_reorder))
        {
            if(vtype == FILE_VAR_ACCESS)
            {
                if(NCL_compound == varnode->type)
                {
                    char *struct_name = NULL;
                    char *component_name = NULL;

                    component_name = _getComponentName(NrmQuarkToString(var_name), &struct_name);
		    NclFree(struct_name);
                    if(NULL != component_name)
                    {
                      /*
                       *fprintf(stderr, "\n\tfile: %s, line: %d\n", __FILE__, __LINE__);
                       *fprintf(stderr, "\ttotal_elements = %d\n", total_elements);
                       *fprintf(stderr, "\tvarname: <%s>\n", NrmQuarkToString(var_name));
                       *fprintf(stderr, "\tcomponent_name: <%s>, struct_name: <%s>\n",
                       *                   component_name, struct_name);
		       */

                        compnode = _getComponentNodeFromVarNode(varnode, component_name);

                      /*
                       *fprintf(stderr, "\n\tfile: %s, line: %d\n", __FILE__, __LINE__);
                       *fprintf(stderr, "\tcompnode->name: <%s>\n", NrmQuarkToString(compnode->name));
                       *fprintf(stderr, "\tcompnode->nvals: %d\n", compnode->nvals);
                       */

                        if(NCL_string == compnode->type)
                            val = (void*)NclMalloc(total_elements * _NclSizeOf(compnode->type));
                        else
                            val = (void*)NclMalloc(total_elements * compnode->nvals * _NclSizeOf(compnode->type));

                        (*thefile->advancedfile.format_funcs->read_var)
                         (thefile->advancedfile.grpnode,
                          var_name,
                          start, finish, stride, val);

                        NclFree(component_name);
                    }
                    else
                    {
                        val = (void*) (*thefile->advancedfile.format_funcs->read_var)
                                      (thefile->advancedfile.grpnode,
                                       var_name,
                                       start, finish, stride, val);

                        tmp_md = (NclMultiDValData) val;
                        return (tmp_md);

                      /*
                       *NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                       *    "MyAdvancedFileReadVarValue: Invalid component in struct: <%s>",
                       *     NrmQuarkToString(var_name)));
                       */
                    }
                }
                else if((NCL_vlen == varnode->type) || (NCL_list == varnode->type))
                {
                  /*
                   *fprintf(stderr, "\n\tfile: %s, line: %d\n", __FILE__, __LINE__);
                   *fprintf(stderr, "\tvar_name: <%s>\n", NrmQuarkToString(var_name));
                   */

                    val = (*thefile->advancedfile.format_funcs->read_var)
                     (thefile->advancedfile.grpnode,
                      var_name,
                      start, finish, stride, val);

                    tmp_md = (NclMultiDValData) val;
                    return (tmp_md);
                }
                else if(NCL_opaque == varnode->type)
                {
                  /*
                   *fprintf(stderr, "\n\tfile: %s, line: %d\n", __FILE__, __LINE__);
                   *fprintf(stderr, "\tvar_name: <%s>\n", NrmQuarkToString(var_name));
                   */

                    val = (*thefile->advancedfile.format_funcs->read_var)
                     (thefile->advancedfile.grpnode,
                      var_name,
                      start, finish, stride, val);
 
                    opaquerec = (NclFileOpaqueRecord *) val;
                }
                else if(NCL_enum == varnode->type)
                {
                  /*
                   *fprintf(stderr, "\n\tfile: %s, line: %d\n", __FILE__, __LINE__);
                   *fprintf(stderr, "\tvar_name: <%s>\n", NrmQuarkToString(var_name));
                   */

                    val = (*thefile->advancedfile.format_funcs->read_var)
                     (thefile->advancedfile.grpnode,
                      var_name,
                      start, finish, stride, val);
 
                    enumrec = (NclFileEnumRecord *) val;
                }
                else
                {
                    val = (void*)NclMalloc(total_elements*_NclSizeOf(varnode->type));
                    (*thefile->advancedfile.format_funcs->read_var)
                     (thefile->advancedfile.grpnode,
                      var_name,
                      start, finish, stride, val);
                }
            }
            else
            {
                val = (void*)NclMalloc(total_elements*_NclSizeOf(varnode->type));
                (*thefile->advancedfile.format_funcs->read_coord)
                 (thefile->advancedfile.grpnode,
                  var_name,
                  start, finish, stride, val);
            }
        
            n_dims_output = n_dims_input;
            if(sel_ptr != NULL)
            {
                i = 0;
                while((i <  n_dims_output)&&(n_dims_output > 1))
                {
                    if((output_dim_sizes[i] == 1)&&!(keeper[i]))
                    {
                        for(j = i; j < n_dims_output-1; j++)
                        {
                            output_dim_sizes[j] = output_dim_sizes[j+1];
                            keeper[j] = keeper[j+1];
                            (dim_info)[j] = (dim_info)[j+1];
                        }
                        n_dims_output--;
                    }
                    else
                    {
                        i++;
                    }
                }
            }
        }
        else if((has_reverse)&&(!has_vectors)&&(!has_reorder))
        {
         /*
          * If a reverse is detected it is quicker to read everything in inorder and
          * perform swapping in memory. This is easiest done if selections containing
          * vectors and dimension reordering are excluded. Unfortunately swap space
          * is needed and this could be as large as the product of the sizes of all 
          * dimensions with the exception of dimension 0.
          */
            val = (void*)NclMalloc(total_elements*_NclSizeOf(varnode->type));
            assert(val);
            i = 0;
            while((i<n_dims_input)&&(compare_sel[i] != NCLFILE_DEC))
            {
                i++;
            }

            swap_size = 1;
            for(j = i + 1; j < n_dims_input; j++)
            {    
                swap_size *= output_dim_sizes[j];
            }

            swap_space = (void*)NclMalloc(swap_size * _NclSizeOf(varnode->type));
            assert(swap_space);
            for(i = 0;i < n_dims_input; i++)
            {
                switch(compare_sel[i])
                {
                case NCLFILE_INC:
                    current_index[i] = start[i];
                    current_finish[i] = finish[i];
                    real_stride[i] = labs(stride[i]);
                    break;
                case NCLFILE_DEC:
                 /*
                  * Problem here is that selecting in reverse order could
                  * alter selection when (finish - start )%stride != 0
                  * Therefore a new start and finish must be computed to
                  * produce desired selection
                  */
                    real_stride[i] = labs(stride[i]);
                    current_finish[i] = start[i];
                    if(( start[i] - finish[i])%labs(stride[i]) == 0)
                    {
                        current_index[i] = finish[i] ;
                    }
                    else
                    {
                        current_index[i] = finish[i]+ (start[i] - finish[i])%labs(stride[i]);
                    }
                    break;
                }
            }

            if(vtype == FILE_VAR_ACCESS)
            {
                (*thefile->advancedfile.format_funcs->read_var)
                 (thefile->advancedfile.grpnode,
                  var_name,
                  current_index, current_finish, real_stride, (void*)val);
            }
            else
            {
                (*thefile->advancedfile.format_funcs->read_coord)
                 (thefile->advancedfile.grpnode,
                  var_name,
                  current_index, current_finish, real_stride, (void*)val);
            }

            n_dims_output = n_dims_input;
            if(sel_ptr != NULL)
            {
                i = 0;
                while((i <  n_dims_output)&&(n_dims_output > 1))
                {
                    if((output_dim_sizes[i] == 1)&&!(keeper[i]))
                    {
                        for(j = i; j < n_dims_output-1; j++)
                        {
                            output_dim_sizes[j] = output_dim_sizes[j+1];
                            keeper[j] = keeper[j+1];
                            compare_sel[j] = compare_sel[j+1];
                            (dim_info)[j] = (dim_info)[j+1];
                        }
                        n_dims_output--;
                    }
                    else
                    {
                        i++;
                    }
                }
            }

            ReverseIt(val,swap_space,n_dims_output,compare_sel,output_dim_sizes,_NclSizeOf(varnode->type));
            NclFree(swap_space);
        }
        else
        {
            val = (void*)NclMalloc(total_elements*_NclSizeOf(varnode->type));
            assert(val);
            to = 0;
            block_read_limit = n_dims_input - 1 ;
         /*
          * Find out what size chunks can be read in at once
          */
            for(i = n_dims_input-1 ; i>= 0; i--)
            {
                if((compare_sel[index_map[i]] != NCLFILE_INC)||(index_map[i] != i))
                {
                    block_read_limit = i;
                    break;
                }
            }

         /*
          * Initialize starting index, finish and stride values for first read
          */
            n_elem_block = 1;
            for(i = 0; i < n_dims_input ; i++)
            {
                current_index[index_map[i]] = start[index_map[i]];
                if(i > block_read_limit)
                {
                 /*
                  * OK to use i here since these indices are in order
                  */
                    n_elem_block *= output_dim_sizes[index_map[i]];
                    current_finish[index_map[i]] = finish[index_map[i]];
                    real_stride[index_map[i]] = labs(stride[index_map[i]]);
                }
                else
                {
                    switch(compare_sel[index_map[i]])
                    {
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
            while(!done)
            {
                if(vtype == FILE_VAR_ACCESS)
                {
                    (*thefile->advancedfile.format_funcs->read_var)
                     (thefile->advancedfile.grpnode, var_name,
                      current_index, current_finish, real_stride,
                      (void*)&(((char*)val)[to]));
                }
                else
                {
                    (*thefile->advancedfile.format_funcs->read_coord)
                     (thefile->advancedfile.grpnode, var_name,
                      current_index, current_finish, real_stride,
                      (void*)&(((char*)val)[to]));
                }

                to += n_elem_block * _NclSizeOf(varnode->type);
                if(compare_sel[index_map[block_read_limit]] < 0)
                {
                    current_index[index_map[block_read_limit]] += stride[index_map[block_read_limit]];
                    current_finish[index_map[block_read_limit]] = current_index[index_map[block_read_limit]];
                }
                else
                {
                    compare_sel[index_map[block_read_limit]]++;
                }

                for(i = block_read_limit; i > 0; i--)
                {
                    switch(compare_sel[index_map[i]])
                    {
                    case NCLFILE_INC:
                        if(current_index[index_map[i]] > finish[index_map[i]])
                        {
                            current_index[index_map[i]] = start[index_map[i]];
                            if(compare_sel[index_map[i-1]] < 0 )
                            {
                                current_index[index_map[i-1]] += stride[index_map[i-1]];
                            }
                            else
                            {
                                compare_sel[index_map[i-1]]++;
                            }

                        }
                        else
                        {
                            inc_done = 1;
                        }    
                        current_finish[index_map[i]] = current_index[index_map[i]] ;
                        break;
                    case NCLFILE_DEC:
                        if(current_index[index_map[i]] < finish[index_map[i]])
			{
                            current_index[index_map[i]] = start[index_map[i]];
                            if(compare_sel[index_map[i-1]] < 0)
                            {
                                current_index[index_map[i-1]] += stride[index_map[i-1]];
                            }
                            else
                            {
                                compare_sel[index_map[i-1]]++;
                            }
                        }
                        else
                        {    
                            inc_done = 1;
                        }
                        current_finish[index_map[i]] = current_index[index_map[i]];
                        break;
                    default:
                        if(compare_sel[index_map[i]] >= sel[i].u.vec.n_ind)
                        {
                            compare_sel[index_map[i]] = 0;
                            current_index[index_map[i]] = sel[i].u.vec.ind[0];
                            if(compare_sel[index_map[i-1]] < 0 )
                            {
                                current_index[index_map[i-1]] += stride[index_map[i-1]];
                            }
                            else
                            {
                                compare_sel[index_map[i-1]]++;
                            }
                        }
                        else
                        {
                            current_index[index_map[i]] = sel[i].u.vec.ind[compare_sel[index_map[i]]];
                            inc_done = 1;
                        }
                        current_finish[index_map[i]] = current_index[index_map[i]];
                        break;
                    } 

                    if(inc_done)
                    {
                        inc_done = 0;
                        break;
                    }
                }

                switch(compare_sel[index_map[0]])
                {
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
                    if(compare_sel[index_map[0]] >= sel[0].u.vec.n_ind)
                    {
                            done = 1;
                    }
                    else
                    {
                        current_index[index_map[0]] = sel[0].u.vec.ind[compare_sel[index_map[0]]];
                    }
                    current_finish[index_map[0]] = current_index[index_map[0]]; 
                }
            }

            n_dims_output = n_dims_input;
            if(sel_ptr != NULL)
            {
                i = 0;
                while((i <  n_dims_output)&&(n_dims_output > 1))
                {
                    if((output_dim_sizes[i] == 1)&&!(keeper[i]))
                    {
                        for(j = i; j < n_dims_output-1; j++)
                        {
                            output_dim_sizes[j] = output_dim_sizes[j+1];
                            keeper[j] = keeper[j+1];
                            (dim_info)[j] = (dim_info)[j+1];
                        }
                        n_dims_output--;
                    }
                    else
                    {
                        i++;
                    }
                }
            }
        }
    }
    else if((vtype == FILE_VAR_ACCESS ? thefile->advancedfile.format_funcs->read_var_ns != NULL : thefile->advancedfile.format_funcs->read_coord_ns!= NULL))
    {
        if(!has_stride)
        {
            if((!has_vectors)&&(!has_reverse)&&(!has_reorder))
            {
                val = (void*)NclMalloc(total_elements*_NclSizeOf(varnode->type));
                assert(val);
                if(vtype == FILE_VAR_ACCESS)
                {
                    (*thefile->advancedfile.format_funcs->read_var_ns)
                     (thefile->advancedfile.grpnode, var_name,
                      start, finish, val);
                }
                else
                {
                    (*thefile->file.format_funcs->read_coord_ns)
                     (thefile->advancedfile.grpnode, var_name,
                      start, finish, val);
                }
            
                n_dims_output = n_dims_input;
                if(sel_ptr != NULL)
                {
                    i = 0;
                    while((i <  n_dims_output)&&(n_dims_output > 1))
                    {
                        if((output_dim_sizes[i] == 1)&&!(keeper[i]))
                        {
                            for(j = i; j < n_dims_output-1; j++)
                            {
                                output_dim_sizes[j] = output_dim_sizes[j+1];
                                keeper[j] = keeper[j+1];
                                (dim_info)[j] = (dim_info)[j+1];
                            }
                            n_dims_output--;
                        }
                        else
                        {
                            i++;
                        }
                    }
                }
            }
            else if((has_reverse)&&(!has_vectors)&&(!has_reorder))
            {
             /*
              * If a reverse is detected it is quicker to read everything in inorder and
              * perform swapping in memory. This is easiest done if selections containing
              * vectors and dimension reordering are excluded. Unfortunately swap space
              * is needed and this could be as large as the product of the sizes of all 
              * dimensions with the exception of dimension 0.
              */
                val = (void*)NclMalloc(total_elements*_NclSizeOf(varnode->type));
                assert(val);
                i = 0;
                while((i<n_dims_input)&&(compare_sel[i] != NCLFILE_DEC))
                {
                    i++;
                }

                swap_size = 1;
                for(j = i + 1; j < n_dims_input; j++)
                {    
                    swap_size *= output_dim_sizes[j];
                }

                swap_space = (void*)NclMalloc(swap_size * _NclSizeOf(varnode->type));
                assert(swap_space);
                for(i = 0;i < n_dims_input; i++)
                {
                    switch(compare_sel[i])
                    {
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

                if(vtype == FILE_VAR_ACCESS)
                {
                    (*thefile->advancedfile.format_funcs->read_var_ns)
                     (thefile->advancedfile.grpnode, var_name,
                      current_index, current_finish, (void*)val);
                }
                else
                {
                    (*thefile->advancedfile.format_funcs->read_coord_ns)
                     (thefile->advancedfile.grpnode, var_name,
                      current_index, current_finish, (void*)val);
                }

                n_dims_output = n_dims_input;
                if(sel_ptr != NULL)
                {
                    i = 0;
                    while((i <  n_dims_output)&&(n_dims_output > 1))
                    {
                        if((output_dim_sizes[i] == 1)&&!(keeper[i]))
                        {
                            for(j = i; j < n_dims_output-1; j++)
                            {
                                output_dim_sizes[j] = output_dim_sizes[j+1];
                                keeper[j] = keeper[j+1];
                                compare_sel[j] = compare_sel[j+1];
                                (dim_info)[j] = (dim_info)[j+1];
                            }
                            n_dims_output--;
                        }
                        else
                        {
                            i++;
                        }
                    }
                }

                ReverseIt(val,swap_space,n_dims_output,compare_sel,output_dim_sizes,_NclSizeOf(varnode->type));
                NclFree(swap_space);
            }
            else
            {
                val = (void*)NclMalloc(total_elements*_NclSizeOf(varnode->type));
                assert(val);
                to = 0;
                block_read_limit = n_dims_input - 1 ;
             /*
              * Find out what size chunks can be read in at once
              */
                for(i = n_dims_input-1 ; i>= 0; i--)
                {
                    if((compare_sel[index_map[i]] != NCLFILE_INC)||(index_map[i] != i))
                    {
                        block_read_limit = i;
                        break;
                    }
                }
             /*
              * Initialize starting index, finish and stride values for first read
              */
                n_elem_block = 1;
                for(i = 0; i < n_dims_input ; i++)
                {
                    current_index[index_map[i]] = start[index_map[i]];
                    if(i > block_read_limit)
                    {
                     /*
                      * OK to use i here since these indices are in order
                      */
                        n_elem_block *= output_dim_sizes[i];
                        current_finish[index_map[i]] = finish[index_map[i]];
                    }
                    else
                    {
                        switch(compare_sel[index_map[i]])
                        {
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
                while(!done)
                {
                    if(vtype == FILE_VAR_ACCESS)
                    {
                        (*thefile->advancedfile.format_funcs->read_var_ns)
                         (thefile->advancedfile.grpnode, var_name,
                          current_index, current_finish,
                          (void*)&(((char*)val)[to]));
                    }
                    else
                    {
                        (*thefile->advancedfile.format_funcs->read_coord_ns)
                         (thefile->advancedfile.grpnode, var_name,
                          current_index, current_finish,
                          (void*)&(((char*)val)[to]));
                    }

                    to += n_elem_block * _NclSizeOf(varnode->type);
                    if(compare_sel[index_map[block_read_limit]] < 0)
                    {
                        current_index[index_map[block_read_limit]] += 1;
                        current_finish[index_map[block_read_limit]] = current_index[index_map[block_read_limit]];
                    }
                    else
                    {
                        compare_sel[index_map[block_read_limit]]++;
                    }

                    for(i = block_read_limit; i > 0; i--)
                    {
                        switch(compare_sel[index_map[i]])
                        {
                        case NCLFILE_INC:
                            if(current_index[index_map[i]] > finish[index_map[i]])
                            {
                                current_index[index_map[i]] = start[index_map[i]];
                                if(compare_sel[index_map[i-1]] < 0 )
                                {
                                    current_index[index_map[i-1]] += 1; 
                                }
                                else
                                {
                                    compare_sel[index_map[i-1]]++;
                                }
                            }
                            else
                            {
                                inc_done = 1;
                            }    
                            current_finish[index_map[i]] = current_index[index_map[i]] ;
                            break;
                        case NCLFILE_DEC:
                            if(current_index[index_map[i]] < finish[index_map[i]])
                            {
                                current_index[index_map[i]] = start[index_map[i]];
                                if(compare_sel[index_map[i-1]] < 0)
                                {
                                    current_index[index_map[i-1]] += 1; 
                                }
                                else
                                {
                                    compare_sel[index_map[i-1]]++;
                                }
                            }
                            else
                            {    
                                inc_done = 1;
                            }
                            current_finish[index_map[i]] = current_index[index_map[i]];
                            break;
                        default:
                            if(compare_sel[index_map[i]] >= sel[index_map[i]].u.vec.n_ind)
                            {
                                compare_sel[index_map[i]] = 0;
                                current_index[index_map[i]] = sel[index_map[i]].u.vec.ind[0];
                                if(compare_sel[index_map[i-1]] < 0 )
                                {
                                    current_index[index_map[i-1]] += 1; 
                                }
                                else
                                {
                                    compare_sel[index_map[i-1]]++;
                                }
                            }
                            else
                            {
                                current_index[index_map[i]] = sel[index_map[i]].u.vec.ind[compare_sel[index_map[i]]];
                                inc_done = 1;
                            }
                            current_finish[index_map[i]] = current_index[index_map[i]];
                            break;
                        } 
                        if(inc_done)
                        {
                            inc_done = 0;
                            break;
                        }
                    }

                    switch(compare_sel[index_map[0]])
                    {
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
                        if(compare_sel[index_map[0]] >= sel[0].u.vec.n_ind)
                        {
                                done = 1;
                        } 
                        else
                        {
                            current_index[index_map[0]] = sel[0].u.vec.ind[compare_sel[index_map[0]]];
                        }
                        current_finish[index_map[0]] = current_index[index_map[0]]; 
                    }
                }

                n_dims_output = n_dims_input;
                if(sel_ptr != NULL )
                {
                    i = 0;
                    while((i <  n_dims_output)&&(n_dims_output > 1))
                    {
                        if((output_dim_sizes[i] == 1)&&!(keeper[i]))
                        {
                            for(j = i; j < n_dims_output-1; j++)
                            {
                                output_dim_sizes[j] = output_dim_sizes[j+1];
                                (dim_info)[j] = (dim_info)[j+1];
                            }
                            n_dims_output--;
                        }
                        else
                        {
                            i++;
                        }
                    }
                }
            }
        }
        else
        {
            if((!has_vectors)&&(!has_reverse)&&(!has_reorder))
            {
             /*
              * Loop through and find block size. Then implement read like it is vectors
              */
                for(i = n_dims_input-1; i >= 0; i--)
                {
                    if(stride[i] != 1)
                    {
                        block_read_limit = i;
                        break;
                    }
                }

                n_elem_block = 1;
                for(i = 0; i < n_dims_input; i++)
                {
                    current_index[i] = start[i];
                    if(i > block_read_limit)
                    {
                        n_elem_block *= output_dim_sizes[i];
                        current_finish[i] = finish[i];
                    }
                    else
                    {
                        current_finish[i] = current_index[i];
                    }
                }

                val = (void*)NclMalloc(total_elements*_NclSizeOf(varnode->type));
                assert(val);
                to = 0;
                while(!done)
                {
                    if(vtype == FILE_VAR_ACCESS)
                    {
                        (*thefile->advancedfile.format_funcs->read_var_ns)
                         (thefile->advancedfile.grpnode, var_name,
                          current_index, current_finish,
                          (void*)&(((char*)val)[to]));
                    }
                    else
                    {
                        (*thefile->advancedfile.format_funcs->read_coord_ns)
                         (thefile->advancedfile.grpnode, var_name,
                          current_index, current_finish, val);
                    }

                    to += n_elem_block * _NclSizeOf(varnode->type);

                 /*
                  * No reverse here so all is just added
                  */
                    for(i = block_read_limit; i >= 0 ; i--)
                    {
                        if((current_index[i] + stride[i] > finish[i])&&(i != 0))
                        {
                            current_index[i] = start[i];
                            current_finish[i] = current_index[i];
                        }
                        else
                        {
                            current_index[i] = current_index[i] + stride[i];
                            current_finish[i] = current_index[i];
                            break;
                        }
                    }

                    if(current_index[0]  > finish[0])
                    {
                        done = 1;
                    }
                    current_finish[0] = current_index[0];
                }

                n_dims_output = n_dims_input;
                if(sel_ptr != NULL)
                {
                    i = 0;
                    while((i <  n_dims_output)&&(n_dims_output > 1))
                    {
                        if((output_dim_sizes[i] == 1)&&!(keeper[i]))
                        {
                            for(j = i; j < n_dims_output-1; j++)
                            {
                                output_dim_sizes[j] = output_dim_sizes[j+1];
                                (dim_info)[j] = (dim_info)[j+1];
                            }
                            n_dims_output--;
                        }
                        else
                        {
                            i++;
                        }
                    }
                }
            }
            else if((has_reverse)&&(!has_vectors)&&(!has_reorder))
            {
             /*
              * Loop through and file block size. Then implment read like it contains vectors. Finnally call ReverseIt.
              */
                for(i = n_dims_input-1; i >= 0; i--)
                {
                    if(stride[i] != 1)
                    {
                        block_read_limit = i;
                        break;
                    }
                }
                n_elem_block = 1;
                for(i = 0; i < n_dims_input; i++)
                {
                    if(i > block_read_limit)
                    {
                     /*
                      * To be in this range stride is 1
                      * Still could be reverse.
                      */
                        n_elem_block *= output_dim_sizes[i];
                        switch(compare_sel[i])
                        {
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
                    }
                    else
                    {
                        switch(compare_sel[i])
                        {
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

                val = (void*)NclMalloc(total_elements*_NclSizeOf(varnode->type));
                assert(val);
                swap_space = NclMalloc(n_elem_block * _NclSizeOf(varnode->type));
                assert(swap_space);
                to = 0;

                while(!done)
                {
                    if(vtype == FILE_VAR_ACCESS)
                    {
                        (*thefile->advancedfile.format_funcs->read_var_ns)
                         (thefile->advancedfile.grpnode, var_name,
                          current_index, current_finish,
                          (void*)&(((char*)val)[to]));
                    }
                    else
                    {
                        (*thefile->advancedfile.format_funcs->read_coord_ns)
                         (thefile->advancedfile.grpnode, var_name,
                          current_index, current_finish, val);
                    }

                    if((n_dims_input - (block_read_limit + 1))>=1)
                    {
                        ReverseIt((void*)&(((char*)val)[to]),swap_space,n_dims_input - (block_read_limit + 1),
                                  &(compare_sel[block_read_limit+1]),
                                  &(output_dim_sizes[block_read_limit+1]),_NclSizeOf(varnode->type));
                    }

                    to += n_elem_block * _NclSizeOf(varnode->type);
                 /*
                  * No reverse here so all is just added
                  */
                    for(i = block_read_limit; i >= 0 ; i--)
                    {
                        switch(compare_sel[i])
                        {
                        case NCLFILE_DEC:
                            if((current_index[i] + real_stride[i] < finish[i])&&(i != 0))
                            {
                                current_index[i] = start[i];
                                current_finish[i] = current_index[i];
                            }
                            else
                            {
                                current_index[i] +=  real_stride[i];
                                current_finish[i] = current_index[i];
                                inc_done = 1;
                            }
                            break;
                        case NCLFILE_INC:
                            if((current_index[i] + real_stride[i]> finish[i])&&(i != 0))
                            {
                                current_index[i] = start[i];
                                current_finish[i] = current_index[i];
                            }
                            else
                            {
                                current_index[i] +=  real_stride[i];
                                current_finish[i] = current_index[i];
                                inc_done = 1;
                            }
                            break;
                        }

                        if(inc_done)
                        {
                            inc_done = 0;
                            break;
                        }
                    }

                    if(compare_sel[0] == NCLFILE_INC)
                    {
                        if(current_index[0] > finish[0])
                        {
                            done = 1;
                        }
                    }
                    else
                    {
                        if(current_index[0] < start[0])
                        {
                            done = 1;
                        }
                    }
                    current_finish[0] = current_index[0];
                }

                NclFree(swap_space);
                n_dims_output = n_dims_input;

                if(sel_ptr != NULL) 
                {
                    i = 0;
                    while((i <  n_dims_output)&&(n_dims_output > 1))
                    {
                        if((output_dim_sizes[i] == 1)&&!(keeper[i]))
                        {
                            for(j = i; j < n_dims_output-1; j++)
                            {
                                output_dim_sizes[j] = output_dim_sizes[j+1];
                                (dim_info)[j] = (dim_info)[j+1];
                            }
                            n_dims_output--;
                        }
                        else
                        {
                            i++;
                        }
                    }
                }
            }
            else
            {
             /*
              * has vectors or reorder or both
              */
                val = (void*)NclMalloc(total_elements*_NclSizeOf(varnode->type));
                assert(val);
                to = 0;
                block_read_limit = n_dims_input - 1 ;
             /*
              * Find out what size chunks can be read in at once
              */
                for(i = n_dims_input-1 ; i>= 0; i--)
                {
                    if((compare_sel[index_map[i]] != NCLFILE_INC)||(index_map[i] != i)||(stride[index_map[i]] != 1))
                    {
                        block_read_limit = i;
                        break;
                    }
                }

             /*
              * Initialize starting index, finish and stride values for first read
              */
                n_elem_block = 1;
                for(i = 0; i < n_dims_input ; i++)
                {
                    current_index[index_map[i]] = start[index_map[i]];
                    if(i > block_read_limit)
                    {
                     /*
                     * OK to use i here since these indices are in order
                     * also above loop filter strides so stride = 1 ###
                     */
                        n_elem_block *= output_dim_sizes[index_map[i]];
                        current_finish[index_map[i]] = finish[index_map[i]];
                        real_stride[index_map[i]] = stride[index_map[i]];
                    }
                    else
                    {
                        switch(compare_sel[index_map[i]])
                        {
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

                while(!done)
                {
                    if(vtype == FILE_VAR_ACCESS)
                    {
                        (*thefile->advancedfile.format_funcs->read_var_ns)
                         (thefile->advancedfile.grpnode, var_name,
                          current_index, current_finish,
                          (void*)&(((char*)val)[to]));
                    }
                    else
                    {
                        (*thefile->advancedfile.format_funcs->read_coord_ns)
                         (thefile->advancedfile.grpnode, var_name,
                          current_index, current_finish,
                          (void*)&(((char*)val)[to]));
                    }

                    to += n_elem_block * _NclSizeOf(varnode->type);
                    if(compare_sel[index_map[block_read_limit]] < 0)
                    {
                        current_index[index_map[block_read_limit]] += real_stride[index_map[block_read_limit]];
                        current_finish[index_map[block_read_limit]] = current_index[index_map[block_read_limit]];
                    }
                    else
                    {
                        compare_sel[index_map[block_read_limit]]++;
                    }
                    for(i = block_read_limit; i > 0; i--)
                    {
                        switch(compare_sel[index_map[i]])
                        {
                        case NCLFILE_INC:
                            if(current_index[index_map[i]] > finish[index_map[i]])
                            {
                                current_index[index_map[i]] = start[index_map[i]];
                                if(compare_sel[index_map[i-1]] < 0 )
                                {
                                    current_index[index_map[i-1]] += real_stride[index_map[i-1]];
                                }
                                else
                                {
                                    compare_sel[index_map[i-1]]++;
                                }
    
                            }
                            else
                            {
                                inc_done = 1;
                            }    
                            current_finish[index_map[i]] = current_index[index_map[i]] ;
                            break;
                        case NCLFILE_DEC:
                            if(current_index[index_map[i]] < finish[index_map[i]])
                            {
                                current_index[index_map[i]] = start[index_map[i]];
                                if(compare_sel[index_map[i-1]] < 0)
                                {
                                    current_index[index_map[i-1]] += real_stride[index_map[i-1]];
                                }
                                else
                                {
                                    compare_sel[index_map[i-1]]++;
                                }
                            }
                            else
                            {    
                                inc_done = 1;
                            }
                            current_finish[index_map[i]] = current_index[index_map[i]];
                            break;
                        default:
                            if(compare_sel[index_map[i]] >= sel[index_map[i]].u.vec.n_ind)
                            {
                                compare_sel[index_map[i]] = 0;
                                current_index[index_map[i]] = sel[index_map[i]].u.vec.ind[0];
                                if(compare_sel[index_map[i-1]] < 0 )
                                {
                                    current_index[index_map[i-1]] += real_stride[index_map[i-1]];
                                }
                                else
                                {
                                    compare_sel[index_map[i-1]]++;
                                }
                            }
                            else
                            {
                                current_index[index_map[i]] = sel[index_map[i]].u.vec.ind[compare_sel[index_map[i]]];
                                inc_done = 1;
                            }
                            current_finish[index_map[i]] = current_index[index_map[i]];
                            break;
                        } 
                        if(inc_done)
                        {
                            inc_done = 0;
                            break;
                        }
                    }
                    switch(compare_sel[index_map[0]])
                    {
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
                        if(compare_sel[index_map[0]] >= sel[0].u.vec.n_ind)
                        {
                                done = 1;
                        }
                        else
                        {
                            current_index[index_map[0]] = sel[0].u.vec.ind[compare_sel[index_map[0]]];
                        }
                        current_finish[index_map[0]] = current_index[index_map[0]]; 
                    }
                }
                n_dims_output = n_dims_input;
                if(sel_ptr != NULL)
                {
                    i = 0;
                    while((i <  n_dims_output)&&(n_dims_output > 1))
                    {
                        if((output_dim_sizes[i] == 1)&&!(keeper[i]))
                        {
                            for(j = i; j < n_dims_output-1; j++)
                            {
                                output_dim_sizes[j] = output_dim_sizes[j+1];
                                (dim_info)[j] = (dim_info)[j+1];
                            }
                            n_dims_output--;
                        }
                        else
                        {
                            i++;
                        }
                    }
                }
            }
        } 
    } 

  /*
   *if(AdvancedFileIsVarAtt(infile,var_name,NrmStringToQuark(NCL_MISSING_VALUE_ATT)) != -1)
   */
    if(AdvancedVarAttIndex(varnode, NrmStringToQuark(NCL_MISSING_VALUE_ATT)) != -1)
    {
        mis_md = AdvancedFileReadVarAtt(infile,var_name,NrmStringToQuark(NCL_MISSING_VALUE_ATT),NULL);
        if(mis_md != NULL)
        {
            has_missing = 1;
            if (mis_md->multidval.val == NULL)
            {
                NHLPERROR((NhlWARNING,NhlEUNKNOWN,
                      "MyAdvancedFileReadVarValue: _FillValue attribute for  variable (%s) in file (%s) has NULL value, %s\n",
                       NrmQuarkToString(var_name),NrmQuarkToString(thefile->advancedfile.fname),
                      "substituting default fill value of variable type"));
                _NclGetDefaultFillValue(varnode->type,&missing_value);
            }
            if (mis_md->multidval.data_type == varnode->type)
            {
                memcpy((void*)&missing_value,mis_md->multidval.val,
                       _NclSizeOf(mis_md->multidval.data_type));
            }
            else if (NCL_enum == varnode->type)
            {
                memcpy((void*)&missing_value,mis_md->multidval.val,
                       _NclSizeOf(mis_md->multidval.data_type));
            }
            else
            {
                NHLPERROR((NhlWARNING,NhlEUNKNOWN,
                    "MyAdvancedFileReadVarValue: _FillValue attribute type differs from variable (%s) type in file (%s), %s\n",
                     NrmQuarkToString(var_name),NrmQuarkToString(thefile->advancedfile.fname),
                    "forcing type conversion; may result in overflow and/or loss of precision"));

                _NclScalarForcedCoerce(mis_md->multidval.val, mis_md->multidval.data_type,
                                      (void*)&missing_value, varnode->type);
            }

        }
    } 

    if(vtype == FILE_COORD_VAR_ACCESS)
    {
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
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(varnode->type))
                 );
    }
    else
    {
        NclBasicDataTypes ncl_type = varnode->type;

        if(NCL_compound == varnode->type)
        {
            if(NULL != compnode)
            {
                ncl_type = compnode->type;

              /*
               *i = 0;
               *(dim_info)[i].dim_num = i;
               *(dim_info)[i].dim_size = (ng_size_t)compnode->nvals;
               *(dim_info)[i].dim_quark = compnode->name;
               *n_dims_output = n_dims_input;
               *output_dim_sizes[i] = compnode->nvals;
               */

                n_dims_output = varnode->dim_rec->n_dims;

                for(i = 0; i < varnode->dim_rec->n_dims; ++i)
                {
                    (dim_info)[i].dim_num = i;
                    (dim_info)[i].dim_size = (ng_size_t)varnode->dim_rec->dim_node[i].size;
                    (dim_info)[i].dim_quark = varnode->dim_rec->dim_node[i].name;
                    output_dim_sizes[i] = (ng_size_t)varnode->dim_rec->dim_node[i].size;
                }

                if(1 < compnode->nvals)
                {
                    i = n_dims_output;
                    (dim_info)[i].dim_num = i;
                    (dim_info)[i].dim_quark = compnode->name;
                    if(NCL_string == compnode->type)
                    {
                        (dim_info)[i].dim_size = 1;
                        output_dim_sizes[i] = 1;
                    }
                    else
                    {
                        (dim_info)[i].dim_size = (ng_size_t)compnode->nvals;
                        output_dim_sizes[i] = compnode->nvals;
                    }
                    ++n_dims_output;
                }
            }
            else
            {
                ncl_type = NCL_list;

                (dim_info)[0].dim_num = 0;
                (dim_info)[0].dim_size = 1;
                (dim_info)[0].dim_quark = NrmStringToQuark("CompoundAsList");

                n_dims_output = 1;
                output_dim_sizes[0] = 1;

              /*
               *NHLPERROR((NhlFATAL,NhlEUNKNOWN,
               *    "NclAdvancedFile: Could not get compound data type."));
               */
            }
	}
        else if(NCL_opaque == varnode->type)
        {
            if(NULL != opaquerec)
            {
                ncl_type = opaquerec->type;

                if(opaquerec->size > 1)
                {
                    i = n_dims_input;
                    (dim_info)[i].dim_num = i;
                    (dim_info)[i].dim_size = (ng_size_t)opaquerec->size;
                    (dim_info)[i].dim_quark = opaquerec->name;
                    n_dims_output = n_dims_input + 1;
                    output_dim_sizes[i] = opaquerec->size;
                }
              /*
               *fprintf(stderr, "\n\tfile: %s, line: %d\n", __FILE__, __LINE__);
               *fprintf(stderr, "\ttotal_elements = %d\n", total_elements);
               */

                val = (void *)NclCalloc(total_elements * opaquerec->size,
                              _NclSizeOf(ncl_type));
                assert(val);

                memcpy(val, opaquerec->values,
                       total_elements * opaquerec->size * _NclSizeOf(ncl_type));
                NclFree(opaquerec->values);
                NclFree(opaquerec);
            }
            else
            {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                    "NclAdvancedFile: Could not get opaque data type."));
            }
	}
        else if(NCL_enum == varnode->type)
        {
            if(NULL != enumrec)
            {
                ncl_type = enumrec->type;

              /*
               *fprintf(stderr, "\n\tfile: %s, line: %d\n", __FILE__, __LINE__);
               *fprintf(stderr, "\ttotal_elements = %d\n", total_elements);
               *fprintf(stderr, "\tenumrec->size = %d\n", enumrec->size);
               */

                val = (void *)NclCalloc(total_elements, _NclSizeOf(ncl_type));
                assert(val);

                memcpy(val, enumrec->values, total_elements * _NclSizeOf(ncl_type));
                NclFree(enumrec->values);
                NclFree(enumrec->enum_node);
                NclFree(enumrec);
            }
            else
            {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                    "NclAdvancedFile: Could not get enum data type."));
            }
	}
        else if(NCL_reference == varnode->type)
        {
          /*
           *fprintf(stderr, "\n\tfile: %s, line: %d\n", __FILE__, __LINE__);
           *fprintf(stderr, "\ttotal_elements = %d\n", total_elements);
           */
            ncl_type = NCL_string;
	}
#if 0
        else if(NCL_vlen == varnode->type)
        {
              /*
               *fprintf(stderr, "\n\tfile: %s, line: %d\n", __FILE__, __LINE__);
               *fprintf(stderr, "\ttotal_elements = %d\n", total_elements);
               */
	}
        else
        {
              /*
               *fprintf(stderr, "\n\tfile: %s, line: %d\n", __FILE__, __LINE__);
               *fprintf(stderr, "\ttotal_elements = %d\n", total_elements);
               */
	}
#endif

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
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(ncl_type))
                 );
    }
    return(tmp_md);
}

static struct _NclMultiDValDataRec* AdvancedFileReadVarValue(NclFile thefile, NclQuark var_name,
                                                        struct _NclSelectionRecord* sel_ptr)
{
    NclDimRec dim_info[NCL_MAX_DIMENSIONS];

    return(MyAdvancedFileReadVarValue(thefile, var_name, sel_ptr,dim_info,FILE_VAR_ACCESS));
}

static struct _NclVarRec *AdvancedFileReadVar(NclFile infile, NclQuark var_name,
                                         struct _NclSelectionRecord* sel_ptr)
{
    NclAdvancedFile thefile = (NclAdvancedFile) infile;
    NclMultiDValData tmp_md = NULL;
    NclMultiDValData tmp_att_md = NULL;
    NclVar tmp_var = NULL;
    int i,j=0;
    int att_id = -1;
    NclSelectionRecord tmp_sel;
    NclDimRec dim_info[NCL_MAX_DIMENSIONS];
    int coords[NCL_MAX_DIMENSIONS];
    NclSelection *sel = NULL;
    NclObj  att_obj = NULL;
    int single = 0;

    NclFileVarNode *varnode;
    NclFileVarNode *coordnode;
    NclFileDimNode *dimnode;

/*
* By the the time it gets here the file suport routines in that build the selection
* record have made sure var_name is valid and all the demensions in sel_ptr
* are valid. However, the values have not been checked for out_of_ranges
* subscripts
*/

    varnode = _getVarNodeFromNclFileGrpNode(thefile->advancedfile.grpnode, var_name);

    if(NULL != varnode)
    {
        for(i = 0 ; i < NCL_MAX_DIMENSIONS; i++)
        {
            coords[i] = -1;
            dim_info[i].dim_quark = -1;
            dim_info[i].dim_num = -1;
            dim_info[i].dim_size = 0;
        }

        tmp_md = MyAdvancedFileReadVarValue(infile,var_name,sel_ptr,dim_info,FILE_VAR_ACCESS);
        if(tmp_md == NULL)
            return(NULL);

        if(NULL != varnode->att_rec)
        {
            if(varnode->att_rec->id < 0)
                AdvancedLoadVarAtts(thefile,var_name);

            att_id = varnode->att_rec->id;

            att_obj = (NclObj)_NclCopyAtt((NclAtt)_NclGetObj(att_id),NULL);
            if(! att_obj)
	    {
                att_id = -1;
            }
            else
	    {
                att_id = att_obj->obj.id;
                if (_NclIsAtt(att_id,"_FillValue"))
                {
                    tmp_att_md = _NclGetAtt(att_id,"_FillValue",NULL);
                    if (tmp_att_md->multidval.data_type != tmp_md->multidval.data_type || tmp_att_md->multidval.val == NULL)
                    {
                        ng_size_t tmp_size = 1;
                        NclScalar *tmp_mis = (NclScalar*)NclMalloc((unsigned)sizeof(NclScalar));
                        *tmp_mis = tmp_md->multidval.missing_value.value;
                        tmp_att_md = _NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,
                                         (void*)tmp_mis,NULL,1,&tmp_size,TEMPORARY,NULL,
                                         tmp_md->multidval.type);
                        _NclDeleteAtt(att_id,"_FillValue");
                        _NclAddAtt(att_id,"_FillValue",tmp_att_md,NULL);
                    }
                }
            }
        }

        if(NCL_list == varnode->type)
        {
            sel = NULL;
        }
        else
        {
            NclQuark coordvarname;
            char cvnhead[1024];
            char cvn[1024];
            char *varstr = NrmQuarkToString(var_name);
            char *cptr = strrchr(varstr, '/');
	    int head_leng = 0;

            if(NULL != cptr)
            {
                i = 1 + strlen(varstr) - strlen(cptr);
                strncpy(cvnhead, varstr, i);
                cvnhead[i] = '\0';
                head_leng = i;

              /*
               *fprintf(stderr, "\nfile: %s, line: %d\n", __FILE__, __LINE__);
               *fprintf(stderr, "\ncvnhead: <%s>, varstr: <%s>, cptr: <%s>\n", cvnhead, varstr, cptr);
               */
            }

            if(sel_ptr == NULL)
            {
                for(i = 0 ; i < tmp_md->multidval.n_dims; i++)
                {
                    if(0 < dim_info[i].dim_quark)
                    {
                        if(head_leng)
			{
                            strcpy(cvn, cvnhead);
                            strcat(cvn, NrmQuarkToString(dim_info[i].dim_quark));
                            coordvarname = NrmStringToQuark(cvn);
			}
			else
                            coordvarname = dim_info[i].dim_quark;

                        coordnode = _getCoordVarNodeFromNclFileGrpNode(thefile->advancedfile.grpnode, coordvarname);
                        if(NULL != coordnode)
                        {
                            tmp_var = _NclFileReadCoord((NclFile)thefile,coordvarname,NULL);
                            if(tmp_var != NULL)
                                coords[i] = tmp_var->obj.id;
                            else
                                coords[i] = -1;
                        }
                    }
                    else
                    {
                        coords[i] = -1;
                    }
                }
                sel = NULL;
            }
            else
            {
                sel = sel_ptr->selection;
                tmp_sel.n_entries = 1;
                tmp_sel.selected_from_sym = NULL;
                tmp_sel.selected_from_var = NULL;
                tmp_sel.selection[0].dim_num = 0;
                j = 0;
                for(i = 0 ; i < varnode->dim_rec->n_dims; i++)
                {
                    dimnode = &(varnode->dim_rec->dim_node[i]);

                    if(head_leng)
                    {
                        strcpy(cvn, cvnhead);
                        strcat(cvn, NrmQuarkToString(dimnode->name));
                        coordvarname = NrmStringToQuark(cvn);
                    }
                    else
                        coordvarname = dimnode->name;

                    coordnode = _getCoordVarNodeFromNclFileGrpNode(thefile->advancedfile.grpnode,
                                                                   coordvarname);

                    if(NULL != coordnode)
                    {
                        tmp_sel.selection[0] = sel[i];
                        tmp_sel.selection[0].dim_num = 0;
                        tmp_var = _NclFileReadCoord((NclFile)thefile,coordvarname,&tmp_sel);
                        if(tmp_var != NULL)
                        {
                            if(sel[i].sel_type == Ncl_VECSUBSCR)
                            {
                                if((tmp_var->var.n_dims == 1)&&(tmp_var->var.dim_info[0].dim_size == 1))
                                    single = 1;
                            }
                            else
                            {
                                if(sel[i].u.sub.start == sel[i].u.sub.finish)
                                    single = sel[i].u.sub.is_single;
                            }
                            coords[j] = tmp_var->obj.id;
                        }
                        else
                        {
                            return(NULL);
                        }
                    }
                    else
                    {
                        switch(sel[i].sel_type)
                        {
                            case Ncl_VECSUBSCR:
                                if(sel[i].u.vec.n_ind == 1)
                                    single = 1;
                                break;
                            case Ncl_SUB_ALL:
                                if(dimnode->size == 1)
                                    single = 0;
                                break;
                            case Ncl_SUB_VAL_DEF:
                                if(sel[i].u.sub.start == dimnode->size - 1)
                                    single = 0;
                                break;
                            case Ncl_SUB_DEF_VAL:
                                if(sel[i].u.sub.finish == 0)
                                    single = 0;
                                break;
                            case Ncl_SUBSCR:
                                if(sel[i].u.sub.start == sel[i].u.sub.finish)
                                    single = sel[i].u.sub.is_single;
                                break;
                        }
                        coords[j] = -1;
                    }
                    if(single)
                    {
                        if(coords[j] != -1)
                        {
			        NclMultiDValData coord_md = _NclVarValueRead(tmp_var,NULL,NULL);
			        if(att_id == -1)
			        {
				        att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);
			        } 
			        _NclAddAtt(att_id,NrmQuarkToString(tmp_var->var.var_quark),coord_md,&tmp_sel);

			        coords[j] = -1;
			        if(tmp_var->obj.status != PERMANENT) {
				        _NclDestroyObj((NclObj)tmp_var);
			        }

/*Wei's change
			    if(NULL != attnode)
			    {
				    NclMultiDValData coord_md = _NclVarValueRead(tmp_var,NULL,NULL);
                        	    _NclAddAtt(att_id,NrmQuarkToString(attnode->name),coord_md,&tmp_sel);
			    }

                            coords[j] = -1;
                            if(tmp_var->obj.status != PERMANENT) {
                                _NclDestroyObj((NclObj)tmp_var);
                            }
*/
                        }
                        single = 0;
                    }
                    else
                    {
                        j++;
                    }
                }
            }
        }
    
        tmp_var = NULL;
        if(tmp_md != NULL)
        {
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

            if(tmp_var == NULL)
            {
                _NclDestroyObj((NclObj)tmp_md);
                if(att_id != -1)
                    _NclDestroyObj((NclObj)_NclGetObj(att_id));
            }
        }
    }

    return(tmp_var);
}

void AdvancedLoadVarAtts(NclAdvancedFile thefile, NclQuark var)
{
    NclFileVarNode   *varnode;
    NclFileAttRecord *attrec;
    NclFileAttNode   *attnode;
    int att_id = -1;
    void *val;
    NclMultiDValData tmp_md;
    NhlArgVal udata;
    ng_size_t ne;    
    int n;

    varnode = _getVarNodeFromNclFileGrpNode(thefile->advancedfile.grpnode, var);

    if(NULL == varnode)
        return;

    attrec = varnode->att_rec;

    if(NULL == attrec)
        return;

    if(0 < attrec->id)
        return;

    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,(NclObj)thefile);
    for(n = 0; n < attrec->n_atts; n++)
    {
        attnode = &(attrec->att_node[n]);
        if(NCL_none == attnode->type)
            val = NULL;
        else
            val = attnode->value;

        ne = attnode->n_elem;
	if (attnode->type == NCL_reference || attnode->type == NCL_compound ||
	    (attnode->type == NCL_vlen && attnode->base_type == NCL_reference))
	    continue;
        tmp_md = _NclCreateMultiDVal(
                      NULL, NULL,
                      Ncl_MultiDValData,
                      0, val, NULL, 1,
                      &ne, TEMPORARY, NULL,
                      _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(attnode->type)));

        if(tmp_md != NULL)
            _NclAddAtt(att_id, NrmQuarkToString(attnode->name),tmp_md,NULL);
    }

    udata.ptrval = (void*)NclCalloc(1, sizeof(FileCallBackRec));
    assert(udata.ptrval);
    ((FileCallBackRec*)udata.ptrval)->thefileid = thefile->obj.id;
    ((FileCallBackRec*)udata.ptrval)->theattid = att_id;
    ((FileCallBackRec*)udata.ptrval)->thevar = var;
    attrec->cb = _NclAddCallback((NclObj)_NclGetObj(att_id),NULL,
                                 FileAttIsBeingDestroyedNotify,ATTDESTROYED,&udata);
    attrec->udata = (FileCallBackRec*)udata.ptrval;
    attrec->id = att_id;
}

static struct _NclMultiDValDataRec *AdvancedFileReadVarAtt(NclFile infile,
                                                      NclQuark var,
                                                      NclQuark attname,
                                                      struct _NclSelectionRecord *sel_ptr)
{
    NclAdvancedFile thefile = (NclAdvancedFile) infile;
    NclFileVarNode *varnode;
    NclMultiDValData tmp_md;

    varnode = _getVarNodeFromNclFileGrpNode(thefile->advancedfile.grpnode, var);

    if(NULL != varnode)
    {
        NclScalar missing_value;
        ng_size_t dim_size = 1;
        char *type_name;
        NclTypeClass type_class;

	tmp_md = NULL;
	if (varnode->att_rec) {
		if(varnode->att_rec->id < 0)
			AdvancedLoadVarAtts(thefile, var);

		tmp_md = _NclGetAtt(varnode->att_rec->id,NrmQuarkToString(attname),sel_ptr);
	}
        if(NULL == tmp_md)
	{
    		NHLPERROR((NhlWARNING, NhlEUNKNOWN,
        		"AdvancedFileReadVarAtt: (%s) is not an attribute of (%s)",
         		NrmQuarkToString(attname),NrmQuarkToString(var)));
    		return(_NclCreateMissing());
	}

        if (attname != NrmStringToQuark("_FillValue")) 
            return (tmp_md);
        else if (tmp_md->multidval.val == NULL)
        {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
                  "AdvancedFileReadVarAtt: _FillValue attribute for  variable (%s) in file (%s) has NULL value, substituting default fill value of variable type",
                  NrmQuarkToString(var),NrmQuarkToString(thefile->advancedfile.fname));
        }
        else if (tmp_md->multidval.data_type == varnode->type)
            return (tmp_md);
        else if (NCL_enum == varnode->type)
            return (tmp_md);
        
        _NclGetDefaultFillValue(varnode->type,&missing_value);
        type_name = _NclBasicDataTypeToName(varnode->type);
        type_class = _NclNameToTypeClass(NrmStringToQuark(type_name));
        return (_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void*)&missing_value,NULL,1,&dim_size,PERMANENT,NULL,type_class));
    }

    NHLPERROR((NhlWARNING,NhlEUNKNOWN,
        "AdvancedFileReadVarAtt: (%s) is not an attribute of (%s)",
         NrmQuarkToString(attname),NrmQuarkToString(var)));
    return(_NclCreateMissing());
}

static struct _NclVarRec* AdvancedFileReadCoord(NclFile infile, NclQuark coord_name,
                                           struct _NclSelectionRecord* sel_ptr)
{
    NclAdvancedFile thefile = (NclAdvancedFile) infile;
    NclSelection *sel;
    NclMultiDValData tmp_md;
    NclDimRec dim_info[NCL_MAX_DIMENSIONS];
    int att_id = -1;
    NclObj att_obj = NULL;
    NclVar tmp_var = NULL;

    NclFileVarNode *varnode;

  /*
   *fprintf(stderr, "\nEnter AdvancedFileReadCoord, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tcoord_name: <%s>\n", NrmQuarkToString(coord_name));
   */

    varnode = _getVarNodeFromNclFileGrpNode(thefile->advancedfile.grpnode, coord_name);

    if(NULL != varnode)
    {
        tmp_md = MyAdvancedFileReadVarValue(infile,coord_name,sel_ptr,dim_info,
                                       FILE_COORD_VAR_ACCESS);
        if(NULL == tmp_md) 
            return(NULL);

        if(NULL != varnode->att_rec) 
        {
            if(varnode->att_rec->id < 0) 
                AdvancedLoadVarAtts(thefile, coord_name);

            att_id = varnode->att_rec->id;
    
            att_obj = (NclObj)_NclCopyAtt((NclAtt)_NclGetObj(att_id),NULL);
            if(att_obj != NULL)
                att_id = att_obj->obj.id;
            else
                att_id = -1;
        }

        if(NULL != sel_ptr)
            sel = sel_ptr->selection;
        else
            sel = NULL;

        tmp_var = _NclCoordVarCreate(
                  NULL, NULL, Ncl_CoordVar, 0, NULL,
                  tmp_md, dim_info, att_id, NULL,
                  ((sel== NULL)? COORD:COORDSUBSEL),
                  NrmQuarkToString(coord_name),
                  TEMPORARY);

        if(NULL == tmp_var)
        {
            _NclDestroyObj((NclObj)tmp_md);
            if(att_obj != NULL)
                _NclDestroyObj((NclObj)att_obj);
        }

      /*
       *fprintf(stderr, "Leave AdvancedFileReadCoord, file: %s, line: %d\n\n", __FILE__, __LINE__);
       */
        return(tmp_var);
    }

    NhlPError(NhlFATAL,NhlEUNKNOWN,
        "(%s) is no  a coordinate variable for file (%s)",
        NrmQuarkToString(coord_name),
        NrmQuarkToString(thefile->advancedfile.fname));

    return(NULL);
}

static int AdvancedFileIsCoord(NclFile infile, NclQuark coord_name)
{
    NclAdvancedFile thefile = (NclAdvancedFile) infile;
    NclFileVarNode *varnode;

    varnode = _getVarNodeFromNclFileGrpNode(thefile->advancedfile.grpnode, coord_name);
    if(NULL != varnode)
    {
        int n;
        NclFileCoordVarRecord *coord_var_rec = thefile->advancedfile.grpnode->coord_var_rec;
        if(NULL != coord_var_rec)
        {
            for(n = 0; n < coord_var_rec->n_vars; ++n)
            {
                if(coord_name == coord_var_rec->var_node[n]->name)
                    return n;
                if(coord_name == coord_var_rec->var_node[n]->real_name)
                    return n;
            }
        }
    }

    return (-1);
}

static NhlErrorTypes AdvancedFileWriteAtt(NclFile infile, NclQuark attname,
                                     struct _NclMultiDValDataRec* value,
                                     struct _NclSelectionRecord *sel_ptr)
{
    NclAdvancedFile thefile = (NclAdvancedFile) infile;
    int exists = 0;
    NclMultiDValData tmp_att_md,tmp_md;
    int att_id;
    NhlErrorTypes ret = NhlNOERROR;
    NclBasicDataTypes from_type,to_type;
    NclObjTypes obj_type;
    void *data_type;
    NhlArgVal udata;
    NclFileAttRecord *attrec;

  /*
   *fprintf(stderr, "\nEnter AdvancedFileWriteAtt, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tattname: <%s>\n", NrmQuarkToString(attname));
   */

    if(thefile->advancedfile.wr_status<=0)
    {
        if(NULL == thefile->advancedfile.grpnode->att_rec)
        {
            att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,(NclObj)thefile);
          /*
           *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
           *fprintf(stderr, "\tatt_id: %d\n", att_id);
           */
            udata.ptrval = (void*)NclMalloc(sizeof(FileCallBackRec));
            ((FileCallBackRec*)udata.ptrval)->thefileid = thefile->obj.id;
            ((FileCallBackRec*)udata.ptrval)->theattid = att_id;
            ((FileCallBackRec*)udata.ptrval)->thevar = -1;

	    thefile->advancedfile.grpnode->att_rec = _NclFileAttAlloc(NCL_MINIMUM_ATTS);
	    thefile->advancedfile.grpnode->att_rec->n_atts = 0;
	    thefile->advancedfile.grpnode->att_rec->id = att_id;
	    thefile->advancedfile.grpnode->att_rec->cb = _NclAddCallback((NclObj)_NclGetObj(att_id),NULL,
						 FileAttIsBeingDestroyedNotify,ATTDESTROYED,&udata);
	    thefile->advancedfile.grpnode->att_rec->udata = (FileCallBackRec*)udata.ptrval;
        }
        else
        {
            att_id = thefile->advancedfile.grpnode->att_rec->id;
          /*
           *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
           *fprintf(stderr, "\tatt_id: %d\n", att_id);
           */
        }

        ret = _addNclAttNode(&(thefile->advancedfile.grpnode->att_rec),
                             attname, value->multidval.data_type,
                             value->multidval.totalelements, value->multidval.val);
      /*
       *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\tvalue->multidval.data_type: <%s>\n",
       *                 _NclBasicDataTypeToName(value->multidval.data_type));
       */

        exists = _NclIsAtt(att_id,NrmQuarkToString(attname));
      /*
       *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\texists = %d\n", exists);
       */
        if((exists)&&(thefile->advancedfile.format_funcs->write_att != NULL))
        {
/*
 * Hereis the trick. It is easier to let the _NclAddAtt... functions deal
 * with the coercion than to figure out what it should be 
 */
          /*
           *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
           *fprintf(stderr, "\texists = %d\n", exists);
           */
            ret = _NclAddAtt(att_id,NrmQuarkToString(attname),value,sel_ptr);
            if(ret < NhlWARNING)
            {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                    "Could not write attribute (%s) to attribute list",
                    NrmQuarkToString(attname)));
                ret = NhlFATAL;
                goto done_AdvancedFileWriteAtt;
            }

            tmp_att_md = _NclGetAtt(att_id,NrmQuarkToString(attname),NULL);
            ret = (*thefile->advancedfile.format_funcs->write_att)(
                thefile->advancedfile.grpnode,
                attname,
                tmp_att_md->multidval.val
                );
            goto done_AdvancedFileWriteAtt;
        }
        else if((!exists)&&(thefile->advancedfile.format_funcs->add_att != NULL))
        {
          /*
           *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
           *fprintf(stderr, "\texists = %d\n", exists);
           */
            attrec = thefile->advancedfile.grpnode->att_rec;

            if(value->multidval.data_type == NCL_char)
            {
                tmp_md = _NclCharMdToStringMd(value);
                ret = _NclAddAtt(att_id,NrmQuarkToString(attname),tmp_md,sel_ptr);
                if(ret < NhlWARNING)
                {
                    NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                        "Could not write attribute (%s) to attribute list",
                        NrmQuarkToString(attname)));
                    ret = NhlFATAL;
                    goto done_AdvancedFileWriteAtt;
                }
                ret = (*thefile->advancedfile.format_funcs->add_att)(
                    thefile->advancedfile.grpnode,
                    attname,
                    value->multidval.data_type,
                    value->multidval.totalelements,
                    value->multidval.val
                    );
                if(ret > NhlFATAL)
                {
                    thefile->advancedfile.grpnode->att_rec = attrec;
                }
                if (ret < NhlNOERROR)
                {
                    _NclDeleteAtt(att_id,NrmQuarkToString(attname));
                }
            }
            else
            {
                if((data_type = (*thefile->advancedfile.format_funcs->map_ncl_type_to_format)(value->multidval.data_type)) == NULL)
                {
                    if(value->multidval.data_type == NCL_string)
                    {
                        goto done_AdvancedFileWriteAtt;
                    }
                    else
                    {
                        from_type = value->multidval.data_type;
                        to_type = _NclPromoteType(from_type);
                        while((from_type != to_type )&&
                             ((data_type = (*thefile->advancedfile.format_funcs->map_ncl_type_to_format)(to_type))==NULL))
                        {
                            from_type = to_type;
                            to_type = _NclPromoteType(from_type);
                        }

                        if(data_type != NULL)
                        {
                            NclFree(data_type);
                        }

                        if((data_type = (*thefile->advancedfile.format_funcs->map_ncl_type_to_format)(to_type))==NULL)
                        {
                            NhlPError(NhlFATAL,NhlEUNKNOWN,
                                "The type (%s) is not representable as an attribute in the file (%s)",
                                _NclBasicDataTypeToName(to_type),NrmQuarkToString(thefile->advancedfile.fpath));
                            ret = NhlFATAL;
                            goto done_AdvancedFileWriteAtt;
                        }
                        else
                        {
                            NclFree(data_type);
                            obj_type = _NclBasicDataTypeToObjType(to_type);
                            tmp_md = _NclCoerceData(value,obj_type,NULL);
                        }
                    }
                }
                else
                {
                    NclFree(data_type);
                    tmp_md= value;
                }

              /*
               *ret = _NclAddAtt(att_id,NrmQuarkToString(attname),tmp_md,sel_ptr);
               *if(ret < NhlWARNING)
               *{
               *    NHLPERROR((NhlFATAL,NhlEUNKNOWN,
               *        "Could not write attribute (%s) to attribute list",
               *        NrmQuarkToString(attname)));
               *    ret = NhlFATAL;
               *    goto done_AdvancedFileWriteAtt;
               *}
               */

                ret = (*thefile->advancedfile.format_funcs->add_att)(
                    thefile->advancedfile.grpnode,
                    attname,
                    tmp_md->multidval.data_type,
                    tmp_md->multidval.totalelements,
                    tmp_md->multidval.val
                    );

                if((tmp_md != value)&&(tmp_md->obj.status != PERMANENT))
                {
                    _NclDestroyObj((NclObj)tmp_md);
                }

                if(NhlWARNING >= ret)
                {
                    _NclDeleteAtt(att_id,NrmQuarkToString(attname));
                }
            }
        } 
    }
    else
    {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
            "FileWriteAtt: file (%s) was opened for reading only, can not write",
            NrmQuarkToString(thefile->advancedfile.fname));
        ret = NhlFATAL;
    }

done_AdvancedFileWriteAtt:
  /*
   *fprintf(stderr, "Leave AdvancedFileWriteAtt, file: %s, line: %d\n\n", __FILE__, __LINE__);
   */

    return(ret);
}

static NhlErrorTypes AdvancedFileSetFileOption(NclFile  infile,
                                          NclQuark format,
                                          NclQuark option,
                                          NclMultiDValData value)
{
    NclAdvancedFile thefile = (NclAdvancedFile) infile;
    int i, found, idx;
    NclMultiDValData tmp_md;
    NclQuark loption;
    NclQuark *lvalue = NULL;
    NclFileClassPart *fcp = &(nclAdvancedFileClassRec.file_class);
    NclFormatFunctionRecPtr ffrp = NULL;
    
    loption = _NclGetLower(option);
    if (thefile)
    {
        if (thefile->advancedfile.format_funcs->set_option == NULL)
        {
            NHLPERROR((NhlWARNING,NhlEUNKNOWN,
                  "advancedfile does not support any options"));
            return(NhlWARNING);
        }

        found = 0;
        for (i = 0; i < fcp->num_options; i++)
        {
            if (fcp->options[i].name != loption)
                continue;

            ffrp = _NclGetFormatFuncsWithAdvancedFileStructure(fcp->options[i].format);
            if (thefile->advancedfile.format_funcs == ffrp)
            {
                found = 1;
                idx = i;
                break;
            }
        }

        if(found)
        {
            i = idx;

            if (fcp->options[i].access == 1 && thefile->advancedfile.wr_status != 1)
            {
                NHLPERROR((NhlWARNING,NhlEUNKNOWN,
                    "%option %s is invalid unless file is opened for reading only",
                      NrmQuarkToString(option)));
                return(NhlWARNING);
            }
            else if (fcp->options[i].access == 2 && thefile->advancedfile.wr_status > 0)
            {
                NHLPERROR((NhlWARNING,NhlEUNKNOWN,
                    "AdvancedFileSetFileOption: option %s is invalid unless file is open for writing",
                      NrmQuarkToString(option)));
                return(NhlWARNING);
            }
            else if (fcp->options[i].access == 3 && thefile->advancedfile.wr_status != -1)
            {
                NHLPERROR((NhlWARNING,NhlEUNKNOWN,
                    "option %s can only be set prior to file creation",
                     NrmQuarkToString(option)));
                return(NhlWARNING);
            }

            if (! value)
            {
                /* if no value specified restore default for this file only - it's not an error */
                tmp_md = fcp->options[i].def_value;
                thefile->advancedfile.format_funcs->set_option(thefile->advancedfile.grpnode,
                                 loption,
                                 tmp_md->multidval.data_type,
                                 tmp_md->multidval.totalelements,
                                 tmp_md->multidval.val);
                return NhlNOERROR;
            }

            tmp_md = _NclCoerceData(value,fcp->options[i].value->multidval.type->type_class.type,NULL);
            if(tmp_md == NULL)
            {
                NHLPERROR((NhlWARNING,NhlEUNKNOWN,
                    "Invalid type for %s option value; value must be coercible to %s",
                     NrmQuarkToString(option),
                      NrmQuarkToString(_NclObjTypeToName(fcp->options[i].value->multidval.type->type_class.type))));
                return(NhlWARNING);
            }

            if(fcp->options[i].valid_values)
            {
                int ok = 0;
                int j,k;
                if(fcp->options[i].value->multidval.data_type == NCL_string)
                {
                    lvalue = NclMalloc(tmp_md->multidval.totalelements * sizeof(NclQuark));
                    ok = 0;
                    for (k = 0; k < tmp_md->multidval.totalelements; k++)
                    {
                        lvalue[k] = _NclGetLower(*(NclQuark*)(((char *)tmp_md->multidval.val)+ k * sizeof(NclQuark)));
                        for (j = 0; j < fcp->options[i].valid_values->multidval.totalelements; j++)
                        {
                            NclQuark valid_val = ((NclQuark *)fcp->options[i].valid_values->multidval.val)[j];
                            if (lvalue[k] != valid_val)
                                continue;
                            ok = 1;
                            break;
                        }
                    }

                    if(! ok)
                    {
                        NclFree(lvalue);
                        NHLPERROR((NhlWARNING,NhlEUNKNOWN,
                              "Invalid value supplied for option %s",
                               NrmQuarkToString(option)));
                        return(NhlWARNING);
                    }
                }
                else
                {
                    /* doesn't handle array valued options */
                    for (j = 0; j < fcp->options[i].valid_values->multidval.totalelements; j++)
                    {
                        if (memcmp(tmp_md->multidval.val,
                               (char*)fcp->options[i].valid_values->multidval.val +
                               j * tmp_md->multidval.type->type_class.size,
                               tmp_md->multidval.type->type_class.size))
                        {
                            continue;
                        }
                        ok = 1;
                        break;
                    }
                }

                if(! ok)
                {
                    NHLPERROR((NhlWARNING,NhlEUNKNOWN,
                          "Invalid value supplied for option %s",
                           NrmQuarkToString(option)));
                    return(NhlWARNING);
                }
            }

            if(lvalue)
            {
                thefile->advancedfile.format_funcs->set_option(thefile->advancedfile.grpnode,
                                 loption,
                                 tmp_md->multidval.data_type,
                                 tmp_md->multidval.totalelements,
                                (void *) lvalue);
                NclFree(lvalue);
            }
            else
            {
                thefile->advancedfile.format_funcs->set_option(thefile->advancedfile.grpnode,
                                 loption,
                                 tmp_md->multidval.data_type,
                                 tmp_md->multidval.totalelements,
                                 tmp_md->multidval.val);
            }

            if (tmp_md != value)
                _NclDestroyObj((NclObj)tmp_md);
            if (fcp->options[i].post_set_option)
            {
                return (*fcp->options[i].post_set_option)(infile);
            }

            return NhlNOERROR;
        }

        NHLPERROR((NhlWARNING,NhlEUNKNOWN,
              "%s is not a recognized file option for format %s",
               NrmQuarkToString(option),NrmQuarkToString(format)));
        return(NhlWARNING);
    }
    else if (format != NrmNULLQUARK)
    {
        for(i = 0; i < fcp->num_options; i++)
        {
            if(fcp->options[i].name != loption)
                continue;

            if(! (_NclGetFormatFuncs(format) &&
                  _NclGetFormatFuncs(format) == _NclGetFormatFuncs(fcp->options[i].format)) )
            {
                if(! (_NclGetLower(format) == NrmStringToQuark("bin") &&
                       fcp->options[i].format == _NclGetLower(format)) )
                {
                    if(_NclGetLower(fcp->options[i].format) != NrmStringToQuark("all"))
                    {
                        NHLPERROR((NhlWARNING,NhlEUNKNOWN,
                                   "%s is not a recognized option for format %s",
                                    NrmQuarkToString(option),NrmQuarkToString(format)));
                        return(NhlWARNING);
                    }
                }
            }

            if(! (_NclGetFormatFuncsWithAdvancedFileStructure(format) &&
                  _NclGetFormatFuncsWithAdvancedFileStructure(format) == _NclGetFormatFuncsWithAdvancedFileStructure(fcp->options[i].format)) )
            {
                if(! (_NclGetLower(format) == NrmStringToQuark("bin") &&
                       fcp->options[i].format == _NclGetLower(format)) )
                {
                    if(_NclGetLower(fcp->options[i].format) != NrmStringToQuark("all"))
                    {
                        NHLPERROR((NhlWARNING,NhlEUNKNOWN,
                                   "%s is not a recognized option for format %s",
                                    NrmQuarkToString(option),NrmQuarkToString(format)));
                             return(NhlWARNING);
                    }
                }
            }

            if (! value)
            {
                /* if no value specified restore default - it's not an error */
                tmp_md = fcp->options[i].def_value;
                memcpy(fcp->options[i].value->multidval.val,tmp_md->multidval.val,
                       tmp_md->multidval.type->type_class.size);
                return NhlNOERROR;
            }

            tmp_md = _NclCoerceData(value,fcp->options[i].value->multidval.type->type_class.type,NULL);
            if (tmp_md == NULL)
            {
                NHLPERROR((NhlWARNING,NhlEUNKNOWN,
                    "%Invalid type for %s option value; value must be coercible to %s",
                      NrmQuarkToString(option), 
                      NrmQuarkToString(_NclObjTypeToName(fcp->options[i].value->multidval.type->type_class.type))));
                return(NhlWARNING);
            }

            if (fcp->options[i].valid_values)
            {
                int ok = 0;
                int j,k;
                if(fcp->options[i].value->multidval.data_type == NCL_string)
                {
                    lvalue = NclMalloc(tmp_md->multidval.totalelements * sizeof(NclQuark));
                    ok = 0;
                    for (k = 0; k < tmp_md->multidval.totalelements; k++)
                    {
                        lvalue[k] = _NclGetLower(*(NclQuark*)(((char *)tmp_md->multidval.val)+ k * sizeof(NclQuark)));
                        for (j = 0; j < fcp->options[i].valid_values->multidval.totalelements; j++)
                        {
                            NclQuark valid_val = ((NclQuark *)fcp->options[i].valid_values->multidval.val)[j];
                            if(lvalue[k] != valid_val)
                                continue;
                            ok = 1;
                            break;
                        }
                    }

                    if(! ok)
                    {
                        NclFree(lvalue);
                        NHLPERROR((NhlWARNING,NhlEUNKNOWN,
                              "Invalid value supplied for option %s",
                               NrmQuarkToString(option)));
                        return(NhlWARNING);
                    }
                }
                else
                {
                    /* doesn't handle array valued options yet -- see the string handling */
                    for(j = 0; j < fcp->options[i].valid_values->multidval.totalelements; j++)
                    {
                        if(memcmp(tmp_md->multidval.val,
                               (char*)fcp->options[i].valid_values->multidval.val +
                               j * tmp_md->multidval.type->type_class.size,
                               tmp_md->multidval.type->type_class.size))
                        {
                            continue;
                        }
                        ok = 1;
                        break;
                    }
                }

                if(! ok)
                {
                    NHLPERROR((NhlWARNING,NhlEUNKNOWN,
                          "Invalid value supplied for option %s",
                           NrmQuarkToString(option)));
                    return(NhlWARNING);
                }
            }

            if(lvalue)
            {
                /* store the lower-cased name */
                NclFree(fcp->options[i].value->multidval.val);
                fcp->options[i].value->multidval.val = (void *)lvalue;
                fcp->options[i].value->multidval.totalelements = tmp_md->multidval.totalelements;
            }
            else
            {
                /* doesn't handle array valued options yet -- see the string handling */
                memcpy(fcp->options[i].value->multidval.val,tmp_md->multidval.val,
                       tmp_md->multidval.type->type_class.size);
            }

            if (tmp_md != value)
                _NclDestroyObj((NclObj)tmp_md);
            return NhlNOERROR;
        }
        NHLPERROR((NhlWARNING,NhlEUNKNOWN,
              "%s is not a recognized file option for format %s",
               NrmQuarkToString(option),NrmQuarkToString(format)));
        return(NhlWARNING);
    }
    else
    {
        NHLPERROR((NhlWARNING,NhlEUNKNOWN, "Invalid file or format"));
        return(NhlWARNING);
    }                        
        
    return NhlNOERROR;
}

static NhlErrorTypes AdvancedFileAddDim(NclFile infile, NclQuark dimname,
                                   ng_size_t dimsize, int is_unlimited)
{
    NclAdvancedFile thefile = (NclAdvancedFile) infile;
    NhlErrorTypes ret = NhlNOERROR;
    NclFileDimNode   *dimnode = NULL;
    ng_size_t ds = dimsize;

  /*
   *fprintf(stderr, "\nEnter AdvancedFileAddDim, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tdimname: <%s>, dimsize: %d\n", NrmQuarkToString(dimname), dimsize);
   */

    if(thefile->advancedfile.wr_status <= 0)
    {
      /*
       *if (dimname == NrmStringToQuark("ncl_scalar"))
       *{
       *    NHLPERROR((NhlWARNING,NhlEUNKNOWN,
       *        "AdvancedFileAddDim: <ncl_scalar> is a reserved file dimension name in NCL\n\t\t%s\n",
       *        "it cannot be defined by the user"));
       *    return (NhlWARNING);
       *}
       */

        dimnode = _getDimNodeFromNclFileGrpNode(thefile->advancedfile.grpnode, dimname);

        if(NULL == dimnode)
        {
            if(NULL != thefile->advancedfile.format_funcs->add_dim)
            {
                ret = (*thefile->advancedfile.format_funcs->add_dim)
                       (thefile->advancedfile.grpnode,
                        dimname, ds, is_unlimited);
            }
            else
            {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                    "FATAL:AdvancedFileAddDim: function add_dim undefined.\n"));
                ret = NhlFATAL;
            }
        }
      /*
       *else
       *{
       *    NHLPERROR((NhlINFO,NhlEUNKNOWN,
       *        "AdvancedFileAddDim: Dimension %s is already defined",
       *        NrmQuarkToString(dimname)));
       *    ret = NhlINFO;
       *}
       */
    }

  /*
   *fprintf(stderr, "Leave AdvancedFileAddDim, file: %s, line: %d\n\n", __FILE__, __LINE__);
   */
    return ret;
}

/*
 * Updates the coord info
 */

static NhlErrorTypes AdvancedUpdateCoordInfo(NclAdvancedFile thefile, NrmQuark varname)
{
    NhlErrorTypes ret = NhlNOERROR;

    NclFileVarNode *varnode;
    NclFileDimNode *dimnode;

    dimnode = _getDimNodeFromNclFileGrpNode(thefile->advancedfile.grpnode, varname);

    if(NULL != dimnode)
    {
        varnode = _getVarNodeFromNclFileGrpNode(thefile->advancedfile.grpnode, varname);

        if(NULL != varnode)
        {
            if(varnode->dim_rec->n_dims == 1)
            {
                ret = _addNclCoordVarNode(&(thefile->advancedfile.grpnode->coord_var_rec),
                          varnode);
            }
        }
    }

    return ret;
}

static void AdvancedAdjustForScalarDim(NclAdvancedFile thefile)
{
    NclQuark nsn = NrmStringToQuark("ncl_scalar");

  /*
   *since the scalar dim is always first,
   *all the other dims and coord vars need to shift down one element

   *fprintf(stderr, "\nHit AdvancedAdjustForScalarDim, file: %s, line: %d\n\n", __FILE__, __LINE__);
   */

    AdvancedFileAddDim((NclFile) thefile, nsn, 1, 1);
}

static NhlErrorTypes AdvancedFileAddVar(NclFile infile, NclQuark varname,
                                   NclQuark type, int n_dims, NclQuark *dimnames)
{
    NclAdvancedFile thefile = (NclAdvancedFile) infile;
    NhlErrorTypes ret = NhlNOERROR;
    ng_size_t dim_sizes[NCL_MAX_DIMENSIONS];
    int i;
    NclTypeClass typec;
    int add_scalar_dim = 0;
    NclFileVarNode *varnode;
    NclFileDimNode *dimnode;
    
  /*
   *fprintf(stderr, "\nEnter AdvancedFileAddVar, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tVarname: <%s>, type: <%s>, n_dims: %d, dimname[0]: <%s>\n", 
   *                NrmQuarkToString(varname), NrmQuarkToString(type),
   *                n_dims, NrmQuarkToString(dimnames[0]));
   */

    if((thefile->advancedfile.wr_status <= 0)&&(thefile->advancedfile.format_funcs->add_var != NULL))
    {
        varnode = _getVarNodeFromNclFileGrpNode(thefile->advancedfile.grpnode, varname);

        if(NULL == varnode)
        {
            for(i = 0; i < n_dims; i++)
            {
                dimnode = _getDimNodeFromNclFileGrpNode(thefile->advancedfile.grpnode, dimnames[i]);
                if(NULL == dimnode)
                {
                    if (n_dims == 1 && dimnames[0] == NrmStringToQuark("ncl_scalar"))
                    {
                        add_scalar_dim = 1;
                        dim_sizes[i] = 1;
                    }
                    else
                    {
                        NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                            "AdvancedFileAddVar: Dimension (%s) is not currently defined, can't add variable",
                            NrmQuarkToString(dimnames[i])));
                        ret = NhlFATAL;
                        goto done_AdvancedFileAddVar;
                    }
                }
		else
		{
                    dim_sizes[i] = dimnode->size;
                }
            }

            typec = _NclNameToTypeClass(type); 
            if(typec != NULL)
            {
                ret = (*thefile->advancedfile.format_funcs->add_var)
                       (thefile->advancedfile.grpnode,
                        varname, typec->type_class.data_type,    
                        n_dims, dimnames, dim_sizes);

                if(ret == NhlFATAL)
                {
                    NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                        "AdvancedFileAddVar: Error adding variable <%s> as type: <%s>",
                         NrmQuarkToString(varname), _NclBasicDataTypeToName(type)));
                }
            }
            else
            {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                    "AdvancedFileAddVar Incorrect type specified, can't add variable (%s)",
                    NrmQuarkToString(varname)));
                ret = NhlFATAL;
            }

            if(ret < NhlWARNING) 
                goto done_AdvancedFileAddVar;

            if(add_scalar_dim)
            {
                AdvancedAdjustForScalarDim(thefile);
            }
            
            AdvancedUpdateCoordInfo(thefile,varname); 
            goto done_AdvancedFileAddVar;
        }
        else
        {
            NHLPERROR((NhlWARNING,NhlEUNKNOWN,
                "AdvancedFileAddVar: Variable %s is already defined, can not redefine",
                NrmQuarkToString(varname)));
            ret = NhlWARNING;
            goto done_AdvancedFileAddVar;
        }
    }
    else
    {
        NHLPERROR((NhlFATAL,NhlEUNKNOWN,
            "AdvancedFileAddVar: file (%s) was opened for reading only, can not write",
            NrmQuarkToString(thefile->advancedfile.fname)));
        ret = NhlFATAL;
        goto done_AdvancedFileAddVar;
    }

done_AdvancedFileAddVar:
  /*
   *fprintf(stderr, "Leave AdvancedFileAddVar, file: %s, line: %d\n\n", __FILE__, __LINE__);
   */
    return(ret);
}

static NhlErrorTypes AdvancedFileWriteVarAtt(NclFile infile, NclQuark var, NclQuark attname,
                                        struct _NclMultiDValDataRec* value,
                                        struct _NclSelectionRecord * sel_ptr)
{
    NclAdvancedFile thefile = (NclAdvancedFile) infile;
    int exists;
    NclMultiDValData tmp_att_md,tmp_md,last_att_val_md;
    int att_id = -1;
    NhlErrorTypes ret = NhlNOERROR;
    NclBasicDataTypes from_type,to_type;
    NclObjTypes obj_type;
    void *data_type;
    NhlArgVal udata;
    int i;
    
    NclFileVarNode *varnode;

  /*
   *fprintf(stderr, "\nEnter AdvancedFileWriteVarAtt, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tvarname: <%s>, attname: <%s>\n",
   *        NrmQuarkToString(var), NrmQuarkToString(attname));
   */

    if(thefile->advancedfile.wr_status > 0)
    {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
            "FileWriteVarAtt: file (%s) was opened for reading only, can not write",
            NrmQuarkToString(thefile->advancedfile.fname));
        ret = NhlFATAL;
        goto done_AdvancedFileWriteVarAtt;
    }

    varnode = _getVarNodeFromNclFileGrpNode(thefile->advancedfile.grpnode, var);

    if(NULL == varnode)
    {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
            "(%s) is not a variable in file (%s)",
            NrmQuarkToString(var),NrmQuarkToString(thefile->advancedfile.fname));
        ret = NhlFATAL;
        goto done_AdvancedFileWriteVarAtt;
    }

    if(NULL != varnode->att_rec)
    {
        if(varnode->att_rec->id < 0)
            AdvancedLoadVarAtts(thefile, var);

        att_id = varnode->att_rec->id;

        exists = _NclIsAtt(att_id,NrmQuarkToString(attname));
    }
    else
    {
        att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

      /*
       *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\tatt_id: %d\n", att_id);
       */

        udata.ptrval = (void*)NclMalloc(sizeof(FileCallBackRec));
        ((FileCallBackRec*)udata.ptrval)->thefileid = -1;
        ((FileCallBackRec*)udata.ptrval)->theattid = att_id;
        ((FileCallBackRec*)udata.ptrval)->thevar = -1;

        varnode->att_rec = _NclFileAttAlloc(NCL_MINIMUM_ATTS);
        varnode->att_rec->n_atts = 0;
        varnode->att_rec->id = att_id;
        varnode->att_rec->cb = _NclAddCallback((NclObj)_NclGetObj(att_id),NULL,
                                               FileAttIsBeingDestroyedNotify,ATTDESTROYED,&udata);
        varnode->att_rec->udata = (FileCallBackRec*)udata.ptrval;

        exists = 0;
    }

    if((exists)&&(thefile->advancedfile.format_funcs->write_att != NULL))
    {
      /*
       *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\texists = %d\n", exists);
       */

        /* get the last att val in case there's an error writing the att */
        last_att_val_md = _NclCopyVal(_NclGetAtt(att_id,NrmQuarkToString(attname),NULL),NULL);

        ret = (*thefile->advancedfile.format_funcs->write_var_att)(
                thefile->advancedfile.grpnode,
                var,
                attname,
                value->multidval.val
                );

        if (ret < NhlNOERROR)
        {
            ret = MIN(ret,_NclAddAtt(att_id,NrmQuarkToString(attname),
                last_att_val_md,NULL));
        }
        else
        {
            _NclDestroyObj((NclObj)last_att_val_md);
        }

	/* the value is stored in the att_rec as well (not a copy - it's a pointer to the value 
	   -- but this needs to happen after calling the format specific function; otherwise the
	   format function thinks there's nothing to update and won't call the format specific code */
	   
        ret = _NclAddAtt(att_id,NrmQuarkToString(attname),value,sel_ptr);
        if(ret < NhlWARNING)
        {
                        NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                "Could not write attribute (%s) to attribute list",
                NrmQuarkToString( attname)));
            ret = NhlFATAL;
            goto done_AdvancedFileWriteVarAtt;
        }
        tmp_att_md = _NclGetAtt(att_id,NrmQuarkToString(attname),NULL);
	for (i = 0; i < varnode->att_rec->n_atts; i++) {
		if (varnode->att_rec->att_node[i].name == attname) {
			varnode->att_rec->att_node[i].value = tmp_att_md->multidval.val;
			break;
		}
	}

        goto done_AdvancedFileWriteVarAtt;
    }
    else if((!exists)&&(thefile->advancedfile.format_funcs->add_att != NULL))
    {
      /*
       *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\texists = %d\n", exists);
       */
        ret = _addNclAttNode(&(varnode->att_rec), attname, value->multidval.data_type,
                              value->multidval.totalelements, value->multidval.val);

        if(value->multidval.data_type == NCL_char)
        {
            if (attname != NrmStringToQuark(NCL_MISSING_VALUE_ATT))
                tmp_md = _NclCharMdToStringMd(value);
            else
                tmp_md = value;
    
            ret = _NclAddAtt(att_id,NrmQuarkToString(attname),tmp_md,sel_ptr);
            if(ret < NhlWARNING)
            {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                    "Could not write attribute (%s) to attribute list",
                    NrmQuarkToString(attname)));
                ret = NhlFATAL;
                goto done_AdvancedFileWriteVarAtt;
            }

            ret = (*thefile->advancedfile.format_funcs->add_var_att)(
                thefile->advancedfile.grpnode,
                var,
                attname,
                value->multidval.data_type,
                value->multidval.totalelements,
                value->multidval.val
                );

            if (ret < NhlNOERROR)
            {
                _NclDeleteAtt(att_id,NrmQuarkToString(attname));
            }
        }
        else
        {
            data_type = (void *)(*thefile->advancedfile.format_funcs->map_ncl_type_to_format)
                        (value->multidval.data_type);
            if(data_type == NULL)
            {
                if(value->multidval.data_type == NCL_string)
                {
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
                    ret = _NclFileWriteVarAtt(infile,var,attname,tmp_md,sel_ptr);
                    _NclDestroyObj((NclObj)tmp_md);
                    goto done_AdvancedFileWriteVarAtt;
                }
                else
                {
                    from_type = value->multidval.data_type;
                    to_type = _NclPromoteType(from_type);
                    while((from_type != to_type) &&
                        ((data_type =(*thefile->advancedfile.format_funcs->map_ncl_type_to_format)(to_type))==NULL))
                    {
                        from_type = to_type;
                        to_type = _NclPromoteType(from_type);
                    }
                    if(data_type != NULL)
                        NclFree(data_type);

                    if((data_type = (*thefile->advancedfile.format_funcs->map_ncl_type_to_format)(to_type))==NULL)
                    {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,
                            "The type (%s) is not representable as an attribute in the file (%s)",
                            _NclBasicDataTypeToName(to_type),
                            NrmQuarkToString(thefile->advancedfile.fpath));
                        ret = NhlFATAL;
                        goto done_AdvancedFileWriteVarAtt;
                    }
                    else
                    {
                        NclFree(data_type);
                        obj_type = _NclBasicDataTypeToObjType(to_type);
                        tmp_md = _NclCoerceData(value,obj_type,NULL);
                        ret = _NclFileWriteVarAtt(infile,var,attname,tmp_md,sel_ptr);
                        _NclDestroyObj((NclObj)tmp_md);
                    }
                }
            }
            else
            {
                NclFree(data_type);
                tmp_md = value;
            }

            if(NULL != varnode->att_rec)
            {
                if(varnode->att_rec->id < 0)
                    AdvancedLoadVarAtts(thefile, var);
                att_id = varnode->att_rec->id;
            }

            ret = _NclAddAtt(att_id,NrmQuarkToString(attname),tmp_md,sel_ptr);
            if(ret < NhlWARNING)
            {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                    "Could not write attribute (%s) to attribute list",
                    NrmQuarkToString(attname)));
                ret = NhlFATAL;
                goto done_AdvancedFileWriteVarAtt;
            }

            ret = (*thefile->advancedfile.format_funcs->add_var_att)(
                thefile->advancedfile.grpnode,
                var,
                attname,
                tmp_md->multidval.data_type,
                tmp_md->multidval.totalelements,
                tmp_md->multidval.val
                );

            if(ret <= NhlWARNING)
            {
                _NclDeleteAtt(att_id,NrmQuarkToString(attname));
            }
        }
    }

done_AdvancedFileWriteVarAtt:

  /*
   *fprintf(stderr, "Leave AdvancedFileWriteVarAtt, file: %s, line: %d\n\n", __FILE__, __LINE__);
   */
    return (ret);
}

static NhlErrorTypes AdvancedFileAddChunkDim(NclFile infile, NclQuark chunkdimname,
                                        ng_size_t chunkdimsize, int is_unlimited)
{
    NclAdvancedFile thefile = (NclAdvancedFile) infile;
    NhlErrorTypes ret = NhlNOERROR;
    NclFileDimNode   *chunkdimnode = NULL;
    ng_size_t ds = chunkdimsize;

  /*
   *fprintf(stderr, "\nEnter AdvancedFileAddChunkDim, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tchunkdimname: <%s>, chunkdimsize: %d\n", NrmQuarkToString(chunkdimname), chunkdimsize);
   */

    if(thefile->advancedfile.wr_status <= 0)
    {
        if (chunkdimname == NrmStringToQuark("ncl_scalar"))
        {
            NHLPERROR((NhlWARNING,NhlEUNKNOWN,
                "AdvancedFileAddChunkDim: <ncl_scalar> is a reserved file dimension name in NCL\n\t\t%s\n",
                "it cannot be defined by the user"));
            return (NhlWARNING);
        }

        chunkdimnode = _getChunkDimNodeFromNclFileGrpNode(thefile->advancedfile.grpnode, chunkdimname);

        if(NULL == chunkdimnode)
            thefile->advancedfile.grpnode->is_chunked = 1;

        if(NULL != thefile->advancedfile.format_funcs->add_chunk_dim)
        {
            ret = (*thefile->advancedfile.format_funcs->add_chunk_dim)
                   (thefile->advancedfile.grpnode,
                    chunkdimname, ds, is_unlimited);
        }
        else
        {
            NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                "FATAL:AdvancedFileAddChunkDim: function add_chunk_dim undefined.\n"));
            ret = NhlFATAL;
        }
    }

  /*
   *fprintf(stderr, "Leave AdvancedFileAddChunkDim, file: %s, line: %d\n\n", __FILE__, __LINE__);
   */
    return ret;
}

static NhlErrorTypes AdvancedFileAddVarChunk(NclFile infile, NclQuark varname,
                                        int n_dims, ng_size_t *dims)
{
    NclAdvancedFile thefile = (NclAdvancedFile) infile;
    NhlErrorTypes ret = NhlNOERROR;
    NclFileVarNode *varnode;
    
    if(thefile->advancedfile.wr_status <= 0)
    {
        varnode = _getVarNodeFromNclFileGrpNode(thefile->advancedfile.grpnode, varname);
        if(NULL != varnode)
        {
            varnode->is_chunked = 1;
            if(thefile->advancedfile.format_funcs->add_var_chunk != NULL)
            {
                ret = (*thefile->advancedfile.format_funcs->add_var_chunk)
                       (thefile->advancedfile.grpnode,
                        varname, n_dims, dims);
                if(ret == NhlFATAL)
                {
                    NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                           "AdvancedFileAddVarChunk: an error occurred while adding chunk to variable"));
                }
            }
            else
            {
                ret = NhlWARNING;
                NHLPERROR((NhlWARNING,NhlEUNKNOWN,
                       "AdvancedFileAddVarChunk: add_var_chunk is not defined."));
            }
            return(ret);
        }
        else
        {
            NHLPERROR((NhlWARNING,NhlEUNKNOWN,
                "AdvancedFileAddVarChunk: Variable %s is not defined, can not define chunk",
                 NrmQuarkToString(varname)));
            return(NhlWARNING);
        }
    }
    else
    {
        NHLPERROR((NhlFATAL,NhlEUNKNOWN,
            "AdvancedFileAddVarChunk: file (%s) was opened for reading only, can not write",
             NrmQuarkToString(thefile->advancedfile.fname)));
    }
    return(NhlFATAL);
}

static NhlErrorTypes AdvancedFileAddVarChunkCache(NclFile infile, NclQuark varname,
                                             ng_size_t cache_size, ng_size_t cache_nelems,
                                             float cache_preemption)
{
    NclAdvancedFile thefile = (NclAdvancedFile) infile;
    NhlErrorTypes ret = NhlNOERROR;
    NclFileVarNode *varnode;
    
    if(thefile->advancedfile.wr_status <= 0)
    {
        varnode = _getVarNodeFromNclFileGrpNode(thefile->advancedfile.grpnode, varname);
        if(NULL != varnode)
        {
            if(thefile->advancedfile.format_funcs->add_var_chunk_cache != NULL)
            {
                ret = (*thefile->advancedfile.format_funcs->add_var_chunk_cache)(
                    thefile->advancedfile.grpnode,
                    varname, cache_size, cache_nelems, cache_preemption);
                if(ret == NhlFATAL)
                {
                    NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                        "AdvancedFileAddVarChunkCache: error adding chunk cache to variable: <%s>",
                         NrmQuarkToString(varname)));
                }
            }
            else
            {
                NHLPERROR((NhlWARNING,NhlEUNKNOWN,
                    "AdvancedFileAddVarChunkCache: add_var_chunk_cache is not defined."));
                ret = NhlWARNING;
            }
            return(ret);
        }
        else
        {
            NHLPERROR((NhlWARNING,NhlEUNKNOWN,
                "AdvancedFileAddVarChunkCache: Variable %s is not defined, can not define chunk",
                 NrmQuarkToString(varname)));
            return(NhlWARNING);
        }
    }
    else
    {
        NHLPERROR((NhlFATAL,NhlEUNKNOWN,
            "AdvancedFileAddVarChunkCache: file (%s) was opened for reading only, can not write",
            NrmQuarkToString(thefile->advancedfile.fname)));
    }
    return(NhlFATAL);
}

static NhlErrorTypes AdvancedFileSetVarCompressLevel(NclFile infile, NclQuark varname, int compress_level)
{
    NclAdvancedFile thefile = (NclAdvancedFile) infile;
    NhlErrorTypes ret = NhlNOERROR;
    NclFileVarNode *varnode;
    
    if(thefile->advancedfile.wr_status <= 0)
    {
        varnode = _getVarNodeFromNclFileGrpNode(thefile->advancedfile.grpnode, varname);
        if(NULL != varnode)
        {
            if(thefile->advancedfile.format_funcs->set_var_compress_level != NULL)
            {
                ret = (*thefile->advancedfile.format_funcs->set_var_compress_level)
                       (thefile->advancedfile.grpnode,
                        varname, compress_level);
                if(ret == NhlFATAL)
                {
                    NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                        "AdvancedFileSetVarCompressLevel: an error occurred while adding chunk to variable"));
                }
            }
            else
            {
                ret = NhlWARNING;
                NHLPERROR((NhlWARNING,NhlEUNKNOWN,
                    "AdvancedFileSetVarCompressLevel: set_var_compress_level is not defined."));
            }
            return(ret);
        }
        else
        {
            NHLPERROR((NhlWARNING,NhlEUNKNOWN,
                "AdvancedFileSetVarCompressLevel: Variable %s is not defined, can not define chunk",
                 NrmQuarkToString(varname)));
            return(NhlWARNING);
        }
    }
    else
    {
        NHLPERROR((NhlFATAL,NhlEUNKNOWN,
            "AdvancedFileSetVarCompressLevel: file (%s) was opened for reading only, can not write",
            NrmQuarkToString(thefile->advancedfile.fname)));
    }
    return(NhlFATAL);
}

static int isUnlimitedDimension(NclFileGrpNode *grpnode, NclQuark dimname)
{
    int k;

    if(NULL == grpnode->dim_rec)
        return 0;

    for(k = 0; k < grpnode->dim_rec->n_dims; ++k)
    {
        if(dimname == grpnode->dim_rec->dim_node[k].name)
        {
            return grpnode->dim_rec->dim_node[k].is_unlimited;
        }
    }

    return 0;
}

static NhlErrorTypes MyAdvancedFileWriteVar(NclFile infile, NclQuark var,
                                       struct _NclMultiDValDataRec *value,
                                       struct _NclSelectionRecord *sel_ptr,
                                       NclQuark *dim_names, int type)
{
    NclAdvancedFile thefile = (NclAdvancedFile) infile;
    NclObjTypes lhs_type,rhs_type;
    NclMultiDValData tmp_md = NULL;
    NclMultiDValData mis_md = NULL;
    NclQuark new_dim_quarks[NCL_MAX_DIMENSIONS];
    ng_size_t     new_dim_sizes[NCL_MAX_DIMENSIONS];
    
    int has_missing = 0;
    char buffer[NCL_MAX_STRING];
    void *val;
    NhlErrorTypes ret = NhlNOERROR;
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
    
    int multiplier_target[NCL_MAX_DIMENSIONS];
    int compare_sel[NCL_MAX_DIMENSIONS];
    long current_index[NCL_MAX_DIMENSIONS];
    long current_finish[NCL_MAX_DIMENSIONS];
    int keeper[NCL_MAX_DIMENSIONS];
    int index_map[NCL_MAX_DIMENSIONS];
    ng_size_t selection_dim_sizes[NCL_MAX_DIMENSIONS];
    NclSelection *sel;
    NclScalar *tmp_mis;
    NclScalar tmp_scalar;
    ng_size_t tmp_size = 1;
    long tmpi = 0;
    void *data_type;
    NclBasicDataTypes from_type,to_type;
    NclObjTypes obj_type;
    int result = 0;
    int free_tmp_md = 0;

    NclFileVarNode   *varnode;
    NclFileDimNode   *dimnode;
    NclFileAttNode   *attnode;

  /*
   *fprintf(stderr, "\nEnter MyAdvancedFileWriteVar, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tvarname: <%s>\n", NrmQuarkToString(var));

   *if(NULL != dim_names)
   *    fprintf(stderr, "\tdim_names[0]: <%s>\n", NrmQuarkToString(dim_names[0]));
   *else
   *    fprintf(stderr, "\tdim_names is NULL.\n");
   */

    if(thefile->advancedfile.wr_status <= 0)
    {
        strcpy(buffer, NrmQuarkToString(var));
        if(NULL == strchr(buffer, '/'))
            varnode = _getVarNodeFromThisGrpNode(thefile->advancedfile.grpnode, var);
        else
            varnode = _getVarNodeFromNclFileGrpNode(thefile->advancedfile.grpnode, var);
        if(NULL != varnode)
        {
            /*
            *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
            *fprintf(stderr, "\tget var: <%s>\n", NrmQuarkToString(var));
            */
            if(NCL_none == varnode->type)
            {
                if(NCL_none != value->multidval.data_type)
                    varnode->type = value->multidval.data_type;
            }

            dimnode = varnode->dim_rec->dim_node;
            n_dims_target = varnode->dim_rec->n_dims;
            if(sel_ptr != NULL)
            {
                sel = sel_ptr->selection;
                for(i = 0; i < n_dims_target; i++)
                {
                  /*
                   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
                   *fprintf(stderr, "\tsel->dim_num = %ld\n", sel->dim_num);
                   *fprintf(stderr, "\tdimnode[%d].name = %s, size=%ld\n",
                   *    sel->dim_num, NrmQuarkToString(dimnode[sel->dim_num].name),
                   *    dimnode[sel->dim_num].size);
                   */

                    switch(sel->sel_type)
                    {
                    case Ncl_SUB_ALL:
                        start[sel->dim_num] = 0;
                    case Ncl_SUB_VAL_DEF:
                        if(sel->sel_type == Ncl_SUB_VAL_DEF) {
                            start[sel->dim_num] = sel->u.sub.start;
                        }
                        finish[sel->dim_num] = dimnode[sel->dim_num].size-1;
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
                        }
                        else
                        {
                            stride[sel->dim_num] = sel->u.sub.stride;
                        }
                        if(finish[sel->dim_num] < start[sel->dim_num])
                        {
                            if(stride[sel->dim_num] < 0)
                            {
                                tmpi = finish[sel->dim_num]
                                    + (start[sel->dim_num] - finish[sel->dim_num]) % labs(stride[sel->dim_num]);
                                finish[sel->dim_num] = start[sel->dim_num];
                                start[sel->dim_num] = tmpi;
                                compare_sel[sel->dim_num] = NCLFILE_INC;
                                stride[sel->dim_num] = -(stride[sel->dim_num]); 
                            }
                            else
                            {
                                compare_sel[sel->dim_num] = NCLFILE_DEC;
                                stride[sel->dim_num] = -(stride[sel->dim_num]); 
                                has_reverse = 1;
                            }
                        }
                        else
                        {
                            if(stride[sel->dim_num] < 0)
                            {
                                has_reverse = 1;
                                tmpi = finish[sel->dim_num]
                                     - (finish[sel->dim_num] - start[sel->dim_num])
                                     % labs(stride[sel->dim_num]);
                                finish[sel->dim_num] = start[sel->dim_num];
                                start[sel->dim_num] = tmpi;
                                compare_sel[sel->dim_num] = NCLFILE_DEC;
                                stride[sel->dim_num] = (stride[sel->dim_num]);
                            }
                            else
                            {
                                compare_sel[sel->dim_num] = NCLFILE_INC;
                                stride[sel->dim_num] = (stride[sel->dim_num]);
                            }
                        }

                        if(labs(stride[sel->dim_num]) > 1)
                            has_stride = 1;

                        if(stride[sel->dim_num] != 0)
                        {
                            tmpi = labs(sel->u.sub.stride);
                        }
                        else
                        {
                            NHLPERROR((NhlWARNING,NhlEUNKNOWN,
                                "MyAdvancedFileWriteVar:Invalid stride: %s\n",
                                "stride must be positive non-zero integer"));
                            stride[sel->dim_num] = 1;
                        }
                        n_elem = labs((finish[sel->dim_num] -start[sel->dim_num]) / tmpi) + 1;

                        if((sel->u.sub.start > dimnode[sel->dim_num].size-1 )||(sel->u.sub.start < 0))
                        {
                            if(!( dimnode[sel->dim_num].is_unlimited)||(sel->u.sub.start < 0))
                            {
                                dimnode[sel->dim_num].is_unlimited = isUnlimitedDimension(thefile->advancedfile.grpnode,
                                                                                          dimnode[sel->dim_num].name);

                                if(! dimnode[sel->dim_num].is_unlimited)
                                {
                                    NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                                        "MyAdvancedFileWriteVar: Subscript out of range in subscript #%d",i));
                                    ret = NhlFATAL;
                                    goto done_MyAdvancedFileWriteVar;
                                }
                            }
                            else if(sel->u.sub.start >= dimnode[sel->dim_num].size)
                            {
                                dimnode[sel->dim_num].is_unlimited = 1;
                            }
                        }

                        if((sel->u.sub.finish> dimnode[sel->dim_num].size-1)||(sel->u.sub.finish < 0))
                        {
                            if(!( dimnode[sel->dim_num].is_unlimited)||(sel->u.sub.finish < 0))
                            {
                                dimnode[sel->dim_num].is_unlimited = isUnlimitedDimension(thefile->advancedfile.grpnode,
                                                                                          dimnode[sel->dim_num].name);

                                if(! dimnode[sel->dim_num].is_unlimited)
                                {
                                    NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                                        "MyAdvancedFileWriteVar: Subscript out of range, in subscript #%d",i));
                                    ret = NhlFATAL;
                                    goto done_MyAdvancedFileWriteVar;
                                }
                            }
                            else if(sel->u.sub.finish >= dimnode[sel->dim_num].size)
                            {
                                dimnode[sel->dim_num].is_unlimited = 1;
                            }
                        }

                        if(sel->dim_num != i)
                            has_reorder = 1;

                        index_map[i] = sel->dim_num;
                        break;
                    case Ncl_VECSUBSCR:
                        keeper[sel->dim_num] = 0;
                        if((sel->u.vec.min < 0 ) || (sel->u.vec.min >= dimnode[sel->dim_num].size))
                        {
                            if(!( dimnode[sel->dim_num].is_unlimited)||(sel->u.vec.min < 0))
                            {
                                dimnode[sel->dim_num].is_unlimited = isUnlimitedDimension(thefile->advancedfile.grpnode,
                                                                                          dimnode[sel->dim_num].name);

                                if(! dimnode[sel->dim_num].is_unlimited)
                                {
                                    NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                                        "MyAdvancedFileWriteVar: Vector subscript out of range in subscript #%d",i));
                                    ret = NhlFATAL;
                                    goto done_MyAdvancedFileWriteVar;
                                }
                            }
                            else if(sel->u.vec.min >= dimnode[sel->dim_num].size)
                            {
                                dimnode[sel->dim_num].is_unlimited = 1;
                            }
                        }
                        if((sel->u.vec.max < 0)||(sel->u.vec.max >= dimnode[sel->dim_num].size))
                        {
                            if(!( dimnode[sel->dim_num].is_unlimited)||(sel->u.vec.max < 0))
                            {
                                dimnode[sel->dim_num].is_unlimited = isUnlimitedDimension(thefile->advancedfile.grpnode,
                                                                                          dimnode[sel->dim_num].name);

                                if(! dimnode[sel->dim_num].is_unlimited)
                                {
                                    NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                                        "MyAdvancedFileWriteVar: Vector subscript out of range in subscript #%d",i));
                                    ret = NhlFATAL;
                                    goto done_MyAdvancedFileWriteVar;
                                }
                            }
                            else if(sel->u.vec.max >= dimnode[sel->dim_num].size)
                            {
                                dimnode[sel->dim_num].is_unlimited = 1;
                            }
                        }
                        n_elem = sel->u.vec.n_ind;
                        stride[sel->dim_num] = 0;
                        start[sel->dim_num] = finish[sel->dim_num] = sel->u.vec.ind[0];
                        has_vectors = 1;
                        index_map[i] = sel->dim_num;
                        if(sel->dim_num != i)
                            has_reorder = 1;
                        compare_sel[sel->dim_num] = NCLFILE_VEC;
                        break;
                    }
                    multiplier_target[sel->dim_num] = 1;
                    if(sel->dim_num != n_dims_target - 1)
                    {
                        for(k = sel->dim_num +1 ; k< n_dims_target; k++)
                        {
                            multiplier_target[sel->dim_num] *= (long)dimnode[k].size;
                        }
                    }
                    selection_dim_sizes[i] =n_elem;
                    total_elements = total_elements * n_elem;
                    sel++;
                }
                sel = sel_ptr->selection;
            }
            else
            {
                /*
                *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
                *fprintf(stderr, "\tn_dims_target = %d\n", n_dims_target);
                */
                for(i = 0 ; i < n_dims_target; i++)
                {
                    keeper[i] = 1;
                    start[i] = 0;
                  /*
                   *n = varnode->dimid[i];
                   *if(dimnode[n].is_unlimited)
                   */
                    if(dimnode[i].is_unlimited)
                    {
                        if(value->multidval.dim_sizes[i] > dimnode[i].size)
                        {
                            finish[i] = value->multidval.dim_sizes[i] -1;
                        }
                        else
                        {
                            finish[i] = dimnode[i].size -1;
                        }
                    }
                    else
                    {
                        finish[i] = dimnode[i].size -1;
                    }
                    stride[i] = 1;
                    index_map[i] = i;
                    total_elements *= (finish[i] + 1);
                    selection_dim_sizes[i] = (finish[i]+ 1);
                    compare_sel[i] = NCLFILE_INC;
                    multiplier_target[i] = 1;
                    for(k = i + 1; k < n_dims_target; k++)
                    {
                      /*
                       *n = varnode->dimid[i];
                       *multiplier_target[i] *= (long)dimnode[n].size;
                       */
                        multiplier_target[i] *= (long)dimnode[k].size;
                    }
                }
                sel = NULL;
            }
            n_dims_selection = n_dims_target;        
            /*
            *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
            *fprintf(stderr, "\tn_dims_selection = %d\n", n_dims_selection);
            */
            i = 0;
            while((i < n_dims_selection)&&(n_dims_selection > 1))
            {
                if((selection_dim_sizes[i] == 1)&&!(keeper[i]))
                {
                    for(j = i ; j < n_dims_selection - 1;j++)
                    {
                        selection_dim_sizes[j] = selection_dim_sizes[j+1];
                        keeper[j] = keeper[j+1];
                    }
                    n_dims_selection--;
                }
                else
                {
                    i++;
                }
            }
            /*
            *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
            *fprintf(stderr, "\tvalue->multidval.kind = %d, value->multidval.data_type: <%s>\n",
            *        value->multidval.kind,
            *        _NclBasicDataTypeToName(value->multidval.data_type));
            */
            if(value->multidval.kind != SCALAR)
            {
                for(i = 0, j = 0; i< n_dims_selection; i++)
                {
                    if (selection_dim_sizes[i] == 1 && value->multidval.dim_sizes[j] != 1)
                        continue;
                    else if (selection_dim_sizes[i] != 1 && value->multidval.dim_sizes[j] == 1)
                    {
                        while (value->multidval.dim_sizes[j] == 1) 
                            j++;
                    }

                  /*Comment out this paragraph to allow extend unlimited dimension record.
                   *Wei, 01/10/2013
                   *if(selection_dim_sizes[i] != value->multidval.dim_sizes[j])
                   *{
                   *    NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Dimension sizes of left hand side do not match right hand side"));
                   *                       ret = NhlFATAL;
                   *                       goto done_MyAdvancedFileWriteVar;
                   *}
                   */
                    j++;
                }
            } 
            lhs_type = _NclBasicDataTypeToObjType(varnode->type);
    
            rhs_type = value->multidval.type->type_class.type ;

            /*
            *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
            *fprintf(stderr, "\tlhs_type = %d, rhs_type = %d\n",
            *        lhs_type, rhs_type);
            */

            attnode = _getAttNodeFromNclFileVarNode(varnode,
                                                        NrmStringToQuark(NCL_MISSING_VALUE_ATT));

            if(NULL == attnode)
                has_missing = 0;
            else
                has_missing = 1;

            if((Ncl_Typecompound == lhs_type) && ( Ncl_Typelist == rhs_type))
            {
                tmp_md = value;
            }
            else if((NCL_enum == varnode->type) && (Ncl_Typelist == lhs_type))
            {
                tmp_md = value;
            }
            else if(lhs_type != rhs_type)
            {
              /*
               *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
               *fprintf(stderr, "\tlhs_type = %d, rhs_type = %d\n",
               *        lhs_type, rhs_type);
               */
                if(has_missing)
                {
                    mis_md = AdvancedFileReadVarAtt((NclFile)thefile,var,
                                NrmStringToQuark(NCL_MISSING_VALUE_ATT),NULL);
                    tmp_md = _NclCoerceData(value,lhs_type,(NclScalar*)mis_md->multidval.val);
                }
                else
                {
                    tmp_md = _NclCoerceData(value,lhs_type,NULL);
                }

                if(NULL == tmp_md)
                {
                    NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                              "Type mismatch, can't perform assignment"));
                    ret = NhlFATAL;
                    goto done_MyAdvancedFileWriteVar;
                }
            }
            else
            {
              /*
               *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
               *fprintf(stderr, "\tlhs_type = %d, rhs_type = %d\n",
               *        lhs_type, rhs_type);
               */

                if((has_missing)&&(value->multidval.missing_value.has_missing))
                {
                    mis_md = AdvancedFileReadVarAtt((NclFile)thefile,var,
                                NrmStringToQuark(NCL_MISSING_VALUE_ATT),NULL);
                    _Ncleq(value->multidval.type,(void*)&(result),
                          (void*)&(value->multidval.missing_value.value),
                          (void*)(mis_md->multidval.val),NULL,NULL,1,1);

                    if (result)
                    {
                        tmp_md = value;
                    }
                    else if(value->obj.status != PERMANENT)
                    {
                        tmp_md = value;
                        memcpy(&tmp_scalar,mis_md->multidval.val,mis_md->multidval.totalsize);
                        _NclResetMissingValue(tmp_md,(NclScalar*) &tmp_scalar);
                    }
                    else
                    {
                      /* Situation where missing values are not equal
                       * and can't just overwrite input's
                       */

                        memcpy(&tmp_scalar,mis_md->multidval.val,mis_md->multidval.totalsize);
                        tmp_md = _NclCopyVal(value,&tmp_scalar);
                        free_tmp_md = 1;
                    }
                }
                else if(value->multidval.missing_value.has_missing)
                {
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
                    AdvancedFileWriteVarAtt((NclFile)thefile,var,
                        NrmStringToQuark(NCL_MISSING_VALUE_ATT),mis_md,NULL);
                    tmp_md = value;
                }
                else
                {
                    tmp_md = value;
                }
            }

            if(NULL == tmp_md)
            {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                        "lhs_type = %d, rhs_type = %d, Type mismatch, can't perform assignment",
			lhs_type, rhs_type));
                ret = NhlFATAL;
                goto done_MyAdvancedFileWriteVar;
            }

            if((type == FILE_VAR_ACCESS) ? thefile->advancedfile.format_funcs->write_var != NULL:thefile->advancedfile.format_funcs->write_coord != NULL )
            {
                if((!has_vectors)&&(!has_reverse)&&(!has_reorder)&&(value->multidval.kind != SCALAR))
                {
                    if(type == FILE_VAR_ACCESS)
                    {
                        ret = (*thefile->advancedfile.format_funcs->write_var)(
                            thefile->advancedfile.grpnode,
                            var,
                            tmp_md->multidval.val,
                            start,
                            finish,    
                            stride);
                    }
                    else
                    {
                        ret = (*thefile->advancedfile.format_funcs->write_coord)(
                            thefile->advancedfile.grpnode,
                            var,
                            tmp_md->multidval.val,
                            start,
                            finish,    
                            stride);
                    }

                    if(free_tmp_md)
                    {
                        _NclDestroyObj((NclObj)tmp_md);
                    }

                    goto done_MyAdvancedFileWriteVar;
                }
                else
                {
                    if(value->multidval.kind != SCALAR)
                    {
                        val = tmp_md->multidval.val;
                        from = 0;
                        block_write_limit = n_dims_target -1;
                        for(i = n_dims_target - 1; i >= 0; i--)
                        {
                            if((compare_sel[index_map[i]] != NCLFILE_INC)||(index_map[i] != i)) {
                                block_write_limit = i;
                                break;
                            }
                        }
                    }
                    else
                    {
                        block_write_limit = n_dims_target -1;
                        val = tmp_md->multidval.val;
                        from = 0;
                    }
                    n_elem_block = 1;
                    for(i = 0; i < n_dims_target; i++)
                    {
                        current_index[index_map[i]] = start[index_map[i]];
                        if(i > block_write_limit)
                        {
                            n_elem_block *= selection_dim_sizes[i];
                            current_finish[i] = finish[i];
                            real_stride[i] = labs(stride[i]);
                        }
                        else
                        {
                            current_finish[index_map[i]] = current_index[index_map[i]];
                            real_stride[index_map[i]] = 1;
                        }
                    }

                    while(!done)
                    {
                        if(type == FILE_VAR_ACCESS)
                        {
                            ret = (*thefile->advancedfile.format_funcs->write_var) (
                                thefile->advancedfile.grpnode,
                                var,
                                (void*)&(((char*)val)[from]),
                                current_index,
                                current_finish,
                                real_stride);
                        }
                        else
                        {
                            ret = (*thefile->advancedfile.format_funcs->write_coord) (
                                thefile->advancedfile.grpnode,
                                var,
                                (void*)&(((char*)val)[from]),
                                current_index,
                                current_finish,
                                real_stride);
                        }

                        if(ret < NhlWARNING)
                        {
                            if(free_tmp_md)
                            {
                                _NclDestroyObj((NclObj)tmp_md);
                            }
                            goto done_MyAdvancedFileWriteVar;
                        }
                        else
                        {
                            if(NCL_list == varnode->type)
                            {
                                if(free_tmp_md)
                                {
                                    _NclDestroyObj((NclObj)tmp_md);
                                }
                                goto done_MyAdvancedFileWriteVar;
                            }
                        }

                        if(value->multidval.kind != SCALAR)
                        {
                            from += n_elem_block * _NclSizeOf(varnode->type);
                        }

                        if(compare_sel[index_map[block_write_limit]] < 0)
                        {
                            current_index[index_map[block_write_limit]] += stride[index_map[block_write_limit]];
                            current_finish[index_map[block_write_limit]] = current_index[index_map[block_write_limit]];
                        }
                        else
                        {
                            compare_sel[index_map[block_write_limit]]++;
                        }

                        for( i = block_write_limit; i > 0 ; i--)
                        {
                            switch(compare_sel[index_map[i]])
                            {
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

                    if(free_tmp_md)
                    {
                        _NclDestroyObj((NclObj)tmp_md);
                    }
                    goto done_MyAdvancedFileWriteVar;
                }
            } else if((type == FILE_VAR_ACCESS) ? thefile->advancedfile.format_funcs->write_var_ns != NULL : thefile->advancedfile.format_funcs->write_coord_ns != NULL) {
                if((!has_vectors)&&(!has_reverse)&&(!has_reorder)&&(!has_stride)) {    
                    if(type == FILE_VAR_ACCESS) {
                        ret = (*thefile->advancedfile.format_funcs->write_var_ns)(
                            thefile->advancedfile.grpnode,
                            var,
                            tmp_md->multidval.val,
                            start,
                            finish
                            );
                    } else {
                        ret = (*thefile->advancedfile.format_funcs->write_coord_ns)(
                            thefile->advancedfile.grpnode,
                            var,
                            tmp_md->multidval.val,
                            start,
                            finish
                            );
                    }

                    if(free_tmp_md) {
                        _NclDestroyObj((NclObj)tmp_md);
                    }

                           goto done_MyAdvancedFileWriteVar;
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
                            ret = (*thefile->advancedfile.format_funcs->write_var_ns) (
                                thefile->advancedfile.grpnode,
                                var,
                                (void*)&(((char*)val)[from]),
                                current_index,
                                current_finish
                                );
                        } else {
                            ret = (*thefile->advancedfile.format_funcs->write_coord_ns) (
                                thefile->advancedfile.grpnode,
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
                                   goto done_MyAdvancedFileWriteVar;
                        }
                        if(value->multidval.kind != SCALAR) {
                            from += n_elem_block * _NclSizeOf(varnode->type);
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

                    if(free_tmp_md) {
                        _NclDestroyObj((NclObj)tmp_md);
                    }
                           goto done_MyAdvancedFileWriteVar;
                }
            }
        } else {
/*
* Need to add variable to file situation
*/
          /*
           *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
           *fprintf(stderr, "\tCould not get var: <%s>\n", NrmQuarkToString(var));
           */

            if(type == FILE_COORD_VAR_ACCESS)
            {
                dimnode = _getDimNodeFromNclFileGrpNode(thefile->advancedfile.grpnode, var);

                if(NULL == dimnode)
                {
                    NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                        "(%s) is not a dimension in file (%s), can not add coordinate variable",
                        NrmQuarkToString(var),NrmQuarkToString(thefile->advancedfile.fpath)));
                    ret = NhlFATAL;
                    goto done_MyAdvancedFileWriteVar;
                }

                if((dimnode->size == value->multidval.dim_sizes[0]) ||
                   (dimnode->is_unlimited))
                {
                    if(value->multidval.n_dims != 1)
                    {
                        NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                            "FILE_COORD_VAR_ACCESS."));
                        NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                            "Coordinate variables must be single dimension arrays, attempt to assign (%d) dimension value to coordinate variable",
                            value->multidval.n_dims));
                        ret = NhlFATAL;
                        goto done_MyAdvancedFileWriteVar;
                    }
                    new_dim_quarks[0] = var;
                    new_dim_sizes[0] = value->multidval.dim_sizes[0];
                    start[0] = 0;
                    finish[0] = value->multidval.dim_sizes[0] -1 ;
                    stride[0] = 1;
                }
            }
            else
            {
             /*
              * Since it is imposible to guess names of dimensions they must
              * blindly be added
              */
                if(dim_names == NULL)
                {
                    for(i = 0; i < value->multidval.n_dims; i++)
                    {
			if (thefile->advancedfile.grpnode->dim_rec != NULL)
				sprintf(buffer,"ncl%d",thefile->advancedfile.grpnode->dim_rec->n_dims);
			else
				sprintf(buffer,"ncl%d",0);
			ret = AdvancedFileAddDim(infile,NrmStringToQuark(buffer),value->multidval.dim_sizes[i],False);
			new_dim_quarks[i] = NrmStringToQuark(buffer);
			new_dim_sizes[i] = (long)value->multidval.dim_sizes[i];
                        start[i] = 0;
                        finish[i] = value->multidval.dim_sizes[i] -1;
                        stride[i] = 1;

                        if(ret < NhlWARNING) {
                                   goto done_MyAdvancedFileWriteVar;
                        }
                    }
                }
                else
                {
                    for(i = 0 ; i < value->multidval.n_dims; i++)
                    {
                        if(dim_names[i] != -1) {
                            new_dim_quarks[i] = dim_names[i];
                        } 
			else if (thefile->advancedfile.grpnode->dim_rec != NULL) {
			    sprintf(buffer,"ncl%d",thefile->advancedfile.grpnode->dim_rec->n_dims);
                            new_dim_quarks[i] = NrmStringToQuark(buffer);
			}
			else {
			    sprintf(buffer,"ncl%d",0);
                            new_dim_quarks[i] = NrmStringToQuark(buffer);
                        }
                        new_dim_sizes[i] = value->multidval.dim_sizes[i];
                        start[i] = 0;
                        finish[i] = value->multidval.dim_sizes[i] - 1;
                        stride[i] = 1;
                        dimnode = _getDimNodeFromNclFileGrpNode(thefile->advancedfile.grpnode, dim_names[i]);
                        if(NULL == dimnode)
                        {
                            ret = (*thefile->advancedfile.format_funcs->add_dim)
                                   (thefile->advancedfile.grpnode,
				    new_dim_quarks[i],
				    new_dim_sizes[i],
				    0);
                            if(ret < NhlWARNING)
                                       goto done_MyAdvancedFileWriteVar;

                            if (value->multidval.n_dims == 1 && new_dim_quarks[i] == NrmStringToQuark("ncl_scalar")) {
                                AdvancedAdjustForScalarDim(thefile);
                            }
                        }
                        else
                        {
                            if((dimnode->size != value->multidval.dim_sizes[i])&&(!(dimnode->is_unlimited)))
                            {
                                NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                                    "File dimension conflict, dimension (%s) has a size of (%d) can not set it to requested size (%d)",
                                    NrmQuarkToString(dim_names[i]),
                                    dimnode->size,value->multidval.dim_sizes[i]));
                                ret = NhlFATAL;
                                goto done_MyAdvancedFileWriteVar;
                            }
                        }
                    }
                }
            }
         /*
          * Make sure data can be written
          */
            data_type = (*thefile->advancedfile.format_funcs->map_ncl_type_to_format)
                        (value->multidval.data_type);
            if(data_type == NULL)
            {
                from_type = value->multidval.data_type;
                to_type = _NclPromoteType(from_type);
                while((from_type != to_type )&&
                     ((data_type = (*thefile->advancedfile.format_funcs->map_ncl_type_to_format)(to_type))==NULL))
                {
                    from_type = to_type;
                    to_type = _NclPromoteType(from_type);
                }

                if(data_type != NULL)
                {
                    NclFree(data_type);
                }
                obj_type = _NclBasicDataTypeToObjType(to_type);
                tmp_md = _NclCoerceData(value,obj_type,NULL);
                if((tmp_md == NULL)||( to_type == value->multidval.data_type))
                {
                    NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Attempting to write variable (%s) of type (%s) which is not representable in the format of file (%s)",
                        NrmQuarkToString(var),
                        _NclBasicDataTypeToName(value->multidval.data_type),
                        NrmQuarkToString(thefile->advancedfile.fpath)));
                    NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Trying using a type conversion function"));
                    ret = NhlFATAL;
                    goto done_MyAdvancedFileWriteVar;
                }
                else
                {
                    ret = (*thefile->advancedfile.format_funcs->add_var)
                           (thefile->advancedfile.grpnode,
                            var,
                            tmp_md->multidval.data_type,
                            tmp_md->multidval.n_dims,
                            new_dim_quarks,
                            new_dim_sizes);
                }
            }
            else
            {
                ret = (*thefile->advancedfile.format_funcs->add_var)
                       (thefile->advancedfile.grpnode,
                        var,
                        value->multidval.data_type,
                        value->multidval.n_dims,
                        new_dim_quarks,
                        new_dim_sizes);

                tmp_md = value;
                NclFree(data_type);
            }

            if(ret < NhlWARNING)
                goto done_MyAdvancedFileWriteVar;

            if((type == FILE_VAR_ACCESS) ? thefile->advancedfile.format_funcs->write_var != NULL : thefile->advancedfile.format_funcs->write_coord != NULL)
            {
                if(type == FILE_VAR_ACCESS)
                {
                    ret = (*thefile->advancedfile.format_funcs->write_var)
                           (thefile->advancedfile.grpnode,
                            var,
                            tmp_md->multidval.val,
                            start, finish, stride);
                }
                else
                {
                    ret = (*thefile->advancedfile.format_funcs->write_coord)
                           (thefile->advancedfile.grpnode,
                            var,
                            tmp_md->multidval.val,
                            start, finish, stride);
                }

                if((tmp_md!=value)&&(tmp_md->obj.status != PERMANENT))
                    _NclDestroyObj((NclObj)tmp_md);
            }
            else
            {
                if(type == FILE_VAR_ACCESS)
                {
                    ret = (*thefile->advancedfile.format_funcs->write_var_ns)
                           (thefile->advancedfile.grpnode,
                            var,
                            tmp_md->multidval.val,
                            start, finish);
                }
                else
                {
                    ret = (*thefile->advancedfile.format_funcs->write_coord_ns)
                           (thefile->advancedfile.grpnode,
                            var,
                            tmp_md->multidval.val,
                            start, finish);
                }

                if((tmp_md!=value)&&(tmp_md->obj.status != PERMANENT))
                    _NclDestroyObj((NclObj)tmp_md);
            }

            ret = NhlNOERROR;
            goto done_MyAdvancedFileWriteVar;
        }
    }
    else
    {
        NHLPERROR((NhlFATAL,NhlEUNKNOWN,
            "MyAdvancedFileWriteVar: file (%s) was opened for reading only, can not write",
             NrmQuarkToString(thefile->advancedfile.fname)));
        ret = NhlFATAL;
    }

done_MyAdvancedFileWriteVar:
  /*
   *fprintf(stderr, "\tvarname: <%s>\n", NrmQuarkToString(var));
   *fprintf(stderr, "Leave MyAdvancedFileWriteVar, file: %s, line: %d\n\n", __FILE__, __LINE__);
   */
    return ret;
}

static NhlErrorTypes AdvancedFileWriteVar(NclFile thefile, NclQuark var,
                                     struct _NclMultiDValDataRec *value,
                                     struct _NclSelectionRecord *sel_ptr)
{
    return(MyAdvancedFileWriteVar(thefile,var,value,sel_ptr,NULL,FILE_VAR_ACCESS));
}

static NclObjTypes AdvancedFileVarRepValue(NclFile infile, NclQuark var)
{
    NclAdvancedFile thefile = (NclAdvancedFile) infile;
    NclFileVarNode *varnode;

    varnode = _getVarNodeFromNclFileGrpNode(thefile->advancedfile.grpnode, var);

    if(NULL != varnode)
    {
        return(_NclBasicDataTypeToObjType(varnode->type));
    }

    return(Ncl_None);
}

static NhlErrorTypes AdvancedFileWriteVarVar(NclFile infile, NclQuark lhs_var,
                                        struct _NclSelectionRecord *lhs_sel_ptr,
                                        struct _NclVarRec *rhs_var,
                                        struct _NclSelectionRecord *rhs_sel_ptr)
{
    NclAdvancedFile thefile = (NclAdvancedFile) infile;
    NhlErrorTypes ret = NhlNOERROR;
    struct _NclVarRec* tmp_var;
    struct _NclVarRec* tmp_coord_var;
    int i,j,m;
    NclQuark dim_names[NCL_MAX_DIMENSIONS];
    NclAtt theatt;
    NclAttList *step;
    int index = -1;
    int cindex = -1;
    NclSelectionRecord tmp_sel;
    void *tmp_coord;
    char *tmp_ptr;
    NclMultiDValData tmp_md;
    NclMultiDValData c_md;
    struct _NclVarRec* cvar;
    ng_size_t dimsize = -1;

    tmp_sel.n_entries = 1;
    tmp_sel.selected_from_sym = NULL;
    tmp_sel.selected_from_var = NULL;

    NclFileVarNode *varnode;
    NclFileDimNode *dimnode;

  /*
   *fprintf(stderr, "\nHit AdvancedFileWriteVarVar, file: %s, line: %d\n", __FILE__, __LINE__);
   */

    if(thefile->advancedfile.wr_status<=0)
    {
        tmp_var = _NclVarRead(rhs_var, rhs_sel_ptr);
        if (! tmp_var)
        {
            return NhlFATAL;
        }

        tmp_md = (NclMultiDValData)_NclGetObj(tmp_var->var.thevalue_id);
        if(! tmp_md)
        {
            return NhlFATAL;
        }

        for(i = 0; i < tmp_var->var.n_dims; i++)
        {
            dim_names[i] = tmp_var->var.dim_info[i].dim_quark;
            if(dim_names[i] == NrmStringToQuark("ncl_scalar"))
                continue;
            if(dim_names[i] > 0)
            {
                varnode = _getVarNodeFromNclFileGrpNode(thefile->advancedfile.grpnode, dim_names[i]);
                if(NULL == varnode)
                {
                    ret = AdvancedFileAddDim(infile,dim_names[i],tmp_var->var.dim_info[i].dim_size,False);

                    if(0 < tmp_var->var.coord_vars[i])
                    {
                        cvar = (NclVar)_NclGetObj(tmp_var->var.coord_vars[i]);
                        c_md = (NclMultiDValData)_NclGetObj(cvar->var.thevalue_id);
                        ret = AdvancedFileAddVar(infile, dim_names[i],
                                            NrmStringToQuark(_NclBasicDataTypeToName(c_md->multidval.data_type)),
                                            1, &(dim_names[i]));
                    }
                }
            }
            else
            {
                char buffer[32];

                if(1 == tmp_var->var.dim_info[i].dim_size)
                {
                    if((NULL != thefile->advancedfile.grpnode->dim_rec) &&
                       (NrmStringToQuark("ncl_scalar") == thefile->advancedfile.grpnode->dim_rec->dim_node[0].name))
                        continue;
                }

                if(NULL != thefile->advancedfile.grpnode->dim_rec)
                    sprintf(buffer,"ncl%d",thefile->advancedfile.grpnode->dim_rec->n_dims);
                else
                    sprintf(buffer,"ncl%d",0);
                ret = AdvancedFileAddDim(infile,NrmStringToQuark(buffer),tmp_var->var.dim_info[i].dim_size,False);
                dim_names[i] = NrmStringToQuark(buffer);
            }
        }

        varnode = _getVarNodeFromNclFileGrpNode(thefile->advancedfile.grpnode, lhs_var);
        if(NULL == varnode)
        {
            ret = AdvancedFileAddVar(infile, lhs_var,
                  NrmStringToQuark(_NclBasicDataTypeToName(tmp_md->multidval.type->type_class.data_type)),
                  tmp_var->var.n_dims, dim_names);
            if(ret < NhlWARNING)
            {
                return(ret);
            }
        }

        if(rhs_var->var.att_id != -1)
        {
            theatt = (NclAtt)_NclGetObj(rhs_var->var.att_id);
            step = theatt->att.att_list;
            while(step != NULL)
            {
                ret = AdvancedFileWriteVarAtt(infile,lhs_var,step->quark,step->attvalue,NULL);
                if(ret < NhlNOERROR)
                {
                    NHLPERROR((NhlWARNING,NhlEUNKNOWN,
                        "AdvancedFileWriteVarVar: Could not write attribute (%s) to variable (%s) in file (%s), continuing anyway",
                         NrmQuarkToString(step->quark),
                         NrmQuarkToString(lhs_var),
                         NrmQuarkToString(thefile->advancedfile.fname)));
                    ret = NhlWARNING;
                }
                step = step->next;
            }
        }

        ret = MyAdvancedFileWriteVar(infile,lhs_var,tmp_md,lhs_sel_ptr,dim_names,FILE_VAR_ACCESS);
        if(ret < NhlWARNING)
        {
            return(ret);
        }

        varnode = _getVarNodeFromNclFileGrpNode(thefile->advancedfile.grpnode, lhs_var);
        if(NULL == varnode)
        {
            j = 0;
            for(i = 0; i < lhs_sel_ptr->n_entries; i++)
            {
                dimnode = &(thefile->advancedfile.grpnode->dim_rec->dim_node[lhs_sel_ptr->selection[i].dim_num]);

                if(!lhs_sel_ptr->selection[i].u.sub.is_single )
                {
#if 0
		    ng_size_t lhs_n_elem;    
                    switch(lhs_sel_ptr->selection[i].sel_type)
                    {
                    case Ncl_VECSUBSCR:
                        lhs_n_elem = lhs_sel_ptr->selection[i].u.vec.n_ind;
                        break;
                    default:
                        lhs_n_elem = (ng_size_t)labs((lhs_sel_ptr->selection[i].u.sub.finish
                                                    - lhs_sel_ptr->selection[i].u.sub.start)
                                                    / lhs_sel_ptr->selection[i].u.sub.stride) + 1;
                        break;
                    }
#endif

                    if(tmp_var->var.dim_info[j].dim_quark > 0)
                    {
                        if(dimnode->name != tmp_var->var.dim_info[j].dim_quark)
                        {
                         /*
                          * Dimnames are unequal give warning then overwrite
                          */
                            NHLPERROR((NhlWARNING,NhlEUNKNOWN,
                                "Dimension names of left hand side and right hand side do not match, overwriting dimension (%s), use (/ .. /) if this is not the desired result",
                                 NrmQuarkToString(dimnode->name)));

                            _NclFileWriteDim(infile,thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[lhs_sel_ptr->selection[i].dim_num]]->dim_name_quark,thefile->file.var_info[index]->file_dim_num[lhs_sel_ptr->selection[i].dim_num]);

                        } 

                      /*
                       * Now dimension names are equal, proceed to write coordinate variable
                       */
                        if(tmp_var->var.coord_vars[j] != -1)
                        {
                            cindex = AdvancedFileIsCoord(infile, tmp_var->var.dim_info[j].dim_quark);
                            if(cindex != -1)
                            {
                             /*
                              * Simply write coordinate using sel_ptr
                              */
                                tmp_sel.selection[0] = lhs_sel_ptr->selection[i];
                                tmp_sel.selection[0].dim_num = 0;
                                ret = _NclFileWriteCoord(infile,tmp_var->var.dim_info[j].dim_quark,
                                      _NclVarValueRead((NclVar)_NclGetObj(tmp_var->var.coord_vars[j]),
                                       NULL,NULL),&tmp_sel);
                                cvar = (NclVar)_NclGetObj(tmp_var->var.coord_vars[j]);
                            }
                            else
                            {
                             /*
                              * Need to create a temporary missing value filled array
                              * and write it then make and assigment using sel_ptr
                              */
                                dimsize = (ng_size_t)dimnode->size;
                                cvar = (NclVar)_NclGetObj(tmp_var->var.coord_vars[j]);
                                tmp_md = (NclMultiDValData)_NclGetObj(cvar->var.thevalue_id);
                                tmp_coord = NclMalloc(dimsize*tmp_md->multidval.type->type_class.size);
                                assert(tmp_coord);
                                tmp_ptr = (char*)tmp_coord;
                                for(m = 0; m < dimsize; m++)
                                {
                                    memcpy((void*)tmp_ptr,(void*)&(tmp_md->multidval.type->type_class.default_mis),
                                                  tmp_md->multidval.type->type_class.size);
                                    tmp_ptr = tmp_ptr + tmp_md->multidval.type->type_class.size;
                                }

                                ret = _NclFileWriteCoord(
                                        infile,    
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
                                ret = _NclFileWriteCoord(infile,tmp_var->var.dim_info[j].dim_quark,tmp_md,&tmp_sel);
                            }

                            if(ret < NhlWARNING)
                            {
                                NHLPERROR((NhlWARNING,NhlEUNKNOWN,
                                    "AdvancedFileWriteVarVar: Could not write coordinate variable (%s) to file (%s), continuing anyway",
                                      NrmQuarkToString(tmp_var->var.dim_info[i].dim_quark),
                                      NrmQuarkToString(thefile->advancedfile.fname)));
                                ret = NhlWARNING;
                            }
                            else
                            {
                                if(cvar->var.att_id != -1)
                                {
                                    theatt = (NclAtt)_NclGetObj(cvar->var.att_id);
                                    step = theatt->att.att_list;
                                    while(step != NULL)
                                    {
                                        ret = AdvancedFileWriteVarAtt(infile,tmp_var->var.dim_info[i].dim_quark,
                                                                 step->quark,step->attvalue,NULL);
                                        if(ret < NhlWARNING){
                                            NHLPERROR((NhlWARNING,NhlEUNKNOWN,
                                                "AdvancedFileWriteVarVar: Could not write attribute (%s) to variable (%s) in file (%s), continuing anyway",
                                                 NrmQuarkToString(step->quark),
                                                 NrmQuarkToString(tmp_var->var.dim_info[i].dim_quark),
                                                 NrmQuarkToString(thefile->advancedfile.fname)));
                                            ret = NhlWARNING;
                                        }
                                        step = step->next;
                                    }
                                }
                            }
                        }
                
                    }
                    else if(dimnode->name > 0)
                    {
                     /*
                      * right hand side has no dimension name
                      * and hence no coordinate variable so give warning and proceed
                      */
                        NHLPERROR((NhlWARNING,NhlEUNKNOWN,
                            "Right hand side has no dimension name can not delete dimension of a file, use (/ .. /) to avoid this message"));
                    }
                    j++;
                } 
            } 
        }
        else
        {
            for(i = 0, j = 0; i < varnode->dim_rec->n_dims; i++)
            {
                dimnode = &(varnode->dim_rec->dim_node[i]);

                if(dimnode->size  == 1 && tmp_var->var.dim_info[j].dim_size != 1) 
                    continue;
                else if (dimnode->size != 1 && tmp_var->var.dim_info[j].dim_size == 1)
                {
                    while (tmp_var->var.dim_info[j].dim_size == 1)
                        j++;
                }

                if(tmp_var->var.dim_info[j].dim_quark > 0)
                {
                    if(dimnode->name != tmp_var->var.dim_info[j].dim_quark)
                    {
                     /*
                      * Dimnames are unequal give warning then overwrite
                      */
                        NHLPERROR((NhlINFO,NhlEUNKNOWN,
                            "Dimension names of left hand side and right hand side do not match, overwriting dimension (%s), use (/ .. /) if this is not the desired result",
                             NrmQuarkToString(dimnode->name)));
                        _NclFileWriteDim(infile,tmp_var->var.dim_info[j].dim_quark,dimnode->id);
                    } 
                    if(tmp_var->var.coord_vars[j] != -1)
                    {
                        if (tmp_var->var.dim_info[j].dim_quark == NrmStringToQuark("ncl_scalar"))
                        {
                            NHLPERROR((NhlWARNING,NhlEUNKNOWN,
                                "AdvancedFileWriteVarVar: Variable (%s) has coordinate variable named \"ncl_scalar\"; not writing coodinate variable to file (%s)",
                                  NrmQuarkToString(tmp_var->var.var_quark),
                                  NrmQuarkToString(thefile->advancedfile.fname)));
                            ret = NhlWARNING;
                            continue;
                        }
                        tmp_coord_var = (NclVar)_NclGetObj(tmp_var->var.coord_vars[j]);
                        ret = AdvancedFileWriteCoord(infile,tmp_var->var.dim_info[j].dim_quark,
                                                _NclVarValueRead(tmp_coord_var,NULL,NULL),NULL);
                        if(ret < NhlWARNING)
                        {
                            NHLPERROR((NhlWARNING,NhlEUNKNOWN,
                                "AdvancedFileWriteVarVar: Could not write coordinate variable (%s) to file (%s), continuing anyway",
                                 NrmQuarkToString(tmp_var->var.dim_info[j].dim_quark),
                                 NrmQuarkToString(thefile->advancedfile.fname)));
                            ret = NhlWARNING;
                        }
                        else
                        {
                            if(tmp_coord_var->var.att_id != -1)
                            {
                                theatt = (NclAtt)_NclGetObj(tmp_coord_var->var.att_id);
                                step = theatt->att.att_list;
                                while(step != NULL)
                                {
                                    ret = AdvancedFileWriteVarAtt(infile,tmp_var->var.dim_info[j].dim_quark,
                                                             step->quark,step->attvalue,NULL);
                                    if(ret < NhlWARNING)
                                    {
                                        NHLPERROR((NhlWARNING,NhlEUNKNOWN,
                                            "AdvancedFileWriteVarVar: Could not write attribute (%s) to variable (%s) in file (%s), continuing anyway",
                                              NrmQuarkToString(step->quark),
                                              NrmQuarkToString(tmp_var->var.dim_info[j].dim_quark),
                                              NrmQuarkToString(thefile->advancedfile.fname)));
                                        ret = NhlWARNING;
                                    }
                                    step = step->next;
            
                                }
                            }
                        }
                    }
                    else if(thefile->advancedfile.grpnode->coord_var_rec != NULL)
                    {
			    int k;
			    /* if there is a coordinate for this dimension, then it's an error */
			    for (k = 0; k < thefile->advancedfile.grpnode->coord_var_rec->n_vars; k++) {
				    if (thefile->advancedfile.grpnode->coord_var_rec->var_node[k]->name == dimnode->name) {
					    NHLPERROR((NhlWARNING,NhlEUNKNOWN,
						       "Right hand side has no coordinate variable can not delete coordinate variable of a file, use (/ .. /) to avoid this message"));
					    ret = NhlWARNING;
					    break;
				    }
			    }
                    }
                }
                j++;
            }
        }
        return(ret);
    }
    else
    {
        NHLPERROR((NhlFATAL,NhlEUNKNOWN,
            "AdvancedFileWriteVarVar: file (%s) was opened for reading only, can not write",
             NrmQuarkToString(thefile->advancedfile.fname)));
        return(NhlFATAL);
    }
}

static NhlErrorTypes AdvancedFileWriteDim(NclFile infile, NclQuark dim_name, long dimid)
{
    NclAdvancedFile thefile = (NclAdvancedFile) infile;
    NhlErrorTypes ret = NhlNOERROR;
    NclFileDimNode *dimnode = NULL;

    if((thefile->advancedfile.wr_status <= 0) && (dimid > -1))
    {
        dimnode = _getDimNodeFromNclFileGrpNodeWithID(thefile->advancedfile.grpnode, (int)dimid);
        if(NULL != dimnode)
        {
            if(dim_name != dimnode->name)
            {
                if(thefile->advancedfile.format_funcs->rename_dim != NULL)
                {
                    ret = (*thefile->advancedfile.format_funcs->rename_dim)
                           (thefile->advancedfile.grpnode,
                            dimnode->name, dim_name);

                    if(ret < NhlWARNING)
                    {
                        NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                            "AdvancedFileWriteDim: Could not change dimension (%d) to (%s) for file (%s)",
                             dimid,NrmQuarkToString(dim_name),NrmQuarkToString(thefile->advancedfile.fname)));
                        ret = NhlFATAL;
                    }
                    else
                    {
                        dimnode->name = dim_name;
                    }
                }
            }
        }
        else
        {
            NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                "AdvancedFileWriteDim: Dimension ID (%d) is not in file (%s)",
                 dimid, NrmQuarkToString(thefile->advancedfile.fname)));
            ret = NhlFATAL;
        }
    }
    else
    {
        NHLPERROR((NhlFATAL,NhlEUNKNOWN,
            "AdvancedFileWriteDim: file (%s) was opened for reading only, can not write",
             NrmQuarkToString(thefile->advancedfile.fname)));

        ret = NhlFATAL;
    }

    return (ret);
}

static int AdvancedFileIsDim(NclFile infile, NclQuark dim_name)
{
    NclAdvancedFile thefile = (NclAdvancedFile) infile;
    NclFileDimNode *dimnode;

    dimnode = _getDimNodeFromNclFileGrpNode(thefile->advancedfile.grpnode, dim_name);

    if(NULL != dimnode)
        return (1);
    else
        return (-1);
}

static NhlErrorTypes AdvancedFileWriteCoord(NclFile infile, NclQuark coord_name,
                                       struct _NclMultiDValDataRec *value,
                                       struct _NclSelectionRecord *sel_ptr)
{
    NclAdvancedFile thefile = (NclAdvancedFile) infile;
    NhlErrorTypes ret = NhlNOERROR;
    NclFileVarNode *varnode;
    NclQuark dim_names[NCL_MAX_DIMENSIONS];
    int dindex, n;

    if(thefile->advancedfile.wr_status <= 0)
    {
        dindex = AdvancedFileIsDim(infile, coord_name);
        if(dindex > -1)
        {
            varnode = _getVarNodeFromNclFileGrpNode(thefile->advancedfile.grpnode, coord_name);
            if((NULL == varnode) && (NULL != value))
            {
                for(n = 0; n < NCL_MAX_DIMENSIONS; ++n)
                    dim_names[n] = -1;

                dim_names[0] = coord_name;
                ret = _addNclVarNodeToGrpNode(thefile->advancedfile.grpnode, coord_name,
                                              thefile->advancedfile.grpnode->var_rec->n_vars, value->multidval.data_type,
                                              value->multidval.n_dims, dim_names, value->multidval.dim_sizes);
                varnode = _getVarNodeFromNclFileGrpNode(thefile->advancedfile.grpnode, coord_name);
            }
            if(NULL != varnode)
                ret = _addNclCoordVarNode(&(thefile->advancedfile.grpnode->coord_var_rec), varnode);
            ret = MyAdvancedFileWriteVar(infile, coord_name, value, sel_ptr, NULL, FILE_COORD_VAR_ACCESS);
        }
        else
        {
            NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                "AdvancedFileWriteCoord: Dimension (%s) is not a valid dimension in file (%s), can't write coord_var",
                 NrmQuarkToString(coord_name),NrmQuarkToString(thefile->advancedfile.fname)));
            ret = NhlFATAL;
        }
    }
    else
    {
        NHLPERROR((NhlFATAL,NhlEUNKNOWN,
            "AdvancedFileWriteCoord: file (%s) was opened for reading only, can not write",
             NrmQuarkToString(thefile->advancedfile.fname)));
        ret = NhlFATAL;
    }

    return(ret);
}

static int AdvancedVarAttIndex(NclFileVarNode *varnode, NclQuark theatt)
{
    int i;
    NclFileAttNode *attnode;

    if(NULL != varnode)
    {
        if(NULL != varnode->att_rec)
        {
            for(i = 0; i < varnode->att_rec->n_atts; i++)
            {
                attnode = &(varnode->att_rec->att_node[i]);
                if(theatt == attnode->name)
                    return i;
            }
        }
    }

    return(-1);
}

static int AdvancedFileIsVarAtt(NclFile infile, NclQuark var, NclQuark theatt)
{
    NclAdvancedFile thefile = (NclAdvancedFile) infile;
    NclFileVarNode *varnode;

    varnode = _getVarNodeFromNclFileGrpNode(thefile->advancedfile.grpnode, var);
    return (AdvancedVarAttIndex(varnode, theatt));
}

static int AdvancedFileIsAtt(NclFile infile,NclQuark theatt)
{
    NclAdvancedFile thefile = (NclAdvancedFile) infile;
    NclFileAttNode *attnode;
    int i;

    if(NULL != thefile->advancedfile.grpnode->att_rec)
    {
        for(i = 0; i < thefile->advancedfile.grpnode->att_rec->n_atts; i++)
        {
            attnode = &(thefile->advancedfile.grpnode->att_rec->att_node[i]);
            if(attnode->name == theatt) 
                return(i);
        }
    }
    return(-1);
}

static struct _NclMultiDValDataRec *AdvancedFileReadAtt(NclFile infile, NclQuark attname,
                                                   struct _NclSelectionRecord *sel_ptr)
{
    NclAdvancedFile thefile = (NclAdvancedFile) infile;
    NclFileAttRecord *attrec;
    NclFileAttNode   *attnode;
    int att_id = -1,i;
    NclMultiDValData tmp_md;
    NhlArgVal udata;
    ng_size_t ne;

    attnode = _getAttNodeFromNclFileGrpNode(thefile->advancedfile.grpnode, attname);
    if(NULL != attnode)
    {
        attrec = thefile->advancedfile.grpnode->att_rec;
        if(attrec->id != -1)
        {
            return(_NclGetAtt(attrec->id, NrmQuarkToString(attname), sel_ptr));
        }

        att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,(NclObj)thefile);
        for(i = 0; i < attrec->n_atts; i++)
        {
            attnode = &(attrec->att_node[i]);

            if(NULL == attnode->value)
            {
               if(thefile->advancedfile.format_funcs->read_att != NULL)
               {
                    attnode->value = NclMalloc(_NclSizeOf(attnode->type) * attnode->n_elem);
                    assert(attnode->value);

                    (void)(*thefile->advancedfile.format_funcs->read_att)
                           (thefile->advancedfile.grpnode,
                            attnode->name, attnode->value);
                }
            }

            if(NULL != attnode->value)
            {
                ne = attnode->n_elem;
                tmp_md = _NclCreateMultiDVal(
                        NULL,
                        NULL,
                        Ncl_MultiDValData,
                        0,
                        attnode->value,
                        NULL,
                        1,
                        &ne,
                        TEMPORARY,
                        NULL,
                        _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(attnode->type)));

                if(tmp_md != NULL)
                {
                    _NclAddAtt(att_id,NrmQuarkToString(attnode->name),tmp_md,NULL);
                }
            }
        }

        udata.ptrval = (void*)NclMalloc(sizeof(FileCallBackRec));
        ((FileCallBackRec*)udata.ptrval)->thefileid = thefile->obj.id;
        ((FileCallBackRec*)udata.ptrval)->theattid = att_id;
        ((FileCallBackRec*)udata.ptrval)->thevar = -1;
        attrec->cb = _NclAddCallback((NclObj)_NclGetObj(att_id),NULL,
                                         FileAttIsBeingDestroyedNotify,ATTDESTROYED,&udata);
        attrec->udata = (FileCallBackRec*)udata.ptrval;

        if(att_id != -1)
        {    
            attrec->id = att_id;
            return(_NclGetAtt(att_id,NrmQuarkToString(attname),sel_ptr));
        }
    }

#if 1
    NHLPERROR((NhlINFO,NhlEUNKNOWN,
        "AdvancedFileReadVarAtt: (%s) is not an attribute of (%s)",
         NrmQuarkToString(attname),NrmQuarkToString(thefile->advancedfile.fname)));
#else
    NHLPERROR((NhlWARNING,NhlEUNKNOWN,
        "AdvancedFileReadVarAtt: (%s) is not an attribute of (%s)",
         NrmQuarkToString(attname),NrmQuarkToString(thefile->advancedfile.fname)));
#endif

    return(_NclCreateMissing());
}

static NhlErrorTypes AdvancedFileDelAtt(NclFile infile, NclQuark attname)
{
    NclAdvancedFile thefile = (NclAdvancedFile) infile;
    NhlErrorTypes ret = NhlFATAL;
    NclFileAttRecord *attrec;
    NclFileAttNode   *attnode;

    if(thefile->advancedfile.wr_status <= 0)
    {
        attnode = _getAttNodeFromNclFileGrpNode(thefile->advancedfile.grpnode, attname);
        if(NULL != attnode)
        {
            attrec = thefile->advancedfile.grpnode->att_rec;
            if(thefile->advancedfile.format_funcs->del_att != NULL)
            {
                ret = (*thefile->advancedfile.format_funcs->del_att)(thefile->advancedfile.grpnode, attname);

                ret = _delNclAttNode(&attrec, attname);
            }
            else
            {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Attribute deletion not supported by format"));
            }
        }
        else
        {
            NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Attempt to delete undefined attribute from file"));
        }
    }
    else
    {
        NHLPERROR((NhlFATAL,NhlEUNKNOWN,
           "AdvancedFileDelAtt: file (%s) is read only, can not delete attribute",
            NrmQuarkToString(thefile->advancedfile.fname)));
    }
    return(ret);
}

static NhlErrorTypes AdvancedFileDelVarAtt(NclFile infile, NclQuark var, NclQuark attname)
{
    NclAdvancedFile thefile = (NclAdvancedFile) infile;
    NhlErrorTypes ret = NhlNOERROR;
    NclFileAttRecord *attrec;
    NclFileAttNode   *attnode;
    NclFileVarNode   *varnode;

    if(thefile->advancedfile.wr_status <= 0)
    {
        varnode = _getVarNodeFromNclFileGrpNode(thefile->advancedfile.grpnode, var);
        if(NULL != varnode)
        {
            attnode = _getAttNodeFromNclFileVarNode(varnode, attname);

            if(NULL != attnode)
            {
                attrec = varnode->att_rec;
                if(thefile->advancedfile.format_funcs->del_var_att != NULL)
                {
                    ret = (*thefile->advancedfile.format_funcs->del_var_att)
                           (thefile->advancedfile.grpnode,var,attname);
                    ret = _delNclAttNode(&attrec, attname);
                }
                else
                {
                    NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Attribute deletion not supported by format"));
                    ret = NhlFATAL;
                }
            }
            else
            {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Attempt to delete undefined attribute from variable"));
                ret = NhlFATAL;
            }
        }
        else
        {
            NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Attempt to delete attribute from undefined variable"));
            ret = NhlFATAL;
        }
    }
    else
    {
        NHLPERROR((NhlFATAL,NhlEUNKNOWN,
            "AdvancedFileDelVarAtt: file (%s) is read only, can not delete attribute",
             NrmQuarkToString(thefile->advancedfile.fname)));
        ret = NhlFATAL;
    }

    return (ret);
}

static int myVarIsDimInGrpNode(NclFileGrpNode *grpnode, NclQuark var, NclQuark dim_name)
{
    NclFileDimNode  *dimnode;
    NclFileVarNode  *varnode;
    int i, n;

    varnode = _getVarNodeFromNclFileGrpNode(grpnode, var);
    if(NULL != varnode)
    {
        for(n = 0; n < grpnode->dim_rec->n_dims; n++)
        {
            dimnode = &(grpnode->dim_rec->dim_node[n]);
            if(dim_name == dimnode->name)
                return n;
        }

        if(NULL != grpnode->grp_rec)
        {
            for(n = 0; n < grpnode->grp_rec->n_grps; n++)
            {
                i = myVarIsDimInGrpNode(grpnode->grp_rec->grp_node[n], var, dim_name);
                if(i > -1)
                    return i;
            }
        }
    }

    return(-1);
}

static int AdvancedFileVarIsDim(NclFile infile, NclQuark var, NclQuark dim_name)
{
    NclAdvancedFile thefile = (NclAdvancedFile) infile;

    return (myVarIsDimInGrpNode(thefile->advancedfile.grpnode, var, dim_name));
}

static struct _NclMultiDValDataRec *AdvancedFileVarReadDim(NclFile infile, NclQuark var,
                                                      NclQuark dim_name, long dim_num)
{
    NclAdvancedFile thefile = (NclAdvancedFile) infile;
    NclFileDimNode   *dimnode;
    NclFileVarNode   *varnode;

    int i;
    int *tmpi;
    NclQuark *tmpq;
    ng_size_t output_dim_sizes = 1;
    
    varnode = _getVarNodeFromNclFileGrpNode(thefile->advancedfile.grpnode, var);
    if(NULL != varnode)
    {
        if(dim_name > -1)
        {
            for( i=0; i < varnode->dim_rec->n_dims; i++)
            {
                dimnode = &(varnode->dim_rec->dim_node[i]);
                if(dim_name == dimnode->name)
                {
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
        }
        else if ( dim_num > -1)
        {
	    dimnode = &(varnode->dim_rec->dim_node[dim_num]);
            if(NULL != dimnode)
            {
                tmpq = (NclQuark*)NclMalloc(sizeof(NclQuark));
                *tmpq = dimnode->name;
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

            NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                "Dimension number (%d) is out of range for variable (%s->%s)",
                 dim_num,NrmQuarkToString(thefile->advancedfile.fname),NrmQuarkToString(var)));
        } 
    }

    return(NULL);
}

static NhlErrorTypes AdvancedFileVarWriteDim(NclFile infile, NclQuark var, NclQuark dim_name, long dim_num)
{
    NclAdvancedFile thefile = (NclAdvancedFile) infile;
    NhlErrorTypes ret = NhlNOERROR;
    NclFileDimNode   *dimnode;
    NclFileVarNode   *varnode;
    NclQuark old_name;

    if(thefile->advancedfile.wr_status <= 0)
    {
        varnode = _getVarNodeFromNclFileGrpNode(thefile->advancedfile.grpnode, var);
        if(NULL != varnode)
        {
            if((dim_num > -1)&&(dim_num < varnode->dim_rec->n_dims))
            {

                dimnode = _getDimNodeFromNclFileGrpNodeWithID(thefile->advancedfile.grpnode, (int)dim_num);
                old_name = dimnode->name;

                if(thefile->advancedfile.format_funcs->rename_dim != NULL)
                {
                    if((*thefile->advancedfile.format_funcs->rename_dim)
                        (thefile->advancedfile.grpnode,
                        old_name, dim_name) < NhlWARNING)
                    {
                        
                        NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                            "AdvancedFileVarWriteDim: Cannot rename dimension (%ld) in variable (%s)",
                             dim_num, NrmQuarkToString(var)));
                        ret = NhlFATAL;
                    }
                    else
                    {
                        dimnode->name = dim_name;
                        ret = NhlNOERROR;
                    }
                }
            }
        }
    }
    else
    {
        NHLPERROR((NhlFATAL,NhlEUNKNOWN,
            "AdvancedFileVarWriteDim: file (%s) was opened for reading only, can not write",
             NrmQuarkToString(thefile->advancedfile.fname)));
        ret = NhlFATAL;
    }
    return (ret);
}

static struct _NclMultiDValDataRec* AdvancedFileReadDim(NclFile infile,
                                                   NclQuark dim_name,
                                                   long dim_num)
{
    NclAdvancedFile thefile = (NclAdvancedFile) infile;
    NclQuark *tmps;
    int *tmpl;
    ng_size_t output_dim_sizes = 1;
    NclFileDimNode *dimnode;

    if(dim_name != -1)
    {
        dimnode = _getDimNodeFromNclFileGrpNode(thefile->advancedfile.grpnode, dim_name);
        if(NULL != dimnode)
        {
            tmpl = (int*)NclMalloc(sizeof(int));
            *tmpl = dimnode->id;
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

        NHLPERROR((NhlFATAL,NhlEUNKNOWN,
            "AdvancedFileReadDim: Dimension (%s) is not a defined dimension in file (%s)",
             NrmQuarkToString(dim_name),NrmQuarkToString(thefile->advancedfile.fname)));
        return(NULL);
    }
    else if(dim_num > -1)
    {
        dimnode = _getDimNodeFromNclFileGrpNodeWithID(thefile->advancedfile.grpnode, (int)dim_num);
        if(NULL != dimnode)
        {
            tmps = (NclQuark*)NclMalloc(sizeof(NclQuark));
            *tmps = dimnode->name;
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

        NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Dimension #%ld is out of range",dim_num));
        return(NULL);
    }
    else
    {
        return(NULL);
    }
}

static int _getGroupIdFromGrpNode(NclFileGrpNode *grpnode, NclQuark group)
{
    NclFileGrpNode *tmpgrpnode;
    int i, id;

    if(NULL == grpnode)
        return(-1);

  /*
   *fprintf(stderr, "\nEnter _getGroupIndexFromGrpNode. file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tgroup name: <%s>\n", NrmQuarkToString(group));
   */

    if(NULL != grpnode->grp_rec)
    {
        for(i = 0; i < grpnode->grp_rec->n_grps; i++)
        {
            tmpgrpnode = grpnode->grp_rec->grp_node[i];
          /*
           *fprintf(stderr, "\tCheck %d: grpname <%s>\n", i, 
           *                   NrmQuarkToString(tmpgrpnode->name));
           */

            if(group == tmpgrpnode->name)
            {
              /*
               *fprintf(stderr, "\tFind group <%s>\n\n", NrmQuarkToString(group));
               *fprintf(stderr, "Leave _getGroupIndexFromGrpNode. file: %s, line: %d\n\n", __FILE__, __LINE__);
               */
                return (tmpgrpnode->gid);
            }

            id = _getGroupIdFromGrpNode(tmpgrpnode, group);
            if(-1 != id)
                return (id);
        }
    }
  /*
   *fprintf(stderr, "\tCANNOT FIND group: <%s>\n", NrmQuarkToString(group));
   *fprintf(stderr, "Leave AdvancedFileIsGroup. file: %s, line: %d\n\n", __FILE__, __LINE__);
   */
    return(-1);
}

static int AdvancedFileIsGroup(NclFile infile, NclQuark group)
{
    NclAdvancedFile thefile = (NclAdvancedFile) infile;

    return (_getGroupIdFromGrpNode(thefile->advancedfile.grpnode, group));
}

NclGroup *AdvancedFileReadGroup(NclFile infile, NclQuark group_name)
{
    NclGroup *group_out = NULL;
    int index;

    index = AdvancedFileIsGroup(infile, group_name);

  /*
   *fprintf(stderr, "\nEnter AdvancedFileReadGroup, file: %s, line:%d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tgroup_name: <%s>\n", NrmQuarkToString(group_name));
   *fprintf(stderr, "\tindex = %d\n", index);
   */

    if(index < 0)
        return (NULL);

    group_out = _NclCreateGroup(NULL,NULL,Ncl_File,0,TEMPORARY,infile,group_name);

  /*
   *fprintf(stderr, "Leave FileReadGroup, file: %s, line:%d\n\n", __FILE__, __LINE__);
   */

    return (group_out);
}

static NhlErrorTypes AdvancedFileWriteGrp(NclFile infile, NclQuark grpname)
{
    NclAdvancedFile thefile = (NclAdvancedFile) infile;
    NhlErrorTypes ret = NhlNOERROR;

  /*
   *fprintf(stderr, "\nEnter AdvancedFileWriteGrp, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tgrpname: <%s>\n", NrmQuarkToString(grpname));
   */

    if(thefile->advancedfile.wr_status > 0)
    {
        NHLPERROR((NhlFATAL,NhlEUNKNOWN,
            "AdvancedFileWriteGrp: file (%s) was opened for reading only, can not write",
             NrmQuarkToString(thefile->advancedfile.fname)));
        return (NhlFATAL);
    }

    if(thefile->advancedfile.format_funcs->add_grp != NULL)
    {
      /*
       *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\tgrpname: <%s>\n", NrmQuarkToString(grpname));
       */
        ret = (*thefile->advancedfile.format_funcs->add_grp)
               ((void *)thefile->advancedfile.grpnode, grpname);
    }
    
  /*
   *fprintf(stderr, "Leave AdvancedFileWriteGrp, file: %s, line: %d\n\n", __FILE__, __LINE__);
   */
    return ret;
}

NhlErrorTypes AdvancedFileCreateVlenType(NclFile infile, NclQuark vlen_name, NclQuark var_name,
                                    NclQuark type, NclQuark *dim_names, ng_size_t ndims)
{
    NclAdvancedFile thefile = (NclAdvancedFile) infile;
    NhlErrorTypes ret = NhlNOERROR;

  /*
   *fprintf(stderr, "\nEnter AdvancedFileCreateVlenType, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tvlen_name: <%s>\n", NrmQuarkToString(vlen_name));
   *fprintf(stderr, "\tvar_name: <%s>\n", NrmQuarkToString(var_name));
   *fprintf(stderr, "\ttype: <%s>\n", NrmQuarkToString(type));
   */

    if(thefile->advancedfile.wr_status > 0)
    {
        NHLPERROR((NhlFATAL,NhlEUNKNOWN,
            "AdvancedFileCreateVlenType: file (%s) was opened for reading only, can not write",
             NrmQuarkToString(thefile->advancedfile.fname)));
        return (NhlFATAL);
    }

    if(thefile->advancedfile.format_funcs->add_vlen != NULL)
    {
        ret = (*thefile->advancedfile.format_funcs->add_vlen)
               ((void *)thefile->advancedfile.grpnode, vlen_name, var_name, type, dim_names, ndims);
    }
    
  /*
   *fprintf(stderr, "Leave AdvancedFileCreateVlenType, file: %s, line: %d\n\n", __FILE__, __LINE__);
   */
    return ret;
}

NclVar _NclCreateVlenVar(char *var_name, void *val,
                         int ndims, NclQuark *dimnames,
                         ng_size_t *dimsizes, NclBasicDataTypes type)
{
    NclVar tmp_var = NULL;
    NclMultiDValData tmp_md = NULL;
    NclDimRec dim_info[NCL_MAX_DIMENSIONS];
    int coords[NCL_MAX_DIMENSIONS];
    int att_id = -1;
    int i;
    
  /*
   *fprintf(stderr, "\n\nhit _NclCreateVlenVar, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tvar_name: <%s>\n", var_name);
   *fprintf(stderr, "\tndims = %d\n", ndims);
   */

    for(i = 0; i< ndims; i++)
    {
        dim_info[i].dim_num   = i;
        dim_info[i].dim_size  = dimsizes[i];
        dim_info[i].dim_quark = dimnames[i];
        coords[i] = -1;
    }

    tmp_md = _NclCreateMultiDVal(
             NULL,
             NULL,
             Ncl_MultiDValData,
             0,
             val,
             NULL,
             ndims,
             dimsizes,
             TEMPORARY,
             NULL,
             _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type))
             );

  /*
   *tmp_md = MyAdvancedFileReadVarValue(infile,NrmStrinToQuark(var_name),NULL,dim_info,FILE_VAR_ACCESS);
   */

    if(tmp_md == NULL)
        return(NULL);

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
              type,
              var_name,
              TEMPORARY);

    if(tmp_var == NULL)
    {
        _NclDestroyObj((NclObj)tmp_md);
    }

    return(tmp_var);
}

NhlErrorTypes AdvancedFileCreateEnumType(NclFile infile, NclQuark enum_name, NclQuark var_name,
                                    NclQuark dim_name, NclQuark *mem_name, void *mem_value,
                                    ng_size_t n_mems, NclBasicDataTypes val_type)
{
    NclAdvancedFile thefile = (NclAdvancedFile) infile;
    NhlErrorTypes ret = NhlNOERROR;

  /*
   *fprintf(stderr, "\nEnter AdvancedFileCreateEnumType, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tenum_name: <%s>\n", NrmQuarkToString(enum_name));
   *fprintf(stderr, "\tvar_name: <%s>\n", NrmQuarkToString(var_name));
   *fprintf(stderr, "\tdim_name: <%s>\n", NrmQuarkToString(dim_name));
   */

    if(thefile->advancedfile.wr_status > 0)
    {
        NHLPERROR((NhlFATAL,NhlEUNKNOWN,
            "AdvancedFileCreateEnumType: file (%s) was opened for reading only, can not write",
             NrmQuarkToString(thefile->advancedfile.fname)));
        return (NhlFATAL);
    }

    if(thefile->advancedfile.format_funcs->add_enum != NULL)
    {
        ret = (*thefile->advancedfile.format_funcs->add_enum)
               ((void *)thefile->advancedfile.grpnode, enum_name, var_name, dim_name,
                mem_name, mem_value, n_mems, val_type);
    }
    
  /*
   *fprintf(stderr, "Leave AdvancedFileCreateEnumType, file: %s, line: %d\n\n", __FILE__, __LINE__);
   */
    return ret;
}

NhlErrorTypes AdvancedFileCreateOpaqueType(NclFile infile, NclQuark opaque_name, NclQuark var_name,
                                      int var_size, NclQuark dim_name)
{
    NclAdvancedFile thefile = (NclAdvancedFile) infile;
    NhlErrorTypes ret = NhlNOERROR;

  /*
   *fprintf(stderr, "\nEnter AdvancedFileCreateOpaqueType, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\topaque_name: <%s>\n", NrmQuarkToString(opaque_name));
   *fprintf(stderr, "\tvar_name: <%s>\n", NrmQuarkToString(var_name));
   *fprintf(stderr, "\tvar_size: <%d>\n", var_size);
   *fprintf(stderr, "\tdim_name: <%s>\n", NrmQuarkToString(dim_name));
   */

    if(thefile->advancedfile.wr_status > 0)
    {
        NHLPERROR((NhlFATAL,NhlEUNKNOWN,
            "AdvancedFileCreateOpaqueType: file (%s) was opened for reading only, can not write",
             NrmQuarkToString(thefile->advancedfile.fname)));
        return (NhlFATAL);
    }

    if(thefile->advancedfile.format_funcs->add_opaque != NULL)
    {
        ret = (*thefile->advancedfile.format_funcs->add_opaque)
               ((void *)thefile->advancedfile.grpnode, opaque_name, var_name, var_size, dim_name);
    }
    
  /*
   *fprintf(stderr, "Leave AdvancedFileCreateOpaqueType, file: %s, line: %d\n\n", __FILE__, __LINE__);
   */
    return ret;
}

NhlErrorTypes AdvancedFileCreateCompoundType(NclFile infile, NclQuark compound_name,
                                        NclQuark var_name, 
                                        ng_size_t n_dims, NclQuark *dim_name,
                                        ng_size_t n_mems, NclQuark *mem_name,
                                        NclQuark *mem_type, int *mem_size)
{
    NclAdvancedFile thefile = (NclAdvancedFile) infile;
    NhlErrorTypes ret = NhlNOERROR;

  /*
   *fprintf(stderr, "\nEnter AdvancedFileCreateCompoundType, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tcompound_name: <%s>\n", NrmQuarkToString(compound_name));
   *fprintf(stderr, "\tvar_name: <%s>\n", NrmQuarkToString(var_name));
   *fprintf(stderr, "\tdim_name: <%s>\n", NrmQuarkToString(dim_name[0]));
   */

    if(thefile->advancedfile.wr_status > 0)
    {
        NHLPERROR((NhlFATAL,NhlEUNKNOWN,
            "AdvancedFileCreateCompoundType: file (%s) was opened for reading only, can not write",
             NrmQuarkToString(thefile->advancedfile.fname)));
      /*
       *fprintf(stderr, "Leave AdvancedFileCreateCompoundType, file: %s, line: %d\n\n", __FILE__, __LINE__);
       */
        return (NhlFATAL);
    }

    if(thefile->advancedfile.format_funcs->add_compound != NULL)
    {
        ret = (*thefile->advancedfile.format_funcs->add_compound)
               ((void *)thefile->advancedfile.grpnode, compound_name, var_name,
                n_dims, dim_name,
                n_mems, mem_name, mem_type, mem_size);
    }
    
  /*
   *fprintf(stderr, "Leave AdvancedFileCreateCompoundType, file: %s, line: %d\n\n", __FILE__, __LINE__);
   */
    return ret;
}

NhlErrorTypes AdvancedFileWriteCompound(NclFile infile, NclQuark compound_name, NclQuark var_name, 
                                   ng_size_t n_mems, NclQuark *mem_name, NclList thelist)
{
    NclAdvancedFile thefile = (NclAdvancedFile) infile;
    NhlErrorTypes ret = NhlNOERROR;

  /*
   *fprintf(stderr, "\nEnter AdvancedFileWriteCompound, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tcompound_name: <%s>\n", NrmQuarkToString(compound_name));
   *fprintf(stderr, "\tvar_name: <%s>\n", NrmQuarkToString(var_name));
   *fprintf(stderr, "\tmem_name: <%s>\n", NrmQuarkToString(mem_name[0]));
   */

    if(thefile->advancedfile.wr_status > 0)
    {
        NHLPERROR((NhlFATAL,NhlEUNKNOWN,
            "AdvancedFileWriteCompound: file (%s) was opened for reading only, can not write",
             NrmQuarkToString(thefile->advancedfile.fname)));
      /*
       *fprintf(stderr, "Leave AdvancedFileWriteCompound, file: %s, line: %d\n\n", __FILE__, __LINE__);
       */
        return (NhlFATAL);
    }

    if(thefile->advancedfile.format_funcs->write_compound != NULL)
    {
        ret = (*thefile->advancedfile.format_funcs->write_compound)
               ((void *)thefile->advancedfile.grpnode, compound_name, var_name,
                n_mems, mem_name, thelist);
    }
    
  /*
   *fprintf(stderr, "Leave AdvancedFileWriteCompound, file: %s, line: %d\n\n", __FILE__, __LINE__);
   */
    return ret;
}

NclQuark *_NclGetGrpNames(void *therec, int *num_grps)
{
    NclFileGrpNode *grpnode = (NclFileGrpNode *) therec;
    NclFileGrpNode *tmpgrpnode = NULL;
    NclQuark *out_quarks = NULL;
    NclQuark *tmp_quarks = NULL;
    char carr[2048];
    int n, ng;
    int i;

    *num_grps = 0;
    if(NULL != grpnode->grp_rec)
    {
        if(grpnode->grp_rec->n_grps)
        {
            *num_grps = grpnode->grp_rec->n_grps;

            out_quarks = (NclQuark *)NclCalloc(*num_grps, sizeof(NclQuark));
            assert(out_quarks);

            for(i = 0; i < grpnode->grp_rec->n_grps; ++i)
            {
#if 1
                strcpy(carr, NrmQuarkToString(grpnode->grp_rec->grp_node[i]->real_name));
                n = strlen(carr) - 1;
              /*
               *fprintf(stderr, "\nIn file: %s, line: %d\n", __FILE__, __LINE__);
               *fprintf(stderr, "\tori group No. %d: <%s>, strlen = %d\n", i, carr, n);
               */
                if(carr[n] == '/')
                {
                    carr[n] = '\0';
                  /*
                   *fprintf(stderr, "\tnew group No. %d: <%s>, strlen = %d\n", i, carr, n);
                   */
                    out_quarks[i] = NrmStringToQuark(carr);
                }
                else
                    out_quarks[i] = grpnode->grp_rec->grp_node[i]->real_name;
#else
                out_quarks[i] = grpnode->grp_rec->grp_node[i]->name;
#endif
            }

            for(n = 0; n < grpnode->grp_rec->n_grps; ++n)
            {
                tmpgrpnode = grpnode->grp_rec->grp_node[n];

                tmp_quarks = _NclGetGrpNames((void *)tmpgrpnode, &ng);

                if(ng)
                {
                    out_quarks = (NclQuark *)NclRealloc(out_quarks,
                                                (*num_grps + ng) * sizeof(NclQuark));
                    assert(out_quarks);

                    for(i = 0; i < ng; ++i)
                    {
                        out_quarks[*num_grps + i] = tmp_quarks[i];
                    }
                    NclFree(tmp_quarks);

                    *num_grps += ng;
                }
            }
        }
    }

    return(out_quarks);
}

NclAdvancedFileClassRec nclAdvancedFileClassRec =
{
    {        
        "NclAdvancedFileClass",
        sizeof(NclAdvancedFileRec),
        (NclObjClass)&nclObjClassRec,
        0,
        (NclGenericFunction)                      AdvancedFileDestroy,
        (NclSetStatusFunction)                    NULL,
        (NclInitPartFunction)                     NULL,
        (NclInitClassFunction)                    InitializeAdvancedFileClass,
        (NclAddParentFunction)                    FileAddParent,
        (NclDelParentFunction)                    FileDelParent,
      /* NclPrintSummaryFunction print_summary */ NULL,
        (NclPrintFunction)                        AdvancedFilePrint,
      /* NclCallBackList* create_callback */      NULL,
      /* NclCallBackList* delete_callback */      NULL,
      /* NclCallBackList* modify_callback */      NULL,
      /* NclObtainCall obtain_calldata */         FileObtainCallData
    },
    {
       /*NclFileVarRepValueFunc         rep_val*/                      AdvancedFileVarRepValue,
       /*NclFileIsAFunc                 is_var*/                       AdvancedFileIsVar,
       /*NclAssignFileVarFunc           write_var*/                    AdvancedFileWriteVar,
       /*NclAssignFileVarVarFunc        write_var_var*/                AdvancedFileWriteVarVar,
       /*NclGetFileVarFunc              read_var_func*/                AdvancedFileReadVar,
       /*NclGetFileVarValFunc           read_var_val_func*/            AdvancedFileReadVarValue,
       /*NclFileIsAFunc                 is_att*/                       AdvancedFileIsAtt,
       /*NclReadAttributeFunc           read_att_func*/                AdvancedFileReadAtt,
       /*NclWriteAttributeFunc          write_att_func*/               AdvancedFileWriteAtt,
       /*NclDeleteAttributeFunc         del_att_func*/                 AdvancedFileDelAtt,
       /*NclFileVarIsAFunc              is_var_att*/                   AdvancedFileIsVarAtt,
       /*NclReadVarAttributeFunc        read_var_att_func*/            AdvancedFileReadVarAtt,
       /*NclWriteVarAttributeFunc       write_var_att_func*/           AdvancedFileWriteVarAtt,
       /*NclDeleteVarAttributeFunc      del_var_att_func*/             AdvancedFileDelVarAtt,
       /*NclFileIsAFunc                 is_dim*/                       AdvancedFileIsDim,
       /*NclFileVarIsAFunc              is_var_dim*/                   AdvancedFileVarIsDim,
       /*NclReadVarDimensionFunc        read_var_dim_func*/            AdvancedFileVarReadDim,
       /*NclWriteVarDimensionFunc       write_var_dim_func*/           AdvancedFileVarWriteDim,
       /*NclReadDimensionFunc           read_dim_func*/                AdvancedFileReadDim,
       /*NclWriteDimensionFunc          write_dim_func*/               AdvancedFileWriteDim,
       /*NclFileIsAFunc                 is_coord*/                     AdvancedFileIsCoord,
       /*NclReadFileCoordFunc           read_coord_func*/              AdvancedFileReadCoord,
       /*NclWriteFileCoordFunc          write_coord_func*/             AdvancedFileWriteCoord,
       /*NclAddFileDimFunc              add_dim_func*/                 AdvancedFileAddDim,
       /*NclAddFileChunkDimFunc         add_chunk_dim_func*/           AdvancedFileAddChunkDim,
       /*NclAddFileVarFunc              add_var_func*/                 AdvancedFileAddVar,
       /*NclAddFileVarChunkFunc         add_var_chunk_func*/           AdvancedFileAddVarChunk,
       /*NclAddFileVarChunkCacheFunc    add_var_chunk_cache_func*/     AdvancedFileAddVarChunkCache,
       /*NclSetFileVarCompressLevelFunc set_var_compress_level_func;*/ AdvancedFileSetVarCompressLevel,
       /*NclAddFileVarAttFunc           add_var_att_func*/             NULL,
       /*NclAddFileAttFunc              add_att_func*/                 NULL,
       /*NclSetFileOptionFunc           set_file_option*/              AdvancedFileSetFileOption,
       /*NclFileOption                 *options*/                      file_options,
       /*NclFileIsAFunc                 is_group*/                     AdvancedFileIsGroup,
       /*NclGetFileGroupFunc            read_group_func*/              AdvancedFileReadGroup,
         Ncl_NUMBER_OF_FILE_OPTIONS
    },

    {
       /*NclAssignFileGrpFunc           write_grp*/                    AdvancedFileWriteGrp,
       /*NclAssignFileVlenFunc          create_vlen_type*/             AdvancedFileCreateVlenType,
       /*NclAssignFileEnumFunc          create_enum_type*/             AdvancedFileCreateEnumType,
       /*NclAssignFileOpaqueFunc        create_opaque_type*/           AdvancedFileCreateOpaqueType,
       /*NclAssignFileCompoundFunc      create_compound_type*/         AdvancedFileCreateCompoundType,
       /*NclWriteFileCompoundFunc       write_compound*/               AdvancedFileWriteCompound,
         0
    }
};

NclObjClass nclAdvancedFileClass = (NclObjClass)&nclAdvancedFileClassRec;


/*ARGSUSED*/
static NhlErrorTypes CvtHLUGENTYPEREPToNclData
#if	NhlNeedProto
(NrmValue *from, NrmValue *to, NhlConvertArgList args, int nargs)
#else
(from, to, args, nargs)
NrmValue *from;
NrmValue *to;
NhlConvertArgList args;
int nargs;
#endif
{
	NhlGenArray gen;
	char func[] = "CvtHLUGENTYPEREPToNclData";
	void *val;
	NclMultiDValData tmp_md;
	

	if(nargs != 0) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: called with wrong number of args",func);
		to->size =0;
		return(NhlFATAL);
	}
	gen = (NhlGenArray)from->data.ptrval;
	if(from->typeQ != NrmStringToQuark(((NclMultiDValDATATYPEDataClass)nclMultiDValDATATYPEDataClass)->multid_class.hlu_gen_type_rep)) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: called with wrong input type",func);
		to->size =0;
		return(NhlFATAL);
	}
	if(gen->my_data) {
		val = gen->data;
		gen->my_data = False;
	} else {
		val = NclMalloc((unsigned)gen->size * gen->num_elements);
		memcpy(val,gen->data,(unsigned)gen->size * gen->num_elements);
	}
	tmp_md = _NclMultiDValDATATYPECreate(
		NULL,NULL, Ncl_MultiDValDATATYPEData,
		0,val,NULL,gen->num_dimensions,
		gen->len_dimensions,TEMPORARY,NULL);
	if(to->size < sizeof(NclMultiDValData)) {
		return(NhlFATAL);
	} else {
		*((NclMultiDValData*)(to->data.ptrval)) = (void*)tmp_md;
        	return(NhlNOERROR);
	}
}
/*ARGSUSED*/
static NhlErrorTypes CvtHLUTYPEREPToNclData
#if	NhlNeedProto
(NrmValue *from, NrmValue *to, NhlConvertArgList args, int nargs)
#else
(from, to, args, nargs)
NrmValue *from;
NrmValue *to;
NhlConvertArgList args;
int nargs;
#endif
{
	NhlArgVal * tmp;
	NclMultiDValData tmp_md;
	int n_dims = 1,len_dims = 1;

	tmp = NclMalloc((unsigned)sizeof(NhlArgVal));
	memcpy((void*)tmp,(void*)&from->data,sizeof(NhlArgVal));
	tmp_md = _NclMultiDValDATATYPECreate(
		NULL,NULL, Ncl_MultiDValDATATYPEData,
		0,(void*)tmp,NULL,n_dims,
		&len_dims,TEMPORARY,NULL);
	if(to->size < sizeof(NclMultiDValData)) {
		return(NhlFATAL);
	} else {
		*((NclMultiDValData*)(to->data.ptrval)) = (void*)tmp_md;
        	return(NhlNOERROR);
	}
}

static NhlErrorTypes MultiDVal_DATATYPE_InitClass
#if	NhlNeedProto
(void)
#else
()
#endif
{
        NhlRegisterConverter(HLUGENTYPEREP,NhlTNclData,CvtHLUGENTYPEREPToNclData,NULL,0,False,NULL);
        NhlRegisterConverter(HLUTYPEREP,NhlTNclData,CvtHLUTYPEREPToNclData,NULL,0,False,NULL);
	return(NhlNOERROR);
}


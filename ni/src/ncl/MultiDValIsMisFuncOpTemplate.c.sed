int MultiDVal_DATATYPE_is_mis
#if  __STDC__   
(NclData self, void *v_one)
#else
(self, v_one)
NclData self;
void *v_one;
#endif
{
	DATATYPE *val = NULL;
	NclMultiDValDATATYPEData tmp_md = (NclMultiDValDATATYPEData)self;

	val = (DATATYPE*)v_one;

	if(tmp_md->multidval.missing_value.has_missing) {
		if(tmp_md->multidval.missing_value.value.DATATYPEval == *val) {
			return(1);
		} 
	}
	return(0);
}



#include <stdio.h>
#include <stdlib.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/hlu/Callbacks.h>
#include "defs.h"
#include <netcdf.h>
#include "NclDataDefs.h"
#include "NclFileInterfaces.h"
#include "NclMdInc.h"
#include "DataSupport.h"
#include "NclCCM.h"
#include <math.h>
#include "Symbol.h"
#include "Machine.h"
#include "parser.h"
#include "TypeSupport.h"
#include "NclBuiltInSupport.h"


/* 
 * Function to coerce dimension sizes to int or long
 * Located in ../lib/nfp/wrapper.[ch].
 */
extern ng_size_t *get_dimensions(void *tmp_dimensions,ng_size_t n_dimensions,
				 NclBasicDataTypes type_dimensions,
				 const char *);

static unsigned char cray_missing_value[8] = { 0x40,0x78,0xc0,0x97,0xce,0x7b,0xc9,0x07 };


extern int COSGetRecord(
#if NhlNeedProto
FILE* /*fd*/, 
int /*block_number*/,
int /*offset*/,
char **/*buffer*/,
int* /*finish_block*/,
int* /*finish_offset*/
#endif
);

extern char EndOfRec(
);

extern int forward_index(
#if NhlNeedProto
unsigned char *
#endif
);

extern double DoubleIt(
#if NhlNeedProto
char* /*buffer*/
#endif
);
extern int IntIt(
#if NhlNeedProto
char* /*buffer*/
#endif
);
extern float FloatIt(
#if NhlNeedProto
char* /*buffer*/
#endif
);

extern int* IntEm(
#if NhlNeedProto
char* /*buffer*/,
int /*nwords*/
#endif
);
extern float* FloatEm(
#if NhlNeedProto
char* /*buffer*/,
int /*nwords*/
#endif
);
extern double *DoubleEm(
#if NhlNeedProto
char* /*buffer*/,
int /*nwords*/
#endif
);

#ifdef ByteSwapped
#define BYTE0 3
#define BYTE1 2
#define BYTE2 1
#define BYTE3 0
#else
#define BYTE0 0
#define BYTE1 1
#define BYTE2 2
#define BYTE3 3
#endif


static int extract(int which,char buf[8])
{
        char tmp[8];
 
        switch(which){
        case 0:
                return((int)buf[0]>>4);
        case 1:
                tmp[BYTE0] = 0;
                tmp[BYTE1] = (buf[0]<<4)>>4;
                tmp[BYTE2] = buf[1];
                tmp[BYTE3] = buf[2];
                return(*(int*)tmp);
        case 2:
                return((int)buf[3]>>7);
        case 3:
                tmp[BYTE0] = 0;
                tmp[BYTE1] = buf[4]>>1;
                tmp[BYTE2] = buf[5]>>1 | (buf[4]&(char)0001)<<7;
                tmp[BYTE3] = buf[6]>>1 | (buf[5]&(char)0001)<<7;
                return(*(int*)tmp);
        case 4:
                tmp[BYTE0] = 0;
                tmp[BYTE1] = 0;
                tmp[BYTE2] = buf[6] & (char)0001;
                tmp[BYTE3] = buf[7];
                return(*(int*)tmp);
        }
	return 0;
}

static int IsCOSBlocked
#if NhlNeedProto
(FILE* fd)
#else
(fd)
FILE* fd;
#endif
{
        char thebuff[8];
        char bytes[8][511];
        int type;
        int junk;
        int bnhi;
        int bnlo;
        int fwi;
 
 
 
        fread((void*)thebuff,1,sizeof(BCW),fd);
        type = extract(0,thebuff);
        junk = extract(1,thebuff);
        bnhi = extract(2,thebuff);
        bnlo = extract(3,thebuff);
        fwi = extract(4,thebuff);
 
 
        if ( type != 0 || bnhi != 0 || bnlo != 0 ) return 0;
 
         /* Skip over the rest of the first block. */
 
        if ( fread(bytes,1, WORD_SIZE*511,fd) != 511*WORD_SIZE ) return 0;
 
        /* Read and interpret second control word. */
 
        if ( fread((void*)thebuff,1, sizeof( BCW ),fd) != sizeof(BCW) ) return 0;
 
        type = extract(0,thebuff);
        junk = extract(1,thebuff);
        bnhi = extract(2,thebuff);
        bnlo = extract(3,thebuff);
        fwi = extract(4,thebuff);
 
        if ( type != 0 || bnhi != 0 || bnlo != 1 ) return 0;
 
        fseek(fd,0,SEEK_SET);
 
        return(1);
}


#define READ_SIZE BLOCK_SIZE * 10


static int 	HandleSingle
#if NhlNeedProto
(FILE* fd,int eoff) 
#else
(FILE* fd,int eoff) 
#endif
{
	ng_size_t n =0;
	unsigned char control_word[WORD_SIZE];
	int done = 0;
	int total = 0;
	int end_offset = eoff;
	int len;
/*
	fprintf(stderr,"In Handle Single %d\n",eoff);
*/

	fseek(fd,end_offset,SEEK_SET);
	n = fread(control_word,1,WORD_SIZE,fd);
	if(n != WORD_SIZE)  {
		return(0);
	}
	while(!done) {
		while(forward_index(control_word) == 0) {
			end_offset += WORD_SIZE;
			n = fread(control_word,1,WORD_SIZE,fd);
			if(n != WORD_SIZE) {
				done = 1;	
				break;
			}
		}
		if(!done) {
			control_word[0] = control_word[0] & (char)0017;
			while((EndOfRec(control_word) != CEOR)){
				end_offset += WORD_SIZE;
				len = forward_index(control_word);
				end_offset += len* WORD_SIZE;
				fseek(fd,end_offset,SEEK_SET);
				n = fread(control_word,1,WORD_SIZE,fd);
				if(n != WORD_SIZE) {
					return(total);
				}
			}
			total = total + 1;
		}
	}
	return(total);

}
NhlErrorTypes _NclICrayBinNumRec
#if NhlNeedProto
(void)
#else
()
#endif
{
        NclQuark *fpath;
        NclScalar missing;
        int     has_missing = 0;
        FILE* fd = NULL;
        ng_size_t dimsize = 1;
        long end_offset = 0;
        int total = 0;
        int len;
        ng_size_t n;
        int index = 0;
	int done = 0;
	unsigned char buffer[READ_SIZE];


	fpath = (NclQuark*)NclGetArgValue(
                0,
                1,
                NULL,
                NULL,
                &missing,
                &has_missing,
                NULL,
                0);
        if(has_missing &&(missing.stringval == *fpath)) {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"craybinnumrec: path is a missing value, can't continue");
                return(NhlFATAL);
        }
        fd = fopen(_NGResolvePath(NrmQuarkToString(*fpath)),"r");

        if(fd == NULL) {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"craybinnumrec: could not open (%s) check path and permissions, can't continue",NrmQuarkToString(*fpath));
                return(NhlFATAL);
        }
	if(IsCOSBlocked(fd)) {

		n = fread(buffer,1,READ_SIZE,fd);
		if(n != READ_SIZE) {
			end_offset = 0;
			total = HandleSingle(fd,0);
			return(NclReturnValue(
				&total,
				1,
				&dimsize,
				NULL,
				NCL_int,
				1
				));
		} else {
			end_offset = n;
			index = 0;

			while (!done) {
				while(forward_index(&(buffer[index])) == 0) {
					index += WORD_SIZE;
					if(index == READ_SIZE) {
		/*
		* ERROR
		*/
					        n = fread(buffer,1,READ_SIZE,fd);
						if(n != READ_SIZE) {
							done = 1;	
							total = total + HandleSingle(fd,end_offset);
							break;
						} else {	
							index = 0;
							end_offset += READ_SIZE;
						}
	
					}
				}
				if(!done) {
					buffer[index] = buffer[index] & (char)0017;
					while((EndOfRec(&(buffer[index])) != CEOR)){
						len = forward_index(&(buffer[index]));
						index += len* WORD_SIZE + WORD_SIZE;
						if(index == READ_SIZE) {
							n = fread(buffer,1,READ_SIZE,fd);
							if(n != READ_SIZE) {
								done = 1;
								total = total + HandleSingle(fd,end_offset);
								break;
 
							} else {
								index = 0;
								end_offset += READ_SIZE;
							}
						}
					}
					if(!done)  {
						total = total + 1;
					}
				}
			}
			return(NclReturnValue(
				&total,
				1,
				&dimsize,
				NULL,
				NCL_int,
				1
				));
			
		}

	} else {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"craybinnumrec: (%s) is not a COS blocked file. Can not read",NrmQuarkToString(*fpath));
		fclose(fd);
		return(NhlFATAL);
	}
}

NhlErrorTypes _NclICrayBinRecRead
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclQuark *fpath;
	int	*recnum;
	ng_size_t  *dimensions;
	void  *tmp_dsz;
	ng_size_t  dimsize;
        NclBasicDataTypes type_dsz;
	NclQuark *type;
	NclScalar missing;
	NclMultiDValData tmp_md;
	NclStackEntry data;
	int 	has_missing = 0;
	NclTypeClass thetype;
	unsigned char 	control_word[WORD_SIZE];
	void *value;
	char *cbin_buf,*tmp;
	int cb;
	int cb_off;
	ng_size_t i;
	int ind;
	FILE* fd = NULL;
	ng_size_t size = 1;
	ng_size_t tmp_size = 0;
	ng_size_t n;
	int cur_off = 0;
	NhlErrorTypes ret = NhlNOERROR;
	int done = 0;
	int len;

	
	fpath = (NclQuark*)NclGetArgValue(
		0,
		4,
		NULL,
		NULL,
		&missing,
		&has_missing,
		NULL,
		0);
	if(has_missing &&(missing.stringval == *fpath)) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"craybinrecread: path is a missing value, can't continue");
		return(NhlFATAL);
	}

	recnum = (int*) NclGetArgValue(
		1,
		4,
		NULL,
		NULL,
		&missing,
		&has_missing,
		NULL,
		0);
	if(has_missing &&(missing.intval == *recnum)) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"craybinrecread: record number  is a missing value, can't continue");
		return(NhlFATAL);
	}
	
	tmp_dsz = (void *)NclGetArgValue(
		2,
		4,
		NULL,
		&dimsize,
		&missing,
		&has_missing,
		&type_dsz,
		0);

	dimensions = get_dimensions(tmp_dsz,dimsize,type_dsz,"craybinrecread");
	if(*dimensions!= -1) {
		for(i = 0; i < 	dimsize; i++) {
			if(missing.intval == *(dimensions + i)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"craybinrecread: dimension size contains a missing value, can't continue");
				return(NhlFATAL);
			}
			size *= dimensions[i];
		}
	} else {
/*		size = -1;*/
		tmp_size = -1;
	}
	type = (NclQuark*)NclGetArgValue(
		3,
		4,
		NULL,
		NULL,
		&missing,
		&has_missing,
		NULL,
		0);
	if(has_missing &&(missing.stringval == *type)) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"craybinrecread: path is a missing value, can't continue");
		return(NhlFATAL);
	}
	thetype = _NclNameToTypeClass(*type);
	fd = fopen(_NGResolvePath(NrmQuarkToString(*fpath)),"r");

	if(fd == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"craybinrecread: could not open (%s) check path and permissions, can't continue",NrmQuarkToString(*fpath));
		return(NhlFATAL);
	}

	if(!IsCOSBlocked(fd)) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"craybinrecread: could not open (%s) it is not a COS blocked file",NrmQuarkToString(*fpath));
		return(NhlFATAL);
	}
	cur_off = 0;
	i = 0;
	fseek(fd,cur_off,SEEK_SET);
	n = fread(control_word,1,WORD_SIZE,fd);
	while(i != *recnum) {
		while(forward_index(control_word) == 0) {
			cur_off += WORD_SIZE;
			n = fread(control_word,1,WORD_SIZE,fd);
			if(n != WORD_SIZE) {
				done = 1;	
				NhlPError(NhlFATAL,NhlEUNKNOWN,"craybinrecread: a read error occurred while reading (%s) possibly no such record number, can't continue",NrmQuarkToString(*fpath));
				return(NhlFATAL);
			}
		}
		if(!done) {
			control_word[0] = control_word[0] & (char)0017;
			while((EndOfRec(control_word) != CEOR)){
				len = forward_index(control_word);
				cur_off += len* WORD_SIZE + WORD_SIZE;
				fseek(fd,cur_off,SEEK_SET);
				n = fread(control_word,1,WORD_SIZE,fd);
				if(n != WORD_SIZE) {
					done = 1;
					NhlPError(NhlFATAL,NhlEUNKNOWN,"craybinrecread: a read error occurred while reading (%s) possibly no such record number, can't continue",NrmQuarkToString(*fpath));
					return(NhlFATAL);

				}
			}
			i = i + 1;
		}
	}
	if(i == *recnum) {
		n = COSGetRecord(fd,cur_off/BLOCK_SIZE,(cur_off%BLOCK_SIZE)/WORD_SIZE,&cbin_buf,&cb,&cb_off);
		if(n == -1) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"craybinrecread: a read error occurred while reading (%s) , can't continue",NrmQuarkToString(*fpath));
			fclose(fd);
			return(NhlFATAL);
		}
		ind = n;
		if(size != -1) {
			if(ind < size) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"craybinrecread: size specified is greater than record size, filling with missing values");
				ret = NhlWARNING;
			} else if(ind > size) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"craybinrecread: size specified is less than record size, some data will not be read");
				ret = NhlWARNING;
			}

			if(ind >= size) {
				switch(thetype->type_class.data_type) {
				case NCL_int:
					value = IntEm(cbin_buf,size);
					break;
				case NCL_float:
					value = FloatEm(cbin_buf,size);
					break;
				case NCL_double:
					value = DoubleEm(cbin_buf,size);
					break;
				case NCL_char:
					value = cbin_buf;
				break;
				default:
					value = cbin_buf;
					NhlPError(NhlWARNING,NhlEUNKNOWN,"craybinrecread: The type %s is not directly supported by NCL, the result will be the exact binary data from the file",NrmQuarkToString(*type)); 
					break;
				}
			} else if (ind < size) {
				switch(thetype->type_class.data_type) {
				case NCL_int:
					value = IntEm(cbin_buf,ind);
					break;
				case NCL_float:
					value = FloatEm(cbin_buf,ind);
					break;
				case NCL_double:
					value = DoubleEm(cbin_buf,ind);
					break;
				case NCL_char:
					value = cbin_buf;
				break;
				default:
					value = cbin_buf;
					NhlPError(NhlWARNING,NhlEUNKNOWN,"craybinrecread: The type %s is not directly supported by NCL, the result will be the exact binary data from the file",NrmQuarkToString(*type)); 
					break;
				}
				tmp =NclMalloc(thetype->type_class.size*size);
				memcpy((void*)tmp,(void*)value,thetype->type_class.size*ind);
				for( ; ind < size;ind++) {
					memcpy((void*)((char*)tmp + ind * thetype->type_class.size),&thetype->type_class.default_mis,thetype->type_class.size);
				}
				if(cbin_buf != value)
					NclFree(cbin_buf);
				NclFree(value);
				value = tmp;
			}

			tmp_md = _NclCreateMultiDVal(
				NULL,
				NULL,
				Ncl_MultiDValData,
				0,
				value,
				&(thetype->type_class.default_mis),
				dimsize,
				dimensions,
				TEMPORARY,
				NULL,
				thetype);
		} else {
			switch(thetype->type_class.data_type) {
			case NCL_int:
				value = IntEm(cbin_buf,ind);
				dimsize = ind;
				break;
			case NCL_float:
				value = FloatEm(cbin_buf,ind);
				dimsize = ind;
				break;
			case NCL_double:
				value = DoubleEm(cbin_buf,ind);
				dimsize = ind;
				break;
			case NCL_char:
				value = cbin_buf;
				dimsize = ind*WORD_SIZE;
				break;
			default:
				value = cbin_buf;
				NhlPError(NhlWARNING,NhlEUNKNOWN,"craybinrecread: The type %s is not directly supported by NCL, the result will be the exact binary data from the file",NrmQuarkToString(*type)); 
				break;
			}

			tmp_md = _NclCreateMultiDVal(
				NULL,
				NULL,
				Ncl_MultiDValData,
				0,
				value,
				&(thetype->type_class.default_mis),
				1,
				&dimsize,
				TEMPORARY,
				NULL,
				thetype);
		}
		if(tmp_md == NULL) {
			if(cbin_buf != value) 
				NclFree(cbin_buf);
			NclFree(value);
			fclose(fd);
			return(NhlFATAL);
		}
		data.kind = NclStk_VAL;
		data.u.data_obj = tmp_md;
		_NclPlaceReturn(data);
		if(cbin_buf != value) 
			NclFree(cbin_buf);
		fclose(fd);
		return(ret);
	} else {
		return(NhlFATAL);
	}
	

}

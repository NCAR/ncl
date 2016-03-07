#include <stdlib.h>
#ifdef NIO_LIB_ONLY
#include "niohlu.h"
#include "nioNresDB.h"
#include "nioCallbacks.h"
#else
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/hlu/Callbacks.h>
#endif
#include "defs.h"
#include <netcdf.h>
#include "NclDataDefs.h"
#include "NclFileInterfaces.h"
#include "NclMdInc.h"
#include "DataSupport.h"
#include "date.h"
#include "NclCCM.h"
#include <math.h>
#include "ccmhdr.h"
#include <unistd.h>


#ifndef ByteSwapped
#define BYTE0 0
#define BYTE1 1
#define BYTE2 2
#define BYTE3 3
#else
#define BYTE3 0
#define BYTE2 1
#define BYTE1 2
#define BYTE0 3
#endif
static unsigned char cray_missing_value[8] = { 0x40,0x78,0xc0,0x97,0xce,0x7b,0xc9,0x07 };

#if 0
static int printbinary(FILE *fp,int val,int val2) {

        static int count = 0;
        int fl= 0;

        if(fp == NULL) {
                fp = stdout;
                fl = 1;
        }
#ifndef ByteSwapped
        (val & 020000000000) ? fprintf(fp,"1"),count++ : fprintf(fp,"0");
        (val & 010000000000) ? fprintf(fp,"1"),count++ : fprintf(fp,"0");
        (val & 004000000000) ? fprintf(fp,"1"),count++ : fprintf(fp,"0");
        (val & 002000000000) ? fprintf(fp,"1"),count++ : fprintf(fp,"0");
        (val & 001000000000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000400000000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000200000000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000100000000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000040000000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000020000000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000010000000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000004000000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000002000000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000001000000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000000400000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000000200000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000000100000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000000040000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000000020000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000000010000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000000004000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000000002000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000000001000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000000000400) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000000000200) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000000000100) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000000000040) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000000000020) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000000000010) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000000000004) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000000000002) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000000000001) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");

        (val2 & 020000000000) ? fprintf(fp,"1"),count++ : fprintf(fp,"0");
        (val2 & 010000000000) ? fprintf(fp,"1"),count++ : fprintf(fp,"0");
        (val2 & 004000000000) ? fprintf(fp,"1"),count++ : fprintf(fp,"0");
        (val2 & 002000000000) ? fprintf(fp,"1"),count++ : fprintf(fp,"0");
        (val2 & 001000000000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000400000000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000200000000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000100000000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000040000000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000020000000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000010000000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000004000000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000002000000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000001000000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000000400000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000000200000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000000100000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000000040000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000000020000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000000010000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000000004000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000000002000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000000001000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000000000400) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000000000200) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000000000100) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000000000040) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000000000020) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000000000010) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000000000004) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000000000002) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000000000001) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
#else

        (val & 000000000200) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000000000100) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000000000040) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000000000020) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000000000010) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000000000004) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000000000002) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000000000001) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");

        (val & 000000100000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000000040000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000000020000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000000010000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000000004000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000000002000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000000001000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000000000400) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");

        (val & 000040000000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000020000000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000010000000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000004000000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000002000000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000001000000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000000400000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000000200000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");

        (val & 020000000000) ? fprintf(fp,"1"),count++ : fprintf(fp,"0");
        (val & 010000000000) ? fprintf(fp,"1"),count++ : fprintf(fp,"0");
        (val & 004000000000) ? fprintf(fp,"1"),count++ : fprintf(fp,"0");
        (val & 002000000000) ? fprintf(fp,"1"),count++ : fprintf(fp,"0");
        (val & 001000000000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000400000000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000200000000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val & 000100000000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");

        (val2 & 000000000200) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000000000100) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000000000040) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000000000020) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000000000010) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000000000004) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000000000002) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000000000001) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");

        (val2 & 000000100000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000000040000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000000020000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000000010000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000000004000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000000002000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000000001000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000000000400) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");

        (val2 & 000040000000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000020000000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000010000000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000004000000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000002000000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000001000000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000000400000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000000200000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");

        (val2 & 020000000000) ? fprintf(fp,"1"),count++ : fprintf(fp,"0");
        (val2 & 010000000000) ? fprintf(fp,"1"),count++ : fprintf(fp,"0");
        (val2 & 004000000000) ? fprintf(fp,"1"),count++ : fprintf(fp,"0");
        (val2 & 002000000000) ? fprintf(fp,"1"),count++ : fprintf(fp,"0");
        (val2 & 001000000000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000400000000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000200000000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
        (val2 & 000100000000) ? fprintf(fp,"1") ,count++: fprintf(fp,"0");
#endif

        if(fl)
                fprintf(fp,"\n");
        return(count);
}
#endif

long sz(int n)
{
        return(WORD_SIZE * n);
}
NrmQuark CcmVarName2
#if	NhlNeedProto
(char *buffer)
#else
(buffer)
char *buffer;
#endif
{
	char tmpc[WORD_SIZE+1];
	char *tc;
	int i;
	for(i = 0; i < WORD_SIZE;i++) {
		tmpc[i] = buffer[i];
	}


	i = WORD_SIZE;
	tmpc[i] = '\0';
	i--;
	while((i>=0)&&(buffer[i] == ' ')) {	
		tmpc[i] = '\0';
		i--;
	}
	tc = tmpc;
	i = 0;
	while((i < WORD_SIZE)&&(*tc == ' ')) {
		tc++;
		i++;
	}
	return(NrmStringToQuark(tc));
}
NrmQuark CcmVarName
#if	NhlNeedProto
(char *buffer)
#else
(buffer)
char *buffer;
#endif
{
	char tmpc[WORD_SIZE+1];
	int i,k;

	i = 0;	
	k = 0;
	while((i<WORD_SIZE)&&(buffer[i] == ' ')) {	
		i++;
	}
	while((i<WORD_SIZE)&&(buffer[i] != ' ')) {
		tmpc[k] = buffer[i];
		i++;
		k++;
	}
	tmpc[k] = '\0';
	return(NrmStringToQuark(tmpc));
}

static void *CcmCreateFile
#if	NhlNeedProto
(void *rec,NclQuark path)
#else
(rec,path)
void *rec;
NclQuark path;
#endif
{
	NhlPError(NhlFATAL,NhlEUNKNOWN,"NclCCM: NCL does not support creating files in the CCM history format, create netCDF instead");
	return(NULL);
}

unsigned short IntIt2(char* buffer) {
	char b2[WORD_SIZE];
	int zero = 0;
	int total = 1;
	int tmp_out;
	b2[0] = (char)0;
	b2[1] = (char)0;
	b2[2] = (char)0;
	b2[3] = (char)0;
        b2[4] = (char)0;
        b2[5] = (char)0;
        b2[6] = buffer[0];
        b2[7] = buffer[1];
	ctospi(b2,&tmp_out,&total,&zero);
	return((unsigned short)tmp_out);
}
unsigned int IntIt4(char* buffer) {
	char b2[WORD_SIZE];
	int zero = 0;
	int total = 1;
	int tmp_out;
	b2[0] = (char)0;
	b2[1] = (char)0;
	b2[2] = (char)0;
	b2[3] = (char)0;
        b2[4] = buffer[0];
        b2[5] = buffer[1];
        b2[6] = buffer[2];
        b2[7] = buffer[3];
	ctospi(b2,&tmp_out,&total,&zero);
	return(tmp_out);
}

static int extractCCM(int which,char buf[8])
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
	NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
	return -9999;
}

static int IsF77Blocked
#if NhlNeedProto
(FILE* fd)
#else
(fd)
FILE* fd;
#endif
{
	int n;
	char control_word[4];
	int ind;
	
	n = fread((control_word),1,4,fd);
	if(n!=4) return(0);

	ind = *(int*)control_word;
	fseek(fd,ind+ 4,SEEK_SET);
	n = fread((control_word),1,4,fd);
	if(n!=4) return(0);
	if(ind != *(int*)control_word) {
		return(0);
	} else {
		return(2);
	}
	
}

static int IsCOSBlockedCCM
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
	type = extractCCM(0,thebuff);
	junk = extractCCM(1,thebuff);
	bnhi = extractCCM(2,thebuff);
	bnlo = extractCCM(3,thebuff);
	fwi = extractCCM(4,thebuff);


	if ( type != 0 || bnhi != 0 || bnlo != 0 ) {
		fseek(fd,0,SEEK_SET);
		return (IsF77Blocked(fd));
	}

	 /* Skip over the rest of the first block. */

  	if ( fread(bytes,1,WORD_SIZE*511,fd) != 511*WORD_SIZE ) {
		fseek(fd,0,SEEK_SET);
		return (IsF77Blocked(fd));
	}

  	/* Read and interpret second control word. */

  	if ( fread((void*)thebuff,1, sizeof( BCW ),fd) != sizeof(BCW) ) {
		fseek(fd,0,SEEK_SET);
		return (IsF77Blocked(fd));
	}

	type = extractCCM(0,thebuff);
	junk = extractCCM(1,thebuff);
	bnhi = extractCCM(2,thebuff);
	bnlo = extractCCM(3,thebuff);
	fwi = extractCCM(4,thebuff);

  	if ( type != 0 || bnhi != 0 || bnlo != 1 ) {
		fseek(fd,0,SEEK_SET);
		return (IsF77Blocked(fd));
	}

	fseek(fd,0,SEEK_SET);

	return(1);
}
double *DoubleEm(unsigned char* buffer,int nwords) {
	int zero = 0;
	int total = nwords;
	double *tmp_out = NclMalloc(sizeof(double)*nwords);
	ctodpf(buffer,tmp_out,&total,&zero);
	return(tmp_out);
}
double DoubleIt(unsigned char* buffer) {
	int zero = 0;
	int total = 1;
	double tmp_out;
	ctodpf(buffer,&tmp_out,&total,&zero);
	return(tmp_out);
}
int *IntEm(unsigned char* buffer,int nwords) {
	int zero = 0;
	int total = nwords;
	int *tmp_out = NclMalloc(sizeof(int)*nwords);
	ctospi(buffer,tmp_out,&total,&zero);
	return(tmp_out);
}
int IntIt(unsigned char* buffer) {
	int zero = 0;
	int total = 1;
	int tmp_out;
	ctospi(buffer,&tmp_out,&total,&zero);
	return(tmp_out);
}
float *FloatEm(unsigned char* buffer,int nwords) {
	int zero = 0;
	int total = nwords;
	float *tmp_out = NclMalloc(sizeof(float)*nwords);
	ctospf(buffer,tmp_out,&total,&zero);
	return(tmp_out);
}
float FloatIt(unsigned char* buffer) {
	int zero = 0;
	int total = 1;
	float tmp_out;
	ctospf(buffer,&tmp_out,&total,&zero);
	return(tmp_out);
}
int CompareHeaders(CCMI *initial_iheader,CCMC *initial_cheader,CCMR *initial_rheader,CCMI* tmp_iheader,CCMC* tmp_cheader,CCMR* tmp_rheader) {
	if(initial_iheader->NOREC != tmp_iheader->NOREC) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"NclCCM: Number of latitude records varies between timesteps (0) and (%d). Can not read this CCM file.",tmp_iheader->MFILH);
		return(0);
	}
	if(initial_iheader->NLON!= tmp_iheader->NLON) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"NclCCM: Number of longitude points varies between timesteps (0) and (%d). Can not read this CCM file.",tmp_iheader->MFILH);
		return(0);
	}
	if(initial_iheader->NLEV > tmp_iheader->NLEV) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"NclCCM: Number of levels varies between timesteps (0) and (%d). Can not read this CCM file.",tmp_iheader->MFILH);
		return(0);
	} else if(initial_iheader->NLEV < tmp_iheader->NLEV) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"NclCCM: Number of levels varies between timesteps (0) and (%d). Trying to continue anyways. Coordinate levels may be incorrect!",tmp_iheader->MFILH);
	}
	if(initial_iheader->MAXSIZ != tmp_iheader->MAXSIZ) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"NclCCM: Record size varies between timesteps (0) and (%d). Trying to continue anyways. Coordinate levels may be incorrect!",tmp_iheader->MFILH);
	}
	if(initial_iheader->NDAVU/initial_iheader->MAXSIZ != tmp_iheader->NDAVU/tmp_iheader->MAXSIZ) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"NclCCM: Packing varies between timesteps (0) and (%d). Can not read this CCM file.",tmp_iheader->MFILH);
		return(0);
	}
	return(1);
}

long COSRecSeek(FILE* fd,int n_words, long current_offset) 
{
	long totsz = sz(n_words);
	int cb = current_offset / BLOCK_SIZE;
	int cof = current_offset % BLOCK_SIZE;
	int nb = 0;

	if((BLOCK_SIZE - cof) > totsz) {
		fseek(fd,cb * BLOCK_SIZE + cof + totsz,SEEK_SET);
		return(cb * BLOCK_SIZE + cof + totsz);
	} else {
		if(cof != 0) {
			totsz = totsz - (BLOCK_SIZE - cof);
		} 
		nb = (totsz)/(BLOCK_SIZE-sz(1));
		totsz -= nb * (BLOCK_SIZE-sz(1));
		if(totsz >= BLOCK_SIZE) {
			fprintf(stdout,"Error1\n");
		}
		fseek(fd,(cb+nb)*BLOCK_SIZE + totsz + BLOCK_SIZE + sz(1),SEEK_SET);
		return((cb+nb)*BLOCK_SIZE + totsz + BLOCK_SIZE + sz(1));
	}
}
long MySeek
#if NhlNeedProto
(CCMFileRec *therec,FILE* fd,int nwords,long start_off)
#else
(therec,fd,nwords,start_off)
CCMFileRec *therec;
FILE* fd;
int nwords;
long start_off;
#endif
{
	if(nwords == 0) {
		fseek(fd,start_off,SEEK_SET);
		return(start_off);
	} else if(therec->cos_blocking==1) {
		return(COSRecSeek(fd,nwords, start_off));
	} else if(therec->cos_blocking==0){
		fseek(fd,start_off+sz(nwords-1),SEEK_SET);
		return(start_off + sz(nwords-1));
	} else {
		fseek(fd,start_off+sz(nwords-1),SEEK_SET);
		return(start_off + sz(nwords-1));
	}
}



char EndOfRec(unsigned char cw[]) {
	unsigned char tmpc;
	
	tmpc = cw[0] & 0360;
	tmpc = tmpc >> 4;
	if(tmpc == (char)010) {
		return(CEOR);
	} else if(tmpc == (char)017) {
		return(CEOD);
	} else if(tmpc == (char)016) {
		return(CEOF);
	} else if(tmpc == (char)0) {
		return(CBCW);
	} else {
	/*	fprintf(stdout, "ERROR:\n");*/
		return((char)0377);
	}
}
int forward_index(unsigned char cw[]) {
	unsigned char buffer[WORD_SIZE];
	
	buffer[0] =0;
	buffer[1] = 0;
	buffer[2] = 0;
	buffer[3] = 0;
	buffer[4] = 0;
	buffer[5] = 0;
	buffer[6] = (unsigned char)01 & cw[6];
	buffer[7] = (unsigned char)0377 & cw[7];
	return(IntIt(buffer));
}

long COSGetNWords(FILE* fd,int num_words,long current_offset,unsigned char *buffer)
{
	long totsz = sz(num_words);
	int cb = current_offset / BLOCK_SIZE;
	int cof = current_offset % BLOCK_SIZE;
	int nb;
	int i;
	int n = 0;
	int index = 0;
	char control[WORD_SIZE];
	
	fseek(fd,current_offset,SEEK_SET);
	if(totsz < (BLOCK_SIZE - cof)) {
		n = fread(buffer,1,totsz,fd);
		index +=n;
		return(cb * BLOCK_SIZE + cof + totsz);
	} else {
	
		totsz = totsz - (BLOCK_SIZE - cof);
		n = fread (buffer,1,(BLOCK_SIZE - cof),fd);
		index += n;
		nb = (totsz)/(BLOCK_SIZE-sz(1));
		for(i = 0; i < nb; i++) {
			n = fread(control,1,WORD_SIZE,fd);
			n = fread (&(buffer[index]),1,(BLOCK_SIZE - sz(1)),fd);
			index += n;
		} 
		totsz -= nb * (BLOCK_SIZE - sz(1));
		if(totsz >= BLOCK_SIZE) {
			fprintf(stdout,"Error1\n");
		}
		n = fread(control,1,WORD_SIZE,fd);
		n = fread (&(buffer[index]),1,totsz,fd);
		index += n;
		return(COSRecSeek(fd,num_words,current_offset));
	}
}

long MyRead
#if NhlNeedProto
(CCMFileRec *therec,FILE* fd,unsigned char *buffer,int nwords, long start_off)
#else
(therec,fd,buffer,nwords, start_off)
CCMFileRec *therec;
FILE* fd;
unsigned char *buffer;
int nwords;
long start_off;
#endif
{
	int n;
        if(nwords == 0) {
        	fseek(fd,start_off,SEEK_SET);
                return(start_off);
        } else if(therec->cos_blocking==1){
		return(COSGetNWords(fd,nwords,start_off,buffer));
        } else if(therec->cos_blocking==0) {
		fseek(fd,start_off,SEEK_SET);
                n = fread(buffer,1,nwords*WORD_SIZE,fd);
		return(start_off + n);
        } else {
		fseek(fd,start_off,SEEK_SET);
                n = fread(buffer,1,nwords*WORD_SIZE,fd);
		return(start_off + n);
	}
}
int COSGetRecord(FILE* fd, int block_number,int offset,unsigned char **buffer,int* finish_block,int* finish_offset)
{
	long real_offset = (block_number * BLOCK_SIZE) + sz(offset);
	long end_offset;
	unsigned char control_word[BLOCK_SIZE];
	int total = 0;
	int len;
	int n;
	int index = 0;

  	fseek(fd,real_offset,SEEK_SET);
	end_offset = real_offset;
	n = fread(control_word,1,WORD_SIZE,fd);
	if(n != WORD_SIZE) 
		return(-1);
	while(forward_index(control_word)==0) {
		end_offset += sz(1);
		n = fread(control_word,1,WORD_SIZE,fd);
		if(n != WORD_SIZE) {
			return(-1);
		}
	}
        control_word[0] = control_word[0] & (char)0017;
	while((EndOfRec(control_word) != CEOR)){
		end_offset += sz(1);
		len = forward_index(control_word);	
		total += len;
		end_offset += sz(len);
		fseek(fd,end_offset,SEEK_SET);
		n = fread(control_word,1,WORD_SIZE,fd);
		if( n != WORD_SIZE) 
			return(-1);
	};
	*buffer = (unsigned char*)NclMalloc(sz(total));
	fseek(fd,real_offset,SEEK_SET);
	n = fread(control_word,1,WORD_SIZE,fd);
	if( n != WORD_SIZE) 
		return(-1);
        control_word[0] = control_word[0] & (char)0017;
	while(EndOfRec(control_word)!=CEOR){
                len = forward_index(control_word);
		n = fread(&((*buffer)[index]),1,sz(len),fd);
		if( n != sz(len)) 
			return(-1);
		index += sz(len);
		n =fread(control_word,1,WORD_SIZE,fd);
		if( n != WORD_SIZE) 
			return(-1);
        }
	fseek(fd,end_offset,SEEK_SET);
	*finish_block = end_offset / BLOCK_SIZE;
	*finish_offset = (end_offset % BLOCK_SIZE)/WORD_SIZE;
	return(total);
}
int MyGetRecord
#if NhlNeedProto
(CCMFileRec *therec,FILE* fd, int start_block ,int start_offset,unsigned char **buffer,int *finish_block,int *finish_offset,int* rec_size)
#else
(therec,fd,start_block ,start_offset,buffer,finish_block,finish_offset,rec_size)
CCMFileRec *therec;
FILE* fd;
int start_block;
int start_offset;
unsigned char **buffer;
int *finish_block;
int *finish_offset;
int *rec_size;
#endif
{
	long end_offset;
	char control_word[4];
	int n;
	if(therec->cos_blocking==1) {
		return(COSGetRecord(fd,start_block ,start_offset,buffer,finish_block,finish_offset));
	} else if(therec->cos_blocking ==0) {
		if(rec_size != NULL) {
			*buffer = NclMalloc(*rec_size * WORD_SIZE);
			end_offset = start_block * BLOCK_SIZE + start_offset * WORD_SIZE + *rec_size * WORD_SIZE;
			*finish_block = end_offset / BLOCK_SIZE;
			*finish_offset = (end_offset % BLOCK_SIZE) / WORD_SIZE;
			fseek(fd,start_block * BLOCK_SIZE + start_offset * WORD_SIZE, SEEK_SET);
			return(fread(*buffer,1,*rec_size * WORD_SIZE,fd));
		} else {
			return(-1);
		}
	} else {
		fseek(fd,start_block * BLOCK_SIZE + start_offset * WORD_SIZE, SEEK_SET);
		n = fread(control_word,1,4,fd);
		if(n != 4) return(-1);

		*buffer = NclMalloc(*(int*)control_word);
		end_offset = start_block * BLOCK_SIZE + start_offset * WORD_SIZE + *(int*)control_word + 8;
		*finish_block = end_offset / BLOCK_SIZE;
		*finish_offset = (end_offset % BLOCK_SIZE) / WORD_SIZE;

		n = fread(*buffer,1,*(int*)control_word,fd);
		fread(control_word,1,4,fd);
		return(n);
		
	}
	
}



long UnPackRealHeader(CCMFileRec *therec,FILE* fd,CCMI *iheader, CCMR *header,int start_block, int start_offset)
{
	int index;
	int n;
	unsigned char *buffer;
	int finish_block;
	int finish_offset;
	
	

	n = MyGetRecord(therec,fd, start_block ,start_offset,&buffer,&finish_block,&finish_offset,&(iheader->LENHDR));
	if(n == -1) {
		return(n);
	}

	if(iheader->LENHDR ==  (3*(2*iheader->NLEV + 1) + 2 * iheader->NOREC)) {

		index = 0;
		header->sigapb = DoubleEm(&(buffer[index]),iheader->NLEV*2 + 1);
		index += sz(iheader->NLEV*2 + 1);
		header->siga = DoubleEm(&(buffer[index]),iheader->NLEV*2 + 1);
		index += sz(iheader->NLEV*2 + 1);
		header->sigb = DoubleEm(&(buffer[index]),iheader->NLEV*2 + 1);
		index += sz(iheader->NLEV*2 + 1);

		header->mplat = DoubleEm(&(buffer[index]),iheader->NOREC);
		index += sz(iheader->NOREC);
	
		header->mpwts = DoubleEm(&(buffer[index]),iheader->NOREC);
		index += sz(iheader->NOREC);
	
		free(buffer);	
		return((finish_block * BLOCK_SIZE) + sz(finish_offset));
	} else if(iheader->LENHDR == iheader->NLEV + 2 * iheader->NOREC){
		index = 0;
		header->sigapb = DoubleEm(&(buffer[index]),iheader->NLEV);
		index += sz(iheader->NLEV);
		header->siga = NULL;
		header->sigb = NULL;
		
		header->mplat = DoubleEm(&(buffer[index]),iheader->NOREC);
		index += sz(iheader->NOREC);
	
		header->mpwts = DoubleEm(&(buffer[index]),iheader->NOREC);
		index += sz(iheader->NOREC);

		free(buffer);	
		return((finish_block * BLOCK_SIZE) + sz(finish_offset));
	} else {
		return(-1);
	}

}
long UnPackCharHeader(CCMFileRec *therec,FILE* fd,CCMI *iheader, CCMC *header,int start_block, int start_offset)
{
	int index;
	int n;
	unsigned char *buffer;
	int finish_block;
	int finish_offset;

	n = MyGetRecord(therec,fd, start_block ,start_offset,&buffer,&finish_block,&finish_offset,&(iheader->LENHDC));
	if(n == -1) {
		return(n);
	}

	if(iheader->LENHDC < CHAR_HEADER_SIZE) {
		index = 0;
		memcpy(header,buffer,sz(iheader->LENHDC-2*iheader->NFLDH));
		index += sz(iheader->LENHDC-2*iheader->NFLDH);
	} else {
		index = 0;
		memcpy(header,buffer,sz(CHAR_HEADER_SIZE));
		index += sz(CHAR_HEADER_SIZE);
	}

	
	header->MCFLDS = (char*)NclMalloc(sz(2 * iheader->NFLDH));

	memcpy(header->MCFLDS,&(buffer[index]),sz(2 * iheader->NFLDH));
	index += 2 * iheader->NFLDH;

	free(buffer);	
	return((finish_block * BLOCK_SIZE) + sz(finish_offset));

}
long UnPackIntHeader(CCMFileRec *therec,FILE* fd, CCMI *header,int start_block, int start_offset)
{
	int zero = 0;
	int total;
	int index;
	int n;
	unsigned char *buffer;
	int finish_block;
	int finish_offset;
	int len;
	unsigned char cw[WORD_SIZE];
	static int first = 1;

	if(therec->cos_blocking==0 ) {
		n = MyRead(therec,fd,cw,1, start_block * BLOCK_SIZE + sz(start_offset));
		total = 1;
		ctospi(cw,&len,&total,&zero);
		n = MyGetRecord(therec,fd, start_block ,start_offset,&buffer,&finish_block,&finish_offset,&len);
	} else {
		n = MyGetRecord(therec,fd, start_block ,start_offset,&buffer,&finish_block,&finish_offset,NULL);

	}
	
	if(n == -1) {
		return(n);
	} else if(first) {
		first = 0;
/*
* This is a kludgy test to avoid a core dump
*/
		if(((( finish_block * BLOCK_SIZE + (finish_offset-1) *WORD_SIZE)/WORD_SIZE) - ((start_block* BLOCK_SIZE +  start_offset * WORD_SIZE)/WORD_SIZE)) != n ) {
			NhlPError( NhlFATAL,NhlEUNKNOWN,"CCM file is not a CRAY binary. Non CRAY binary files are not currently supported by NCL.\nUse \"ccm2nc\" to convert file to netCDF. (Currently, as of 1999, found at: http://goldhill.cgd.ucar.edu/cms/ccm3/tools/)");
			return(-1);
		}
	}

	
		total = INT_HEADER_SIZE;	
		index = 0;
		ctospi(buffer,header,&total,&zero);
		index += sz(INT_HEADER_SIZE);


		total = 3 * header->NFLDH;	
		header->MFLDS = IntEm(&(buffer[index]),3 * header->NFLDH);
		index += sz(3 * header->NFLDH);
	

	free(buffer);	
	return(finish_block * BLOCK_SIZE + sz(finish_offset));
}

static int qsort_compare_func
#if     NhlNeedProto
(Const void* s1,Const void* s2)
#else
(s1,s2)
void* s1;
void* s2;
#endif
{
        NclQList *ind1 = (NclQList*)s1;
        NclQList *ind2 = (NclQList*)s2;

	return((int)(ind1->var_quark - ind2->var_quark));
}

CcmIntVarInqRecList *CcmAddIntVar
#if	NhlNeedProto
(CCMFileRec *therec,NclQuark vname,ng_size_t size,NclTypeClass type,void *value,int status,int dim_number)
#else
(therec,vname,size,type,value,status,dim_number)
CCMFileRec  *therec;
NclQuark vname;
ng_size_t size;
NclTypeClass type;
void *value;
int status;
int dim_number;
#endif
{
	CcmIntVarInqRecList *tmp;

	tmp = therec->int_vars;
	
	therec->int_vars = (CcmIntVarInqRecList*)NclMalloc(sizeof(CcmIntVarInqRecList));
	therec->int_vars->next = tmp;
	therec->int_vars->var_name_q = vname;
	therec->int_vars->var_info.var_name_quark = vname;
	therec->int_vars->var_info.num_dimensions = 1;
	therec->int_vars->var_info.dim_sizes[0] = size;
	therec->int_vars->var_info.file_dim_num[0] = dim_number;
	therec->int_vars->var_info.data_type = type->type_class.data_type;
	therec->int_vars->n_atts = 0;
	therec->int_vars->theatts = NULL;
	
	therec->int_vars->thevalue = _NclCreateMultiDVal(
                                                        NULL,
                                                        NULL,
                                                        Ncl_MultiDValData,
                                                        0,
                                                        value,
                                                        NULL,
                                                        1,
                                                        therec->int_vars->var_info.dim_sizes,
                                                        status,
                                                        NULL,
                                                        type);
	therec->n_int_vars++;
	return(therec->int_vars);;
}

void CcmAddStringVarAtt
#if     NhlNeedProto
(CcmIntVarInqRecList *tmp_var,NclQuark att_name, NclQuark att_val)
#else
(tmp_var,att_name,att_val)
CcmIntVarInqRecList *tmp_var;
NclQuark att_name;
NclQuark att_val;
#endif
{
	CcmAttInqRecList *tmp_att = tmp_var->theatts;
	NclQuark *tmp_q;
	ng_size_t dimsizes = 1;

	tmp_q = (NclQuark*)NclMalloc(sizeof(NclQuark));
	*tmp_q = att_val;
	
	tmp_var->theatts =(CcmAttInqRecList*) NclMalloc(sizeof(CcmAttInqRecList));
	tmp_var->theatts->attname = att_name;
	tmp_var->theatts->next = tmp_att;
	tmp_var->theatts->thevalue = _NclCreateMultiDVal(
                                                        NULL,
                                                        NULL,
                                                        Ncl_MultiDValData,
                                                        0,
                                                        tmp_q,
                                                        NULL,
                                                        1,
                                                        &dimsizes,
                                                        PERMANENT,
                                                        NULL,
                                                        _NclNameToTypeClass(NrmStringToQuark("string")));

	tmp_var->n_atts++;
	return;
}

static char *time_units( int date, int sec,char* units)
{
  /* construct time units string
 
     Input args:
 
     date    yymmdd
     sec     seconds relative to date at 0Z
 
     Output arg:
 
     units   string in the form "days since 0000-00-00 00:00:00"
 
     Return values:
 
      0  Successful return.
  */
 
  int year, month, day, hours, minutes, seconds;
 
  year = date/10000;
  month = (date%10000)/100;
  day = date%100;
  hours = sec/3600;
  minutes = (sec - 3600*hours)/60;
  seconds = sec - 3600*hours - 60*minutes;
 
  sprintf( units, "days since %04i-%02i-%02i %02i:%02i:%02i", year, month,
                   day, hours, minutes, seconds );
  return(units);
 
}

static void *CcmInitializeFileRec
#if	NhlNeedProto
(NclFileFormat *format)
#else
(format)
NclFileFormat *format;
#endif
{
	CCMFileRec *therec = NULL;

	therec = (CCMFileRec*)NclCalloc(1, sizeof(CCMFileRec));
	if (! therec) {
		NhlPError(NhlFATAL,ENOMEM,NULL);
	}
	*format = _NclCCM;
	return (void *) therec;
}

static void *CcmOpenFile
#if	NhlNeedProto
(void * rec,NclQuark path,int wr_status)
#else
(therec,path,wr_status)
void *therec;
NclQuark path;
int	wr_status;
#endif
{
	CCMFileRec *therec = (CCMFileRec *)rec;
	CCMI initial_iheader;
	CCMC initial_cheader;
	CCMR initial_rheader;
	CCMI tmp_iheader;
	CCMC tmp_cheader;
	CCMR tmp_rheader;
	CcmIntVarInqRecList *tmp_var = NULL;
	int i,j;
	int dim_num;
	long coff = 0;  /* keeps track of current offset in bytes into the file */
	int cb = 0; 	/* keeps track of current block in numbers of blocks */
	int cb_off = 0; /* keeps track of word offset into block number cb  cb * BLOCK_SIZE + cb_off * WORD_SIZE == coff */
	int tmp_off;
	unsigned char buffer[BLOCK_SIZE];
	int index;
	static int first = 1;
	static NclQList *qlist = NULL;
	static int n_quarks = 0;
	FILE* fd;
	char units[31];
	float spacing;
	float *tmp_lon,*tmp_siga,*tmp_sigb;
	int done = 0;
	double *time_var;
	int time_var_size = 0;
	int n_time;
	int *date;
	int *datesec;
	int bogus_mfilth = 0;
	void *vbuf;

	if(first) {
		n_quarks = sizeof(ccm_name_tab)/sizeof(CCMNAMES);
		qlist = (NclQList*)NclMalloc(sizeof(NclQList)*n_quarks);
		i = 0;
		while(ccm_name_tab[i].std_name != NULL) {
			qlist[i].var_quark = NrmStringToQuark(ccm_name_tab[i].std_name);
			qlist[i].var_q_index = i;
			i++;
		}	
		n_quarks = i;
		qsort(qlist,n_quarks,sizeof(NclQList),qsort_compare_func);
/*
		for(i = 0; i < n_quarks; i++) {
			fprintf(stdout,"%d\n",qlist[i].var_quark);
		}
*/
		first = 0;
	}
	fd = fopen(NrmQuarkToString(path),"r");
		
	if(fd!=NULL) {
		vbuf = (void*)NclMalloc(4*getpagesize());
		setvbuf(fd,vbuf,_IOFBF,4*getpagesize());
/*
* If its COS blocked it will set up MySeek and MyRead pointers for COS freads
* If its NON blocked cray binary file the MySeek and MyRead pointer 
*/
		therec->cos_blocking = IsCOSBlockedCCM(fd);
		therec->file_path_q = path;
		therec->wr_status = wr_status;
		therec->n_headers = 0;
		therec->n_vars = 0;	
		therec->vars = NULL;
		therec->n_file_atts = 0;
		therec->file_atts= NULL;
		therec->n_int_vars = 0;
		therec->int_vars = NULL;
/*
* Obtain first integer header and set up info arrays
*/
		coff = UnPackIntHeader(therec,fd,&initial_iheader,cb,cb_off);
		if(coff == -1) {
			return(NULL);
		}
		therec->header.iheader = initial_iheader;
		switch((initial_iheader.MFTYP%100)/10) {
		case 0:
			NhlPError(NhlFATAL,NhlEUNKNOWN," A %s \"sigma coordinate tape\" has been detected.\n\t(MFTYP = %d), this is a non-standard CCM file.\n\tNCL only supports the type \"hybrid coordinate tape\"",(initial_iheader.MFTYP<100)?"model generated":"processor generated",initial_iheader.MFTYP);
			NclFree(therec);
			fclose(fd);
			NclFree(vbuf);
			return(NULL);
			break;
		case 1:
			NhlPError(NhlFATAL,NhlEUNKNOWN," A %s \"pressure coordinate tape\" has been detected.\n\t(MFTYP = %d), this is a non-standard CCM file.\n\tNCL only supports the type \"hybrid coordinate tape\"",(initial_iheader.MFTYP<100)?"model generated":"processor generated",initial_iheader.MFTYP);
			NclFree(therec);
			fclose(fd);
			NclFree(vbuf);
			return(NULL);
			break;
		case 3:
			NhlPError(NhlFATAL,NhlEUNKNOWN," A %s \"theta coordinate tape\" has been detected.\n\t(MFTYP = %d), this is a non-standard CCM file.\n\tNCL only supports the type \"hybrid coordinate tape\"",(initial_iheader.MFTYP<100)?"model generated":"processor generated",initial_iheader.MFTYP);
			NclFree(therec);
			fclose(fd);
			NclFree(vbuf);
			return(NULL);
			break;
		case 4:
			break;
		case 5:
			NhlPError(NhlFATAL,NhlEUNKNOWN," A %s \"pressure coordinate tape\" has been detected.\n\t(MFTYP = %d), this is a non-standard CCM file.\n\tNCL only supports the type \"hybrid coordinate tape\"",(initial_iheader.MFTYP<100)?"model generated":"processor generated",initial_iheader.MFTYP);
			NclFree(therec);
			fclose(fd);
			NclFree(vbuf);
			return(NULL);
			break;
		default:
			NhlPError(NhlFATAL,NhlEUNKNOWN," A %s \"unknown tape\" has been detected.\n\t(MFTYP = %d), this is a non-standard CCM file.\n\tNCL only supports the type \"hybrid coordinate tape\"",(initial_iheader.MFTYP<100)?"model generated":"processor generated",initial_iheader.MFTYP);
			NclFree(therec);
			fclose(fd);
			NclFree(vbuf);
			return(NULL);
			break;
		}
		if(coff == -1) {
			NclFree(therec);
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Error opening CCM integer header for file (%s)",NrmQuarkToString(path));
					fclose(fd);
			NclFree(vbuf);
			return(NULL);
		}

		therec->n_dims = LONGITUDE_DIM_NUMBER + 1;
		therec->dims = (CcmDimInqRecList*)NclMalloc(therec->n_dims * sizeof(CcmDimInqRecList));

		therec->dims[TIME_DIM_NUMBER].dim_name= NrmStringToQuark("time");
		therec->dims[LATITUDE_DIM_NUMBER].dim_name= NrmStringToQuark("lat");
		therec->dims[LONGITUDE_DIM_NUMBER].dim_name= NrmStringToQuark("lon");
		therec->dims[TIME_DIM_NUMBER].size = initial_iheader.MFILTH;
		therec->dims[LATITUDE_DIM_NUMBER].size = initial_iheader.NOREC;
		therec->dims[LONGITUDE_DIM_NUMBER].size = initial_iheader.NLON;
		therec->dims[SCALAR_DIM_NUMBER].dim_name = NrmStringToQuark("ncl_scalar");
		therec->dims[SCALAR_DIM_NUMBER].size = 1;
		therec->dims[CHAR_DIM_NUMBER].dim_name= NrmStringToQuark("len_char");
		therec->dims[CHAR_DIM_NUMBER].size = 80;
	
		cb = coff / BLOCK_SIZE;
		cb_off = (coff % BLOCK_SIZE) / WORD_SIZE;
		coff = UnPackCharHeader(therec,fd,&initial_iheader,&initial_cheader,cb,cb_off);
		therec->header.cheader = initial_cheader;
		if(coff == -1) {
			NclFree(therec);
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Error opening CCM character header for file (%s)",NrmQuarkToString(path));
					fclose(fd);
			NclFree(vbuf);
			return(NULL);
		}
		cb = coff / BLOCK_SIZE;
		cb_off = (coff % BLOCK_SIZE) / WORD_SIZE;
		coff = UnPackRealHeader(therec,fd,&initial_iheader,&initial_rheader,cb,cb_off);
		therec->header.rheader = initial_rheader;
		if(coff == -1) {
			NclFree(therec);
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Error opening CCM real header for file (%s)",NrmQuarkToString(path));
					fclose(fd);
			NclFree(vbuf);
			return(NULL);
		}
		if((initial_iheader.MPLAT - initial_iheader.MPSIG)==(3*(2*initial_iheader.NLEV + 1))) {
                        therec->dims[ILEV_DIM_NUMBER].dim_name= NrmStringToQuark("ilev");
                        therec->dims[MLEV_DIM_NUMBER].dim_name= NrmStringToQuark("lev");
                        therec->dims[ILEV_DIM_NUMBER].size = initial_iheader.NLEV + 1;
                        therec->dims[MLEV_DIM_NUMBER].size = initial_iheader.NLEV;
			tmp_lon = (float*)NclMalloc(sizeof(float)*(initial_iheader.NLEV+1));
			tmp_siga = (float*)NclMalloc(sizeof(float)*(initial_iheader.NLEV+1));
			tmp_sigb = (float*)NclMalloc(sizeof(float)*(initial_iheader.NLEV+1));
/*
			fprintf(stdout,"ilev:\n");
*/
			for(i = 0; i < initial_iheader.NLEV+1; i++) {
				tmp_lon[i] = 1000.0 * ( *(initial_rheader.siga + 2*i) + *(initial_rheader.sigb + 2*i));
				tmp_siga[i] = *(initial_rheader.siga + 2*i);
				tmp_sigb[i] = *(initial_rheader.sigb + 2*i);
/*
				fprintf(stdout,"%f\n",tmp_lon[i]);
*/
			}
			tmp_var = CcmAddIntVar(
					therec,
					therec->dims[ILEV_DIM_NUMBER].dim_name,
					therec->dims[ILEV_DIM_NUMBER].size,
					_NclNameToTypeClass(NrmStringToQuark("float")),
					tmp_lon,
					PERMANENT,
					ILEV_DIM_NUMBER);
			CcmAddStringVarAtt(tmp_var,NrmStringToQuark("hybrid_PS_var"),NrmStringToQuark("PS"));
			CcmAddStringVarAtt(tmp_var,NrmStringToQuark("hybrid_P0_var"),NrmStringToQuark("P0"));
			CcmAddStringVarAtt(tmp_var,NrmStringToQuark("hybrid_B_var"),NrmStringToQuark("hybi"));
			CcmAddStringVarAtt(tmp_var,NrmStringToQuark("hybrid_A_var"),NrmStringToQuark("hyai"));
			CcmAddStringVarAtt(tmp_var,NrmStringToQuark("long_name"),NrmStringToQuark("hybrid level at layer interfaces (1000*(A+B))"));
			tmp_var = CcmAddIntVar(therec,NrmStringToQuark("hyai"),therec->dims[ILEV_DIM_NUMBER].size,_NclNameToTypeClass(NrmStringToQuark("float")),tmp_siga,PERMANENT,ILEV_DIM_NUMBER);
			CcmAddStringVarAtt(tmp_var,NrmStringToQuark("long_name"),NrmStringToQuark("hybrid A coefficient at layer interfaces"));
			tmp_var = CcmAddIntVar(therec,NrmStringToQuark("hybi"),therec->dims[ILEV_DIM_NUMBER].size,_NclNameToTypeClass(NrmStringToQuark("float")),tmp_sigb,PERMANENT,ILEV_DIM_NUMBER);
			CcmAddStringVarAtt(tmp_var,NrmStringToQuark("long_name"),NrmStringToQuark("hybrid B coefficient at layer interfaces"));
	
			tmp_lon = (float*)NclMalloc(sizeof(float)*initial_iheader.NLEV);
			tmp_siga = (float*)NclMalloc(sizeof(float)*initial_iheader.NLEV);
			tmp_sigb = (float*)NclMalloc(sizeof(float)*initial_iheader.NLEV);
/*
			fprintf(stdout,"lev:\n");
*/
			for(i = 0; i < initial_iheader.NLEV; i++) {
				tmp_lon[i] = 1000.0 * ( *(initial_rheader.siga + 2*i+1) + *(initial_rheader.sigb + 2*i+1));
				tmp_siga[i] = *(initial_rheader.siga + 2*i+1);
				tmp_sigb[i] = *(initial_rheader.sigb + 2*i+1);
/*
				fprintf(stdout,"%f\n",tmp_lon[i]);
*/
			}
			tmp_var = CcmAddIntVar(therec,therec->dims[MLEV_DIM_NUMBER].dim_name,therec->dims[MLEV_DIM_NUMBER].size,_NclNameToTypeClass(NrmStringToQuark("float")),tmp_lon,PERMANENT,MLEV_DIM_NUMBER);
                        CcmAddStringVarAtt(tmp_var,NrmStringToQuark("hybrid_PS_var"),NrmStringToQuark("PS"));
                        CcmAddStringVarAtt(tmp_var,NrmStringToQuark("hybrid_P0_var"),NrmStringToQuark("P0"));
                        CcmAddStringVarAtt(tmp_var,NrmStringToQuark("hybrid_B_var"),NrmStringToQuark("hybm"));
                        CcmAddStringVarAtt(tmp_var,NrmStringToQuark("hybrid_A_var"),NrmStringToQuark("hyam"));
			CcmAddStringVarAtt(tmp_var,NrmStringToQuark("long_name"),NrmStringToQuark("hybrid level at layer midpoints (1000*(A+B))"));
			tmp_var = CcmAddIntVar(therec,NrmStringToQuark("hyam"),therec->dims[MLEV_DIM_NUMBER].size,_NclNameToTypeClass(NrmStringToQuark("float")),tmp_siga,PERMANENT,MLEV_DIM_NUMBER);
			CcmAddStringVarAtt(tmp_var,NrmStringToQuark("long_name"),NrmStringToQuark("hybrid A coefficient at layer midpoints"));
			tmp_var = CcmAddIntVar(therec,NrmStringToQuark("hybm"),therec->dims[MLEV_DIM_NUMBER].size,_NclNameToTypeClass(NrmStringToQuark("float")),tmp_sigb,PERMANENT,MLEV_DIM_NUMBER);
			CcmAddStringVarAtt(tmp_var,NrmStringToQuark("long_name"),NrmStringToQuark("hybrid B coefficient at layer midpoints"));

		} else {
			therec->dims[ILEV_DIM_NUMBER].dim_name= NrmStringToQuark("ilev");
                        therec->dims[MLEV_DIM_NUMBER].dim_name= NrmStringToQuark("lev");
                        therec->dims[ILEV_DIM_NUMBER].size = initial_iheader.NLEV;
                        therec->dims[MLEV_DIM_NUMBER].size = initial_iheader.NLEV;
			tmp_lon = (float*)NclMalloc(sizeof(float)*initial_iheader.NLEV);
			for(i = 0; i < initial_iheader.NLEV; i++) {
				tmp_lon[i] = initial_rheader.sigapb[i];
			}
			tmp_var = CcmAddIntVar(therec,therec->dims[MLEV_DIM_NUMBER].dim_name,therec->dims[MLEV_DIM_NUMBER].size,_NclNameToTypeClass(NrmStringToQuark("float")),tmp_lon,PERMANENT,MLEV_DIM_NUMBER);
		}
		tmp_lon = (float*)NclMalloc(sizeof(float)*initial_iheader.NLON);
		spacing = 360.0/(initial_iheader.NLON);
		for(i = 0; i < initial_iheader.NLON; i++) {
			tmp_lon[i] = i * spacing;
		}
		tmp_var = CcmAddIntVar(therec,therec->dims[LONGITUDE_DIM_NUMBER].dim_name,therec->dims[LONGITUDE_DIM_NUMBER].size,_NclNameToTypeClass(NrmStringToQuark("float")),tmp_lon,PERMANENT,LONGITUDE_DIM_NUMBER);
		CcmAddStringVarAtt(tmp_var,NrmStringToQuark("units"),NrmStringToQuark("degrees_east"));
		CcmAddStringVarAtt(tmp_var,NrmStringToQuark("long_name"),NrmStringToQuark("longitude"));

		tmp_lon = (float*)NclMalloc(sizeof(float)*initial_iheader.NOREC);
		tmp_siga = (float*)NclMalloc(sizeof(float)*initial_iheader.NOREC);
		for(i = 0; i < initial_iheader.NOREC; i++) {
			tmp_lon[i] = (float)initial_rheader.mplat[i];
			tmp_siga[i] = (float)initial_rheader.mpwts[i];
		}
		tmp_var = CcmAddIntVar(therec,therec->dims[LATITUDE_DIM_NUMBER].dim_name,therec->dims[LATITUDE_DIM_NUMBER].size,_NclNameToTypeClass(NrmStringToQuark("float")),tmp_lon,PERMANENT,LATITUDE_DIM_NUMBER);
		CcmAddStringVarAtt(tmp_var,NrmStringToQuark("units"),NrmStringToQuark("degrees_north"));
		CcmAddStringVarAtt(tmp_var,NrmStringToQuark("long_name"),NrmStringToQuark("latitude"));

		tmp_var = CcmAddIntVar(therec,NrmStringToQuark("gw"),therec->dims[LATITUDE_DIM_NUMBER].size,_NclNameToTypeClass(NrmStringToQuark("float")),tmp_siga,PERMANENT,LATITUDE_DIM_NUMBER);
		CcmAddStringVarAtt(tmp_var,NrmStringToQuark("long_name"),NrmStringToQuark("gauss weights"));


		tmp_var = CcmAddIntVar(therec,NrmStringToQuark("ntrm"),1,_NclNameToTypeClass(NrmStringToQuark("integer")),&therec->header.iheader.NTRM,STATIC,SCALAR_DIM_NUMBER);
		CcmAddStringVarAtt(tmp_var,NrmStringToQuark("long_name"),NrmStringToQuark("spectral truncation parameter M"));

		tmp_var = CcmAddIntVar(therec,NrmStringToQuark("ntrn"),1,_NclNameToTypeClass(NrmStringToQuark("integer")),&therec->header.iheader.NTRN,STATIC,SCALAR_DIM_NUMBER);
		CcmAddStringVarAtt(tmp_var,NrmStringToQuark("long_name"),NrmStringToQuark("spectral truncation parameter N"));

		tmp_var = CcmAddIntVar(therec,NrmStringToQuark("ntrk"),1,_NclNameToTypeClass(NrmStringToQuark("integer")),&therec->header.iheader.NTRK,STATIC,SCALAR_DIM_NUMBER);
		CcmAddStringVarAtt(tmp_var,NrmStringToQuark("long_name"),NrmStringToQuark("spectral truncation parameter K"));

		tmp_var = CcmAddIntVar(therec,NrmStringToQuark("ndbase"),1,_NclNameToTypeClass(NrmStringToQuark("integer")),&therec->header.iheader.NDBASE,STATIC,SCALAR_DIM_NUMBER);
		CcmAddStringVarAtt(tmp_var,NrmStringToQuark("long_name"),NrmStringToQuark("base day for this case"));

		tmp_var = CcmAddIntVar(therec,NrmStringToQuark("nsbase"),1,_NclNameToTypeClass(NrmStringToQuark("integer")),&therec->header.iheader.NSBASE,STATIC,SCALAR_DIM_NUMBER);
		CcmAddStringVarAtt(tmp_var,NrmStringToQuark("units"),NrmStringToQuark("s"));
		CcmAddStringVarAtt(tmp_var,NrmStringToQuark("long_name"),NrmStringToQuark("seconds to complete base day"));


		tmp_var = CcmAddIntVar(therec,NrmStringToQuark("nbdate"),1,_NclNameToTypeClass(NrmStringToQuark("integer")),&therec->header.iheader.NBDATE,STATIC,SCALAR_DIM_NUMBER);
		CcmAddStringVarAtt(tmp_var,NrmStringToQuark("long_name"),NrmStringToQuark("base date as 6 digit integer (YYMMDD)"));

		tmp_var = CcmAddIntVar(therec,NrmStringToQuark("nbsec"),1,_NclNameToTypeClass(NrmStringToQuark("integer")),&therec->header.iheader.NBSEC,STATIC,SCALAR_DIM_NUMBER);
		CcmAddStringVarAtt(tmp_var,NrmStringToQuark("units"),NrmStringToQuark("s"));
		CcmAddStringVarAtt(tmp_var,NrmStringToQuark("long_name"),NrmStringToQuark("seconds to complete base date"));



		tmp_var = CcmAddIntVar(therec,NrmStringToQuark("mdt"),1,_NclNameToTypeClass(NrmStringToQuark("integer")),&therec->header.iheader.MDT,STATIC,SCALAR_DIM_NUMBER);
		CcmAddStringVarAtt(tmp_var,NrmStringToQuark("units"),NrmStringToQuark("s"));
		CcmAddStringVarAtt(tmp_var,NrmStringToQuark("long_name"),NrmStringToQuark("model timestep"));
		
		tmp_var = CcmAddIntVar(therec,NrmStringToQuark("mhisf"),1,_NclNameToTypeClass(NrmStringToQuark("integer")),&therec->header.iheader.MHISF,STATIC,SCALAR_DIM_NUMBER);
		CcmAddStringVarAtt(tmp_var,NrmStringToQuark("long_name"),NrmStringToQuark("frequency of model writes (timesteps)"));

		tmp_var = CcmAddIntVar(therec,NrmStringToQuark("mfstrt"),1,_NclNameToTypeClass(NrmStringToQuark("integer")),&therec->header.iheader.MFSTRT,STATIC,SCALAR_DIM_NUMBER);


		tmp_var = CcmAddIntVar(therec,NrmStringToQuark("ozone_mss"),80,_NclNameToTypeClass(NrmStringToQuark("character")),&therec->header.cheader.LNHSTVO,STATIC,CHAR_DIM_NUMBER);
		CcmAddStringVarAtt(tmp_var,NrmStringToQuark("seq"),CcmVarName2(therec->header.cheader.LSHSTVO));
		CcmAddStringVarAtt(tmp_var,NrmStringToQuark("time"),CcmVarName(therec->header.cheader.LTHSTVO));
		CcmAddStringVarAtt(tmp_var,NrmStringToQuark("date"),CcmVarName(therec->header.cheader.LDHSTVO));
		CcmAddStringVarAtt(tmp_var,NrmStringToQuark("long_name"),NrmStringToQuark("MSS pathname of ozone boundary data"));

		tmp_var = CcmAddIntVar(therec,NrmStringToQuark("tibnd_mss"),80,_NclNameToTypeClass(NrmStringToQuark("character")),&therec->header.cheader.LNHSTT,STATIC,CHAR_DIM_NUMBER);
		CcmAddStringVarAtt(tmp_var,NrmStringToQuark("seq"),CcmVarName2(therec->header.cheader.LSHSTT));
		CcmAddStringVarAtt(tmp_var,NrmStringToQuark("time"),CcmVarName(therec->header.cheader.LTHSTT));
		CcmAddStringVarAtt(tmp_var,NrmStringToQuark("date"),CcmVarName(therec->header.cheader.LDHSTT));
		CcmAddStringVarAtt(tmp_var,NrmStringToQuark("long_name"),NrmStringToQuark("MSS pathname of time-invariant boundary data"));

		tmp_var = CcmAddIntVar(therec,NrmStringToQuark("sst_mss"),80,_NclNameToTypeClass(NrmStringToQuark("character")),&therec->header.cheader.LNHSTVS,STATIC,CHAR_DIM_NUMBER);
		CcmAddStringVarAtt(tmp_var,NrmStringToQuark("seq"),CcmVarName2(therec->header.cheader.LSHSTVS));
		CcmAddStringVarAtt(tmp_var,NrmStringToQuark("time"),CcmVarName(therec->header.cheader.LTHSTVS));
		CcmAddStringVarAtt(tmp_var,NrmStringToQuark("date"),CcmVarName(therec->header.cheader.LDHSTVS));
		CcmAddStringVarAtt(tmp_var,NrmStringToQuark("long_name"),NrmStringToQuark("MSS pathname of SST boundary data"));

		tmp_var = CcmAddIntVar(therec,NrmStringToQuark("init_mss"),80,_NclNameToTypeClass(NrmStringToQuark("character")),&therec->header.cheader.LNHSTI,STATIC,CHAR_DIM_NUMBER);
		CcmAddStringVarAtt(tmp_var,NrmStringToQuark("seq"),CcmVarName2(therec->header.cheader.LSHSTI));
		CcmAddStringVarAtt(tmp_var,NrmStringToQuark("time"),CcmVarName(therec->header.cheader.LTHSTI));
		CcmAddStringVarAtt(tmp_var,NrmStringToQuark("date"),CcmVarName(therec->header.cheader.LDHSTI));
		CcmAddStringVarAtt(tmp_var,NrmStringToQuark("long_name"),NrmStringToQuark("MSS pathname of initial data"));

		tmp_var = CcmAddIntVar(therec,NrmStringToQuark("first_mss"),80,_NclNameToTypeClass(NrmStringToQuark("character")),&therec->header.cheader.LNHSTF,STATIC,CHAR_DIM_NUMBER);
		CcmAddStringVarAtt(tmp_var,NrmStringToQuark("seq"),CcmVarName2(therec->header.cheader.LSHSTF));
		CcmAddStringVarAtt(tmp_var,NrmStringToQuark("time"),CcmVarName(therec->header.cheader.LTHSTF));
		CcmAddStringVarAtt(tmp_var,NrmStringToQuark("date"),CcmVarName(therec->header.cheader.LDHSTF));
		CcmAddStringVarAtt(tmp_var,NrmStringToQuark("long_name"),NrmStringToQuark("MSS pathname of first file for this case"));

		tmp_var = CcmAddIntVar(therec,NrmStringToQuark("current_mss"),80,_NclNameToTypeClass(NrmStringToQuark("character")),&therec->header.cheader.LNHSTC,STATIC,CHAR_DIM_NUMBER);
		CcmAddStringVarAtt(tmp_var,NrmStringToQuark("seq"),CcmVarName2(therec->header.cheader.LSHSTC));
		CcmAddStringVarAtt(tmp_var,NrmStringToQuark("time"),CcmVarName(therec->header.cheader.LTHSTC));
		CcmAddStringVarAtt(tmp_var,NrmStringToQuark("date"),CcmVarName(therec->header.cheader.LDHSTC));
		CcmAddStringVarAtt(tmp_var,NrmStringToQuark("long_name"),NrmStringToQuark("MSS pathname of this file"));


/*
* coff is NOW POINTING!!! to the top of the first latitude record!!!!
*/
		therec->n_vars = initial_iheader.NFLDH;
		if(initial_iheader.MFILTH == 0) {
			therec->n_headers = 1;
			therec->n_lat_recs = therec->n_headers * initial_iheader.NOREC;
		} else {
			therec->n_headers = initial_iheader.MFILTH;
			therec->n_lat_recs = initial_iheader.MFILTH*initial_iheader.NOREC;
		}

		therec->lat_rec_offsets = (long*)NclMalloc(therec->n_lat_recs*sizeof(long));
		for(i = 0; i < therec->n_lat_recs; i++) {
			therec->lat_rec_offsets[i] = -1;
		}
		therec->vars = (CcmVarInqRecList*)NclMalloc(sizeof(CcmVarInqRecList)*therec->n_vars);
		for(i = 0; i < therec->n_vars; i ++) {
			 
			therec->vars[i].offset = initial_iheader.MFLDS[i*3+ 1];
			therec->vars[i].packing = initial_iheader.MFLDS[i*3 + 2];
			therec->vars[i].level_type = initial_iheader.MFLDS[i*3] % 10;
			therec->vars[i].sample_type = initial_iheader.MFLDS[i*3] / 10;

			therec->vars[i].var_name_q = therec->vars[i].var_info.var_name_quark = CcmVarName(&(initial_cheader.MCFLDS[2*WORD_SIZE * i]));
			therec->vars[i].ccm_var_index = -1;
			for(j = 0; j < n_quarks; j++) {
				if(therec->vars[i].var_name_q == qlist[j].var_quark) {
					therec->vars[i].ccm_var_index = qlist[j].var_q_index;
					break;
				}
			}
			

			if(therec->vars[i].packing  > 1) {
				therec->vars[i].var_info.data_type = NCL_float;
			} else {
				therec->vars[i].var_info.data_type = NCL_double;
			}
			dim_num = 0;
			if(therec->n_headers > 1) {
				therec->vars[i].var_info.dim_sizes[dim_num] = therec->n_headers;
				therec->vars[i].var_info.file_dim_num[dim_num] = TIME_DIM_NUMBER;
				dim_num++;
			}
			if(initial_iheader.NOREC > 1) {
				therec->vars[i].var_info.dim_sizes[dim_num] = initial_iheader.NOREC;
				therec->vars[i].var_info.file_dim_num[dim_num] = LATITUDE_DIM_NUMBER;
				dim_num++;
			}
			switch(therec->vars[i].level_type) {
			case 0:
				break;
			case 1:
				therec->vars[i].var_info.dim_sizes[dim_num] = initial_iheader.NLEV + 1;
				therec->vars[i].var_info.file_dim_num[dim_num] = ILEV_DIM_NUMBER;
				dim_num++;
				break;
			case 2:	
				therec->vars[i].var_info.dim_sizes[dim_num] = initial_iheader.NLEV;
				therec->vars[i].var_info.file_dim_num[dim_num] = MLEV_DIM_NUMBER;
				dim_num++;
				break;
			default:
				NhlPError(NhlFATAL,NhlEUNKNOWN,"NclCCM: An incorrect value was detected suggesting an error in the CCM file (%s)",NrmQuarkToString(path));
				NclFree(therec);
					fclose(fd);
			NclFree(vbuf);
				return(NULL);
			}
			therec->vars[i].var_info.dim_sizes[dim_num] = initial_iheader.NLON;
			therec->vars[i].var_info.file_dim_num[dim_num] = LONGITUDE_DIM_NUMBER;
			dim_num++;
			therec->vars[i].var_info.num_dimensions = dim_num;
		}
		if(therec->cos_blocking == 2) coff += 4;
		cb = coff / BLOCK_SIZE;
		cb_off = (coff % BLOCK_SIZE) / WORD_SIZE;

		if((initial_iheader.MFTYP%10) == 3) {
			tmp_off = MySeek(therec,fd,1,coff);
			tmp_off = MyRead(therec,fd,buffer,1,tmp_off);
			index = (int)FloatIt(buffer);
			if((index < 1)||(index > initial_iheader.NOREC)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"NclCCM: An error occurred while indexing latitude data records. This file is not a valid CCM history file");
				NclFree(therec);
				fclose(fd);
			NclFree(vbuf);
				return(NULL);
			}
			therec->lat_rec_offsets[(index-1)] = coff;
			if(therec->cos_blocking == 2) coff += 4;
			coff = MySeek(therec,fd,initial_iheader.MAXSIZ + 1,coff);
			tmp_iheader = initial_iheader;
		} else {
			index = 1;
			if(therec->cos_blocking == 2) coff += 4;
			therec->lat_rec_offsets[(index-1)] = coff;
			coff = MySeek(therec,fd,initial_iheader.MAXSIZ + 1,coff);
			tmp_iheader = initial_iheader;
		}


		j = 1;
/*
		for(i = 0; i< initial_iheader.MFILTH;i++) {
*/
		done = 0;
		i = 0;
		n_time = 0;
		if(initial_iheader.MFILTH == 0) {
			time_var = (double*)NclMalloc(sizeof(double)*therec->n_headers);
			date = (int*)NclMalloc(sizeof(int)*therec->n_headers);
			datesec = (int*)NclMalloc(sizeof(int)*therec->n_headers);
			time_var_size = therec->n_headers;
			time_var[n_time] = (double)initial_iheader.NDCUR + initial_iheader.NSCUR/86400.;
			date[n_time] = initial_iheader.NCDATE;
			datesec[n_time++] = initial_iheader.NCSEC;
			bogus_mfilth = 1;
		} else {
			time_var = (double*)NclMalloc(sizeof(double)*initial_iheader.MFILTH);
			date = (int*)NclMalloc(sizeof(int)*initial_iheader.MFILTH);
			datesec = (int*)NclMalloc(sizeof(int)*initial_iheader.MFILTH);
			time_var_size = initial_iheader.MFILTH;
			time_var[n_time] = (double)initial_iheader.NDCUR + initial_iheader.NSCUR/86400.;
			date[n_time] = initial_iheader.NCDATE;
			datesec[n_time++] = initial_iheader.NCSEC;
			bogus_mfilth = 0;

		
		}
		while(!done) {
/*
* coff NOW POINTING TO BEGINING OF NEXT TIME STEP, have to unpack or skip headers
* Need to actual compare verses pervious header so errors can be detected but for now this is ok
* note that tmp_iheader.MFILH is used as time_index just in case time isn't written
* linearly into history files similar to lat records.
*/
			for( ; j < initial_iheader.NOREC; j++) {
				if(therec->cos_blocking == 2) coff += 4;
				if((initial_iheader.MFTYP%10) == 3) {

					tmp_off = MySeek(therec,fd,1,coff);
					tmp_off = MyRead(therec,fd,buffer,1,tmp_off);
					index = (int)FloatIt((buffer));
					if((index < 1)||(index > tmp_iheader.NOREC)) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"NclCCM: An error occurred while indexing latitude data records. This file is not a valid CCM history file");
						NclFree(therec);
						fclose(fd);
						NclFree(vbuf);
						return(NULL);
					}
					therec->lat_rec_offsets[i*tmp_iheader.NOREC+(index-1)] = coff;
					if(therec->cos_blocking == 2) coff += 4;
					coff = MySeek(therec,fd,tmp_iheader.MAXSIZ + 1,coff);
				} else {
					if(j%2 == 0) {
						index = j/2;
					} else {
						index = initial_iheader.NOREC - (int)(j/2) -1;
					}
				}
			}
			if( i < initial_iheader.MFILTH-1) {
				cb = coff / BLOCK_SIZE;
				cb_off = (coff % BLOCK_SIZE) / WORD_SIZE;
				tmp_off = UnPackIntHeader(therec,fd,&tmp_iheader,cb,cb_off);
/*
				fprintf(stdout,"reading timestep %d\n",tmp_iheader.MFILH);
*/
				if(tmp_off == -1) {
					therec->dims[TIME_DIM_NUMBER].size = i+1;	
					therec->header.iheader.MFILTH = i+1;       
					therec->n_headers = i+1;	
					for(j = 0; j < therec->n_vars; j++) {
						struct _NclCcmFVarRec *vinfo = &therec->vars[j].var_info;
						if (vinfo->file_dim_num[0] != TIME_DIM_NUMBER) {
							int k;
							for (k = vinfo->num_dimensions; k > 0; k--) {
								vinfo->dim_sizes[k] = vinfo->dim_sizes[k-1];
								vinfo->file_dim_num[k] = vinfo->file_dim_num[k-1];
							}
							vinfo->file_dim_num[0] = TIME_DIM_NUMBER;
							vinfo->num_dimensions++;
						}
						vinfo->dim_sizes[0] = i+1;
					}
					done = 1;
					break;
				}
				time_var[n_time] = (double)tmp_iheader.NDCUR + tmp_iheader.NSCUR/86400.;
				date[n_time] = tmp_iheader.NCDATE;
				datesec[n_time++] = tmp_iheader.NCSEC;
				cb = tmp_off / BLOCK_SIZE;
				cb_off = (tmp_off % BLOCK_SIZE) / WORD_SIZE;
				tmp_off= UnPackCharHeader(therec,fd,&tmp_iheader,&tmp_cheader,cb,cb_off);
				if(tmp_off == -1) {
					NclFree(therec);
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Error opening CCM character header for file (%s)",NrmQuarkToString(path));
					fclose(fd);
					NclFree(vbuf);
					return(NULL);
				}
				cb = tmp_off / BLOCK_SIZE;
				cb_off = (tmp_off % BLOCK_SIZE) / WORD_SIZE;
				tmp_off = UnPackRealHeader(therec,fd,&tmp_iheader,&tmp_rheader,cb,cb_off);
				if(tmp_off == -1) {
					NclFree(therec);
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Error opening CCM real header for file (%s)",NrmQuarkToString(path));
					fclose(fd);
					NclFree(vbuf);
					return(NULL);
				}
				if(!CompareHeaders(&initial_iheader,&initial_cheader,&initial_rheader,
						    &tmp_iheader,&tmp_cheader,&tmp_rheader)) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Comparison of headers failed, headers for timestep number (%d) vary unacceptably from initial header, NCL doesn't handle this",i+1);
					NclFree(therec);
					fclose(fd);
					NclFree(vbuf);
					return(NULL);

				} else {
					coff = tmp_off;
				}
				j = 0;
			} else if(i >= initial_iheader.MFILTH-1){
				cb = coff / BLOCK_SIZE;
				cb_off = (coff % BLOCK_SIZE) / WORD_SIZE;
				tmp_off = UnPackIntHeader(therec,fd,&tmp_iheader,cb,cb_off);
/*
				fprintf(stdout,"reading timestep %d\n",tmp_iheader.MFILH);
*/
				if(tmp_off == -1) {
					done = 1;
					therec->dims[TIME_DIM_NUMBER].size = i+1;	
					therec->header.iheader.MFILTH = i+1;       
					therec->n_headers = i+1;	
					for(j = 0; j < therec->n_vars; j++) {
						struct _NclCcmFVarRec *vinfo = &therec->vars[j].var_info;
						if (vinfo->file_dim_num[0] != TIME_DIM_NUMBER) {
							int k;
							for (k = vinfo->num_dimensions; k > 0; k--) {
								vinfo->dim_sizes[k] = vinfo->dim_sizes[k-1];
								vinfo->file_dim_num[k] = vinfo->file_dim_num[k-1];
							}
							vinfo->file_dim_num[0] = TIME_DIM_NUMBER;
							vinfo->num_dimensions++;
						}
						vinfo->dim_sizes[0] = i+1;
					}
					break;
				}
				if((n_time == time_var_size)||((n_time == time_var_size-1)&&(bogus_mfilth))) {
					time_var_size *=2;
					time_var = (double*)NclRealloc(time_var,sizeof(double)*time_var_size);
					date = (int*)NclRealloc(date,sizeof(int)*time_var_size);
					datesec= (int*)NclRealloc(datesec,sizeof(int)*time_var_size);
				}
				time_var[n_time] = (double)tmp_iheader.NDCUR + tmp_iheader.NSCUR/86400.;
				date[n_time] = tmp_iheader.NCDATE;
				datesec[n_time++] = tmp_iheader.NCSEC;
				therec->lat_rec_offsets = NclRealloc(therec->lat_rec_offsets,(therec->n_lat_recs+tmp_iheader.NOREC)*sizeof(long));
				therec->n_lat_recs += tmp_iheader.NOREC;
				cb = tmp_off / BLOCK_SIZE;
				cb_off = (tmp_off % BLOCK_SIZE) / WORD_SIZE;
				tmp_off= UnPackCharHeader(therec,fd,&tmp_iheader,&tmp_cheader,cb,cb_off);
				if(tmp_off == -1) {
					NclFree(therec);
/*
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Error opening CCM character header for file (%s)",NrmQuarkToString(path));
*/
					fclose(fd);
					NclFree(vbuf);
					return(NULL);
				}
				cb = tmp_off / BLOCK_SIZE;
				cb_off = (tmp_off % BLOCK_SIZE) / WORD_SIZE;
				tmp_off = UnPackRealHeader(therec,fd,&tmp_iheader,&tmp_rheader,cb,cb_off);
				if(tmp_off == -1) {
					NclFree(therec);
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Error opening CCM real header for file (%s)",NrmQuarkToString(path));
					fclose(fd);
					NclFree(vbuf);
					return(NULL);
				}
				if(!CompareHeaders(&initial_iheader,&initial_cheader,&initial_rheader,
						    &tmp_iheader,&tmp_cheader,&tmp_rheader)) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Comparison of headers failed, headers for timestep number (%d) vary unacceptably from initial header, NCL doesn't handle this",i+1);
					NclFree(therec);
					fclose(fd);
					NclFree(vbuf);
					return(NULL);

				} else {
					coff = tmp_off;
				}
				j = 0;
			}
			i++;
		}
		tmp_var = CcmAddIntVar(therec,NrmStringToQuark("time"),n_time,_NclNameToTypeClass(NrmStringToQuark("double")),time_var,PERMANENT,TIME_DIM_NUMBER);
		CcmAddStringVarAtt(tmp_var,NrmStringToQuark("long_name"),NrmStringToQuark("time"));
		CcmAddStringVarAtt(tmp_var,NrmStringToQuark("units"),NrmStringToQuark(time_units(initial_iheader.NBDATE,initial_iheader.NBSEC,units)));
		
		tmp_var = CcmAddIntVar(therec,NrmStringToQuark("date"),n_time,_NclNameToTypeClass(NrmStringToQuark("integer")),date,PERMANENT,TIME_DIM_NUMBER);
		CcmAddStringVarAtt(tmp_var,NrmStringToQuark("long_name"),NrmStringToQuark("current date as 6 digit integer (YYMMDD)"));

		tmp_var = CcmAddIntVar(therec,NrmStringToQuark("datesec"),n_time,_NclNameToTypeClass(NrmStringToQuark("integer")),datesec,PERMANENT,TIME_DIM_NUMBER);
		CcmAddStringVarAtt(tmp_var,NrmStringToQuark("units"),NrmStringToQuark("s"));
		CcmAddStringVarAtt(tmp_var,NrmStringToQuark("long_name"),NrmStringToQuark("seconds to complete current date"));
/*
		for(i = 0; i < initial_iheader.MFILTH * initial_iheader.NOREC; i++) {
			fprintf(stdout,"%ld\n",therec->lat_rec_offsets[i]);
		}
*/
		fclose(fd);
			NclFree(vbuf);
		return(therec);
	} else if(fd == NULL) {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"NclCCM: Could not open (%s) check permissions",NrmQuarkToString(path));
			NclFree(vbuf);
        }
        return(NULL);
}

static void CcmFreeFileRec
#if	NhlNeedProto
(void * therec) 
#else
(therec)
void *therec;
#endif
{
	return;
}

static NclQuark *CcmGetVarNames
#if	NhlNeedProto
(void * therec, int *n_vars)
#else
(therec, n_vars)
void * therec;
int *n_vars;
#endif
{
	CCMFileRec *thefile = (CCMFileRec*)therec;
	NclQuark *arout;
	CcmIntVarInqRecList *tmp;
	int i;
	*n_vars = thefile->n_vars + thefile->n_int_vars;

	i = 0;

	arout = NclMalloc(sizeof(NclQuark)* *n_vars);
	tmp = thefile->int_vars;
	for(i = 0; i < thefile->n_int_vars; i++) {
		arout[i] = tmp->var_name_q;
		tmp = tmp->next;
	}
	for( ; i < *n_vars; i++ ) {
		arout[i] = thefile->vars[i-thefile->n_int_vars].var_info.var_name_quark;
	}	
	return(arout);
}

static NclFVarRec* CcmGetVarInfo
#if     NhlNeedProto
(void * therec, NclQuark var_name)
#else
(therec, n_vars)
void * therec;
NclQuark var_name;
#endif
{
	CCMFileRec *thefile = (CCMFileRec*)therec;
	NclFVarRec *tmp = NULL;
	CcmIntVarInqRecList *vtmp;
	int i,j;

	vtmp = thefile->int_vars;
	for(i = 0; i < thefile->n_int_vars; i++ ) {
		
		if(var_name == vtmp->var_name_q) {
			tmp = (NclFVarRec*)NclMalloc(sizeof(NclFVarRec));
                        tmp->var_name_quark  = vtmp->var_info.var_name_quark;
                        tmp->var_full_name_quark  = vtmp->var_info.var_name_quark;
                        tmp->var_real_name_quark  = vtmp->var_info.var_name_quark;
                        tmp->data_type  = vtmp->var_info.data_type;
                        tmp->num_dimensions  = vtmp->var_info.num_dimensions;
                        for(j=0;j< tmp->num_dimensions;j++) {
                                tmp->file_dim_num[j]  = vtmp->var_info.file_dim_num[j];
                        }
			return(tmp);
		}
		vtmp = vtmp->next;
	}
	for(i = 0; i < thefile->n_vars; i++ ) {

		if(var_name == thefile->vars[i].var_name_q) {
			tmp = (NclFVarRec*)NclMalloc(sizeof(NclFVarRec));
                        tmp->var_name_quark  = thefile->vars[i].var_info.var_name_quark;
                        tmp->data_type  = thefile->vars[i].var_info.data_type;
                        tmp->num_dimensions  = thefile->vars[i].var_info.num_dimensions;
                        for(j=0;j< tmp->num_dimensions;j++) {
                                tmp->file_dim_num[j]  = thefile->vars[i].var_info.file_dim_num[j];
                        }
        		return(tmp);
		}
	}
	return NULL;
}

static NclQuark* CcmGetDimNames
#if     NhlNeedProto
(void * therec, int * n_dims) 
#else
(therec, n_dims) 
void * therec;
int * n_dims;
#endif
{
	CCMFileRec *thefile = (CCMFileRec*)therec;
	NclQuark *dims;
	int i;

	*n_dims = thefile->n_dims;
	dims = (NclQuark*)NclMalloc(sizeof(NclQuark)*thefile->n_dims);
	for(i = 0; i < thefile->n_dims; i++) {
		dims[i] = thefile->dims[i].dim_name;
	}
	return(dims);
}

static NclFDimRec * CcmGetDimInfo
#if	NhlNeedProto
(void * therec, NclQuark dim_name)
#else
(therec, dim_name)
void * therec;
NclQuark dim_name;
#endif
{
	CCMFileRec *thefile = (CCMFileRec*)therec;
	NclFDimRec *dims = NULL;
	int i;
	for(i = 0; i < thefile->n_dims; i++) {
		if(thefile->dims[i].dim_name == dim_name) {
			dims = (NclFDimRec*)NclMalloc(sizeof(NclFDimRec));
			dims->dim_name_quark = thefile->dims[i].dim_name;
			dims->dim_size = thefile->dims[i].size;
			dims->is_unlimited = 0;
		}
	}
	return(dims);
}

static NclQuark *CcmGetAttNames
#if NhlNeedProto
(void *therec, int *n_atts)
#else
(therec, n_atts)
void *therec;
int *n_atts;
#endif
{
	NclQuark* arout = NclMalloc(sizeof(NclQuark)*2);
	*n_atts = 2;
	arout[0] = NrmStringToQuark("case");
	arout[1] = NrmStringToQuark("title");
	return(arout);
}

static NclFAttRec * CcmGetAttInfo
#if NhlNeedProto
(void * therec, NclQuark attname)
#else
(therec,attname)
void *therec;
NclQuark attname;
#endif
{
	NclFAttRec *tmp = NclMalloc(sizeof(NclFAttRec));

	tmp->att_name_quark = attname;
	tmp->data_type = NCL_string;
	tmp->num_elements = 1;
	return(tmp);
}
static NclQuark *CcmGetVarAttNames
#if NhlNeedProto
(void * therec, NclQuark var_name, int *n_atts)
#else
(therec, var_name, n_atts)
void * therec;
NclQuark var_name;
int *n_atts;
#endif
{
	CCMFileRec *thefile = (CCMFileRec*)therec;
	CcmAttInqRecList *tmp_att = NULL;
	CcmIntVarInqRecList *tmp_var= NULL;
	int i,j;
	NclQuark *arout;
	*n_atts = 0;

	for(i = 0; i < thefile->n_vars; i++ ) {

		if(var_name == thefile->vars[i].var_name_q) {
			arout = (NclQuark*)NclMalloc(sizeof(NclQuark)*4);
			if(thefile->vars[i].ccm_var_index != -1) {
				*n_atts = 4;
				arout[0] = NrmStringToQuark("long_name");
				arout[1] = NrmStringToQuark("units");
				arout[2] = NrmStringToQuark("t_op");
				arout[3] = NrmStringToQuark("_FillValue");
        			return(arout);
			} else {
				*n_atts = 4;
				arout[0] = NrmStringToQuark("long_name");
				arout[1] = NrmStringToQuark("units");
				arout[2] = NrmStringToQuark("t_op");
				arout[3] = NrmStringToQuark("_FillValue");
        			return(arout);
			}
			
		}
	}
	tmp_var = thefile->int_vars;
	for(i = 0; i < thefile->n_int_vars; i++) {
		if(var_name == tmp_var->var_name_q) {
			arout = (NclQuark*)NclMalloc(sizeof(NclQuark)*tmp_var->n_atts);
			j = 0;
			tmp_att = tmp_var->theatts;
			while(tmp_att != NULL ){
				arout[j] = tmp_att->attname;
				tmp_att = tmp_att->next;
				j++;
			}
			*n_atts = j;
			return(arout);
		} else {
			tmp_var = tmp_var->next;
		}
	}
	*n_atts = 0;	
	return(NULL);
	
}
static NclFAttRec *CcmGetVarAttInfo
#if     NhlNeedProto
(void *therec, NclQuark var_name, NclQuark att_name) 
#else 
(therec, var_name, att_name) 
void *therec;
NclQuark var_name;
NclQuark att_name;
#endif
{
	NclFAttRec *tmp = NclMalloc(sizeof(NclFAttRec));
	CCMFileRec *thefile = (CCMFileRec*)therec;
	int i;

	if(att_name == NrmStringToQuark("_FillValue"))  {
		for(i = 0; i < thefile->n_vars; i++ ) {
			if(var_name == thefile->vars[i].var_name_q) {
				tmp->att_name_quark = att_name;
				tmp->data_type = thefile->vars[i].var_info.data_type;
				tmp->num_elements = 1;
				return(tmp);
			}
		}
		NclFree(tmp);
		return(NULL);
	} else {
		tmp->att_name_quark = att_name;
		tmp->data_type = NCL_string;
		tmp->num_elements = 1;
	}
	return(tmp);
}


static NclFVarRec *CcmGetCoordInfo
#if	NhlNeedProto
(void *therec, NclQuark dim_name)
#else 
(therec, dim_name)
void *therec;
NclQuark dim_name;
#endif
{
	return(CcmGetVarInfo(therec,dim_name));
}


static int MyUnPack
#if	NhlNeedProto
(CCMFileRec* therec,FILE* fd,void *rbuffer, void* buffer, int poff, long coff, int packing,ng_size_t *dimsizes,int level_type )
#else
(therec, fd, rbuffer, buffer,poff, coff, packing,dimsizes, level_type)
CCMFileRec* therec;
FILE* fd;
void *rbuffer;
void* buffer;
int poff;
long coff;
int packing;
ng_size_t *dimsizes;
int level_type;
#endif
{
	long tmp_off;
	int zero = 0;
	ng_size_t total = 0;
	ng_size_t n_elem= 0;
	double ll[2];
	unsigned int uval;
	unsigned short sval;
	int k,i,j;
	ng_size_t mul_lev = 0;
    ng_size_t index;

	switch(level_type) {
	case 1:
		n_elem = (dimsizes[0]-1)*dimsizes[1];
		mul_lev = (dimsizes[0]-1);
		break;
	case 0:
	case 2:
		n_elem = dimsizes[0]*dimsizes[1];
		mul_lev = (dimsizes[0]);
		break;
	}


	
	tmp_off = MySeek(therec,fd,poff,coff);
	switch(packing) {
	case 1:
		if(level_type ==1) {
			for(i = 0; i < dimsizes[1]; i++) {
				((double*)rbuffer)[i] = DoubleIt(cray_missing_value);
			}
			index = dimsizes[1];
		} else {
			index = 0;
		}
		
		tmp_off = MyRead(therec,fd,buffer,n_elem,tmp_off);
		total = n_elem;
		ctodpf(buffer,&((double*)rbuffer)[index],&total,&zero);
		return(tmp_off);
		break;
	case 2:
		tmp_off = MyRead(therec,fd,buffer,(n_elem/2) + 2*mul_lev,tmp_off);
		k = 0;
		j = 0;
		if(level_type == 1) {
			for(i = 0; i < dimsizes[1]; i++) {
				((float*)rbuffer)[j*dimsizes[1] + i] = FloatIt(cray_missing_value);
			}
		} else {
			total = 2;
			ctodpf(&(((char*)buffer)[k*4]),ll,&total,&zero);
			k += 4;
			for(i = 0; i < dimsizes[1]; i++) {

/*
				memcpy(&uval,&(((char*)buffer)[k*4]),4);
*/
				uval = IntIt4(&(((char*)buffer)[k*4]));
				((float*)rbuffer)[j*dimsizes[1] + i] = (float)(ll[0] + ((double)uval)/ll[1]);
				k++;
			}
		}
		for(j = 1; j < dimsizes[0]; j++) {
			total = 2;
			ctodpf(&(((char*)buffer)[k*4]),ll,&total,&zero);
			k += 4;
			for(i = 0; i < dimsizes[1]; i++) {
/*
				memcpy(&uval,&(((char*)buffer)[k*4]),4);
*/
				uval = IntIt4(&(((char*)buffer)[k*4]));
				((float*)rbuffer)[j*dimsizes[1] + i] = (float)(ll[0] + ((double)uval)/ll[1]);
				k++;
			}
		}
		return(tmp_off);
		break;
	case 4:
		tmp_off = MyRead(therec,fd,buffer,(n_elem/4) + 2*mul_lev,tmp_off);
		k = 0;
		j = 0;
		if(level_type == 1) {
			for(i = 0; i < dimsizes[1]; i++) {
				((float*)rbuffer)[j*dimsizes[1] + i] = FloatIt(cray_missing_value);
			}
		} else {
			total = 2;
			ctodpf(&(((char*)buffer)[k*2]),ll,&total,&zero);
			k += 8;
			for(i = 0; i < dimsizes[1]; i++) {
/*
				memcpy(&sval,&(((char*)buffer)[k*2]),2);
*/
				sval = IntIt2(&(((char*)buffer)[k*2]));
				((float*)rbuffer)[j*dimsizes[1] + i] = (float)(ll[0] + ((double)sval)/ll[1]);
				k++;
			}
		}
		for(j = 1; j < dimsizes[0]; j++) {
			total = 2;
			ctodpf(&(((char*)buffer)[k*2]),ll,&total,&zero);
			k += 8;
			for(i = 0; i < dimsizes[1]; i++) {
/*
				memcpy(&sval,&(((char*)buffer)[k*2]),2);
*/
				sval = IntIt2(&(((char*)buffer)[k*2]));
				((float*)rbuffer)[j*dimsizes[1] + i] = (float)(ll[0] + ((double)sval)/ll[1]);
				k++;
			}
		}
		return(tmp_off);
		break;
	default:
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Packing for this CCM file is not 1,2, or 4. This is an severe error can't continue");
		return(-1);
	}
}

static void *CcmReadVar
#if	NhlNeedProto
(void *therec, NclQuark var_name,long *start,long* finish, long * stride, void *storage)
#else
(therec, var_name,start,finish, stride, storage)
void *therec;
NclQuark var_name;
long *start;
long* finish
long * stride
void *storage
#endif
{
	CCMFileRec *thefile = (CCMFileRec*)therec;
	NclSelectionRecord  sel_ptr;
	CcmVarInqRecList *tmp_var = NULL;
	int i,j,k;
	int buf_size,ns;
	char *buffer = NULL;
	char *rbuffer = NULL;
	long coff;
	int lstart,tstart;
	int lfinish,tfinish;
	int lstride,tstride;
	int nl,nt;
	FILE* fd;
	int to = 0;
	NclMultiDValData tmp_md;
	NclMultiDValData tmp_md2;
	ng_size_t dimsizes[4];
	CcmIntVarInqRecList *tmp;
	void *vbuf;

	for (i = 0; i < thefile->n_vars; i++) {
		if(thefile->vars[i].var_info.var_name_quark == var_name) {
			tmp_var = &(thefile->vars[i]);	
			break;	
		}
	}
	if(i == thefile->n_vars) {
		tmp = thefile->int_vars;
		for(i = 0; i < thefile->n_int_vars; i++) {
			if(var_name == tmp->var_name_q) {
				sel_ptr.n_entries = 1;
				sel_ptr.selection[0].sel_type = Ncl_SUBSCR;
                        	sel_ptr.selection[0].dim_num = 0;
                        	sel_ptr.selection[0].u.sub.start = start[0];
                        	sel_ptr.selection[0].u.sub.finish = finish[0];
                        	sel_ptr.selection[0].u.sub.stride = stride[0];
                        	sel_ptr.selection[i].u.sub.is_single = abs((finish[0] - start[0])/stride[0]) > 0 ? 0 : 1;
				if(tmp->thevalue != NULL) {
					tmp_md2 = (NclMultiDValData)_NclReadSubSection((NclData)tmp->thevalue,&sel_ptr,NULL);
					memcpy(storage,tmp_md2->multidval.val,tmp_md2->multidval.totalsize);
					_NclDestroyObj((NclObj)tmp_md2);
				}
				return(storage);
				break;
			} else {	
				tmp = tmp->next;
			}
		}
		NhlPError(NhlFATAL,NhlEUNKNOWN,"CCMReadVar: Variable (%s) undefined",NrmQuarkToString(var_name));
		return(NULL);
	} else {
		fd = fopen(NrmQuarkToString(thefile->file_path_q),"r");
		if(fd == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Ack! The file (%s) has been removed or no longer exists, can't continue");
			return(NULL);
		}
		vbuf = (void*)NclMalloc(4*getpagesize());
		setvbuf(fd,vbuf,_IOFBF,4*getpagesize());
		
		k = 0;
		if(tmp_var->var_info.file_dim_num[0] == TIME_DIM_NUMBER) {
			tstart = start[k];
			tfinish = finish[k];	
			tstride = stride[k];
			k++;
			lstart = start[k];
			lfinish = finish[k];
			lstride = stride[k];
			k++;
		} else { 
			tstart = 0;
			tfinish = 0;
			tstride = 1;
			lstart = start[k];
			lfinish = finish[k];
			lstride = stride[k];
			k++;
		}
		nt = ((tfinish-tstart)/ tstride) +1;
		nl = ((lfinish-lstart)/ lstride) +1;
		ns = 1;
		buf_size = 1;
		i = 0;
		while(k < tmp_var->var_info.num_dimensions) {
			sel_ptr.selection[i].sel_type = Ncl_SUBSCR;
                        sel_ptr.selection[i].dim_num = i;
                        sel_ptr.selection[i].u.sub.start = start[k];
                        sel_ptr.selection[i].u.sub.finish = finish[k];
                        sel_ptr.selection[i].u.sub.stride = stride[k];
                        sel_ptr.selection[i].u.sub.is_single = abs((finish[k] - start[k])/stride[k]) > 0 ? 0 : 1;

			dimsizes[i] = tmp_var->var_info.dim_sizes[k];
			ns *= ((finish[k] - start[k]) / stride[k]) + 1;
			buf_size *= tmp_var->var_info.dim_sizes[k];
			k++;i++;
		}
		sel_ptr.n_entries = i;
		buffer = (char*)NclMalloc((WORD_SIZE*buf_size/tmp_var->packing)+2*WORD_SIZE*(thefile->header.iheader.NLEV +1));
		rbuffer = (char*)NclMalloc(buf_size*sizeof(double));
		tmp_md = _NclCreateMultiDVal(
			NULL,
			NULL,
			Ncl_MultiDValData,
			0,
			rbuffer,
			NULL,
			i,
			dimsizes,
			PERMANENT,
			NULL,
			_NclNameToTypeClass((tmp_var->packing != 1) ? NrmStringToQuark("float"):NrmStringToQuark("double")));
		for(i = 0; i < nt; i++) {
			for(j = 0; j < nl; j++)  {
				coff = thefile->lat_rec_offsets[(tstart + i * tstride) * thefile->header.iheader.NOREC + (lstart + j * lstride)];
				if(tmp_md->multidval.n_dims == 1) {
					dimsizes[0] = 1;
					dimsizes[1] = tmp_md->multidval.dim_sizes[0];
				} else {
					dimsizes[0] = tmp_md->multidval.dim_sizes[0];
					dimsizes[1] = tmp_md->multidval.dim_sizes[1];
				}
				MyUnPack(thefile,fd,rbuffer,buffer,tmp_var->offset,coff,tmp_var->packing,dimsizes,tmp_var->level_type);
				tmp_md2 = (NclMultiDValData)_NclReadSubSection((NclData)tmp_md,&sel_ptr,NULL);
				memcpy(&((char*)storage)[to],tmp_md2->multidval.val,tmp_md2->multidval.totalsize);
				to += tmp_md2->multidval.totalsize;
				_NclDestroyObj((NclObj)tmp_md2);
				
				
			}
		}
		NclFree(buffer);
		_NclDestroyObj((NclObj)tmp_md);
		NclFree(vbuf);
		fclose(fd);
	}
	return(NULL);
}

static void *CcmReadCoord
#if	NhlNeedProto
(void * therec, NclQuark dim_name,long *start,long *finish, long* stride, void *storage)
#else
(therec, dim_name,start,finish, stride, storage)
void * therec;
NclQuark dim_name;
long *start;
long *finish;
long* stride;
void *storage;
#endif
{
	
	return(CcmReadVar(therec, dim_name,start,finish, stride, storage));
}
static void *CcmReadAtt
#if	NhlNeedProto
(void *therec, NclQuark att_name, void *storage)
#else
(therec, att_name, storage)
void *therec;
NclQuark att_name;
void *storage;
#endif
{
	CCMFileRec *thefile = (CCMFileRec*)therec;
	char buffer[81];
	int i;
	if(att_name == NrmStringToQuark("case")) {
		*(NclQuark*)storage = CcmVarName(thefile->header.cheader.MCASE);
		return(storage);
	} else if(att_name == NrmStringToQuark("title")) {
		memcpy(buffer,thefile->header.cheader.MCSTIT,80);
		buffer[80] = '\0';
		for(i = 79; i >= 0; i--) {
			if((buffer[i] == (char)32)||(buffer[i] == (char)10)) {
				buffer[i] = '\0';
			} else {
				break;
			}
		}
		*(NclQuark*)storage = NrmStringToQuark(buffer);
		return(storage);
	}
	return(NULL);
}

static void *CcmReadVarAtt
#if	NhlNeedProto
(void *therec, NclQuark var_name,NclQuark att_name, void *storage)
#else
(therec, var_name, att_name, storage)
void *therec;
NclQuark var_name;
NclQuark att_name;
void *storage;
#endif
{
	CCMFileRec *thefile = (CCMFileRec*)therec;
	int i;
	NclQuark *arout;
	CcmAttInqRecList *tmp_att = NULL;
	CcmIntVarInqRecList *tmp_var= NULL;

	for(i = 0; i < thefile->n_vars; i++ ) {

		if(var_name == thefile->vars[i].var_name_q) {
/*
				arout[0] = NrmQuarkToString("long_name");
				arout[1] = NrmQuarkToString("units");
*/
			if(NrmStringToQuark("units") == att_name) {
				if((thefile->vars[i].ccm_var_index != -1)&&(ccm_name_tab[thefile->vars[i].ccm_var_index].udunit!=NULL)) {
					*(NclQuark*)storage = NrmStringToQuark(ccm_name_tab[thefile->vars[i].ccm_var_index].udunit);
				} else {
					*(NclQuark*)storage = CcmVarName(&(thefile->header.cheader.MCFLDS[2*WORD_SIZE * i + WORD_SIZE]));
				}
			} else if(NrmStringToQuark("long_name") == att_name) {
				if(thefile->vars[i].ccm_var_index == -1) {
					*(NclQuark*)storage = thefile->vars[i].var_name_q;
				} else {
					*(NclQuark*)storage = NrmStringToQuark(ccm_name_tab[thefile->vars[i].ccm_var_index].long_name);
				}
			} else if(NrmStringToQuark("t_op") == att_name) {
				switch(thefile->vars[i].sample_type) {
				case 0:
					*(NclQuark*)storage = NrmStringToQuark("instantaneous");
					break;
				case 1:
					*(NclQuark*)storage = NrmStringToQuark("averaged");
					break;
				case 2:
					*(NclQuark*)storage = NrmStringToQuark("minimum_val");
					break;
				case 3:
					*(NclQuark*)storage = NrmStringToQuark("maximum_val");
					break;
				}
			} else if(NrmStringToQuark("_FillValue") == att_name) {
				if(thefile->vars[i].var_info.data_type == NCL_float) {
					*(float*)storage = FloatIt(cray_missing_value);
				} else if(thefile->vars[i].var_info.data_type == NCL_double){
					*(double*)storage = DoubleIt(cray_missing_value);
				}
			}
			return(storage);
		}
	}
	tmp_var = thefile->int_vars;
	for(i = 0; i < thefile->n_int_vars; i++) {
		if(var_name == tmp_var->var_name_q) {
			arout = (NclQuark*)NclMalloc(sizeof(NclQuark)*tmp_var->n_atts);
			tmp_att = tmp_var->theatts;
			while(tmp_att != NULL ){
				if(tmp_att->attname == att_name) {
					memcpy(storage,tmp_att->thevalue->multidval.val,tmp_att->thevalue->multidval.totalsize);
					return(storage);
				}
				tmp_att = tmp_att->next;
			}
			break;
		} else {
			tmp_var = tmp_var->next;
		}
	}
	return(NULL);
	
}

static NclBasicDataTypes CcmMapToNcl
#if     NhlNeedProto
( void * type)
#else
(type)
void *type;
#endif
{
	return(NCL_obj);
}

static void* CcmMapFromNcl 
#if     NhlNeedProto
(NclBasicDataTypes type)
#else
(type)
NclBasicDataTypes type;
#endif
{
	return(NULL);
}




NclFormatFunctionRec CCMRec = {
/* NclInitializeFileRecFunc initialize_file_rec */      CcmInitializeFileRec,
/* NclCreateFileFunc	   create_file; */		CcmCreateFile,
/* NclOpenFileFunc         open_file; */		CcmOpenFile,
/* NclFreeFileRecFunc      free_file_rec; */		CcmFreeFileRec,
/* NclGetVarNamesFunc      get_var_names; */		CcmGetVarNames,
/* NclGetVarInfoFunc       get_var_info; */		CcmGetVarInfo,
/* NclGetDimNamesFunc      get_dim_names; */		CcmGetDimNames,
/* NclGetDimInfoFunc       get_dim_info; */		CcmGetDimInfo,
/* NclGetAttNamesFunc      get_att_names; */		CcmGetAttNames,
/* NclGetAttInfoFunc       get_att_info; */		CcmGetAttInfo,
/* NclGetVarAttNamesFunc   get_var_att_names; */	CcmGetVarAttNames,
/* NclGetVarAttInfoFunc    get_var_att_info; */		CcmGetVarAttInfo,
/* NclGetCoordInfoFunc     get_coord_info; */		CcmGetCoordInfo,
/* NclReadCoordFunc        read_coord; */		CcmReadCoord,
/* NclReadCoordFunc        read_coord; */		NULL,
/* NclReadVarFunc          read_var; */			CcmReadVar,
/* NclReadVarFunc          read_var; */			NULL,
/* NclReadAttFunc          read_att; */			CcmReadAtt,
/* NclReadVarAttFunc       read_var_att; */		CcmReadVarAtt,
/* NclWriteCoordFunc       write_coord; */		NULL,
/* NclWriteCoordFunc       write_coord; */		NULL,
/* NclWriteVarFunc         write_var; */		NULL,
/* NclWriteVarFunc         write_var; */		NULL,
/* NclWriteAttFunc         write_att; */		NULL,
/* NclWriteVarAttFunc      write_var_att; */		NULL,
/* NclAddDimFunc           add_dim; */			NULL,
/* NclAddChunkDimFunc      add_chunk_dim; */		NULL,
/* NclRenameDimFunc        rename_dim; */		NULL,
/* NclAddVarFunc           add_var; */			NULL,
/* NclAddVarChunkFunc      add_var_chunk; */		NULL,
/* NclAddVarChunkCacheFunc add_var_chunk_cache; */	NULL,
/* NclSetVarCompressLevel  set_var_compress_level; */	NULL,
/* NclAddVarFunc           add_coord_var; */		NULL,
/* NclAddAttFunc           add_att; */			NULL,
/* NclAddVarAttFunc        add_var_att; */		NULL,
/* NclMapFormatTypeToNcl   map_format_type_to_ncl; */	CcmMapToNcl,
/* NclMapNclTypeToFormat   map_ncl_type_to_format; */	CcmMapFromNcl,
/* NclDelAttFunc           del_att; */			NULL,
/* NclDelVarAttFunc        del_var_att; */		NULL,
/* NclSetOptionFunc           set_option;  */           NULL
};

NclFormatFunctionRecPtr CcmAddFileFormat 
#if	NhlNeedProto
(void)
#else 
()
#endif
{
	
	return(&CCMRec);
}

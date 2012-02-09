#include <stdio.h>
#include "grib2.h"

extern gtemplate *getdrstemplate(int);
extern gtemplate *extdrstemplate(int,g2int *);

/*
int main(void)
{
  char mug[]={'A','B','C','D'};
  int imug[20];
  int i,num=8;
  gbits(mug,imug,0,4,0,num);
  for (i=0;i<num;i++) {
     printf(" %d %x \n",imug[i],imug[i]);
  }
}

int main(void)
{
  char mug[10];
  int imug[4]={41,42,43,44};
  int i,num=4;
  sbits(mug,imug,0,4,0,num);
  for (i=0;i<num;i++) {
     printf(" %c %x \n",mug[i],mug[i]);
  }
}

int main(void)
{
  gtemplate *t;
  int *imug;
  g2int list[100]={0,0,0,0,0,0,0,0,0,0,0,5,0,6,0,0,0,0,0};
  int i,len,need,iret,num=1;
  t=getdrstemplate(num);
  printf(" sagret1: %x %d %d %d %d %d\n",t,t->type,t->num,t->maplen,t->needext,t->extlen);
  for (i=0;i<t->maplen;i++) {
     printf(" %d ",t->map[i]);
  }
  printf(" \n");
  t=extdrstemplate(num,list);
  printf(" sagret1: %x %d %d %d %d %d\n",t,t->type,t->num,t->maplen,t->needext,t->extlen);
  for (i=0;i<t->extlen;i++) {
     printf(" %d ",t->ext[i]);
  }
  printf(" \n");
}
*/

int main(void)
{
    unsigned char mug[300],*cout;
    int iret,j;
    long iof=0;
    g2int *ids,idslen,*tmp,*igdt,igdtlen,*def,deflen,ip,ipdtlen,*ipdt;
    g2int idrtlen,*idrt,ndpts,drtnum;
    g2float crdlen,*crd;
    for (j=0;j<300;j++) {
       mug[j]=(char)0;
    }
    mug[1]=(char)0;
    mug[2]=(char)0;
    mug[3]=(char)21;
    mug[4]=(char)5;
    mug[8]=(char)254;
    mug[9]=(char)0;
    mug[10]=(char)0;
    mug[14]=(char)222;
    mug[16]=(char)66;
    mug[18]=(char)7;
    mug[22]=(char)128;
    mug[25]=(char)88;
    mug[29]=(char)111;
    mug[33]=(char)112;
    mug[37]=(char)113;
    mug[67]=(char)3;
    mug[68]=(char)3;
    mug[69]=(char)3;
    mug[70]=(char)3;
    mug[71]=(char)3;
    mug[72]=(char)3;
//    iret=g2_unpack1(mug,&iof,&ids,&idslen);
//    iret=g2_unpack2(mug,&iof,&idslen,&cout);
//    iret=g2_unpack3(mug,&iof,&ids,&igdt,&igdtlen,&def,&deflen);
//    iret=g2_unpack4(mug,&iof,&ip,&ipdt,&ipdtlen,&crd,&crdlen);
    iret=g2_unpack5(mug,&iof,&ndpts,&drtnum,&idrt,&idrtlen);
    printf(" SAGRET = %d %d %d %d\n",iret,(int)ndpts,(int)idrtlen,(int)drtnum);
//    tmp=*ids;
//    for (j=0;j<5;j++) {
//       printf(" %d ",(int)ids[j]);
//    }
//    printf("\n");
    for (j=0;j<idrtlen;j++) {
       printf(" %d ",(int)idrt[j]);
    }
//    printf("\n");
//    for (j=0;j<crdlen;j++) {
//       printf(" %d ",(int)crd[j]);
//    }
    printf("\n");
    
}

#include <stdio.h>
#include "grib2.h"

extern gtemplate *getdrstemplate(int);
extern gtemplate *extdrstemplate(int,g2int *);
extern gtemplate *getgridtemplate(int);
extern gtemplate *extgridtemplate(int,g2int *);
extern gtemplate *getpdstemplate(int);
extern gtemplate *extpdstemplate(int,g2int *);

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
*/

int main(void)
{
  gtemplate *t;
  int *imug;
  g2int list[100]={0,10,0,0,0,0,0,0,0,0,0,5,0,6,0,0,0,0,0,0,0,3,0,0,0,5,7,0,0,0,0,0,0,0,0,0,2,1};
  int i,len,need,iret,num=13;
/*  t=getdrstemplate(num); */
/*  t=getgridtemplate(num); */
  t=getpdstemplate(num);
  printf(" sagret1: %x %d %d %d %d %d\n",t,t->type,t->num,t->maplen,t->needext,t->extlen);
  for (i=0;i<t->maplen;i++) {
     printf(" %d ",t->map[i]);
  }
  printf(" \n");
/*  t=extdrstemplate(num,list); */
/*  t=extgridtemplate(num,list);*/
  t=extpdstemplate(num,list);
  printf(" sagret1: %x %d %d %d %d %d\n",t,t->type,t->num,t->maplen,t->needext,t->extlen);
  for (i=0;i<t->extlen;i++) {
     printf(" %d ",t->ext[i]);
  }
  printf(" \n");
}

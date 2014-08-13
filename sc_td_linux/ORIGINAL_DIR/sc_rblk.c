#include <string.h>
#include <stdio.h>
/* #include <stdlib.h> */
#include <sys/resource.h>

int sc_rblk_(unsigned char * block,char * orc, char * filename, 
             int * istat, int * len);

void sc_rblk(unsigned char * block,char * orc, char * filename, 
             int * istat, int * len) 
{
static FILE * ptr;
char   filen[49]; /* Note 1 less that the fortran decl */
int i, res;
static struct rlimit rlp;

*istat = 0;
rlp.rlim_cur = RLIM_INFINITY;
rlp.rlim_max = RLIM_INFINITY; 

if(*orc=='o') 
  {
  res=setrlimit(RLIMIT_FSIZE,&rlp); 
  if(res < 0){
    perror("setrlimit: RLIMIT_CORE");
    exit(-2);
  }
  
  strncpy(filen,filename,*len);
  filen[*len] = '\0';
  *istat = (int)(ptr = fopen64(filen,"r"));
  }
 if(*orc=='r') 
   {
     *istat = fread(block,49792,1,ptr);
     /*  for (i=0; i<640; i++) printf("%c",block[i]);*/
     if(*istat > 0) *istat = *istat * 49792;
     if(feof(ptr) != 0) *istat = 0;
     if(ferror(ptr) != 0) printf("Read Error - %d \n",ferror(ptr));
   }
 if(*orc=='c') fclose(ptr);
 /* printf("sc_rblk %c %d %d %d \n",*orc,*istat,*len,sizeof(block));*/
 
}

int sc_rblk_(unsigned char * block,char * orc, char * filename, 
             int * istat, int * len) 
{
sc_rblk(block, orc, filename, istat, len);
return 0;
}


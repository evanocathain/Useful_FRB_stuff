#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "functions.h"
#include <math.h>
#include <fcntl.h>
#include <sys/stat.h>

void get_string(FILE* inputfile, int* nbytes, char* string);
long long sizeof_file(char name[]);
long long nsamples(char *filename,int headersize, int nbits, int nifs, int nchans);

float* read_data(char filename[],float* series, int* nsamps, int sigproc, int ascii, double* dm, double* raj, double* decj, double* tstart, double* tsamp, double* fch1, double* foff)
{
  FILE *ifp;
  int i, n=0, check;
  /* Sigproc header stuff ... */
  char string[80], rawdatafile[80], source_name[80];
  int itmp, nbytes=0, totalbytes, headersize, datasize, expecting_rawdatafile=0, expecting_source_name=0, expecting_frequency_table=0, channel_index, nchans=1, nifs=1, nbits, telescope_id, machine_id, data_type, barycentric;
  double az_start, za_start;

  /* Read in the data */
  if (sigproc==1 && ascii==0){              // Sigproc binary format
    /* For sigproc binary format need to read header first */
    ifp=fopen(filename,"rb");
    if (ifp == NULL){
      fprintf(stderr,"Cannot open input file. Exiting ...\n");
      exit(2);
    }      
    get_string(ifp,&nbytes,string);
    if (!strings_equal(string,"HEADER_START")){
      rewind(ifp);
      fprintf(stderr,"Binary input file in non-standard format. Exiting ...\n");
      exit(1);
    }
    totalbytes=nbytes;
    /* Loop through all possible header KEYWORDS until HEADER_END is found */
    while (!feof(ifp)){ 
      get_string(ifp,&nbytes,string);
      if (strings_equal(string,"HEADER_END")) break;
      totalbytes+=nbytes;
      if (strings_equal(string,"rawdatafile")) {
	expecting_rawdatafile=1;
      }else if (strings_equal(string,"source_name")) {
	expecting_source_name=1;
      }else if (strings_equal(string,"FREQUENCY_START")) {
	expecting_frequency_table=1;
	channel_index=0;
      }else if (strings_equal(string,"FREQUENCY_END")) {
	expecting_frequency_table=0;
      }else if (strings_equal(string,"az_start")) {
	fread(&az_start,sizeof(az_start),1,ifp);
	totalbytes+=sizeof(az_start);
      }else if (strings_equal(string,"za_start")) {
	fread(&za_start,sizeof(za_start),1,ifp);
	totalbytes+=sizeof(za_start);
      }else if (strings_equal(string,"src_raj")) {
	fread(&raj,sizeof(raj),1,ifp);
	totalbytes+=sizeof(raj);
      }else if (strings_equal(string,"src_dej")) {
	fread(&decj,sizeof(decj),1,ifp);
	totalbytes+=sizeof(decj);
      }else if (strings_equal(string,"tstart")) {
	fread(&tstart,sizeof(tstart),1,ifp);
	totalbytes+=sizeof(tstart);
      }else if (strings_equal(string,"tsamp")) {
	fread(&tsamp,sizeof(tsamp),1,ifp);
	totalbytes+=sizeof(tsamp);
	//}else if (strings_equal(string,"period")) {
	//fread(&period,sizeof(period),1,ifp);
	//totalbytes+=sizeof(period);
      }else if (strings_equal(string,"fch1")) {
	fread(&fch1,sizeof(fch1),1,ifp);
	totalbytes+=sizeof(fch1);
	//}else if (strings_equal(string,"fchannel")) {
	//fread(&frequency_table[channel_index++],sizeof(double),1,ifp);
	//totalbytes+=sizeof(double);
	//fch1=foff=0.0; // set to 0.0 to signify that a table is in use 
	}else if (strings_equal(string,"foff")) {
    fread(&foff,sizeof(foff),1,ifp);
	totalbytes+=sizeof(foff);
      }else if (strings_equal(string,"nchans")) {
	fread(&nchans,sizeof(nchans),1,ifp);
	totalbytes+=sizeof(nchans);
      }else if (strings_equal(string,"telescope_id")) {
	fread(&telescope_id,sizeof(telescope_id),1,ifp);
	totalbytes+=sizeof(telescope_id);
      }else if (strings_equal(string,"machine_id")) {
	fread(&machine_id,sizeof(machine_id),1,ifp);
	totalbytes+=sizeof(machine_id);
      }else if (strings_equal(string,"data_type")) {
	fread(&data_type,sizeof(data_type),1,ifp);
	totalbytes+=sizeof(data_type);
	//}else if (strings_equal(string,"ibeam")) {
	//fread(&ibeam,sizeof(ibeam),1,ifp);
	//totalbytes+=sizeof(ibeam);
	//}else if (strings_equal(string,"nbeams")) {
	//fread(&nbeams,sizeof(nbeams),1,ifp);
	//totalbytes+=sizeof(nbeams);
      }else if (strings_equal(string,"nbits")) {
	fread(&nbits,sizeof(nbits),1,ifp);
	totalbytes+=sizeof(nbits);
      }else if (strings_equal(string,"barycentric")) {
	fread(&barycentric,sizeof(barycentric),1,ifp);
	totalbytes+=sizeof(barycentric);
	//}else if (strings_equal(string,"pulsarcentric")) {
	//fread(&pulsarcentric,sizeof(pulsarcentric),1,ifp);
	//totalbytes+=sizeof(pulsarcentric);
      }else if (strings_equal(string,"nbins")) {
	fread(&n,sizeof(n),1,ifp);
	//fprintf(stdout,"header: %s %d\n", string, n);
	totalbytes+=sizeof(n);
	//}else if (strings_equal(string,"nsamples")) {
	//fread(&itmp,sizeof(itmp),1,ifp);
	//totalbytes+=sizeof(itmp);
      }else if (strings_equal(string,"nifs")) {
	fread(&nifs,sizeof(nifs),1,ifp);
	totalbytes+=sizeof(nifs);
	//}else if (strings_equal(string,"npuls")) {
	//fread(&npuls,sizeof(npuls),1,ifp);
	//totalbytes+=sizeof(npuls);
      }else if (strings_equal(string,"refdm")) {
	fread(dm,sizeof(*dm),1,ifp);
	//fprintf(stdout,"header: %s %.1f\n", string, *dm);
	totalbytes+=sizeof(*dm);
      }else if (expecting_rawdatafile) {
	strcpy(rawdatafile,string);
	expecting_rawdatafile=0;
      }else if (expecting_source_name) {
	strcpy(source_name,string);
	expecting_source_name=0;
      }else {
	//	fprintf(stderr,"read_data: unknown header parameter: %s\n",string);
	//	fprintf(stderr,"ERROR\n");
	//	exit(1); 
      }
    }
    totalbytes+=nbytes;
    headersize=totalbytes;
    /* Now determine size of the actual data */
    datasize=sizeof_file(filename)-headersize;
    n=nsamples(filename,headersize,nbits,nifs,nchans);
    if (*nsamps==0) *nsamps=n;                        // if nsamps has not been set on command line, set it to entire filesize
    fprintf(stdout,"Filename:\t\t%s\n",filename);
    if (n<=0) {
      printf("nsamps = %d !! Exiting ...\n",n);
      exit(1);
    }

    /* NOW START TO READ IN THE DATA! */
    fprintf(stdout,"Reading in data: \tnsamps = %d\n",n);
    series=(float*)malloc(n*sizeof(float));          // CREATE ARRAY
    /* Silly sigproc way to read in the file, i.e. calling fread nsamps times */
    //i=0;
    //while(!feof(ifp)){      
    //  fread(&series[i],sizeof(float),1,ifp);
    //  i++;
    //}
    //fprintf(stdout,"%d samples read in\n",(i-1));*/
    i = fread(series,sizeof(float),*nsamps,ifp);           // fread returns number of items successfully read
    fprintf(stdout,"%d samples read in\n",i);
  }

  else if (ascii==1 && sigproc==0){                 // Ascii format
    n=*nsamps;
    fprintf(stdout,"Filename:\t\t%s\n",filename);
    fprintf(stdout,"Reading in data: \tnsamps = %d\n",n);
    series=(float*)malloc(n*sizeof(float));          /* CREATE ARRAY */
    if (n<=0) {
      printf("nsamps = %d !! Exiting ...\n",n);
      exit(1);
    }
    ifp=fopen(filename,"r");
    if (ifp == NULL){
      fprintf(stderr,"Cannot open input file. Exiting ...\n");
      exit(2);
    }
    for(i=0; i<n; i++){
      fscanf(ifp,"%f",&series[i]);
    }
  }
  else{                                     // This should not occur!
    fprintf(stderr,"Error reading in files ... Exiting\n");
    exit(1);
  }

  /* Close Input file */
  check=fclose(ifp);

  return(series);
}

void get_string(FILE* inputfile, int* nbytes, char string[])
{
  int nchar;
  strcpy(string,"ERROR");
  fread(&nchar, sizeof(int), 1, inputfile);
  if (feof(inputfile)) exit(0);
  if (nchar>80 || nchar<1) return; // must be a mistake, or dud keyword
  *nbytes=sizeof(int);
  fread(string, nchar, 1, inputfile);
  string[nchar]='\0';
  *nbytes+=nchar;
}

long long sizeof_file(char name[]) 
{
  struct stat stbuf;
  
  if(stat(name,&stbuf) == -1)
    {
      fprintf(stderr, "f_siz: can't access %s\n",name);
      exit(0);
    }
  return(stbuf.st_size);
}

long long nsamples(char *filename,int headersize, int nbits, int nifs, int nchans) 
{
  long long datasize,numsamps;
  datasize=sizeof_file(filename)-headersize;
  numsamps=(long long) (long double) (datasize)/ (((long double) nbits) / 8.0) 
                 /(long double) nifs/(long double) nchans;
  return(numsamps);
}

#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<math.h>

#define min(a,b) ((a<b)?a:b)

void print_usage(void);
int strings_equal (char *string1, char *string2);

int main(int argc, char **argv)
{
  double tsamp, tint, tdm_olddm, tdm_newdm, tDdm, tscatt;
  double dmtol=1.25, dmstep=0.001;
  double fch1, foff, f_top, f_bot, f_cent, bw;
  double dm=0.0, dmstart=0.0, dmold=0.0, maxdm;
  double width_olddm, width_newdm, width_Ddm;
  double response=0.0;
  int nchans, lina=0;
  int i=0,j=0,nsteps=5000,oldsteps;

  // Set Defaults

  // HTRU-S
  tint   = 0.000040;   // in sec
  tscatt = 0.0;        // in sec
  tsamp  = 0.000064;   // in sec
  fch1 = 1581.8046875; // in MHz
  foff = -0.390625;    // in MHz
  nchans = 1024;  

  if ( argc == 1 ) {
    print_usage();
  }

  while (i<argc){
    if (strings_equal(argv[i],"-o")){
      i++;
    }else if (strings_equal(argv[i],"-fch1")){
      i++;
      fch1=atof(argv[i]);
    }else if (strings_equal(argv[i],"-foff")){
      i++;
      foff=atof(argv[i]);
    }else if (strings_equal(argv[i],"-nchans")){
      i++;
      nchans=atoi(argv[i]);
    }else if (strings_equal(argv[i],"-tsamp")){
      i++;
      tsamp=atof(argv[i]);
    }else if (strings_equal(argv[i],"-dmstart")){
      i++;
      dmstart=atof(argv[i]);
    }else if (strings_equal(argv[i],"-dmtol")){
      i++;
      dmtol=atof(argv[i]);
    }else if (strings_equal(argv[i],"-lina")){
      i++;
      lina=1;
    }else if (strings_equal(argv[i],"-maxdm")){
      i++;
      maxdm=atof(argv[i]);
    }else if (strings_equal(argv[i],"-h")){
      print_usage();
    }else if (strings_equal(argv[i],"--help")){
      print_usage();
    }
    i++;
  }

  // Set up some values common to both methods below
  dm = dmold = dmstart;
  if (fch1 < 200.0) dmstep=0.0001;
  f_top  = fch1;
  f_bot  = fch1+(nchans-1)*foff;
  f_cent = fch1+(nchans-1)*0.5*foff;
  oldsteps = 0;

/* 			*
*	Method 1	*
*			*
Calculate the DM trial steps to use for the given observing
specifications and the requested DM tolerance using the 
definition of the DM tolerance such that the maximum S/N 
drop _between_ DM trials is 1/sqrt(dmtol).

Note that this currently uses the centre frequency channel to
determine the DM smearing in a channel 
*/
  if (lina == 0){
    for (i = 0; i < nsteps; i++)
    {
      for (j = -oldsteps+1; j < nsteps; j++)
      {
        dm = dmold+j*dmstep;
        tdm_olddm   = 4149.0*dmold*(pow(f_cent+foff*0.5,-2.0) - pow(f_cent-foff*0.5,-2.0));
        tdm_newdm   = 4149.0*dm*(pow(f_cent+foff*0.5,-2.0) - pow(f_cent-foff*0.5,-2.0));
        tDdm        = 4149.0*(dm-dmold)*(pow(f_bot,-2.0) - pow(f_top,-2.0));
        width_olddm = pow((pow(tsamp,2.0)+pow(tint,2.0)+pow(tdm_olddm,2.0)+pow(tscatt,2.0)),0.5);
        width_newdm = pow((pow(tsamp,2.0)+pow(tint,2.0)+pow(tdm_newdm,2.0)+pow(tDdm,2.0)+pow(tscatt,2.0)),0.5);
        response    = sqrt(width_olddm/width_newdm);
        printf("dm %lf tdm %lf tDdm %lf width_newdm %lf response %lf\n",dm,tdm_newdm,tDdm,width_newdm,response);
        if (width_newdm/width_olddm > dmtol){
          break;
        }   
      }	
      dm = dmold+2*j*dmstep;
      if (dm > dmstart + 5.0) break;
      dmold = dm;
      oldsteps = j;
    }
  } else if (lina == 1){
/* 			*
*	Method 2	*
*			*
Calculate the DM trial steps to use for the given observing
specifications and the requested DM tolerance using the algorithm 
used by dedisp, e.g. from here: 
https://github.com/ajameson/dedisp/blob/0f763a3bd1c726d9363c63cae855c844e9549d88/src/kernels.cuh#L64-L88

This uses the algorithm from Lina Levin's thesis (page 31+32), 
as originally developed for dedisperse_all
*/
    for (i = 0; i < nsteps; i++)
    {
      for (j = -oldsteps+1; j < nsteps; j++)
      {
        dm = dmold+j*dmstep;
        tdm_olddm   = 8.3*pow(10.0,-6.0)*(-foff)*dmold*pow(f_cent*0.001,-3.0);
        tdm_newdm   = 8.3*pow(10.0,-6.0)*(-foff)*dm*pow(f_cent*0.001,-3.0);
        tDdm        = 8.3*pow(10.0,-6.0)*((float)nchans/4.0)*(-foff)*(dm-dmold)*pow(f_cent*0.001,-3.0);
        width_olddm = pow((pow(tsamp,2.0)+pow(tint,2.0)+pow(tdm_olddm,2.0)+pow(tscatt,2.0)),0.5);
        width_newdm = pow((pow(tsamp,2.0)+pow(tint,2.0)+pow(tdm_newdm,2.0)+pow(tDdm,2.0)+pow(tscatt,2.0)),0.5);
        response    = sqrt(width_olddm/width_newdm);
        printf("dm %lf tdm %lf tDdm %lf width_newdm %lf response %lf\n",dm,tdm_newdm,tDdm,width_newdm,response);
        if (width_newdm/width_olddm > dmtol){
          break;
        }   
      }	
//      dm = dmold+2*j*dmstep;
      if (dm > dmstart + 5.0) break;
      dmold = dm;
      oldsteps = j;
    }
  }
  return(0);
}

void print_usage(void)
{
//  fprintf(stdout,"\ndestroy - 'seeks' out transient signals in a noisey time series\n\n");
  fprintf(stdout,"usage: dedisp_steps_new -{options}\n\n");
  fprintf(stdout,"options:\n\n");
  fprintf(stdout,"-fch1 val       - set centre frequency of top channel in MHz (def=1581.8046875)\n");
  fprintf(stdout,"-foff val       - set channel bandwidth, offset from fch1, in MHz (def=-0.390625)\n");
  fprintf(stdout,"-nchans val     - set number of frequency channels (def=1024)\n");
  fprintf(stdout,"-tsamp val      - set the sampling time in seconds (def=0.000064)\n");
  fprintf(stdout,"-tint val       - set intrinsic pulse width in seconds (def=0.000040)\n");
  fprintf(stdout,"-tscatt val     - set scattering timescale in seconds (def=0.0)\n");
  fprintf(stdout,"-dmstart val    - set the first DM trial, in pc/cc (def=0.0)\n");
  fprintf(stdout,"-dmtol val      - set the DM trial step tolerance (def=1.25)\n");
  fprintf(stdout,"-lina val       - calculate DM trial steps using the dedisp algorithm (def=don't)\n");  fprintf(stdout,"-dmtol val      - set the DM trial step tolerance (def=1.25)\n");  fprintf(stdout,"-dmtol val      - set the DM trial step tolerance (def=1.25)\n");
  fprintf(stdout,"\n-h,--help     - prints this usage message\n");
  fprintf(stdout,"\n");

  exit(0);
}

int strings_equal (char *string1, char *string2)
{
  if (!strcmp(string1,string2)) {
    return 1;
  } else {
    return 0;
  }
}

#include<stdio.h>
#include<stdlib.h>
#include<math.h>

#define min(a,b) ((a<b)?a:b)

int main(int argc, char **argv)
{
  double weff, tscatt, tsamp, tint, tdm, tDdm;
  double tol;
  double DM, foff, fcent;
  int i, nchans=1024;
  double alpha=1.0/(16.0 + nchans*nchans), beta;
  double dm, dm_max;
  int just_dms=1;

  if ( argc == 1 ) {
    printf("Usage: dedisp_steps <dm_tol> <dm_max> <scallop>\n");
    exit(1);
  }
  tol = atof(argv[1]);
  dm_max = atof(argv[2]);
  if ( argc == 4 ){ 
    just_dms=0;
  }

  // Set Defaults
  // These are the parameters of HTRU-S
  tint = 40.0;       // in microsec
  tscatt = 1000.0;   // in microsec
  tscatt = 0.0;
  tsamp = 64.0;      // in microsec
  fcent = 1.382;     // in GHz
  foff = 0.390625;   // in MHz
  
  beta=tscatt*tscatt + tsamp*tsamp + tint*tint;

  /*  
   *  Lina's Algorithm
   *
   *  Lina's thesis can be found here: 
   *
   *  https://researchbank.swinburne.edu.au/items/7cc421b6-b7f3-45e2-8832-36f0155ba2b1/1/?search=%2Fsearching.do&index=0&available=50
   *	
   *  tdm = 8.3*foff*DM/(fcent*fcent*fcent);
   *  tDdm = tdm*
   */

  dm = 0.0;
  i = 0;
  int j=0;
  double dmold, dmnew, Ddm, dm_to_use;
  int nsteps=50;
  //printf("%d %lf\n", i, dm);
  double dm_cm=0.0, dm_lina=0.0;
  while (dm < dm_max){

    // CM Way
    /*    if (dm_cm > 0.0){
      tdm = 8.3*foff*dm_cm/(fcent*fcent*fcent);
      weff = sqrt( beta + tdm*tdm);
      dm_cm += dm_cm*(sqrt(tol*tol-1.0)*weff/tdm);
    }else{
      tdm = sqrt(tol*tol-1.0)*sqrt(beta);
      dm_cm = pow(fcent,3.0)/(8.3*foff);
      printf("hello %.12lf\n", dm_cm);
      }*/

    // Lina Way
    if (dm_lina > 0.0){
      tdm = 8.3*foff*dm_lina/(fcent*fcent*fcent);
      weff = sqrt( beta + tdm*tdm);
      dm_lina += dm_lina*((sqrt(tol*tol - 1.0))*(4.0/nchans)*(weff/tdm));
    }else{
      tdm = sqrt((tol*tol - 1)*(beta));
      dm_lina = (4.0/nchans)*(pow(fcent,3.0)/(8.3*foff))*tdm;
      }

    // Lina Thesis Way
    dmold = dm;
    dm = dm_lina;
    //dm = pow(nchans,2.0)*alpha*dm + sqrt( 16*alpha*(pow(tol,2.0) - pow(nchans,2.0)*alpha)*pow(dm,2.0) + 16*alpha*beta*(pow(tol,2.0) - 1)*pow((pow(fcent,3.0)/(8.3*foff)),2.0) );

    // Print out DM vals
    if (just_dms == 1){
      printf("%d %lf\n", i, dm);
    }
    i++;

    if (just_dms == 0){
      for (j=-nsteps; j<2*nsteps; j++){
	dmnew = dmold + ((double)j/(double)nsteps)*(dm - dmold);
	dm_to_use = min((dmnew-dmold), (dm-dmnew)) + dmold; // which DM trial to use for the tdm calc
	tdm = 8.3*foff*dm_to_use/(pow(fcent,3.0));
	Ddm = min((dmnew-dmold), (dm-dmnew)); // which DM trial to use for the tDdm calc
	tDdm = (8.3*foff*Ddm*nchans)/(4.0*pow(fcent,3.0));
	weff = sqrt( beta + tdm*tdm + tDdm*tDdm );
	printf("%lf %lf %lf %lf\n", weff, dmnew, tdm, tDdm);
      }
    }
    
  }

  return(0);
}


// CODE SNIPPET FROM DEDISP
/*void generate_dm_list(std::vector<dedisp_float>& dm_table,
		      dedisp_float dm_start, dedisp_float dm_end,
		      double dt, double ti, double f0, double df,
		      dedisp_size nchans, double tol)
{
  // Note: This algorithm originates from Lina Levin
  // Note: Computation done in double precision to match MB's code

  dt *= 1e6;
  double f    = (f0 + ((nchans/2) - 0.5) * df) * 1e-3;
  double tol2 = tol*tol;
  double a    = 8.3 * df / (f*f*f);
  double a2   = a*a;
  double b2   = a2 * (double)(nchans*nchans / 16.0);
  double c    = (dt*dt + ti*ti) * (tol2 - 1.0);

  dm_table.push_back(dm_start);
  while( dm_table.back() < dm_end ) {
    double prev     = dm_table.back();
    double prev2    = prev*prev;
    double k        = c + tol2*a2*prev2;
    double dm = ((b2*prev + sqrt(-a2*b2*prev2 + (a2+b2)*k)) / (a2+b2));
    dm_table.push_back(dm);
  }
}


}*/


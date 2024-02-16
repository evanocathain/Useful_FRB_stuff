int get_args(int argc, char **argv, char filename[], int* ascii, int* sigproc, int *nsamps, float* spthresh, int* nsmax, int* subzero, double* dm, int* boxcar_option);
int strings_equal(char *string1, char *string2);
float* read_data(char filename[],float* series, int* nsamps, int sigproc, int ascii, double* dm, double* raj, double* decj, double* tstart, double* tsamp, double* fch1, double* foff);
void sp_search(float* series, float thresh, int nsmax, int nsamps, double dm, double tsamp, int boxcar_option);

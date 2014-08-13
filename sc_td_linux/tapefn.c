#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <assert.h>
#include <string.h>
#include <sys/mtio.h>
#include <sys/types.h>

/*  C routines to replace fortran topen routines. 
    The fortran routines are not supported by linux g77.
    RNM April 7, 2000 */

int topenc_(int *tfd, char *tapedev, int clen){

  char tdev[24]="/dev/nst1";

  //  strncpy(tdev,tapedev,clen);
  //  strcat(tdev,'\0');
  *tfd = open(tdev, O_RDONLY);
  if (*tfd < 0) return -1;

  return 0;
}
 
int tclosec_(int *tfd){

  close(*tfd);
  return 0;
}

int treadc_(int *tfd, char *buff, int *len){

  int retval = read(*tfd, buff, *len);

  return retval;
}

int tstatec_(int *tfd, int *nfil, int *nrec, int *eoff, int *eotf){

  int retval;
  struct mtget tapestat;

  retval = ioctl(*tfd, MTIOCGET, &tapestat);
  if(retval < 0) return -1;
  *nfil = tapestat.mt_fileno+1;
  *nrec = tapestat.mt_blkno+1;

  /* Don't know about eoff or eotf */
  *eoff = 0;
  *eotf = 0;

  return retval;
}

int tskipfc_(int *tfd, int *nsk){

  int retval;
  struct mtop tapeop;

  tapeop.mt_op = MTFSF;
  tapeop.mt_count = *nsk;

  retval = ioctl(*tfd, MTIOCTOP, &tapeop);

  return retval;
}

int tskiprc_(int *tfd, int *nsk){

  int retval;
  struct mtop  tapeop;

  tapeop.mt_op = MTFSR;
  tapeop.mt_count = *nsk;

  retval = ioctl(*tfd, MTIOCTOP, &tapeop);

  return retval;
}

int trewinc_(int *tfd){

  int retval;
  struct mtop  tapeop;

  tapeop.mt_op = MTREW;
  tapeop.mt_count = 1;

  retval = ioctl(*tfd, MTIOCTOP, &tapeop);

  return retval;
}










#include <math.h>
#include <stdio.h>
#include "endian.h"
/* -mkeith: I commented out two printf statements that refered to an unasigned variable 'worddest'. This had caused the program to segfault. */
/*

      program test_linear

      implicit none

c     AGL,JFB 970731 Simulate how we would linearise the frequency spcae of the
c     pmsurv data prior to dedisping using tree et al

      integer i,j,m,n,nn
      real    f,df,last

      m  = 96     ! No of input chans
      n  = 128    ! No of output chans
      f  = 1516.5 ! Frequency of channel 1
      df = -3     ! Channel bandwidth

      do i = 1,m ! loop over input chans
        j = nint((n-1)*(((f+(i-1)*df)**-2 - f**-2)/
     &                  ((f+(m-1)*df)**-2 - f**-2))) + 1
c        write(*,*)i,j
      enddo

c     Invert

      do j = 1,n ! loop over output chans
        if(n.gt.1)then
          nn = n - 1 
          last = ((((f+(m-1)*df)**-2 - f**-2)*(j-1)/(nn-1) + f**-2)**-0.5
     &       - f)/df
	endif
        i = nint(((((f+(m-1)*df)**-2 - f**-2)*(j-1)/(n-1) + f**-2)**-0.5
     &       - f)/df) + 1
        write(*,*)i,j,i-j
      enddo 

      end

This produced to following (annotated) output:

     O
I    U
N    T   S
C    C   H
H    H   I
A    A   F
N    N   T
 
01   1   0 b0     
02   2   0 b0    
03   3   0 b0    
04   4   0 b0    
05   5   0 b0    
06   6   0 b0    
07   7   0 b0    
08   8   0 b0    
09   9   0 b0    
10  10   0 b0    
11  11   0 b0    
12  12   0 b0    
13  13   0 b0    
14  14   0 b0    
15  15   0 b0    
16  16   0 b0    
17  17   0 b1    
18  18   0 b1    
19  19   0 b1    
    20     b1    
20  21  -1 b1    
21  22  -1 b1    
22  23  -1 b1    
23  24  -1 b1    
24  25  -1 b1    
25  26  -1 b1    
26  27  -1 b1    
27  28  -1 b1    
    29     b1    
28  30  -2 b1    
29  31  -2 b1    
30  32  -2 b1    
31  33  -2 m1    
32  34  -2 m1    
33  35  -2 b2    
    36     b2    
34  37  -3 b2    
35  38  -3 b2    
36  39  -3 b2    
37  40  -3 b2    
38  41  -3 b2    
    42     b2    
39  43  -4 b2    
40  44  -4 b2    
41  45  -4 b2    
42  46  -4 b2    
    47     b2    
43  48  -5 b2    
44  49  -5 m2    
45  50  -5 m2    
46  51  -5 m2    
    52     m2    
47  53  -6 m2    
48  54  -6 m2    
49  55  -6 b3    
    56     b3    
50  57  -7 b3    
51  58  -7 b3    
52  59  -7 b3    
    60     b3    
53  61  -8 b3    
54  62  -8 b3    
55  63  -8 b3    
    64     b3    
56  65  -9 m3    
57  66  -9 m3    
58  67  -9 m3    
    68     m3    
59  69 -10 m3    
60  70 -10 m3    
    71     m3    
61  72 -11 m3    
62  73 -11 m3    
    74     m3    
63  75 -12 m3    
64  76 -12 m3    
65  77 -12 b4    
    78     b4    
66  79 -13 b4    
67  80 -13 b4    
    81     m4    
68  82 -14 m4    
69  83 -14 m4    
    84     m4    
70  85 -15 m4    
71  86 -15 m4    
    87     m4    
72  88 -16 m4    
73  89 -16 m4    
    90     m4    
74  91 -17 m4    
    92     m4    
75  93 -18 m4    
76  94 -18 m4    
    95     m4    
77  96 -19 m4    
78  97 -19 t4    
    98     t4    
79  99 -20 t4    
   100     t4    
80 101 -21 m5    
81 102 -21 m5    
   103     m5    
82 104 -22 m5    
83 105 -22 m5    
   106     m5    
84 107 -23 m5    
   108     m5    
85 109 -24 m5    
86 110 -24 m5    
   111     m5    
87 112 -25 m5    
   113     t5    
88 114 -26 t5    
   115     t5    
89 116 -27 t5    
90 117 -27 t5    
   118     t5    
91 119 -28 t5    
   120     t5    
92 121 -29 t5    
   122     t5    
93 123 -30 t5    
94 124 -30 t5    
   125     t5    
95 126 -31 t5    
   127     t5    
96 128 -32 t5    
 */ 

/* Output channels - note 0 channels are lost which mean S/N is not reduced */

/*Wrapper */
int ubd128_(unsigned short int * iblk, unsigned short int * oblk, int * nsampp);

void ubd128(unsigned short int * iblk, unsigned short int * oblk, int * nsampp)
{
  /*3 tables */
int bottab[6][16] = {
{ 1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16},
{17, 18, 19, 21, 22, 23, 24, 25, 26, 27, 28, 30, 31, 32,  0,  0}, 
{35, 37, 38, 39, 40, 41, 43, 44, 45, 46, 48,  0,  0,  0,  0,  0},
{55, 57, 58, 59, 61, 62, 63,  0,  0,  0,  0,  0,  0,  0,  0,  0},
{77, 79, 80,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0},
{ 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0}};

int midtab[6][16] = {
{  0,   0,   0,   0,   0,   0,   0,  0,  0,  0,  0,  0,  0,  0,  0,  0},
{  0,   0,   0,   0,   0,   0,   0,  0,  0,  0,  0,  0,  0,  0, 33, 34},
{  0,   0,   0,   0,   0,   0,   0,  0,  0,  0,  0, 49, 50, 51, 53, 54},
{  0,   0,   0,   0,   0,   0,   0, 65, 66, 67, 69, 70, 72, 73, 75, 76},
{  0,   0,   0,  82,  83,  85,  86, 88, 89, 91, 93, 94, 96,  0,  0,  0},
{102, 104, 105, 107, 109, 110, 112,  0,  0,  0,  0,  0,  0,  0,  0,  0}};

int toptab[6][16] = { 
{ 0,  0,  0,  0,  0,  0,  0,   0,   0,   0,   0,   0,   0,   0,   0,   0}, 
{ 0,  0,  0,  0,  0,  0,  0,   0,   0,   0,   0,   0,   0,   0,   0,   0}, 
{ 0,  0,  0,  0,  0,  0,  0,   0,   0,   0,   0,   0,   0,   0,   0,   0}, 
{ 0,  0,  0,  0,  0,  0,  0,   0,   0,   0,   0,   0,   0,   0,   0,   0}, 
{ 0,  0,  0,  0,  0,  0,  0,   0,   0,   0,   0,   0,   0,  97,  99, 101}, 
{ 0,  0,  0,  0,  0,  0,  0, 114, 116, 117, 119, 121, 123, 124, 126, 128}};

  /* 6 tables to do both upper and lower duplicates
int bottab[6][16] = {
{ 1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16},
{17, 18, 19, 21, 22, 23, 24, 25, 26, 27, 28, 29, 31, 32,  0,  0},
{35, 36, 38, 39, 40, 41, 42, 44, 45, 46, 47,  0,  0,  0,  0,  0},
{55, 56, 58, 59, 60, 62, 63,  0,  0,  0,  0,  0,  0,  0,  0,  0},
{77, 79, 80,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0},
{ 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0}};

int midtab[6][16] = {
{  0,   0,   0,   0,   0,   0,   0,  0,  0,  0,  0,  0,  0,  0,  0,  0},
{  0,   0,   0,   0,   0,   0,   0,  0,  0,  0,  0,  0,  0,  0, 33, 34},
{  0,   0,   0,   0,   0,   0,   0,  0,  0,  0,  0, 49, 50, 51, 53, 54},
{  0,   0,   0,   0,   0,   0,   0, 65, 66, 67, 69, 70, 71, 73, 74, 76},
{  0,   0,   0,  82,  83,  85,  86, 88, 89, 91, 92, 94, 95,  0,  0,  0},
{102, 103, 105, 107, 108, 110, 112,  0,  0,  0,  0,  0,  0,  0,  0,  0}};

int toptab[6][16] = {
{ 0,  0,  0,  0,  0,  0,  0,   0,   0,   0,   0,   0,   0,   0,   0,   0},
{ 0,  0,  0,  0,  0,  0,  0,   0,   0,   0,   0,   0,   0,   0,   0,   0},
{ 0,  0,  0,  0,  0,  0,  0,   0,   0,   0,   0,   0,   0,   0,   0,   0},
{ 0,  0,  0,  0,  0,  0,  0,   0,   0,   0,   0,   0,   0,   0,   0,   0},
{ 0,  0,  0,  0,  0,  0,  0,   0,   0,   0,   0,   0,   0,  97,  99, 100},
{ 0,  0,  0,  0,  0,  0,  0, 114, 115, 117, 119, 120, 122, 124, 126, 128}};

int bottab[6][16] = {
{ 1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16},
{17, 18, 20, 21, 22, 23, 24, 25, 26, 27, 28, 30, 31, 32,  0,  0},
{35, 37, 38, 39, 40, 41, 43, 44, 45, 46, 48,  0,  0,  0,  0,  0},
{55, 57, 58, 59, 61, 62, 64,  0,  0,  0,  0,  0,  0,  0,  0,  0},
{78, 79,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0},
{ 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0}};

int midtab[6][16] = {
{  0,   0,   0,   0,   0,   0,   0,  0,  0,  0,  0,  0,  0,  0,  0,  0},
{  0,   0,   0,   0,   0,   0,   0,  0,  0,  0,  0,  0,  0,  0, 33, 34},
{  0,   0,   0,   0,   0,   0,   0,  0,  0,  0,  0, 49, 50, 52, 53, 54},
{  0,   0,   0,   0,   0,   0,   0, 65, 66, 68, 69, 70, 72, 73, 75, 76},
{  0,   0,  81,  82,  84,  85,  87, 88, 90, 91, 93, 94, 96,  0,  0,  0},
{102, 104, 106, 107, 109, 111,   0,  0,  0,  0,  0,  0,  0,  0,  0,  0}};

int toptab[6][16] = {
{ 0,  0,  0,  0,  0,  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0},
{ 0,  0,  0,  0,  0,  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0},
{ 0,  0,  0,  0,  0,  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0},
{ 0,  0,  0,  0,  0,  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0},
{ 0,  0,  0,  0,  0,  0,   0,   0,   0,   0,   0,   0,   0,  98,  99, 101},
{ 0,  0,  0,  0,  0,  0, 113, 114, 116, 118, 119, 121, 123, 125, 127, 128}};
*/
/* Duplicated input channels - 7 of them */

int bottabdup[6][16] = {
{ 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0},
{ 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0},
{ 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0},
{ 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0},
{ 0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 70,  0,  0,  0,  0, 76},
{ 0,  0,  0, 81,  0,  0,  0, 86,  0,  0,  0, 91,  0,  0, 95,  0}};
							       
int toptabdup[6][16] = {				       
{ 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0},
{ 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0},
{ 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0},
{ 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0},
{ 0,  0,  0, 62,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0},
{ 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0}};

/* Tables for null test, ie no bending, just check the algorithm structure 
int bottab[6][16] = {
{ 1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16},
{17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30 ,31, 32},
{33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48},
{49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64},
{65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80},
{81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96}};

int toptab[6][16] = {
{ 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0},
{ 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0},
{ 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0},
{ 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0},
{ 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0},
{ 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0}};

*/
/*
int bottab[6][16] = {
{ 1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12,  0,  0,  0,  0},
{17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28,  0,  0,  0,  0},
{33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44,  0,  0,  0,  0},
{49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,  0,  0,  0,  0},
{65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76,  0,  0,  0,  0},
{81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92,  0,  0,  0,  0}};

int toptab[6][16] = {
{ 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 13, 14, 15, 16},
{ 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 29, 30 ,31, 32},
{ 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 45, 46, 47, 48},
{ 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 61, 62, 63, 64},
{ 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 77, 78, 79, 80},
{ 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 93, 94, 95, 96}};
*/
/*
int toptab[6][16] = {
{  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0},
{ 13, 14, 15, 16,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0},
{ 29, 30 ,31, 32,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0},
{ 45, 46, 47, 48,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0},
{ 61, 62, 63, 64,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0},
{ 77, 78, 79, 80,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0}};

int bottab[6][16] = {
{  0,  0,  0,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12},
{  0,  0,  0,  0, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28},
{  0,  0,  0,  0, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44},
{  0,  0,  0,  0, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60},
{  0,  0,  0,  0, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76},
{  0,  0,  0,  0, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92}};
*/

/* Table variables */
static unsigned short int ttab[6][65536]; /* */
static unsigned short int mtab[6][65536]; /* */
static unsigned short int btab[6][65536]; /* */

unsigned short int bit1,bit2,temp,mask;
short int ibit,nshift;
unsigned int iword,iworddest;
unsigned short val;

static int first = 1;
int i,k,j,nsamp, shiftofs;

nsamp = *nsampp;

#ifdef LITTLE_ENDIAN
shiftofs = 0;
#else
shiftofs = 8;      /* shitftofs --> shiftofs : bug fixed  AP 28/07/99 */
#endif

if(first)
{
for (iword=0; iword<6; iword++)
  {
      for (j=0; j<65536; j++)    
      {ttab[iword][j] = 0;}
  for (ibit=0; ibit<16; ibit++) /* loop over input channels */
    {
    nshift = toptab[iword][ibit];
    /* printf("toptab %d %d %d \n",iword,ibit,nshift);  */
    if(nshift != 0) 
      {
      nshift = (int)fmod(((double)(nshift-1+shiftofs)),16.0);
      bit1 = 1;
      bit1 = bit1<<(nshift);  /* bit in output word */
      nshift = (int)fmod(((double)(ibit+shiftofs)),16.0);
      bit2 = 1;
      bit2 = bit2<<nshift;    /* bit in input word */
      /*printf("toptab %d %d %d %d \n",ibit,nshift,bit2,bit1);*/
      for (j=0; j<65536; j++)
        {
        if((bit2 & j) != 0) ttab[iword][j] = ttab[iword][j] + bit1;
	}
      }
    } 
  }

for (iword=0; iword<6; iword++)
  {
  for (j=0; j<65536; j++)    
    {mtab[iword][j] = 0;}
  for (ibit=0; ibit<16; ibit++)
    {
    nshift = midtab[iword][ibit];
    /*printf("midtab %d %d %d \n",iword,ibit,nshift); */
    if(nshift != 0) 
      {
      nshift = (int)fmod(((double)(nshift-1+shiftofs)),16.0);
      bit1 = 1;
      bit1 = bit1<<(nshift);   /* bit in output word */
      nshift = (int)fmod(((double)(ibit+shiftofs)),16.0);
      bit2 = 1;
      bit2 = bit2<<nshift;     /* bit in input word */
      /*printf("midtab %d %d %d %d \n",ibit,nshift,bit2,bit1);*/
      for (j=0; j<65536; j++)
        {
        if((bit2 & j) != 0) mtab[iword][j] = mtab[iword][j] + bit1;
	}
      }
    /*if(mtab[iworddest][j] !=0) printf("btab %d %d %4.4x\n",iword,j,mtab[iworddest][j]);*/
    } 
  }

for (iword=0; iword<6; iword++)
  {
  for (j=0; j<65536; j++)    
    {btab[iword][j] = 0;}
  for (ibit=0; ibit<16; ibit++)
    {
    nshift = bottab[iword][ibit];
    /*printf("bottab %d %d %d \n",iword,ibit,nshift); */
    if(nshift != 0) 
      {
      nshift = (int)fmod(((double)(nshift-1+shiftofs)),16.0);
      bit1 = 1;
      bit1 = bit1<<(nshift);   /* bit in output word */
      nshift = (int)fmod(((double)(ibit+shiftofs)),16.0);
      bit2 = 1;
      bit2 = bit2<<nshift;     /* bit in input word */
      /*printf("bottab %d %d %d %d \n",ibit,nshift,bit2,bit1);*/
      for (j=0; j<65536; j++)
        {
       if((bit2 & j) != 0) btab[iword][j] = btab[iword][j] + bit1;
	}
      }
    /*if(btab[iworddest][j] !=0) printf("btab %d %d %4.4x\n",iword,j,btab[iworddest][j]);*/
    } 
  }

/*
for (j=0; j<65536; j++)
  {
  printf("%4.4x %4.4x %4.4x %4.4x %4.4x %4.4x %4.4x   %4.4x %4.4x %4.4x %4.4x %4.4x %4.4x \n",j,ttab[0][j],ttab[1][j],ttab[2][j],ttab[3][j],ttab[4][j],ttab[5][j],btab[0][j],btab[1][j],btab[2][j],btab[3][j],btab[4][j],btab[5][j]);
  }
  */
first = 0;
}


for (i=0; i<nsamp; i++) 
  {
  /* Output word 0 */
  oblk[8*i+0] = btab[0][iblk[6*i+0]];
  /* Output word 1 */
  oblk[8*i+1] = btab[1][iblk[6*i+1]]; 
  /* Output word 2 */
  oblk[8*i+2] = mtab[1][iblk[6*i+1]] + btab[2][iblk[6*i+2]];
  /* Output word 3 */		    
  oblk[8*i+3] = mtab[2][iblk[6*i+2]] + btab[3][iblk[6*i+3]];
  /* Output word 4 */		    
  oblk[8*i+4] = mtab[3][iblk[6*i+3]] + btab[4][iblk[6*i+4]];
  /* Output word 5 */
  oblk[8*i+5] = mtab[4][iblk[6*i+4]];
  /* Output word 6 */
  oblk[8*i+6] = ttab[4][iblk[6*i+4]] + mtab[5][iblk[6*i+5]];
  /* Output word 7 */
  oblk[8*i+7] = ttab[5][iblk[6*i+5]];
  }

}

int ubd128_(unsigned short int * iblk, unsigned short int * oblk, int * nsampp)
{
ubd128(iblk, oblk, nsampp);
return 0;
}









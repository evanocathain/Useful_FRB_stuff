c  Include file for tape header. Original version SC_SHARE.INC
c  Modified to conform with tape_hdr.h (PM system) 
c  Ascii header for tape output. Total length = 640 char.

c  $Id: sc_pmhead.inc,v 2.4 2003/11/14 01:25:18 rmanches Exp $

cM  JFB    01 Jan 1997 - new multibeam/pmdaq header
cM                     - scan_rate_hd*8   -> feed_angle_hd*8
cM                     - samp_grp_hd(2)*4 -> samp_grp_hd(2)*4 (=1 for pmdaq)
cM                     - grp_blk_hd*8     -> samp_blk_hd*8
cM                     - status_hd(4)*8   -> atten_hd(4)*4
cM                     - tpower_hd(8)*8   -> tpower_hd(20)*4
cM                     - byte_grp_hd*4    -> nbeam_hd*4
cM  JFB    24 Jan 1997 - synch_hd*8       -> program_hd*6
cM                     - tapenr_hd*4      -> tapenr_hd*6
cM  JFB    05 Mar 1997 - Changed some things back so pdm, foldch etal work
cM                       reserve*42       -> pangle_hd*8
cM                                           smp_bsw_hd*8,
cM                                           cal_cycle_hd*4
cM                                           cal_amp_hd*8
cM                                           reserve*14
cM  JFB    13 Mar 1997 - cal_amp_hd*8     
cM                     - reserve*14       -> reserve*26
cM                     - data1_hd*4       -> data_hd(2)*2
cM                     - data2_hd*4       -> spare*4
cM                     - scan_cntr_hd*4   -> file_cntr_hd*4
cM                     - scan_len_hd*8    -> obs_time_hd*8
cM                     - Made comments consistent with tape_hdr.h
cM  JFB    24 APR 1997 - data_hd(2)*2     -> fdcntrl_hd*2
cM                                        -> data_hd(2)*1
cM  JFB    09 MAY 1997 - psrname_hd*12
cM                       unused_hd*4      -> pname_hd*16
c   RNM    16 Apr 1998 - grp_blk          -> samp_blk
c   RNM    07 Jul 1998 - Reversed above change.

	character program_hd*6,	!  Program name, was (Synch words tape block)
     :	version_hd*4,		!  Program version 'vv.v'
     :  tapenr_hd*6,		!  Tape label
     :  file_cntr_hd*4,		!  File number on tape
     :  blk_cntr_hd*8,		!  Block counter in file
     :	date_hd*8,		!  UT date 'yy/mm/dd' 
     :	mjd_hd*8,		!  MJD at 00h UT
     :	ut_strt_hd*16,		!  UT at file start ' hh:mm:ss.ssssss'
     :	lst_strt_hd*12,		!  LST at file start ' hh:mm:ss.ss'
     :	coord_type_hd*2  	!  Coord system  04 = Galactic	05 = J2000

     	character ra_strt_hd*16,!  RA at file start ' hh:mm:ss.ssss'
     :	dec_strt_hd*16,		!  Dec at file start '-dd:mm:ss.sss'
     : 	gl_strt_hd*8,		!  Gal longitude at start 'ddd.dddd'
     :	gb_strt_hd*8,		!  Gal latitude at file start  '+dd.dddd'
     :	feed_angle_hd*8,	!  Feed angle (degrees) (was scan_rate_hd*8)
     :  obs_time_hd*8,		!  Obs length (seconds of time) 'ssss.sss'
     :	comment_hd*64,		!  Comment
     :	nfilter_hd*2,		!  No. of filter systems (01 or 02)
     :  chanbw_hd(2)*8,		!  Chan inc (MHz, -ve if inverted) 'bb.bbbbb'
     :	nchan_hd(2)*4,		!  No. of channels in each filter system
     :	freq_chan1_hd(2)*12 	!  RFof 1st chan center    'ffffff.fffff'

        character samp_int_hd(2)*12, !  Sample interval in ms 'mm.mmmmmmmmm'
     :  samp_grp_hd(2)*4,	!  Samples per group (=1 for pmdaq)
     :  grp_blk_hd*8,           !  Groups per blk 
     :  blk_sec_hd*8,		!  Seconds per tape block 'ss.sssss'
     :  fdcntrl_hd*2,           !  0 = none, 1 = fixed FA, 2 = fixed PA or GPA 
     :  data_hd(2)*1,		!  Data type code (2=pol, 3=norm, 5=dedisp)
     :	ut_hd*16,		!  UT of block start ' hh:mm:ss.ssssss'
     :	lst_hd*12,		!  LST of block start ' hh:mm:ss.ss'
     :	RA_hd*16,		!  RA at block start ' hh:mm:ss.ssss'
     :	Dec_hd*16,		!  Dec at block start '-dd:mm:ss.sss'
     :	gl_hd*8,		!  Gal long at block start 'ddd.dddd'
     :	gb_hd*8,		!  Gal lat at block start '+dd.dddd'
     :  tel_zen_hd*8		!  Zenith angle at block start 'ddd.dddd'

        character tel_az_hd*8,  !  Azimuth at block start 'ddd.dddd'
     :  atten_hd(4)*4,		!  Attenuator settings (db) (was status_hd(4)*8)
     :	tpower_hd(20)*4,	!  Total powers (Jy) 'iiii' (was tpower_hd(8)*8)
     :  nblk_read_hd*8,		!  Nr of tape blocks in disk file
     :  scan_ratel_hd*8,	!  Scan rate (deg/min) in 'long' '-r.rrrrr'
     :  scan_rateb_hd*8,	!  Scan rate (deg/min) in 'lat'  '-r.rrrrr'
     :  nbeam_hd*4,		!  Total number of beams (was byte_grp_hd*4)
     :  ibeam_hd*4,		!  Beam number
     :  pname_hd*16,		!  Pulsar name (including B or J)
     :  obs_filen_hd*16,	!  Observe file name
     :  nbits_hd(2)*2,		!  Number of bits per sample
     :  ddm_hd*8,		!  DM for dedispersion 'ddddd.dd'
     :  nddch_hd(2)*4,		!  Number of channels per dedispersed band
     :  move_hd*2,		!  Tel stat 'k0': On pos k(0-9), '01': grd posn
		                !   '02': Off position, '03': Scanning 

     :  pnterr_hd*6,		!  Pointing error in arc min mmm.mm
     :  tree_hd*2,		!  ' ' normal, 'T'tree dedisp, 'D'pdm dedisp
     :  nsys_hd*2, 		!  Filter systems disk file
     :  telid_hd*10,    	!  Telescope ID (as for tel.def)
     :  pangle_hd*8,            !  Parallactic angle (degrees) '-ddd.ddd' (new)
     :  smp_bsw_hd*8,           !  Number of samples per beam switch (new)
     :  cal_cycle_hd*4,         !  Cal cycle period in samples
     :  reserve*22              !  spare

c  Decoded header data (250 i*2 words)
	
	integer*2 junk(22)
	integer*4 tapenr, scan_cntr, blk_cntr, mjd, coord_type,
     :    data1, nfilter, nchan(2), samp_grp(2), grp_blk, nsys,
     :    nblk_read, byte_grp, ibeam, data2, nbits(2), nddch(2)

	real*4 versionv, scan_rate, scan_len, chanbw(2), blk_sec,
     :	tel_zen, tel_az, tpower(8), scan_ratel, scan_rateb, ddm,
     :  pnterr

	real*8  ut_strt, lst_strt, ra_strt, 
     :	dec_strt, gl_strt, gb_strt, freq_chan1(2), samp_int(2),
     :  gl_stop, gb_stop, ra_stop, dec_stop,
     :	ut, lst, ra, dec, gl, gb 

	character comment*64, date*8, status(4)*8, psrname*12, 
     :    obs_filen*16, telid*10, resv*64

c  Common area

	common/head/
     :  program_hd,version_hd,
     :  tapenr_hd,file_cntr_hd,blk_cntr_hd,
     :	date_hd,mjd_hd,ut_strt_hd,lst_strt_hd,coord_type_hd,
     :	ra_strt_hd,dec_strt_hd,gl_strt_hd,gb_strt_hd,
     :	feed_angle_hd,obs_time_hd,
     :	comment_hd,
     :	nfilter_hd,chanbw_hd,nchan_hd,freq_chan1_hd,samp_int_hd,
     :  samp_grp_hd,grp_blk_hd,blk_sec_hd,data_hd,fdcntrl_hd,
     :	ut_hd,lst_hd,ra_hd,dec_hd,gl_hd,gb_hd,
     :	tel_zen_hd,tel_az_hd,atten_hd,tpower_hd,nblk_read_hd,
     :  scan_ratel_hd,scan_rateb_hd,nbeam_hd,ibeam_hd,pname_hd,
     :  obs_filen_hd,nbits_hd,nddch_hd,ddm_hd,move_hd,
     :  pnterr_hd,tree_hd,nsys_hd,telid_hd,
     :  pangle_hd,smp_bsw_hd,cal_cycle_hd,reserve

c  Decoded header
        common/head_var/
     :	versionv,tapenr,scan_cntr,blk_cntr,mjd,
     :  coord_type,scan_rate,scan_len,
     :  nfilter,chanbw,nchan,samp_grp,grp_blk,
     :  blk_sec,data1,tel_zen,tel_az,
     :  tpower,nblk_read,scan_ratel,scan_rateb,byte_grp,ibeam,
     :  data2,nbits,nddch,ddm,pnterr,nsys,junk

	common/head_var_ch/comment,date,status,psrname,obs_filen,
     :  telid,resv

	common/head_var_8/ut_strt, lst_strt, ra_strt, 
     :	dec_strt, gl_strt, gb_strt, freq_chan1, samp_int,
     :  gl_stop, gb_stop, ra_stop, dec_stop,
     :	ut, lst, ra, dec, gl, gb 


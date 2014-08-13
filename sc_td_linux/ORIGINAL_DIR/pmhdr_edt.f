      program pmhdr_edt

      implicit none

c     A utility to edit headers from sc_td using any editor

c     pmhead takes 2 command line arguments, the name of the editor you 
c     wish to use, and the name of the header file that you wish edit
c     pmhead opens the header and reads it in, then writes it out in a
c     more readily editable form to pmhead.tmp. 

c     pmhead then starts up your chosen editor allowing you to edit the 
c     header. Upon quiting from your editor, pmhead reads pmhead.tmp and
c     writes out a new version of the header with .new appended to 
c     the filename.

c     Packaged with sc_td in Epping:/psr/soft/search/util/new_sc_td.tar.gz
c     It does not require any libraries, to compile use
c     f77 -o pmhead pmhead.f; mv pmhead /psr/bin/$LOGIN_ARCH/

c     To use:   pmhead editor header.hdr

c     In principle any editor is acceptable,
c     It has been tested with emacs, vi and textedit

c     JFB  970312  first version
c     VMK  980718  modified for changes in sc_pmhead.inc
c     RNM  980719  renamed to pmhdr_edt

      integer*4 headl
      parameter (headl=640)
      
      include 'sc_pmhead.inc'
      integer lf,le,iargc,narg,istat,system
      character hdr*(headl), filename*80, comm*80
      character editor*20,junky*2
      equivalence (hdr,program_hd)
      
      narg=iargc()
      if (narg.ne.2) then
        write(*,*) 'Usage: pmhead editor header.hdr'
        stop
      endif

      call getarg(1,editor)
      le=index(editor,' ')-1

      call getarg(2,filename)
      lf=index(filename,' ')-1

      open(unit=34,file=filename(1:lf),status='old',
     +     form='unformatted')
      open(unit=35,file='pmhead.tmp',status='unknown',
     +     form='formatted')

      read(34)hdr

      write(*,*)'Read header'

      write(35,*)'##################################################'
      write(35,*)' Change values only - not position!'
      write(35,*)' NOTE  blank space at the beginning of each line'
      write(35,*)'##################################################'
      write(35,*)'    '

      write(35,*)program_hd,'    ! Program name was (Synch words )'
      write(35,*)version_hd,'      ! Program version vv.'
      write(35,*)tapenr_hd,'    ! Tape number'
      write(35,*)file_cntr_hd,'      ! File number on tape'
      write(35,*)blk_cntr_hd,'  ! Block counter from file start'
      write(35,*)date_hd,'  ! UT date yy/mm/dd at file start'
      write(35,*)mjd_hd,'  ! Modified Julian Date at 00h UT'
      write(35,*)ut_strt_hd,'   ! UT at start  hh:mm:ss.ssssss'
      write(35,*)lst_strt_hd, '       ! LST at start  hh:mm:ss.s'
      write(35,*)coord_type_hd,'                 ! coord system:'
      write(35,*)'                   ! 02 = Equat.  03 = B1950'
      write(35,*)'                   ! 04 = Galactic 05 = J2000'
      write(35,*)ra_strt_hd,'   ! RA at start  hh:mm:ss.ssss'
      write(35,*)dec_strt_hd,'   ! Dec at start -dd:mm:ss.sss'
      write(35,*)gl_strt_hd,'   ! Gal long at start ddd.dddd'
      write(35,*)gb_strt_hd,'   ! Gal lat at start  +dd.dddd'
      write(35,*)feed_angle_hd,'   ! Feed ang (deg) '
      write(35,*)obs_time_hd,'   ! Obs len (secs) ssss.sss'
      write(35,*)comment_hd,'   ! Comment'
      write(35,*)nfilter_hd,'            ! No. of filter sys '
      write(35,*)chanbw_hd(1),'      ! Channel BW in MHz bb.bbbbb'
      write(35,*)chanbw_hd(2),'      ! Channel BW in MHz bb.bbbbb'
      write(35,*)nchan_hd(1),'          ! No. of chan '
      write(35,*)nchan_hd(2),'          ! No. of chan  '
      write(35,*)freq_chan1_hd(1),'  ! Radio freq filter chan 1'
      write(35,*)freq_chan1_hd(2),'  ! Radio freq filter chan 2'
      write(35,*)'              !   ffffff.fffff'
      write(35,*)'              ! Freq increases with chan no.'
      write(35,*)samp_int_hd(1),'  ! Samp int in ms mm.mmmmmmmmm'
      write(35,*)samp_int_hd(2),'  ! Samp int in ms mm.mmmmmmmmm'
      write(35,*)samp_grp_hd(1),'          ! Samples per group'
      write(35,*)samp_grp_hd(2),'          ! Samples per blk '
      write(35,*)grp_blk_hd,'      ! Samples per blk '
      write(35,*)blk_sec_hd,'      ! Seconds per block ss.sssss'
      write(35,*)fdcntrl_hd,'   ! 0=none, 1=fixed FA, 2=fixed PA or GPA'
      write(35,*)data_hd(1),'            ! Code for data type  1'
      write(35,*)data_hd(2),'            ! Code for data type  2'
      write(35,*)ut_hd,' ! UT of block start  hh:mm:ss.ssssss'
      write(35,*)lst_hd,'     ! LST of blk start  hh:mm:ss.ss'
      write(35,*)RA_hd,' ! RA at block start hh:mm:ss.ssss'
      write(35,*)Dec_hd,' ! Dec at block start -dd:mm:ss.sss'
      write(35,*)gl_hd,'         ! Gal long at blk start ddd.dddd'
      write(35,*)gb_hd,'         ! Gal lat at blk start +dd.dddd'
      write(35,*)tel_zen_hd,'   ! zenith ang at block start'
      write(35,*)'           !           ddd.dddd'
      write(35,*)tel_az_hd,'   ! Telescope az at  start ddd.dddd'
      write(35,*)atten_hd(1),'       ! Attenuator  (db) '
      write(35,*)atten_hd(2),'       ! Attenuator  (db) '
      write(35,*)atten_hd(3),'       ! Attenuator  (db) '
      write(35,*)atten_hd(4),'       ! Attenuator  (db) '
      write(35,*)tpower_hd(1),'       ! Tot pow (Jy) iiii'
      write(35,*)tpower_hd(2),'       ! Tot pow (Jy) iiii'
      write(35,*)tpower_hd(3),'       ! Tot pow (Jy) iiii'
      write(35,*)tpower_hd(4),'       ! Tot pow (Jy) iiii'
      write(35,*)tpower_hd(5),'       ! Tot pow (Jy) iiii'
      write(35,*)tpower_hd(6),'       ! Tot pow (Jy) iiii'
      write(35,*)tpower_hd(7),'       ! Tot pow (Jy) iiii'
      write(35,*)tpower_hd(8),'       ! Tot pow (Jy) iiii'
      write(35,*)tpower_hd(9),'       ! Tot pow (Jy) iiii'
      write(35,*)tpower_hd(10),'       ! Tot pow (Jy) iiii'
      write(35,*)tpower_hd(11),'       ! Tot pow (Jy) iiii'
      write(35,*)tpower_hd(12),'       ! Tot pow (Jy) iiii'
      write(35,*)tpower_hd(13),'       ! Tot pow (Jy) iiii'
      write(35,*)tpower_hd(14),'       ! Tot pow (Jy) iiii'
      write(35,*)tpower_hd(15),'       ! Tot pow (Jy) iiii'
      write(35,*)tpower_hd(16),'       ! Tot pow (Jy) iiii'
      write(35,*)tpower_hd(17),'       ! Tot pow (Jy) iiii'
      write(35,*)tpower_hd(18),'       ! Tot pow (Jy) iiii'
      write(35,*)tpower_hd(19),'       ! Tot pow (Jy) iiii'
      write(35,*)tpower_hd(20),'       ! Tot pow (Jy) iiii'
      write(35,*)nblk_read_hd,'   ! Nr of tape blocks read'
      write(35,*)scan_ratel_hd,'   ! Scan rate  long -r.rrrrr'
      write(35,*)scan_rateb_hd,'   ! Scan rate  lat  -r.rrrrr'
      write(35,*)nbeam_hd,'       ! number of beams '
      write(35,*)ibeam_hd,'       ! Beam number'
      write(35,*)pname_hd,'! Pulsar name'
      write(35,*)obs_filen_hd,'! Observe file name'
      write(35,*)nbits_hd(1),'         ! bits per sample'
      write(35,*)nbits_hd(2),'         ! bits per sample'
      write(35,*)ddm_hd,'   ! DM for dedispersion ddddd.dd'
      write(35,*)nddch_hd(1),'       ! channels/dedispersed band'
      write(35,*)nddch_hd(2),'       ! channels/dedispersed band'
      write(35,*)move_hd,'         ! 00: not GRID 01:  02'
      write(35,*)pnterr_hd,'     ! Pointing error in arc min'
      write(35,*)tree_hd,'         ! normal T tree  pdm dedisp'
      write(35,*)nsys_hd,'         ! Filt sys compact or disk '
      write(35,*)telid_hd,' ! Telescope ID (as for tel.def)'
      write(35,*)pangle_hd,'   ! Parallactic angle (deg)  (new)'
      write(35,*)smp_bsw_hd,'   ! samps per beam switch (new)'
      write(35,*)cal_cycle_hd,'       ! Cal cycle period in samples'
      write(35,*)reserve,'! spare'

      close(34)
      close(35)

      write(*,*)'Wrote editable header - Starting editor'

      comm = editor(1:le)//' pmhead.tmp'
      istat = system(comm)

      write(*,*)'Header edited - writing ',filename(1:lf)//'.new'
      
      open(unit=34,file=filename(1:lf)//'.new',status='unknown',
     +     form='unformatted')
      open(unit=35,file='pmhead.tmp',status='old',
     +     form='formatted')

      read(35,'(a2)')junky
      read(35,'(a2)')junky
      read(35,'(a2)')junky
      read(35,'(a2)')junky
      read(35,'(a2)')junky
      read(35,'(x,a6)')program_hd
      read(35,'(x,a4)')version_hd
      read(35,'(x,a6)')tapenr_hd
      read(35,'(x,a4)')file_cntr_hd
      read(35,'(x,a8)')blk_cntr_hd
      read(35,'(x,a8)')date_hd
      read(35,'(x,a8)')mjd_hd
      read(35,'(x,a16)')ut_strt_hd
      read(35,'(x,a12)')lst_strt_hd
      read(35,'(x,a2)')coord_type_hd
      read(35,'(x,a2)')junky
      read(35,'(x,a2)')junky
      read(35,'(x,a16)')ra_strt_hd
      read(35,'(x,a16)')dec_strt_hd
      read(35,'(x,a8)')gl_strt_hd
      read(35,'(x,a8)')gb_strt_hd
      read(35,'(x,a8)')feed_angle_hd
      read(35,'(x,a8)')obs_time_hd
      read(35,'(x,a64)')comment_hd
      read(35,'(x,a2)')nfilter_hd
      read(35,'(x,a8)')chanbw_hd(1)
      read(35,'(x,a8)')chanbw_hd(2)
      read(35,'(x,a4)')nchan_hd(1)
      read(35,'(x,a4)')nchan_hd(2)
      read(35,'(x,a12)')freq_chan1_hd(1)
      read(35,'(x,a12)')freq_chan1_hd(2)
      read(35,'(x,a2)')junky
      read(35,'(x,a2)')junky
      read(35,'(x,a12)')samp_int_hd(1)
      read(35,'(x,a12)')samp_int_hd(2)
      read(35,'(x,a4)')samp_grp_hd(1)
      read(35,'(x,a4)')samp_grp_hd(2)
      read(35,'(x,a8)')grp_blk_hd
      read(35,'(x,a8)')blk_sec_hd
      read(35,'(x,a2)')fdcntrl_hd
      read(35,'(x,a2)')data_hd(1)
      read(35,'(x,a2)')data_hd(2)
      read(35,'(x,a16)')ut_hd
      read(35,'(x,a12)')lst_hd
      read(35,'(x,a16)')RA_hd
      read(35,'(x,a16)')Dec_hd
      read(35,'(x,a8)')gl_hd
      read(35,'(x,a8)')gb_hd
      read(35,'(x,a8)')tel_zen_hd
      read(35,'(x,a2)')junky
      read(35,'(x,a8)')tel_az_hd
      read(35,'(x,a4)')atten_hd(1)
      read(35,'(x,a4)')atten_hd(2)
      read(35,'(x,a4)')atten_hd(3)
      read(35,'(x,a4)')atten_hd(4)
      read(35,'(x,a4)')tpower_hd(1)
      read(35,'(x,a4)')tpower_hd(2)
      read(35,'(x,a4)')tpower_hd(3)
      read(35,'(x,a4)')tpower_hd(4)
      read(35,'(x,a4)')tpower_hd(5)
      read(35,'(x,a4)')tpower_hd(6)
      read(35,'(x,a4)')tpower_hd(7)
      read(35,'(x,a4)')tpower_hd(8)
      read(35,'(x,a4)')tpower_hd(9)
      read(35,'(x,a4)')tpower_hd(10)
      read(35,'(x,a4)')tpower_hd(11)
      read(35,'(x,a4)')tpower_hd(12)
      read(35,'(x,a4)')tpower_hd(13)
      read(35,'(x,a4)')tpower_hd(14)
      read(35,'(x,a4)')tpower_hd(15)
      read(35,'(x,a4)')tpower_hd(16)
      read(35,'(x,a4)')tpower_hd(17)
      read(35,'(x,a4)')tpower_hd(18)
      read(35,'(x,a4)')tpower_hd(19)
      read(35,'(x,a4)')tpower_hd(20)
      read(35,'(x,a8)')nblk_read_hd
      read(35,'(x,a8)')scan_ratel_hd
      read(35,'(x,a8)')scan_rateb_hd
      read(35,'(x,a4)')nbeam_hd
      read(35,'(x,a4)')ibeam_hd
      read(35,'(x,a12)')pname_hd
      read(35,'(x,a16)')obs_filen_hd
      read(35,'(x,a2)')nbits_hd(1)
      read(35,'(x,a2)')nbits_hd(2)
      read(35,'(x,a8)')ddm_hd
      read(35,'(x,a4)')nddch_hd(1)
      read(35,'(x,a4)')nddch_hd(2)
      read(35,'(x,a2)')move_hd
      read(35,'(x,a6)')pnterr_hd
      read(35,'(x,a2)')tree_hd
      read(35,'(x,a2)')nsys_hd
      read(35,'(x,a10)')telid_hd
      read(35,'(x,a8)')pangle_hd
      read(35,'(x,a8)')smp_bsw_hd
      read(35,'(x,a4)')cal_cycle_hd

      write(34)hdr

      close(34)
      close(35)

      write(*,*)'Finished'

      end

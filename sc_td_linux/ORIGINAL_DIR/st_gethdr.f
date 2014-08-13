      subroutine st_gethdr(istat)

c Read header data

c $Log: st_gethdr.f,v $
c Revision 2.16  2004/07/03 07:36:26  rmanches
c Handle SCAMP files, >2GB files
c
c Revision 2.15  2003/12/24 03:49:10  rmanches
c Fix dual-band operation
c
c Revision 2.14  2003/11/18 03:57:40  rmanches
c disk file kill
c
c Revision 2.13  2000/08/25 13:53:59  pulsar
c RTE Fixed bug for files on tapes in kill.chans on systems not listed therein
c
c Revision 2.12  2000/08/19 07:48:16  pulsar
c rte: fixes for killchans stuff
c
c Revision 2.11  2000/08/19 06:22:07  pulsar
c AGL mods to kill.chan, RTE add naskip to disk topen
c
c Revision 2.10  2000/02/29 21:08:02  istairs
c Tweak to sample and block clipping
c
c Revision 2.9  2000/02/23 06:13:34  rmanches
c update sc_tt and include dlt_extraskip in sct
c
c Revision 2.8  2000/02/04 06:30:01  istairs
c IHS/AGL Changed kill-channel filler to eliminate Nyquist birdies.  Also changed sample and block clipping to be guided by rms and running means of channels instead of the now-defunct digitizer offsets.
c
c Revision 2.7  1999/03/05 22:22:10  pulsar
c RNM: fix RA, Dec print
c
c Revision 2.6  1998/09/02 04:39:58  rmanches
c Tape name over-ride (AGL)
c
c Revision 2.5  1998/08/12 23:10:17  alyne
c Added channel-killing and tape-name overwrite in header
c
c Revision 2.4  1998/07/07 01:41:33  rmanches
c Changed samp_blk back to grp_blk
c
c Revision 2.3  1998/05/21 04:38:43  rmanches
c Fixed beam select with linearize on.
c Check input options and check that tape/file name entered.
c Compute digitiser offsets and write summary file when not writing to disk.
c Don't rewind Exabyte if naskip != 0.
c Don't read in digitiser offsets if clip =0.
c Changed default to no summary file. Altered summary and file skip keys.
c
c Revision 2.2  1998/05/16 06:33:50  rmanches
c Command line options changed - tape/file on end, no questions. More than one
c beam can be selected for writing. Block clipping now allows mean between nchan/2
c and nchan/2+beam_offset to allow for variable Tsys. Digitizer offsets now
c in file 'dititizer.offsets'.
c
c Revision 2.1  1998/04/23 23:22:55  jbell
c Dicks new v3_0 - added st_fclose.f st_fopen.f st_gethdr.f st_init.f st_inpar.f st_rdblk.f st_topen.f st_trans.f sc_td.inc - removed clops sc_cline.f sc_declare.f sc_fclose.f sc_fopen.f sc_format.f sc_gethead.f sc_grskip.f sc_preset.f sc_setpar.f sc_skip.f sc_tapeerr.f sc_td_files sc_topen.f sc_trans.f sc_unltape.f testbeamblk.f unbend.c README.multibeam
c

      implicit none
      include 'sc_td.inc'
      include 'sc_pmhead.inc'

      integer istat, j

c  Read header data
      read(nfilter_hd,'(i2)')nfilter
      if(nfilter.eq.1)then
         lsys(2)=.false.
         nchan(2)=0
      else
         if(rsys2)then
            lsys(2)=.true.
         else
            lsys(2)=.false.
         endif
      endif

      do j=1,nfilter
         read(samp_grp_hd(j),'(i4)')samp_grp(j)
         read(samp_int_hd(j),'(f12.9)')samp_int(j)
         read(nchan_hd(j),'(i4)')nchan(j)
         read(freq_chan1_hd(j),'(f12.6)')freq_chan1(j)
         read(chanbw_hd(j),'(f8.5)')chanbw(j)

c  Find kill system number
         do ksys=1,nksys
            if(abs(freq_chan1(j)-kfreq(ksys)).lt.1.0 .and.
     +          abs(chanbw(j)-kbw(ksys)).lt.0.05) goto 10
         enddo
         ksys=0
         write(*,*)'No channels being killed'
         if (nksys.gt.0) then
            write (*,'(a,i2,a,2f8.2,a)')
     +        'Filter',j,': ',freq_chan1(j),chanbw(j),
     +        ' not listed in kill.chans!'
         end if
 10      if(ksys.gt.0.and.lsys(j))then
            kss(j)=ksys
            if(ksys.ne.0) write(*,'(a,i2,a,2f8.2)')
     +        'Using kill system for Filter',j,': ',
     +         kfreq(ksys),kbw(ksys)
         endif
         read(nddch_hd(j),'(i4)')nddch(j)
         if(nbits_hd(j).ne.' ') then
            read(nbits_hd(j),'(i2)')nbits(j)
         else
            nbits(j) = 1
            write(nbits_hd(j),'(i2)')nbits(j)
         endif
      enddo

      read(grp_blk_hd,'(i8)')grp_blk
      samp_blk1=grp_blk*samp_grp(1)
      samp_blk2=grp_blk*samp_grp(2)
      
      if(scamp)then
         version_hd(1:2)=version_hd(3:4)
         version_hd(3:4)=tapenr_hd(1:2)
         tapenr_hd(1:2)='  '
         data_hd(2)=pname_hd(16:16)
         pname_hd(16:16)=' '
      endif

      read(data_hd(1),'(i1)')data1
      if(data1.ne.1)then
         grp_blk=samp_blk1
         samp_blk2=samp_blk1
         samp_grp(1)=1
         samp_grp(2)=1
         samp_int(2)=samp_int(1)
         write(grp_blk_hd,'(i8)')grp_blk 
         write(samp_grp_hd(1),'(i4)')samp_grp(1) 
         write(samp_grp_hd(2),'(i4)')samp_grp(2) 
         write(samp_int_hd(2),'(f12.9)')samp_int(2)
      endif
          
      nbyte_smp1=nchan(1)*nbits(1)/8
      nbyte_smp2=nchan(2)*nbits(2)/8
      nbyte_grp1=nbyte_smp1*samp_grp(1)
      nbyte_grp2=nbyte_smp2*samp_grp(2)
      nbyte_smp=nbyte_smp1+nbyte_smp2
      nbyte_grp=nbyte_grp1+nbyte_grp2
      
      if(tree_hd.eq.'T')then
         treed=.true.
      else
         treed=.false.
      endif


       if(diskf)then                 ! Set file nr and tape label if disk input
          read(file_cntr_hd,'(i4)')infile
          if(scamp)then
             tlabel=diskfile(1:16)
             ltn=min(ldfn,16)
          else
             tlabel=tapenr_hd
             ltn=index(tlabel,' ')-1 
          endif
       else
         if(tapenr_hd.ne.tlabel.and.overwrite)then
            write(*,'(a/a,a,a,a)') 
     +        ' Warning: Given tape name does not match header name',
     +        '    Header name:          ',tapenr_hd,
     +        '    changed to given name:',tlabel
            tapenr_hd=tlabel
         endif
      endif

      if(verbose)write(*,'('' Header data, filt 1:'')')
      if(verbose)write(*,'('' Tape, file:'',a,2x,a)')tapenr_hd,
     +      file_cntr_hd
      if(verbose)write(*,'('' nchan,freq,bw:'', i5,f10.3,f8.3)')
     +    nchan(1),freq_chan1(1),chanbw(1)
      if(verbose)write(*,'('' nbits,samp_grp,grp_blk:'',3i8)')
     +    nbits(1),samp_grp(1),grp_blk


      call repair_hdr               ! Repair faults in header data    
             
c  Set levels and zero counters
      if(lsys(1))then
         nbyte_blk1=samp_blk1*nbyte_smp1
         imean1=nchan(1)/2
         iclip1=nint(0.5*clip*sqrt(float(nchan(1))))
C Factor of 4 seems to be OK empirically, rather than old 10 IHS 000121
C Bump up to 6 IHS 000229
         bclip1=6.*iclip1/sqrt(float(samp_blk1)) 
         if(verbose)write(*,*)' Clip1:',imean1,iclip1,bclip1 
      endif                                       
      if(lsys(2))then
         nbyte_blk2=samp_blk2*nbyte_smp2
         imean2=nchan(2)/2
         iclip2=nint(0.5*clip*sqrt(float(nchan(2))))
         bclip2=6.*iclip2/sqrt(float(samp_blk2))
         if(verbose)write(*,*)' Clip2:',imean2,iclip2,bclip2
      else
         nbyte_blk2=0
      endif

C No longer need digitizer.offsets file

      istat=0
      return
      end



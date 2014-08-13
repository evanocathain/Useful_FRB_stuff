      subroutine st_init

c Initialise parameters for sc_td

c $Log: st_init.f,v $
c Revision 2.12  2004/07/03 07:36:26  rmanches
c Handle SCAMP files, >2GB files
c
c Revision 2.11  2003/12/24 03:49:10  rmanches
c Fix dual-band operation
c
c Revision 2.10  2003/11/18 03:57:40  rmanches
c disk file kill
c
c Revision 2.9  2001/07/23 02:29:01  rmanches
c linux changes
c
c Revision 2.8  2000/08/19 06:22:08  pulsar
c AGL mods to kill.chan, RTE add naskip to disk topen
c
c Revision 2.7  2000/02/04 06:30:21  istairs
c IHS/AGL Changed kill-channel filler to eliminate Nyquist birdies.  Also changed sample and block clipping to be guided by rms and running means of channels instead of the now-defunct digitizer offsets.
c
c Revision 2.6  1999/07/15 14:15:19  pulsar
c Automated killing of chans from file kill.chans. -x inhibits - AGL
c
c Revision 2.5  1999/03/05 19:40:37  pulsar
c RNM: Added -t, -I options
c
c Revision 2.4  1998/08/12 00:45:46  alyne
c Added channel-killing facility
c
c Revision 2.3  1998/05/21 04:38:44  rmanches
c Fixed beam select with linearize on.
c Check input options and check that tape/file name entered.
c Compute digitiser offsets and write summary file when not writing to disk.
c Don't rewind Exabyte if naskip != 0.
c Don't read in digitiser offsets if clip =0.
c Changed default to no summary file. Altered summary and file skip keys.
c
c Revision 2.2  1998/05/16 06:33:51  rmanches
c Command line options changed - tape/file on end, no questions. More than one
c beam can be selected for writing. Block clipping now allows mean between nchan/2
c and nchan/2+beam_offset to allow for variable Tsys. Digitizer offsets now
c in file 'dititizer.offsets'.
c
c Revision 2.1  1998/04/23 23:22:56  jbell
c Dicks new v3_0 - added st_fclose.f st_fopen.f st_gethdr.f st_init.f st_inpar.f st_rdblk.f st_topen.f st_trans.f sc_td.inc - removed clops sc_cline.f sc_declare.f sc_fclose.f sc_fopen.f sc_format.f sc_gethead.f sc_grskip.f sc_preset.f sc_setpar.f sc_skip.f sc_tapeerr.f sc_td_files sc_topen.f sc_trans.f sc_unltape.f testbeamblk.f unbend.c README.multibeam
c

      implicit none
      include 'sc_td.inc'

      integer j

      lsys(1)=.true.
      rsys2=.true.

      indev='D'
      gskip=.false.
      wsumm=.false.
      wdat=.true.
      nfskip=0
      naskip=0
      nfread=0
      nbskip=0
      nbread=0
      nbeam=1
      nfill=0
      clip=0.0
      killfile=.true.
      killtlabel='none'
      skeof=.true.
      wrascu=.true.
      dzero=.false.
      unbnd=.false.
      f128c=.false.
      toff=.false.
      singlebeamonly=.false.
      overwrite=.true.
      skpinit=.false.
      verbose=.false.
      vverbose=.false.

      do j=1,nbeammax
         nclip1(j)=0
         nbclip1(j)=0
         nclip2(j)=0
         nbclip2(j)=0
         wbeam(j)=0
      enddo

      nksys=0
      do ksys=1,ksysmax
        do j=1,nkillmax
          kmask2(j,ksys)=X'ff'
          kmask3(j,ksys)=0
         enddo
      enddo
      nblk=0
      nfile=0
      scfile=0
      scblk=0
      lastmove='  '

      return
      end

      subroutine st_topen(istat)

c Open files, skip tape files. 

c $Log: st_topen.f,v $
c Revision 2.8  2004/07/03 07:36:26  rmanches
c Handle SCAMP files, >2GB files
c
c Revision 2.7  2003/11/18 03:57:41  rmanches
c disk file kill
c
c Revision 2.6  2000/08/19 06:22:13  pulsar
c AGL mods to kill.chan, RTE add naskip to disk topen
c
c Revision 2.5  2000/02/23 06:13:35  rmanches
c update sc_tt and include dlt_extraskip in sct
c
c Revision 2.4  1999/03/05 19:40:43  pulsar
c RNM: Added -t, -I options
c
c Revision 2.3  1998/05/21 04:38:44  rmanches
c Fixed beam select with linearize on.
c Check input options and check that tape/file name entered.
c Compute digitiser offsets and write summary file when not writing to disk.
c Don't rewind Exabyte if naskip != 0.
c Don't read in digitiser offsets if clip =0.
c Changed default to no summary file. Altered summary and file skip keys.
c
c Revision 2.2  1998/05/16 06:33:52  rmanches
c Command line options changed - tape/file on end, no questions. More than one
c beam can be selected for writing. Block clipping now allows mean between nchan/2
c and nchan/2+beam_offset to allow for variable Tsys. Digitizer offsets now
c in file 'dititizer.offsets'.
c
c Revision 2.1  1998/04/23 23:22:58  jbell
c Dicks new v3_0 - added st_fclose.f st_fopen.f st_gethdr.f st_init.f st_inpar.f st_rdblk.f st_topen.f st_trans.f sc_td.inc - removed clops sc_cline.f sc_declare.f sc_fclose.f sc_fopen.f sc_format.f sc_gethead.f sc_grskip.f sc_preset.f sc_setpar.f sc_skip.f sc_tapeerr.f sc_td_files sc_topen.f sc_trans.f sc_unltape.f testbeamblk.f unbend.c README.multibeam
c

      implicit none
      include 'sc_td.inc'
      include 'sc_pmhead.inc'

      byte block(headl+datl)
      integer istat, sctopen, sctread, sctskipf

      equivalence(block,program_hd)

c Open disk file
      if(diskf)then
          call sc_rblk(block,"o",diskfile(1:ldfn),istat,ldfn)
          if(istat.eq.0) then
            write(*,*)'Failed to open disk file: ',diskfile
            STOP
          else
            write(*,*)'Opened disk file: ',diskfile(1:ldfn)
          endif
c Skip init file (on Scamp Exabytes)
          if(skpinit)then
             if(verbose)write(*,'(''  Skipping INIT file'')')
             call sc_rblk(block,"r",diskfile(1:ldfn),istat,ldfn)
             if(istat.eq.0)then ! EOF
                istat=1
                RETURN
             endif
             istat=sctread(inunit,block,len)
             if(istat.le.0 .or. istat.ne.len)then
                write(*,'('' Error checking INIT file, istat:'',i6,
     +               ''  len:'',i6)')istat,len
                STOP
             endif
             if(block(1).ne.0)then
              write(*,'('' *** Warning: First file not an INIT file'')')
             endif
             istat=sctskipf(inunit,1)  
             if(istat.lt.0)STOP ' Error skipping INIT file'
          endif

          if(naskip.gt.0)infile=1+naskip
       else
c Tape input
          istat=sctopen(inunit,intape)
          if(istat.lt.0)STOP 'Error opening tape unit'

c Skip init file (on Scamp Exabytes)
          if(skpinit)then
             if(verbose)write(*,'(''  Skipping INIT file'')')
             istat=sctread(inunit,block,len)
             if(istat.le.0 .or. istat.ne.len)then
                write(*,'('' Error checking INIT file, istat:'',i6,
     +               ''  len:'',i6)')istat,len
                STOP
             endif
             if(block(1).ne.0)then
              write(*,'('' *** Warning: First file not an INIT file'')')
             endif
             istat=sctskipf(inunit,1)  
             if(istat.lt.0)STOP ' Error skipping INIT file'
          endif

c Skip files
          if(nfskip.gt.0)then
             write(*,'('' Skipping'',i4,'' files'')')nfskip
             istat=sctskipf(inunit,nfskip)  
             if(istat.lt.0)STOP ' Error skipping files'
          endif
          infile=1+naskip+nfskip
       endif  
       
       istat=0
       return
       end



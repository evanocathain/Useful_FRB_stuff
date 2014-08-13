      subroutine st_fclose(istat)

c Opens, modifies and closes header files. Closes data files. 
c Open, writes and closes summary file

c $Log: st_fclose.f,v $
c Revision 2.7  2003/12/24 03:49:10  rmanches
c Fix dual-band operation
c
c Revision 2.6  2002/09/05 09:01:02  ghobbs
c Mods for Linux - Cobra
c
c Revision 2.5  2000/02/04 06:29:39  istairs
c IHS/AGL Changed kill-channel filler to eliminate Nyquist birdies.  Also changed sample and block clipping to be guided by rms and running means of channels instead of the now-defunct digitizer offsets.
c
c Revision 2.4  1998/07/07 01:41:32  rmanches
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
c Revision 2.1  1998/04/23 23:22:54  jbell
c Dicks new v3_0 - added st_fclose.f st_fopen.f st_gethdr.f st_init.f st_inpar.f st_rdblk.f st_topen.f st_trans.f sc_td.inc - removed clops sc_cline.f sc_declare.f sc_fclose.f sc_fopen.f sc_format.f sc_gethead.f sc_grskip.f sc_preset.f sc_setpar.f sc_skip.f sc_tapeerr.f sc_td_files sc_topen.f sc_trans.f sc_unltape.f testbeamblk.f unbend.c README.multibeam
c

      implicit none
      include 'sc_td.inc'
      include 'sc_pmhead.inc'

      integer istat, iunit, j, rename, ln
      real*4 fac
      character hdr*(headl)

      equivalence (hdr,program_hd)

      write(*,'('' Final block count:'',i6)')nbblk
      
      if(wdat)then
         do ibm=1,nbeam
            if(wbeam(ibm).ne.0)then
               if(lsys(1))then
                  mbfile(ibm)=mbfile(ibm)(1:ltf-1)//'1'
                  open(unit=10,file=mbfile(ibm)(1:ltf)//'.hdr',
     +                 form='unformatted',status='old',iostat=istat)
                  if(istat.ne.0)then
                     write(*,'('' Error opening '',a)')
     +                    mbfile(ibm)(1:ltf)//'.hdr'
                     RETURN
                  endif
                  read(10)hdr
                  close(10)

                  write(nsys_hd,'(a)')' 1'
                  write(ibeam_hd,'(i4)')ibm
                  write(nblk_read_hd,'(i8)')nbblk
                  
                  if(pmtape .and. nbeam.eq.13 .and. ibm.gt.1) then
                     write(pname_hd(2:8),'(i7)')ngid(ibm)
                  endif

cM Put L in tree_hd if linearised data
cM Adjust header if 32 empty channels are added
                  if(unbnd) then
                     write(tree_hd(1:1),'(a)')'L' 
                     if(nbeam.eq.13) then
                        write(nblk_read_hd,'(i8)')(nblk/39)*4 
                     else
                        write(nblk_read_hd,'(i8)')(nblk/3)*4 
                     endif
                     write(nchan_hd(1),'(i4)')(nchan(1)*4)/3
                     write(grp_blk_hd,'(i8)')(samp_blk1*3)/4
                     read(blk_sec_hd,*)blk_sec
                     write(blk_sec_hd,'(f8.5)')(blk_sec*3)/4
                  endif

                  open(unit=10,file=mbfile(ibm)(1:ltf)//'.hdr',
     +                 form='unformatted',status='unknown')
                  write(10)hdr
                  close(10)
               
c       Update the ascu file and add the clipping info
                  if(wrascu) then
                     open(unit=10,file=mbfile(ibm)(1:ltf)//'.ascu',
     +                  status='unknown')
                     write(10,100)pname_hd(2:8),tlabel(1:6),infile,
     +                  blk_cntr,ra_hd(1:12),dec_hd(1:12),gl_hd(1:8),
     +                  gb_hd(1:8),date_hd(1:8),mjd_hd(4:8),ut_hd(1:9),
     +                  lst_hd(1:9),grp_blk_hd(1:8), nblk_read_hd(1:8)
                     write(10,110)clip,nint(rmean(ibm)),iclip1,
     +                    nclip1(ibm),nbclip1(ibm)
                     close(10)
 100                 format('ST',1x,a,1x,a,i4,i6,1x,a,1x,a,1x,a,1x,a/
     +                    'ST',1x,a,1x,a,1x,a,1x,a,a,a)
 110                 format('ST',f6.2,2i4,i8,i6)
                     
                     open(unit=10,file=mbfile(ibm)(1:ltf)//'.ascu.orig',
     +                    status='unknown')
                     write(10,100)pname_hd(2:8),tlabel(1:6),infile,
     +                  blk_cntr,ra_hd(1:12),dec_hd(1:12),gl_hd(1:8),
     +                  gb_hd(1:8),date_hd(1:8),mjd_hd(4:8),ut_hd(1:9),
     +                  lst_hd(1:9),grp_blk_hd(1:8), nblk_read_hd(1:8)
                     write(10,110)clip,nint(rmean(ibm)),iclip1,
     +                    nclip1(ibm),nbclip1(ibm)
                     close(10)
                  endif

c  Move .tmp files so scripts can see it's ready to process
                  iunit=20+ibm   
                  close(iunit)
                  if (tree_hd(1:1).eq.'T') then
                     istat = rename(mbfile(ibm)(1:ltf)//'.tmp',
     &                    mbfile(ibm)(1:ltf)//'.tree')
                  elseif(dzero) then
                     istat = rename(mbfile(ibm)(1:ltf)//'.tmp',
     &                    mbfile(ibm)(1:ltf)//'.zerodm') 
                  else
                     istat = rename(mbfile(ibm)(1:ltf)//'.tmp',
     &                    mbfile(ibm)(1:ltf)//'.dat') 
                  endif
                  if(istat.ne.0)then
                     write(*,'('' Error renaming '',a)')
     +                    mbfile(ibm)(1:ltf)//'.tmp'
                     RETURN
                  endif
               endif

               if(lsys(2))then
                  mbfile(ibm)=mbfile(ibm)(1:ltf-1)//'2'
                  open(unit=10,file=mbfile(ibm)(1:ltf)//'.hdr',
     +                 form='unformatted',status='old',iostat=istat)
                  if(istat.ne.0)then
                     write(*,'('' Error opening '',a)')
     +                    mbfile(ibm)(1:ltf)//'.hdr'
                     RETURN
                  endif
                  read(10)hdr
                  close(10)

                  write(nsys_hd,'(a)')' 2'
                  write(ibeam_hd,'(i4)')ibm
                  write(nblk_read_hd,'(i8)')nbblk
                  
                  open(unit=10,file=mbfile(ibm)(1:ltf)//'.hdr',
     +                 form='unformatted',status='unknown')
                  write(10)hdr
                  close(10)
               
                  if(wrascu) then
                     open(unit=10,file=mbfile(ibm)(1:ltf)//'.ascu',
     +                    status='unknown')
                     write(10,100)pname_hd(2:8),tlabel(1:6),infile,
     +                  blk_cntr,ra_hd(1:12),dec_hd(1:12),gl_hd(1:8),
     +                  gb_hd(1:8),date_hd(1:8),mjd_hd(4:8),ut_hd(1:9),
     +                  lst_hd(1:9),grp_blk_hd(1:8), nblk_read_hd(1:8)
                     write(10,110)clip,nint(rmean(ibm)),iclip2,
     +                    nclip2(ibm),nbclip2(ibm)
                     close(10)
                     
                     open(unit=10,file=mbfile(ibm)(1:ltf)//'.ascu.orig',
     +                    status='unknown')
                     write(10,100)pname_hd(2:8),tlabel(1:6),infile,
     +                  blk_cntr,ra_hd(1:12),dec_hd(1:12),gl_hd(1:8),
     +                  gb_hd(1:8),date_hd(1:8),mjd_hd(4:8),ut_hd(1:9),
     +                  lst_hd(1:9),grp_blk_hd(1:8), nblk_read_hd(1:8)
                     write(10,110)clip,nint(rmean(ibm)),iclip2,
     +                    nclip2(ibm),nbclip2(ibm)
                     close(10)
                  endif

c Move .tmp files so scripts can see it's ready to process
                  iunit=33+ibm   
                  close(iunit)
                  if (tree_hd(1:1).eq.'T') then
                     istat = rename(mbfile(ibm)(1:ltf)//'.tmp',
     &                    mbfile(ibm)(1:ltf)//'.tree')
                  elseif(dzero) then
                     istat = rename(mbfile(ibm)(1:ltf)//'.tmp',
     &                    mbfile(ibm)(1:ltf)//'.zerodm') 
                  else
                     istat = rename(mbfile(ibm)(1:ltf)//'.tmp',
     &                    mbfile(ibm)(1:ltf)//'.dat') 
                  endif
                  if(istat.ne.0)then
                     write(*,'('' Error renaming '',a)')
     +                    mbfile(ibm)(1:ltf)//'.tmp'
                     RETURN
                  endif
               endif

               call exstat(-1,' '//mbfile(ibm)(1:ltf),'sc_td',ltf,5)
            endif
         enddo
      endif
               
      if(iclip1.gt.0)then
         write(*,120),clip,'1'
 120     format(/' Clip statistics (Clip level:',f4.1,') Filter ',a)
         fac=100./(nbblk*samp_blk1)
         write(*,130)(fac*nclip1(j),j=1,nbeam)
 130     format(' % samples clipped   ',13f5.1)
         write(*,140)(nbclip1(j),j=1,nbeam)
 140     format(' Nr blks clipped     ',13i5)
         write(*,150)(fac*(nclip1(j)+nbclip1(j)*samp_blk1),j=1,nbeam)
 150     format(' Total % samp clipped',13f5.1)
      endif

      if(iclip2.gt.0)then
         write(*,120),clip,'2'
         fac=100./(nbblk*samp_blk2)
         write(*,130)(fac*nclip2(j),j=1,nbeam)
         write(*,140)(nbclip2(j),j=1,nbeam)
         write(*,150)(fac*(nclip2(j)+nbclip2(j)*samp_blk2),j=1,nbeam)
      endif
         
      if(clip.gt.0.)then
         write(*,'(/'' Mean digitizer offsets for each beam:''/13f5.1)')
     +                   (rmean(j)-float(imean1),j=1,nbeam)
      endif

      if(nerr.gt.0)then
         write(*,'(/''There were'',i3,'' tape read errors'')')nerr
      endif

c Open summary file if required

      if(wsumm)then
         open(unit=10,file=summfile(1:lsm),status='new',err=12)
         if(verbose)write(*,'(/'' New summary file '',a,'' opened''/)')
     +        summfile(1:lsm)
         go to 14
 12      open(unit=10,file=summfile(1:lsm),status='old',access='append')
         if(verbose)write(*,
     +   '(/'' Existing summary file '',a,'' opened''/)')summfile(1:lsm)
 14      continue
         ln=index(pname_hd,' ')-1
         if(lsys(1))then
            write(10,200)'#1',infile,date_hd,mjd,ut_strt_hd(1:9),
     +           nbblk,freq_chan1(1),nchan(1),chanbw(1),
     +           samp_int(1),ra_hd(1:11),dec_hd(1:9),pname_hd(1:ln)
            fac=100./(nbblk*samp_blk1)
            write(10,210)(fac*nclip1(j),nbclip1(j),j=1,nbeam)
         endif
         if(lsys(2))then
            write(10,200)'#2',infile,date_hd,mjd,ut_strt_hd(1:9),
     +           nbblk,freq_chan1(2),nchan(2),chanbw(2),
     +           samp_int(2),ra_hd(1:11),dec_hd(1:9),pname_hd(1:ln)
            fac=100./(nbblk*samp_blk2)
            write(10,210)(fac*nclip2(j),nbclip2(j),j=1,nbeam)
         endif
 200     format(a,i4,1x,a,i6,1x,a,i6,f10.3,i4,f7.3,f9.6,1x,
     +             a,1x,a,1x,a)
 210     format(13(f4.1,i4))
         close(10)
      endif

      nblk=0                    ! Reset counters
      nerr=0
      do j=1,nbeam
         nclip1(j)=0
         nclip2(j)=0
         nbclip1(j)=0
         nbclip2(j)=0
      enddo
         
      nfile=nfile+1             ! Increment file counters
      infile=infile+1
      istat=0
      return
      end

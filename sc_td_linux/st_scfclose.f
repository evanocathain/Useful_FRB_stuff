      subroutine st_scfclose(istat)

c Opens, modifies and closes header scamp files. Closes scamp data files. 
c Open, writes and closes scamp summary file

      implicit none
      include 'sc_td.inc'
      include 'sc_pmhead.inc'

      integer istat, iunit, j, rename, ln, irah, iram
      real*4 fac, ras
      character hdr*(headl)

      equivalence (hdr,program_hd)

      write(*,'('' Final block count:'',i6)')scblk
      
      if(wdat)then
         mbfile(ibm)=mbfile(ibm)(1:ltf-1)//'1'
         open(unit=10,file=mbfile(ibm)(1:ltf)//'.hdr',
     +        form='unformatted',status='old',iostat=istat)
         if(istat.ne.0)then
            write(*,'('' Error opening '',a)')
     +           mbfile(ibm)(1:ltf)//'.hdr'
            RETURN
         endif
         read(10)hdr
         close(10)

         write(nsys_hd,'(a)')' 1'
         write(ibeam_hd,'(i4)')ibm
         write(nblk_read_hd,'(i8)')scblk
                  
         open(unit=10,file=mbfile(ibm)(1:ltf)//'.hdr',
     +                 form='unformatted',status='unknown')
         write(10)hdr
         close(10)
               
c  Move .tmp files so scripts can see it's ready to process
         iunit=20+ibm   
         close(iunit)
         if (tree_hd(1:1).eq.'T') then
            istat = rename(mbfile(ibm)(1:ltf)//'.tmp',
     &           mbfile(ibm)(1:ltf)//'.tree')
         elseif(dzero) then
            istat = rename(mbfile(ibm)(1:ltf)//'.tmp',
     &           mbfile(ibm)(1:ltf)//'.zerodm') 
         else
            istat = rename(mbfile(ibm)(1:ltf)//'.tmp',
     &           mbfile(ibm)(1:ltf)//'.dat') 
         endif
         if(istat.ne.0)then
            write(*,'('' Error renaming '',a)')
     +           mbfile(ibm)(1:ltf)//'.tmp'
            RETURN
         endif
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
         if(verbose)write(*,'('' New summary file '',a,'' opened'')')
     +        summfile(1:lsm)
         go to 14
 12      open(unit=10,file=summfile(1:lsm),status='old',access='append')
         if(verbose)write(*,
     +   '('' Existing summary file '',a,'' opened'')')summfile(1:lsm)
 14      continue
         ln=index(pname_hd,' ')-1
         read(ra_hd,'(i3,1x,i2,1x,f5.0)')irah,iram,ras
         ras=irah*3600 + iram*60 +ras
         write(10,200)'#1',tlabel(1:ltn),scfile,date_hd,mjd,
     +     ut_strt_hd(1:9),nbblk,scblk,freq_chan1(1),nchan(1),chanbw(1),
     +     samp_int(1),ra_hd(1:11),ras,dec_hd(1:9),pname_hd(1:ln)
         fac=100./(nbblk*samp_blk1)
         write(10,210)(fac*nclip1(j),nbclip1(j),j=1,nbeam)

 200     format(a,1x,a,i4,1x,a,i6,1x,a,2i6,f10.3,i4,f7.3,f9.6,1x,
     +             a,1x,'(',f7.1,') ',a,1x,a)
 210     format(13(f4.1,i4))
         close(10)
      endif

      nerr=0
      do j=1,nbeam
         nclip1(j)=0
         nclip2(j)=0
         nbclip1(j)=0
         nbclip2(j)=0
      enddo

      istat=0

      return
      end

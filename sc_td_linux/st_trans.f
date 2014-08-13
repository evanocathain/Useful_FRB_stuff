      subroutine st_trans(istat)

c Expand and linearise data, form zerodm sum, clip data and write to disk

c $Log: st_trans.f,v $
c Revision 2.10  2003/12/24 03:49:11  rmanches
c Fix dual-band operation
c
c Revision 2.9  2000/08/19 06:22:14  pulsar
c AGL mods to kill.chan, RTE add naskip to disk topen
c
c Revision 2.8  2000/02/29 21:09:41  istairs
c Tweak to sample and block clipping
c
c Revision 2.7  2000/02/04 06:31:01  istairs
c IHS/AGL Changed kill-channel filler to eliminate Nyquist birdies.  Also changed sample and block clipping to be guided by rms and running means of channels instead of the now-defunct digitizer offsets.
c
c Revision 2.6  1998/08/12 23:10:18  alyne
c Added channel-killing and tape-name overwrite in header
c
c Revision 2.5  1998/07/07 01:41:33  rmanches
c Changed samp_blk back to grp_blk
c
c Revision 2.4  1998/05/21 04:38:45  rmanches
c Fixed beam select with linearize on.
c Check input options and check that tape/file name entered.
c Compute digitiser offsets and write summary file when not writing to disk.
c Don't rewind Exabyte if naskip != 0.
c Don't read in digitiser offsets if clip =0.
c Changed default to no summary file. Altered summary and file skip keys.
c
c Revision 2.3  1998/05/16 06:33:52  rmanches
c Command line options changed - tape/file on end, no questions. More than one
c beam can be selected for writing. Block clipping now allows mean between nchan/2
c and nchan/2+beam_offset to allow for variable Tsys. Digitizer offsets now
c in file 'dititizer.offsets'.
c
c Revision 2.2  1998/05/06 07:26:13  rmanches
c Change zerodm file to reals
c
c Revision 2.1  1998/04/23 23:22:59  jbell
c Dicks new v3_0 - added st_fclose.f st_fopen.f st_gethdr.f st_init.f st_inpar.f st_rdblk.f st_topen.f st_trans.f sc_td.inc - removed clops sc_cline.f sc_declare.f sc_fclose.f sc_fopen.f sc_format.f sc_gethead.f sc_grskip.f sc_preset.f sc_setpar.f sc_skip.f sc_tapeerr.f sc_td_files sc_topen.f sc_trans.f sc_unltape.f testbeamblk.f unbend.c README.multibeam
c

      implicit none
      include 'sc_td.inc'
      include 'sc_pmhead.inc'

      byte dblk1(datl), dblk2(datl), dblk3(nbeammax,4*datl), 
     +     dblk4(4*datl), block(headl+datl), bcl1, bcl2
      integer istat, nbit(-128:127), isum, jsum, ncl, j, jj, 
     +     ilim1, ilim2, jk, k, i, ibunit, b39, b13, b03, stride, pmb
      real*4 dedat(datl), blkmn, blim1, blim2
      real*4 meannc,sumsq,sumsqnc,rms,rmsnc
      real*4 rrms(nbeammax)
      real*4 mnwgt
      
      save rrms

      equivalence (block,program_hd)

      data bcl1/'55'x/,bcl2/'aa'x/
      data nbit/
     &     1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,
     &     2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
     &     2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
     &     3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
     &     2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
     &     3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
     &     3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
     &     4,5,5,6,5,6,6,7,5,6,6,7,6,7,7,8,
     &     0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4,
     &     1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,
     &     1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,
     &     2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
     &     1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,
     &     2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
     &     2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
     &     3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7/

      data mnwgt/0.1/

c  Transfer filter 1 data
      if(lsys(1))then
         
c     Move data to local array
         do j=1,grp_blk
            jj=(j-1)*nbyte_grp
            jk=(j-1)*nbyte_grp1
            do k=1,nbyte_grp1
               dblk1(jk+k)=block(headl+jj+k)
            enddo
         enddo
         
c Kill any channels if required
         if(kss(1).ne.0)then
            do j=1,samp_blk1
               jk=(j-1)*nbyte_smp1
               do k=1,nbyte_smp1
                  dblk1(jk+k)=and(dblk1(jk+k),kmask2(k,kss(1)))
                  dblk1(jk+k)=or(dblk1(jk+k),kmask3(k,kss(1)))
               enddo
            enddo
         endif

c Initialize the running mean and set the clipping limits

         if(nblk.le.nbeam) then      ! Start off at nominal
            rmean(ibm) = imean1   
         endif
         if(nblk.le.10*nbeam) then   ! Allow bigger slop for first 10 blocks 
            ilim1=nchan(1)/2-iclip1 ! since we don't know where the mean is yet
C            ilim1=rmean(ibm)-iclip1 ! ihs 20000229 change back
            blim1=rmean(ibm)-10.*bclip1
            ilim2=nchan(1)/2+iclip1
            blim2=rmean(ibm)+10.*bclip1
         else
            ilim1=nchan(1)/2-iclip1
            blim1=rmean(ibm)-bclip1
            ilim2=nchan(1)/2+iclip1
            blim2=rmean(ibm)+bclip1
         endif

c     Check zero DM sum
         if(((iclip1.gt.0).or.dzero).and.(.not.treed))then
            if(vverbose)write(*,*)'Checking zero DM sum'
            jsum=0
            sumsqnc = 0
            do j=1,samp_blk1
               jk=(j-1)*nbyte_smp1
               isum=0
               do k=1,nbyte_smp1
                  isum=isum+nbit(dblk1(jk+k))
               enddo
               dedat(j) = isum  !cM dedisp data
               sumsqnc = sumsqnc +isum*isum
               jsum=jsum+isum
            enddo
            meannc = float(jsum)/samp_blk1
            rmsnc= sqrt((sumsqnc-meannc*meannc*samp_blk1)/samp_blk1)
            jsum = 0
            sumsq = 0
            ncl=0
            if(iclip1.gt.0)then
               do j=1,samp_blk1
                  jk=(j-1)*nbyte_smp1
                  if(dedat(j).lt.ilim1 .or. dedat(j).gt.ilim2)then
                     do k=1,nbyte_smp1 ! Clip
                        dblk1(jk+k)=bcl1
                     enddo
C                     dedat(j)=rmean(ibm) ! Set equal to mean
                     dedat(j)=nchan(1)/2 ! Set equal to mean
                     ncl=ncl+1
                  endif
                  sumsq = sumsq +dedat(j)*dedat(j)
                  jsum=jsum+dedat(j)
               enddo
            endif

c Get mean and rms
         
            blkmn=float(jsum)/samp_blk1
            rms = sqrt((sumsq - blkmn*blkmn*samp_blk1)/samp_blk1)
            if(nblk.le.nbeam) then     
               rrms(ibm) = rms
               rmean(ibm) = blkmn
            else
               rmean(ibm) = (rmean(ibm)*(1.0-mnwgt) + mnwgt*blkmn)
               rrms(ibm) = (rrms(ibm)*(1.0-mnwgt) + mnwgt*rms)
C               rmean(ibm) = (rmean(ibm)*((nblk-1)/nbeam) + blkmn)/
C     +              ((nblk-1)/nbeam+1)
C               rrms(ibm) = (rrms(ibm)*((nblk-1)/nbeam) + rms)/
C     +              ((nblk-1)/nbeam+1)
            endif
c            write(96,*) ((nblk-1)/nbeam),ibm,meannc,rmsnc,blkmn,rms,
c     +           rmean(ibm),rrms(ibm)

c  Reset whole block if mean is more than bclip1 from expected mean or
c  rms is noticeably larger than normal.
c  Assumes rmss from first few blocks are typical.

            if(iclip1.gt.0)then
               if(blkmn.lt.blim1 .or. blkmn.gt.blim2.or. 
     +              rms.gt.rrms(ibm)*1.14 )then ! 1.14 is empirical
                  if(verbose)write(*,*)' Blk1 clip:',
     +                 nblk,ibm,blkmn,bclip1,blim1,blim2
                  do j=1,samp_blk1
                     jk=(j-1)*nbyte_smp1
                     do k=1,nbyte_smp1
                        dblk1(jk+k)=bcl1
                     enddo
                     dedat(j) = rmean(ibm)
                  enddo
                  nbclip1(ibm)=nbclip1(ibm)+1
                  blkmn = rmean(ibm)
                  rms = rrms(ibm)
               else
                  nclip1(ibm)=nclip1(ibm)+ncl
               endif
            endif
         endif   
         
         if(wdat)then
           ibunit = ibm + 20
           if(vverbose) write(*,*)'Write blk to unit ',ibunit
           if(dzero.and.(wbeam(ibm).gt.0)) then                           
              write(ibunit)(dedat(j),j=1,samp_blk1)
           else
              
cM          Linearize in frequency repacking 3 blocks into 4 block
cM          factors of 4/3 inserted to expand things to 128 chans
              if(unbnd) then 
                 b39 = mod(nblk,nbeam*3)
                 if(b39.eq.0) b39 = nbeam*3
                 b13 = mod(b39,nbeam)
                 if(b13.eq.0) b13 = nbeam
                 b03 = (b39-1)/nbeam + 1
                 stride = 4*((b03-1)*datl/3)
                 if(wbeam(b13).gt.0)then
                    call ubd128(dblk1,dblk4,samp_blk1)
                    do j = 1,(4*datl)/3
                       dblk3(b13,stride+j) = dblk4(j)
                    enddo
                 endif
cM          When have read nbeam*3 blocks, we have enough to write out the data
cM             in 4 by 49152 bytes blocks
                 if(b39.eq.nbeam*3) then
                    do i = 0,3 
                       do pmb = 1,nbeam
                          if(wbeam(pmb).gt.0)write(pmb+20)
     +                        (dblk3(pmb,j),j=i*datl+1,(i+1)*datl)
                       enddo
                    enddo
                 endif
              else
                 if(wbeam(ibm).gt.0)
     +                write(ibunit)(dblk1(j),j=1,nbyte_blk1)
              endif
           endif
        endif
      endif

c  Transfer filter 2 data
      if(lsys(2))then

c Move data to local array
         do j=1,grp_blk
            jj=(j-1)*nbyte_grp + nbyte_grp1
            jk=(j-1)*nbyte_grp2
            do k=1,nbyte_grp2
               dblk2(jk+k)=block(headl+jj+k)
            enddo
         enddo

c Kill any channels if required
         if(kss(2).ne.0)then
            do j=1,samp_blk2
               jk=(j-1)*nbyte_smp2
               do k=1,nbyte_smp2
                  dblk2(jk+k)=and(dblk2(jk+k),kmask2(k,kss(2)))
                  dblk2(jk+k)=or(dblk2(jk+k),kmask3(k,kss(2)))
               enddo
            enddo
         endif

C Set clipping limits now as for filter 1

         if(nblk.le.nbeam) then      ! Start off at nominal
            rmean(ibm) = imean2   
         endif
         if(nblk.le.10*nbeam) then   ! Allow bigger slop for first 10 blocks 
            ilim1=nchan(2)/2-iclip2 ! since we don't know where the mean is yet
            blim1=rmean(ibm)-10.*bclip2
            ilim2=nchan(2)/2+iclip2
            blim2=rmean(ibm)+10.*bclip2
         else
            ilim1=nchan(2)/2-iclip2
            blim1=rmean(ibm)-bclip2
            ilim2=nchan(2)/2+iclip2
            blim2=rmean(ibm)+bclip2
         endif

c  Check zero DM sum
         if(((iclip2.gt.0).or.dzero).and.(.not.treed))then
            jsum=0
            sumsqnc = 0
            do j=1,samp_blk2
               jk=(j-1)*nbyte_smp2
               isum=0
               do k=1,nbyte_smp2
                  isum=isum+nbit(dblk2(jk+k))
               enddo
               dedat(j) = isum  !c need to compare to clip later on
               sumsqnc = sumsqnc +isum*isum
               jsum=jsum+isum
            enddo
            meannc = float(jsum)/samp_blk2
            rmsnc= sqrt((sumsqnc-meannc*meannc*samp_blk1)/samp_blk2)
            jsum = 0
            sumsq = 0
            ncl=0
            if(iclip2.gt.0 .) then 
               do j=1,samp_blk2
                  jk=(j-1)*nbyte_smp2
                  if(dedat(j).lt.ilim1 .or. dedat(j).gt.ilim2)then
                     do k=1,nbyte_smp2 ! Clip
                        dblk2(jk+k)=bcl1
                     enddo
                     dedat(j)=rmean(ibm) ! Set equal to mean
                     ncl=ncl+1
                  endif
                  sumsq = sumsq +dedat(j)*dedat(j)
                  jsum=jsum+dedat(j)
               enddo
            endif

c     Get mean and rms
         
            blkmn=float(jsum)/samp_blk2
            rms = sqrt((sumsq - blkmn*blkmn*samp_blk2)/samp_blk2)
            if(nblk.le.nbeam) then     
               rrms(ibm) = rms
               rmean(ibm) = blkmn
            else
               rmean(ibm) = (rmean(ibm)*(1.0-mnwgt) + mnwgt*blkmn)
               rrms(ibm) = (rrms(ibm)*(1.0-mnwgt) + mnwgt*rms)
            endif

c  Reset whole block if mean is more than bclip2 from expected mean or
c  rms is noticeably larger than normal.
c  Assumes rmss from first few blocks are typical.

            if(iclip2.gt.0)then
               if(blkmn.lt.blim1 .or. blkmn.gt.blim2.or. 
     +              rms.gt.rrms(ibm)*1.14 )then ! 1.14 is empirical 
                  if(verbose)write(*,*)' Blk2 clip:',
     +                 nblk,blkmn,bclip2,blim1,blim2
                  do j=1,samp_blk2
                     jk=(j-1)*nbyte_smp2
                     do k=1,nbyte_smp2
                        dblk2(jk+k)=bcl1
                     enddo
                     dedat(j) = imean2 
                  enddo
                  nbclip2(ibm)=nbclip2(ibm)+1
                  blkmn = rmean(ibm)
                  rms = rrms(ibm)
               else
                  nclip2(ibm)=nclip2(ibm)+ncl
               endif
            endif
         endif   

         if(wdat.and.(wbeam(ibm).ne.0))then
            ibunit = ibm + 33
            if(vverbose) write(*,*)'Write blk to unit ',ibunit
            if(dzero) then                           
               write(ibunit)(dedat(j),j=1,samp_blk2)
            else
               write(ibunit)(dblk2(j),j=1,nbyte_blk2)
            endif
         endif
      endif
      
 1020 format(f10.1)

      istat=0
      return
      end


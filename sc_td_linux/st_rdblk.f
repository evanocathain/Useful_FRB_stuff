      subroutine st_rdblk(istat)

c Read a new data block and check for errors

c $Log: st_rdblk.f,v $
c Revision 2.9  2004/07/03 07:36:26  rmanches
c Handle SCAMP files, >2GB files
c
c Revision 2.8  2004/03/15 11:44:53  pulsar
c Fixed bug preventing correct use of skip blocks option: AP
c
c Revision 2.7  2001/12/10 21:51:30  rmanches
c change (fix) error handling
c
c Revision 2.4  2000/02/23 06:13:35  rmanches
c update sc_tt and include dlt_extraskip in sct
c
c Revision 2.3  1998/12/01 03:53:25  pulsar
c repair_hdr modified to fix up bad LO on 28nov98.
c
c Revision 2.2  1998/05/16 06:33:52  rmanches
c Command line options changed - tape/file on end, no questions. More than one
c beam can be selected for writing. Block clipping now allows mean between nchan/2
c and nchan/2+beam_offset to allow for variable Tsys. Digitizer offsets now
c in file 'dititizer.offsets'.
c
c Revision 2.1  1998/04/23 23:22:57  jbell
c Dicks new v3_0 - added st_fclose.f st_fopen.f st_gethdr.f st_init.f st_inpar.f st_rdblk.f st_topen.f st_trans.f sc_td.inc - removed clops sc_cline.f sc_declare.f sc_fclose.f sc_fopen.f sc_format.f sc_gethead.f sc_grskip.f sc_preset.f sc_setpar.f sc_skip.f sc_tapeerr.f sc_td_files sc_topen.f sc_trans.f sc_unltape.f testbeamblk.f unbend.c README.multibeam
c

      implicit none
      include 'sc_td.inc'
      include 'sc_pmhead.inc'

      byte block(headl+datl), bcl1, bcl2, scmphd1, scmphd2, 
     :    sblock(datl)
      integer istat, j, k, nbs, sctread, sctskipr, istt, sctstate, 
     :    sctskipf, stfile, strec, iblk
      logical eotf,eoff
      data nerr/0/, bcl1/'55'x/, bcl2/'aa'x/

      equivalence (block,program_hd)

      if(nfill.eq.0)then
         if(diskf)then
            call sc_rblk(block,"r",diskfile(1:ldfn),istat,ldfn)
            if(istat.eq.0)then  ! EOF
               istat=1
               RETURN
            endif
         else
 10         istat=sctread(inunit,block,len)
            istt=sctstate(inunit,stfile,strec,eoff,eotf)
            if(eotf)then
               write(*,'('' At end of tape, nfile='',i6,'' nrec='',i6)')
     :              stfile,strec
               STOP
            endif
            if(eoff)then
               if(verbose)write(*,'('' Skipping EOF mark, nfile='',i6,
     :              '' nrec='',i6,'' istat='',i6)')stfile,strec,istat
               istt=sctskipf(inunit,1) 
            endif
            
            if(istat.eq.0)then  ! EOF
               istat=1
               RETURN
            endif
            
            if(istat.lt.0 .or. istat.ne.len)then ! Read error
               if(nblk.eq.0)then
                  write(*,'('' Error reading first block, istat:'',i6,
     +                 ''  len:'',i6)')istat,len
                  STOP
               endif
               nerr=nerr+1
               if(nerr.gt.10 .or. nblk.le.nbeam)then ! Too many errors or error on first block
                  istat=-1
                  RETURN
               endif
               go to 10         ! Try again
            endif
         endif

         nblk=nblk+1            ! Increment block counter

         if(nblk.eq.1)then      ! If first block, check data type
            scmphd1=block(1)
            scmphd2=block(2)
            if((scmphd1.eq.0 .and. scmphd2.eq.0) .or. 
     +           (mod(scmphd1+256,256).eq.182 .and. scmphd2.eq.109))then 
               scamp=.true.
               if(verbose)write(*,*)' Reading SCAMP data'
            else 
               scamp=.false.
               if(verbose)write(*,*)' Reading PM data'
            endif
         endif

         if(scamp)then
            nbeam=1
            ibeam=1
         else
            read(nbeam_hd,'(i4)')nbeam ! Number of beams
            read(ibeam_hd,'(i4)')ibeam ! Tape beam counter
         endif

         read(blk_cntr_hd,'(i8)')blk_cntr ! Tape blk cntr
         iblk=(blk_cntr-1)*nbeam+ibeam
         if(vverbose)write(*,*)nblk,blk_cntr,ibeam,nbeam,nbskip,iblk,
     +                        nblk+nbskip
         
         if (nblk.gt.1) nfill=iblk-nblk-(nbskip*nbeam) ! Number of missed blocks AP0304
         if(nfill.lt.0)then
            istat=-1                      ! Serious problem
         write(*,'('' Too much data written at block:'',i6)')nblk+nbskip
            RETURN
         else if(nfill.gt.0)then          ! Some blocks have been missed
         write(*,'('' Missed data at block:'',i6,''...recovering...'')')
     +             nblk+nbskip
            do j=1,datl                   ! Save current block data 
               sblock(j)=block(headl+j)
            enddo
            do j=1,datl,2           ! Fill dummy block
               k=headl+j
               block(k)=bcl1
               block(k+1)=bcl2
            enddo
         endif
      else
         write(*,'('' Missed data at block:'',i6,''...recovering...'')')
     +             nblk+nbskip
         if(nfill.gt.1)then
            do j=1,datl,2           ! Fill dummy block
               k=headl+j
               block(k)=bcl1
               block(k+1)=bcl2
            enddo
         else
            do j=1,datl         ! Get saved block 
               block(headl+j)=sblock(j)
            enddo
         endif
         nblk=nblk+1
         nfill=nfill-1
      endif

      if(nblk.eq.1)then        ! If first block, skip blocks if required
         if(nbskip.gt.0)then                               
            write(*,'('' Skipping'',i6,'' blocks'')')nbskip
            nbs=nbskip*nbeam
            if(diskf)then
               do j=1,nbs
                  call sc_rblk(block,"r",diskfile(1:ldfn),istat,ldfn)
               enddo
            else
               istat=sctskipr(inunit,nbs-1)  
               if(istat.lt.0)STOP ' Error skipping blocks'
               istat=sctread(inunit,block,len)              ! Read next block
            endif
         endif
      endif

      istat=0
      return
      end

         

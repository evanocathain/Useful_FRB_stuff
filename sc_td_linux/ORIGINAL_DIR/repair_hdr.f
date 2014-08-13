      subroutine repair_hdr

      implicit none

c     to repair headers that have incorrect things written in by scamp
c     JFB, MXB  9 July 1993
*     LN 11/11/93 - Fix wrong "blk_sec_hd" (usually in tree type data).
c     RNM  Aug 2, 1994.  Fix wrong UT dates in SCAMP V3.0 hdr
c     RNM Feb 15, 1995. Fix frch1 for MJD 49762
c     RNM March 23, 1995. Fix freq for 70cm obs 13/3/95
c     RNM June 5, 1995. Fix freq for '20cm' obs 30 May, 1995.
cM    JFB June 6, 1996. Put pmhead in instead of sc_head

      include 'sc_td.inc'
      include 'sc_pmhead.inc'

      real dec_pl1,dec_pl2,dec_pl3, bsec,smpint
      integer istat,ngrps,nsmpg,iy,im,id, j
	real*8 dmjd,fd,fr
*--
	read(mjd_hd,'(i8)')mjd
	call str_coord(ut_strt_hd,'T6',ut_strt,istat)
	dmjd=mjd+ut_strt
      READ (BLK_SEC_HD,*) bsec
      READ (SAMP_GRP_HD(1),*) nsmpg
      READ (GRP_BLK_HD,*) ngrps
      READ (SAMP_INT_HD(1),*) smpint
*
      IF(NINT(bsec*1000./smpint) .NE. ngrps*nsmpg) THEN
        WRITE(BLK_SEC_HD,'(F8.6)') ngrps*nsmpg*smpint/1000.
        WRITE(6,*) 'Wrong "blk_sec_hd"! Corrected.'//CHAR(7)
      ENDIF
c    correct jump in ut_strt that occured ~June 1993 
      read(ut_strt_hd(10:16),'(f7.6)')dec_pl1
      read(ut_hd(10:16),'(f7.6)')dec_pl2
      if(dec_pl1.ne.0.000000)then
        write(*,*)'Errors in ut_strt_hd, ut_hd Correcting....'
        write(ut_strt_hd(11:16),'(a6)')'000000'
        dec_pl3 = dec_pl2 - dec_pl1
        write(ut_hd(10:16),'(f7.6)')dec_pl3
      endif
*
	if(version_hd.eq.' 3.0')then
	  if(ut_strt.gt.14.d0/24.d0)then	  
	    write(*,*)'Correcting date (V3.0)'
	    call sla_djcl(dmjd,iy,im,id,fd,istat)
            if(iy.ge.2000)then
               iy=iy-2000
            else
               iy=iy-1900
            endif
	    write(date_hd,'(i2.2,''/'',i2.2,''/'',i2.2)')iy,im,id
	  endif
	endif

c Correct for start time errors if requested
        if(toff)then
           do j=1,ntoff
              if(abs(dmjd - toffmjd(j)) .lt. 0.001d0)then
                 if(verbose)write(*,'(''  Correcting UT start, MJD:'',
     +             f10.3,'' by'',f10.6,'' sec'')')toffmjd(j),toffset(j)
                 ut_strt = ut_strt + toffset(j)/8.64d4
                 if(ut_strt.lt.0.d0 .or. ut_strt.ge.1.d0)then
                    if(ut_strt.lt.0.d0)then
                       ut_strt = ut_strt + 1.d0
                       mjd = mjd - 1
                    else
                       ut_strt = ut_strt - 1.d0
                       mjd = mjd + 1
                    endif
                    dmjd = mjd + ut_strt
                    call sla_djcl(dmjd,iy,im,id,fd,istat)
                    if(iy.ge.2000)then
                       iy=iy-2000
                    else
                       iy=iy-1900
                    endif
                    write(date_hd,'(i2.2,''/'',i2.2,''/'',i2.2)')
     +                   iy,im,id
                    write(mjd_hd,'(i8)')mjd
                 endif
                 call coord_str(ut_strt,'T6',ut_strt_hd,istat)
                 if(istat.ne.0)then
                    write(*,'('' *** Coord_str error:'',f12.9)')ut_strt
                    STOP
                 endif
                 go to 20
              endif
           enddo
        endif
 20     continue

        if(mjd.eq.49574)then
           if(freq_chan1_hd(1).ne.'  1254.76000')then
              freq_chan1_hd(1)='  1254.76000'
              write(*,*)'Corrected frch1(1) to 1254.76 MHz'
            endif
        endif

        if(mjd.eq.49762)then
	   read(freq_chan1_hd(1),'(f12.0)')freq_chan1(1)
           if(freq_chan1(1).gt.1000.0)then
              freq_chan1_hd(1)='  2097.50000'
              write(*,*)'Corrected frch1(1) to 2097.5 MHz'
            endif
        endif

        if(mjd.eq.49789.and.nchan_hd(1).eq.' 256')then
           freq_chan1_hd(1)='   451.93750'
	   chanbw_hd(1)='-0.12500'
           write(*,*)'Corrected frch1(1) to 451.9375 MHz'
        endif

        if(dmjd.gt.49866.d0.and.dmjd.lt.49868.15.and.
     :             nchan_hd(1).eq.'  64')then
           freq_chan1_hd(1)='  2477.76000'
	   chanbw_hd(1)='-5.0000'
           write(*,*)'Corrected frch1(1) to 2477.76 MHz'
        endif

        if(dmjd.gt.51145.0d0.and.dmjd.lt.51145.5)then
           freq_chan1_hd(1)='  1489.00000'
           write(*,*)'Corrected frch1(1) to 1489.00 MHz'
        endif

        if(dmjd.gt.51379.94.and.dmjd.lt.51380.1)then
           freq_chan1_hd(1)='  1489.00000'
           write(*,*)'Corrected frch1(1) to 1489.00 MHz'
        endif

        if(dmjd.gt.52686.0.and.dmjd.lt.52687.5)then
           freq_chan1_hd(1)='  1489.00000'
           write(*,*)'Corrected frch1(1) to 1489.00 MHz'
        endif

        if(dmjd.gt.52687.5.and.dmjd.lt.5269.6)then
           freq_chan1_hd(1)='  1514.90580'
           write(*,*)'Corrected frch1(1) to 1514.9058 MHz'
        endif

        if(dmjd.gt.52826.0.and.dmjd.lt.52826.75)then
           read(freq_chan1_hd(1),*) fr
           fr = fr - 28.872208
           write(freq_chan1_hd(1),'(f12.5)') fr
           write(*,*) 'Corrected frch1(1) to'//
     &              freq_chan1_hd(1)//' MHz'
        endif

c observations done after using the correlator @20 cm.
c probably not rerun lorun. MBy 240204

        if(dmjd.gt.52988.810.and.dmjd.lt.52989.050)then
           if(freq_chan1_hd(1).eq.'  1804.50000')then
              freq_chan1_hd(1)='  1943.50000'
              write(*,*)'Corrected frch1(1) to 1943.50 MHz'
           endif
        endif

        if(dmjd.gt.53057.470.and.dmjd.lt.53057.690)then
           if(freq_chan1_hd(1).eq.'  1804.50000')then
              freq_chan1_hd(1)='  1949.50000'
              write(*,*)'Corrected frch1(1) to 1949.50 MHz'
           endif
        endif

        if(dmjd.gt.53071.3.and.dmjd.lt.53071.68)then
           if(freq_chan1_hd(1).eq.'  1804.50000')then
              freq_chan1_hd(1)='  2007.50000'
              write(*,*)'Corrected frch1(1) to 2005.00 MHz'
           else if(freq_chan1_hd(1).eq.'  1262.25000')then
              freq_chan1_hd(1)='  1230.25000'
              write(*,*)'Corrected frch1(1) to 1230.25 MHz'
           endif
        endif

        if(dmjd.gt.53075.58.and.dmjd.lt.53075.735)then
           if(freq_chan1_hd(1).eq.'  1804.50000')then
              freq_chan1_hd(1)='  1949.50000'
              write(*,*)'Corrected frch1(1) to 1949.50 MHz'
           endif
        endif

        if(dmjd.gt.53151.198.and.dmjd.lt.53151.390)then
           if(freq_chan1_hd(1).eq.'   711.87500')then
              freq_chan1_hd(1)='   716.87500'
              write(*,*)'Corrected frch1(1) to 716.875 MHz'
           endif
        endif
        
           

      return
      end




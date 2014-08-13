	subroutine st_fopen(istat)

c Open output files 

c $Log: st_fopen.f,v $
c Revision 2.6  2004/07/03 07:36:26  rmanches
c Handle SCAMP files, >2GB files
c
c Revision 2.5  2003/12/24 03:49:10  rmanches
c Fix dual-band operation
c
c Revision 2.4  2001/07/23 02:29:01  rmanches
c linux changes
c
c Revision 2.3  1998/05/21 06:49:25  rmanches
c Don't write .snoop if -D
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
	
	integer istat, iunit, ltm, gpb, gpl, j, k
	real*8 dl, db
	character tfile*3, fname*16, hdr*(headl)

	equivalence (hdr,program_hd)


c  Write ID line
	if(nblk.eq.1) then
	   j=index(pname_hd,' ')-1
	   if(date_hd(7:7).eq.' ')then
	      k=6
	   else
	      k=8
	   endif
	   write(*,100)tlabel(1:ltn),infile,file_cntr_hd,pname_hd(1:j),
     +  	date_hd(1:k),ut_strt_hd(1:11)
	endif
 100	format(/' Tape: ',a,'  File:',i4,'  Hdr File:',a,'  Pname: ',a,
     +    '  Date: ',a,'  UT:',a)
	  
c For SCAMP disk files, use file counter to label pointings
	if(scamp.and.diskf)then
	   scfile=scfile+1
	   infile=scfile
	   scblk=0
	endif

	if(verbose)
     +    write(*,*)'Start:',mjd_hd,ut_strt_hd,blk_cntr_hd,blk_sec_hd

c  Open files
	write(tfile,'(i3.3)')infile
	fname=tlabel(1:ltn)//'_'//tfile
	ltm=ltn+4
	
	mbfile(ibm)=fname(1:ltm)
	if(nbeam.gt.1)then
	   ltm=ltm+1
	   write(mbfile(ibm)(ltm:ltm),'(z1)')ibm
	endif

	if(pmtape .and. nbeam.eq.13)then
	   db = 0.46667/2.0*sin(60.0*0.01745329)
	   dl = 0.46667/2.0
	   read(gl_strt_hd,'(f8.4)')gl_strt
	   read(gb_strt_hd,'(f8.4)')gb_strt
	   if((gl_strt.gt.180).and.(gl_strt.le.360))
     +  	gl_strt=gl_strt-360.0
	   gpb = 500 + nint(gb_strt/db)
	   gpl = 5000 + nint(gl_strt/dl - 0.5*mod(gpb,2))
	   ngid(ibm) = gpl*1000+gpb
	   mbfile(ibm)=mbfile(ibm)(1:ltm)//'%'
	   write(mbfile(ibm)(ltm+2:ltm+8),'(i7)')ngid(ibm)
	   ltm=ltm+8
	endif


	ltf=ltm+1
	if(wbeam(ibm).ne.0)then
	   if(wdat.and.lsys(1))then
	      mbfile(ibm)=mbfile(ibm)(1:ltf-1)//'1'
	      iunit=20+ibm
	      open(unit=iunit,file=mbfile(ibm)(1:ltf)//'.hdr', 
     1   	      form='unformatted',status='unknown')
	      write(iunit)hdr
	      close(iunit)
	      open(unit=iunit,file=mbfile(ibm)(1:ltf)//'.tmp',
     1   	      form='unformatted',status='unknown')
	      write(*,'('' Opened files for '',a)')mbfile(ibm)(1:ltf)
	   endif
	   if(wdat.and.lsys(2))then
	      mbfile(ibm)=mbfile(ibm)(1:ltf-1)//'2'
	      iunit=33+ibm
	      open(unit=iunit,file=mbfile(ibm)(1:ltf)//'.hdr',
     1	        form='unformatted',status='unknown')
	      write(iunit)hdr
	      close(iunit)
	      open(unit=iunit,file=mbfile(ibm)(1:ltf)//'.tmp',
     1  	      form='unformatted',status='unknown')
	      write(*,'('' Opened files for '',a)')mbfile(ibm)(1:ltf)
	   endif
	
c    Dump the pname to a file so scripts can snoop for pmsurv grid points
	   if(nblk.eq.1 .and. pmtape .and. wdat) then
	      open(unit=10,file=mbfile(ibm)(1:ltf)//'.snoop',
     1	         status='unknown')
	      write(10,'(a)')pname_hd(2:16)
	      close(10)
	   endif
	endif

	if(nblk.eq.nbeam) write(*,'(/)')

	istat=0
	return
	end


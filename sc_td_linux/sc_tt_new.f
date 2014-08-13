	program sc_tt

c  Copies Pks SCAMP survey tape to tape (MT, GS or EX)
c  RNM  April, 1990
c  RNM  May 5, 1991.  Copes with INIT file. No chance.
c  DF-SJ May 7, 1991. Really copes with INIT files. But not a new tape.
c  RNM  May 30, 1991. INIT file on new o/p tape. Fix skip logic
c  RNM  May 30, 1991. Sun version
c  BMG  Dec 14, 1993  Now writes over the previous row when summarising progress
c                     every 10 blocks. Also gives running total of number of
c                     blocks and megabytes used up so far on output tape.

	implicit none

	byte block(49792)
	integer*4 istat,nskip,outfile,infile,nread,nblk,l1,l2,length,
     :    len,nerr,tlu1,tlu2,n_read,sctopen,sctclose,sctread,
     :    sctwrite,sctrewind,sctskipf,totblks,istt,stfile,strec,
     :    sctstate,sctdltskip,kskip
	real totmegs
	character ans*1,tplb1*4,tplb2*4,uprow*3,intape*40,outtape*40
	logical scamp, dlt, eotf, eoff

	include 'sc_pmhead.inc'
	equivalence(program_hd,block)

	tlu1=1
	tlu2=2
	len=49792
	
	totblks = 0
	totmegs = 0
	uprow=char(27)//'[A'

	write(*,'('' Input tape unit? : ''$)')
	read(*,110)tplb1
	intape="/dev/rmt/"//tplb1
	l1=length(intape)
	write(*,'('' Is input tape a DLT? : ''$)')
	read(*,110)ans
	if(ans.eq.'y'.or.ans.eq.'Y')then
	   dlt=.true.
	else
	   dlt=.false.
	endif

	write(*,'('' Output tape unit?: ''$)')
	read(*,110)tplb2
	outtape="/dev/rmt/"//tplb2
	l2=length(outtape)


	istat=sctopen(tlu1,intape(1:l1))
	if(istat.lt.0)go to 900
	istat=sctopen(tlu2,outtape(1:l2))
	if(istat.lt.0)go to 902

	write(*,'('' Are input tapes SCAMP format? (y/n): ''$)')
	read(*,110)ans
	if(ans.eq.'y'.or.ans.eq.'Y')then
	   scamp=.true.
	else
	   scamp=.false.
	endif

	outfile=1
	write(*,'('' Number of files to skip on output tape: ''$)')
	read(*,*)kskip
	write(*,'('' Number of input files to skip: ''$)')
	read(*,*)nskip
	write(*,'('' Number of input files to read: ''$)')
	read(*,*)nread
	if(nread.eq.0)nread=9999
	n_read=0

	if(kskip.gt.0)then
	   write(*,100)kskip
	   istat=sctskipf(tlu2,kskip)	
	   if(istat.lt.0)go to 920
	   outfile=outfile+kskip
	endif

c  New input tape
10	if(scamp)then
	   istat=sctread(tlu1,block,len)
	   if(istat.le.0)go to 912
	   if(block(1).eq.0)then
	      write(*,'('' Skipping INIT file on input tape'')')
	      istat=sctskipf(tlu1,1)	
	      if(istat.lt.0)go to 912
	   else
	      write(*,'('' No INIT file - rewinding'')')
	      istat=sctrewind(tlu1)
	      if(istat.lt.0)go to 912
	   endif
	endif

	infile=1


 15	if(nskip.gt.0)then
	  write(*,'('' Skipping'',i4,'' files on input tape'')')nskip
	  istat=sctskipf(tlu1,nskip)
	  if(istat.lt.0)go to 920
	endif

	infile=infile+nskip
	nerr=0
	nblk=0

c  Read next block
20	istat=sctread(tlu1,block,len)
	istt=sctstate(tlu1,stfile,strec,eoff,eotf)

	if(eotf)go to 40

	if(eoff)then
c  Reset EOF flag
	  istt=sctskipf(tlu1,1)
	  if(istt.lt.0)go to 920
	  if(dlt)then  ! Do extra one
	     istt=sctdltskip(tlu1)
	     if(istt.lt.0)go to 920
	  endif
	endif

	if(istat.eq.0)then
c  EOF
	  istat=sctclose(tlu2)
	  if(istat.lt.0)go to 930
	  write(*,'('' EOF written on output tape'')') 
	  istat=sctopen(tlu2,outtape(1:l2))
	  if(istat.lt.0)go to 902

	  totblks = totblks + nblk
	  totmegs = 1./(1024.*1024.) * totblks * len 
	  write(6,120)infile,outfile,nblk,totblks,totmegs
	  infile=infile+1
	  outfile=outfile+1
	  n_read=n_read+1

	  if(n_read.ge.nread)then
	     write(*,'('' More from this tape? (y/n): ''$)')
	     read(*,110)ans
	     if(ans.eq.'y'.or.ans.eq.'Y')then
		goto 15
	     else
		go to 40
	     endif
	  else
	     nblk=0
	     go to 20
	  endif
	else
	  if(istat.ne.len)then
	    write(*,140)nblk,istat
	    nerr=nerr+1
	    write(*,'('' Read error. Try another read? (y/n): ''$)')
	    read(*,110)ans
	    if(ans.ne.'y'.and.ans.ne.'Y')then
	      write(*,'('' Write file mark on output tape? (y/n): ''$)')
	      read(*,110)ans
	      if(ans.eq.'y'.or.ans.eq.'Y')then
	        istat=sctclose(tlu2)
	        if(istat.lt.0)go to 930
	        istat=sctopen(tlu2,outtape(1:l2))
	        if(istat.lt.0)go to 930
	        write(*,'('' EOF written'')')

	  	totblks = totblks + nblk
		totmegs = 1./(1024.*1024.) * totblks * len 
	        write(6,120)infile,outfile,nblk,totblks,totmegs
	        infile=infile+1
	        outfile=outfile+1
	      endif
	      write(*,'('' Continue with next input file? (y/n): ''$)')
	      read(*,110)ans
	      if(ans.eq.'y'.or.ans.eq.'Y')then
	        istat=sctskipf(tlu1,1)
	        if(istat.lt.0)go to 920
		nblk=0
	        go to 20
	      else
	        go to 40
	      endif
	    endif
	    go to 20
	  endif

c  write record to output
	  istat=sctwrite(tlu2,block)
	  if(istat.le.0)go to 950
	  nblk=nblk+1
	  if(nblk.eq.1)write(6,150)infile,file_cntr_hd,ut_strt_hd(1:11)
	  if(mod(nblk,10).eq.0)write(6,160)uprow,nblk,ut_hd(1:9),ra_hd(1:9),
     :      dec_hd(1:9)
	  go to 20
	endif

 40	write(*,'('' Rewinding input tape'')')
	istat=sctrewind(tlu1)
	if(istat.lt.0)go to 940
	istat=sctclose(tlu1)
	if(istat.lt.0)go to 940
	write(*,'('' Another tape? (y/n): ''$)')
	read(*,110)ans
	if(ans.eq.'y'.or.ans.eq.'Y')then
	   write(*,'('' Number of input files to skip: ''$)')
	   read(*,*)nskip
	   write(*,'('' Number of input files to read: ''$)')
	   read(*,*)nread
	   if(nread.eq.0)nread=9999
	   n_read=0
 41	   write(*,'('' CR when tape ready: ''$)')
	   read(*,*)
	   istat=sctopen(tlu1,intape(1:l1))
	   if(istat.lt.0)then
	      write(*,'('' Tape not ready'')')
	      go to 41
	   endif
	   go to 10
	endif

c       Rewind output tape
	write(*,'('' Rewinding output tape'')')
	istat=sctrewind(tlu2)
	if(istat.lt.0)go to 930
	write(*,130)nerr
	STOP

900	write(6,'('' Error opening '',a'', status:'',i6)')intape(1:l1),istat
	stop
902	write(6,'('' Error opening '',a,'', status:'',i6)')
     :       outtape(1:l2),istat
	stop
910	write(6,'('' Error writing INIT file, status:'',i6)')istat
	stop
912	write(6,'('' Error skipping INIT file, status:'',i6)')istat
	stop
920	write(6,'('' Error skipping file, status:'',i6)')istat
	stop
930	write(6,'('' Error writing EOF, status:'',i6)')istat
	stop
940	write(6,'('' Error closing input tape, status:'',i6)')istat
	stop
950	write(6,'('' Error writing file, status:'',i6)')istat
	stop

100	format(/' Skipping',i4,' files on output tape')
110	format(a)
120	format(' End of infile',i4,', outfile',i4,',',i8,' blocks;'/
     +         ' Total copied to output tape so far: ',i8,' blocks,', 
     +         f9.2,' MB')
130	format(' End of job,',i4,' read errors')
140	format(' Error reading block',i8,',  block length =',i8/)
150	format(/' File:',i5,'  Header File Nr:',a,'  Start UT:',a/)
160	format(a,' Block:',i8,'  UT:',a,'  RA:',a,'  Dec: ',a)
	end



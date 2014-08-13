      program sc_td

c  Reads SC data from tape to disk.
c  Creates header files (ttttnnns.hdr) and data files (ttttnnns.dat)
c  Includes data clip facility

c $Log: sc_td.f,v $
c Revision 2.26  2004/07/03 07:36:25  rmanches
c Handle SCAMP files, >2GB files
c
c Revision 2.25  2002/09/05 09:01:01  ghobbs
c Mods for Linux - Cobra
c
c Revision 2.24  2000/08/19 06:22:01  pulsar
c AGL mods to kill.chan, RTE add naskip to disk topen
c
c Revision 2.23  2000/02/23 06:13:34  rmanches
c update sc_tt and include dlt_extraskip in sct
c
c Revision 2.22  2000/02/04 06:29:16  istairs
c IHS/AGL Changed kill-channel filler to eliminate Nyquist birdies.  Also changed sample and block clipping to be guided by rms and running means of channels instead of the now-defunct digitizer offsets.
c
c Revision 2.21  2000/01/30 23:53:30  pulsar
c RTE fixed broken DLT behaviour on suns (since AP's alpha mods). Compiled successfully but not tested on alphas -- should be correct though.
c
c Revision 2.20  2000/01/30 22:39:59  pulsar
c RTE: oops fixed cosmetic error in -S output
c
c Revision 2.19  2000/01/30 22:36:29  pulsar
c RTE: added -S flag to skip search obs
c
c Revision 2.18  1999/08/09 06:59:05  pulsar
c AP 09/08/1999:FIRST UNIFIED ALPHA+SUN4SOL VERSION.
c Ubd128 linearizing file splitted in two versions and restored endian.h
c to account for endianess. Ubd128_sun4sol.c previously tested at Epping,
c ubd182_OSF1.c previously tested at Bologna.
c Slight changes in st_inpar.f for preventing errors in reading kill.chans
c with alpha-machines. Fully restored capability of reading from a disk.
c Removed a couple of obsolete files: ubd128.c and sct_alpha.f.
c
c Revision 2.17  1999/07/15 14:12:56  pulsar
c Automated killing of chans from file kill.chans. -x inhibits - AGL
c
c Revision 2.15  1999/03/05 22:22:01  pulsar
c RNM: fix RA, Dec print
c
c Revision 2.14  1999/03/05 19:40:29  pulsar
c RNM: Added -t, -I options
c
c Revision 2.13  1998/07/07 01:41:31  rmanches
c Changed samp_blk back to grp_blk
c
c Revision 2.12  1998/05/21 04:38:42  rmanches
c Fixed beam select with linearize on.
c Check input options and check that tape/file name entered.
c Compute digitiser offsets and write summary file when not writing to disk.
c Don't rewind Exabyte if naskip != 0.
c Don't read in digitiser offsets if clip =0.
c Changed default to no summary file. Altered summary and file skip keys.
c
c Revision 2.11  1998/05/16 06:33:49  rmanches
c Command line options changed - tape/file on end, no questions. More than one
c beam can be selected for writing. Block clipping now allows mean between nchan/2
c and nchan/2+beam_offset to allow for variable Tsys. Digitizer offsets now
c in file 'dititizer.offsets'.
c
c Revision 2.10  1998/04/23 23:22:51  jbell
c Dicks new v3_0 - added st_fclose.f st_fopen.f st_gethdr.f st_init.f st_inpar.f st_rdblk.f st_topen.f st_trans.f sc_td.inc - removed clops sc_cline.f sc_declare.f sc_fclose.f sc_fopen.f sc_format.f sc_gethead.f sc_grskip.f sc_preset.f sc_setpar.f sc_skip.f sc_tapeerr.f sc_td_files sc_topen.f sc_trans.f sc_unltape.f testbeamblk.f unbend.c README.multibeam
c

c  RNM     17 Sep 1991
c  SXJ AGL 21 Sep 1991 Clip whole block if mean offset
c  RNM     08 Oct 1991 Clean up & print clip data.
c  RNM     09 Oct 1991 Convex version
c  AGL     14 Oct 1991 Replace clipped blocks with alternate 0s and 1s in time
c  RNM     16 Mar 1992 Machine independent version. OK for old PS data.
c  AGL     07 Nov 1992 Deal with on-line de-dispersed data.
c  DRL     04 Feb 1993 Creates the data file as a .tmp when loading
c                      which is then moved to a .dat as in tape_master
c                      also can be controlled by a slave driver which writes
c                      a STOP_SC_TD file into the scratch area which this
c                      program now checks for, if it finds one then it sleeps
c                      for 5 minutes...
c  BMG     16 Dec 1993 Now writes over the previous row when summarising 
c                      progress every 10 blocks, and prints total blocks 
c                      read in once file has completely read in. 
c  RNM     25 Jun 1994 Set samp_grp_hd(2) and samp_int_hd(2) so OK for 
c                      dual-band data and old System 1 data.
c  JFB     27 Jun 1994 Add option to read whole tape
c  JFB     30 Jun 1994 Create summary file when reading whole tape
c  VMK     16 Jul 1994 Write nbits back in header if required
c  JFB     01 Aug 1994 If sc_td is killed while reading the whole tape
c                      it will continue from where it left off if you 
c                      restart it reading the whole tape.
c  RNM     02 Aug 1994 Installed in $ID$ $AUTHOR$ $DATE$ $LOG$
c
cM JFB     01 JAN 1997 Hive off stuff into the following sections
cM                     sc_declare.f - Declarations and data statements
cM                     sc_skip.f    - Skip requested files and blocks
cM                     sc_fclose.f  - Write headers, close and rename files
cM                     sc_unltape.f - Check for more files or unload tape
cM                     sc_tapeerr.f - Check for tape errors
cM                     sc_gethead.f - Get header parameters
cM                     sc_fopen.f   - Open Files
cM                     sc_trans.f   - Transfer data to disk and clipping
cM                     sc_format.f  - Format statements
cM
cM JFB     01 JAN 1997 Use rename instead of system(mv) to move .tmp -> .dat
cM                     Use new pmdaq header sc_mbhead.inc 
cM                     Rework to allow reading of old data with new header
cM                     ie, fudge samp_grp_hd, grp_blk_hd in sc_gethhead.f
cM                     New mbeam file naming convention + open nbeam files
cM                     Add new defaults and fix others that did not work
cM                     Write fake multibeam data from normal data
cM JFB     22 Jan 1997 Open accu file and write header info
cM JFB     28 Feb 1997 Write beam no as hex digit in place of system no
cM JFB     03 Mar 1997 Use _ and % as name dividers, move beam number to middle
cM                     Split verbose into verbose in main and vverbose below
cM                     Write samp_blk_hd and nblk_read_hd to .accu
cM                     Do not write the tape name inside the summary file
cM JFB     07 MAR 1997 Use next definition of header
cM JFB     12 MAR 1997 First tests and bug fixes with mbeam data
cM                     rearrange definition of grp_blk
cM                     samp_grp(1) = 1
cM                     samp_grp(2) = 1
cM                     samp_blk    = (eg 4096)
cM                     grp_blk     = (eg 4096)
cM JFB     13 MAR 1997 Next definition of header
cM                     In particular cp data_hd(2) -> data_hd(1) for scamp data
cM JFB     16 APR 1997 Add system to end all file names
cM JFB     24 APR 1997 move sc_mbhead.inc -> sc_pmhead.inc to unify naming 
cM JFB     10 May 1997 Fix bug in the number of blks written to the header
cM                     write ntape*6 to .accu rather than ntape*4
cM                     moved 200 from sc_fclose to sc_format
cM                     Restructure to close file before skipping to EOF
cM                     Bring all gotos and numbered statements to main routine
cM                     write out ascii accu files for debugging purposes
cM JFB     16 May 1997 Add an option to provide dedispersed data at zero DM
cM                     Put sc_td in a separate directory 
cM                     Makefile now in Epping format
cM                     Get rid of "another tape" option now we have command line
cM                     Remove label 10 from tape open call
cM                     Move tape opening functions to sc_topen.f
cM                     Split off filter settings and open sumfile -> sc_preset.f
cM JFB     20 May 1997 New name for PMD in data files test for data type
cM                     .zerodm extension for zerodm files
cM JFB     26 May 1997 Get rid of libraries dependancies (only 2 routines)
cM                     null length strings -> ' ' for alpha compile sc_setpar.f
cM JFB     31 May 1997 Write samp_blk to accu properly
cM                     Move accu writing to after header is update in sc_fclose
cM JFB                 Add option to stop skipping to eof
cM JFB NPFM 5 Jun 1997 Test comment for CVS purposes.
cM JFB     13 Jun 1997 Add an option allowing for preskipping files on the tape
cM                     Restructured topen a bit
cM JFB     18 Jul 1997 Fix EOF skipping bug by putting skeof block after fclose
cM                     Needed and extra call sc_skip with nskipf=1 to get past
cM                     the DLT EOF markers - added if(dlt) and questn in topen
cM                     Make default tape number 0
cM JFB     24 Jul 1997 Facility to add 32 fake channels to pmsurv data, bringing
cM                     it from 96 channels to 128 to ease 1bittree dedisp
cM                     buffer empdat(datl/3) - logical switch f128c
cM AGL,JFB 31 Jul 1997 Added facility for linearizing in frequency (unbend.c)
cM JFB     04 AUG 1997 Put L flag in tree_hd for linearised data
cM AGL,JFB 05 Aug 1997 Expand to 128 chans while linearizing
cM JFB     07 Aug 1997 Add calls to exstat to give status messages
cM                     Remove rewinding and unloading of tape commands
cM                     Changed goto 40 to goto 70
cM JFB     16 Aug 1997 Add option to read data from a disk file
cM                     Moved system number and skipto EOF questions
cM                     Commented out fakemb, fakesmb and add32chan questions  
cM JFB,VMK 28 Aug 1997 Recalculate length of hdr file names before reopen
cM JFB     28 Aug 1997 Revamped device name question - changed all sct routines
cM                     Correct nblk/13 bug for single beam data
cM                     Change logic to force propagate 0=none clipping
cM                     to production of dedisp data as well 
cM JFB     29 Aug 1997 Correct nblk/13 bug for single beam data for linear data
cM JFB                 Generalise 96 -> 128 chan packing to cope with any nbeam
cM JFB     23 Sep 1997 Correct beam and tape entries in accu files
cM JFB     26 Sep 1997 Correct pmsurv grid IDs, by calculating directly from l,b
cM JFB     20 Oct 1997 Get the file number from the header if reading disk files
cM JFB     24 Oct 1997 Get the tape name from the header
cM JFB     01 Nov 1997 Dump the pname to a file so scripts can snoop for pmsurv obs
c  RNM        Apr 1998 Rewrite to give command-line input, structure in subroutines, 
c                      fix file skip logic, fix file nameing, fix and improve clipping, 
c                      write only ascu files. 
c  RNM     07 Jul 1998 Change samp_blk back to grp_blk
c  VMK     14 Jul 1999 Add option of reading off only certain sources, with the
c                      -P option.  Source name has max 9 characters, and can
c                      handle at most 8 different sources.
c  AGL     15 Jul 1999 Facility to read channels for killing from kill.chans
C
c IHS/AGL  Jan 2000    Change clipping limits so that they are now based
C                      on the running mean of each channel.  Also check for
C                      extra-large block rmss and clip those, too.
C                      PLUS redo channel killing so individual channels are
C                      set to all 0s or all 1s to eliminate Nyquist birdies.
C
C GBH     01 July 2004 Mods to allow sc_td to read large (> 2G) files -
C                        changes made to sc_rblk.c

c  Unit numbers
c     10     - temporary files
c     11     - exstat 
c     21-33  - System 1 files
c     34-46  - System 2 files 

        implicit none

        include 'sc_td.inc'
        include 'sc_pmhead.inc'     !cM Header structure definitions

        byte block(headl+datl)
        integer istat, sctskipf, sctdltskip
        character uprow*3, hdr*(headl), utpr*9, rapr*9, decpr*9, 
     +     savhdr*(headl)
        logical st_goodpsr, goodpsr

        equivalence (block,hdr),(block,program_hd)

        uprow=char(27)//'[A'
        len=headl+datl


        call st_init

        call st_inpar(istat)
        if(istat.ne.0)stop

        if(verbose) write(*,*)' Entering st_topen'
        call st_topen(istat)
        if(istat.ne.0)stop

 10     call st_rdblk(istat)                    ! Start of reading loop
        if(istat.ne.0)go to 20                  ! EOF or tape error
        
        if(nblk.eq.1)call st_gethdr(istat)      ! Get header data

	if ((npsr.gt.0).and.(nblk.eq.1)) then   ! Check psr name.  VMK.
	   goodpsr = st_goodpsr(pname_hd,psr,npsr)
           if (.not.goodpsr)  then
	      write(*,*) 'File ', infile,' Source ',pname_hd,
     +	      ' not requested --> skip'
              istat=sctskipf(inunit,1)  ! Skip to EOF
              if(istat.lt.0)STOP 'Error skipping to EOF'
	      nblk = 0
	      infile = infile +1
	      goto 10
           endif
        endif

c redwards 31 Jan 00 option to skip many-beamed files
        if ((singlebeamonly).and.(nblk.eq.1).and.(nbeam.gt.1)) then
           write (*,'(''File '',i3,'' has '',i2,'' beams  --> skip'')'),
     +          infile, nbeam
              istat=sctskipf(inunit,1)  ! Skip to EOF
              if(istat.lt.0)STOP 'Error skipping to EOF'
	      nblk = 0
	      infile = infile +1
	      goto 10
         endif
           

        ibm=nblk - (nblk-1)/nbeam*nbeam   ! Beam number
        nbblk=(nblk-1)/nbeam + 1          ! Block count per beam

        if(nblk.le.nbeam)then             ! Open disk files
           call st_fopen(istat)
           if(istat.ne.0)stop
        endif

        if(scamp.and.diskf)then
           scblk=scblk+1
           if(lastmove.eq.'02'.and.move_hd.eq.'01')then
              savhdr=hdr               ! save header
              call st_scfclose(istat)
              hdr=savhdr               ! replace header
              call st_fopen(istat)
              if(istat.ne.0)stop
           endif
           lastmove=move_hd
        endif

        if(mod(nblk,10*nbeam).eq.1)then
           utpr=ut_hd(1:9)
           rapr=ra_hd(1:9)
           decpr=dec_hd(1:9)
        endif

        if(vverbose) write(*,*)' Entering st_trans'      
        call st_trans(istat)               ! Transfer data to disk
        if(istat.ne.0)stop

        if(mod(nbblk,10).eq.0)write(*,100)uprow,nbblk+nbskip,
     +     utpr,rapr,decpr
100	format(a,' Blocks:',i7,'  UT:',a,'  RA:',a,'  Dec: ',a)

        if(nbblk.ne.nbread)go to 10

 20     if(nerr.gt.10 .or. nblk.lt.1)then
           write(*,'('' Serious tape error, nerr:'',i3,'' nblk:'',i5)')
     +          nerr,nblk
           STOP
        endif

        if(istat.le.0)then      ! Finished with this file, or tape error
           if(skeof)then
              istat=sctskipf(inunit,1)  ! Skip to EOF
              if(istat.lt.0)STOP 'Error skipping to EOF'
           endif
        else if(istat.eq.1)then         ! EOF when reading
           if(dlt) istat=sctdltskip(inunit)
        endif

        if(scamp.and.diskf)then       ! Close files, write summary and ascu files
           call st_scfclose(istat)
        else
           call st_fclose(istat) 
           if(nfile.lt.nfread)go to 10
        endif

        call exstat(0,' Completed ','sc_td',11,5)
        STOP
        end







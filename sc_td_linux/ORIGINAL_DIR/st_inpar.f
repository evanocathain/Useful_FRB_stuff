      subroutine st_inpar(istat)

c  Get input parameters for sc_td

c $Log: st_inpar.f,v $
c Revision 2.24  2004/07/03 07:36:26  rmanches
c Handle SCAMP files, >2GB files
c
c Revision 2.23  2003/12/24 03:49:11  rmanches
c Fix dual-band operation
c
c Revision 2.22  2003/11/18 03:57:40  rmanches
c disk file kill
c
c Revision 2.21  2002/09/05 09:01:02  ghobbs
c Mods for Linux - Cobra
c
c Revision 2.20  2001/12/10 20:39:00  pulsar
c *** empty log message ***
c
c Revision 2.19  2000/08/19 07:48:19  pulsar
c rte: fixes for killchans stuff
c
c Revision 2.18  2000/08/19 06:22:10  pulsar
c AGL mods to kill.chan, RTE add naskip to disk topen
c
c Revision 2.17  2000/02/23 06:13:35  rmanches
c update sc_tt and include dlt_extraskip in sct
c
c Revision 2.16  2000/02/04 06:30:42  istairs
c IHS/AGL Changed kill-channel filler to eliminate Nyquist birdies.  Also changed sample and block clipping to be guided by rms and running means of channels instead of the now-defunct digitizer offsets.
c
c Revision 2.15  2000/01/30 22:36:34  pulsar
c RTE: added -S flag to skip search obs
c
c Revision 2.14  1999/08/09 06:59:07  pulsar
c AP 09/08/1999:FIRST UNIFIED ALPHA+SUN4SOL VERSION.
c Ubd128 linearizing file splitted in two versions and restored endian.h
c to account for endianess. Ubd128_sun4sol.c previously tested at Epping,
c ubd182_OSF1.c previously tested at Bologna.
c Slight changes in st_inpar.f for preventing errors in reading kill.chans
c with alpha-machines. Fully restored capability of reading from a disk.
c Removed a couple of obsolete files: ubd128.c and sct_alpha.f.
c
c Revision 2.13  1999/07/27 06:20:08  pulsar
c AGL- Minor monitoring tweak
c
c Revision 2.12  1999/07/26 04:23:22  pulsar
c AGL: -K to -x, runtime files in envar tapeinfo
c
c Revision 2.11  1999/07/15 14:16:33  pulsar
c Automated killing of chans from file kill.chans. -K inhibits - AGL
c
c Revision 2.9  1999/03/05 19:40:39  pulsar
c RNM: Added -t, -I options
c
c Revision 2.8  1998/08/16 03:40:58  alyne
c Fixed up multiple -k entries
c
c Revision 2.6  1998/05/21 05:32:11  rmanches
c Stop on command line errors
c
c Revision 2.5  1998/05/21 04:38:44  rmanches
c Fixed beam select with linearize on.
c Check input options and check that tape/file name entered.
c Compute digitiser offsets and write summary file when not writing to disk.
c Don't rewind Exabyte if naskip != 0.
c Don't read in digitiser offsets if clip =0.
c Changed default to no summary file. Altered summary and file skip keys.
c
c Revision 2.4  1998/05/16 06:33:51  rmanches
c Command line options changed - tape/file on end, no questions. More than one
c beam can be selected for writing. Block clipping now allows mean between nchan/2
c and nchan/2+beam_offset to allow for variable Tsys. Digitizer offsets now
c in file 'dititizer.offsets'.
c
c Revision 2.3  1998/05/06 07:26:12  rmanches
c Change zerodm file to reals
c
c Revision 2.2  1998/04/30 23:04:52  rmanches
c Fixed i4 question style reads and Makefile to use -I
c
c Revision 2.1  1998/04/23 23:22:57  jbell
c Dicks new v3_0 - added st_fclose.f st_fopen.f st_gethdr.f st_init.f st_inpar.f st_rdblk.f st_topen.f st_trans.f sc_td.inc - removed clops sc_cline.f sc_declare.f sc_fclose.f sc_fopen.f sc_format.f sc_gethead.f sc_grskip.f sc_preset.f sc_setpar.f sc_skip.f sc_tapeerr.f sc_td_files sc_topen.f sc_trans.f sc_unltape.f testbeamblk.f unbend.c README.multibeam
c

      implicit none
      include 'sc_td.inc'

      integer istat, i, j, ia, iargc, isys, ii, ich(42), kch, ik, kmask
      byte km1

      integer ix, ltk

      character arg1*80, arg2*80, name*50, summdir*50, line*1024
      character dir*80, kfile*90, ktlabel*16
      external iargc
      logical killed

      isys=0
      indev='D'
      intape=' '
      npsr = 0

      ia = iargc()
      call getarg(1,arg1)
      if(ia.lt.1 .or. arg1(1:2).eq.'-h')go to 50

      i=1
      do while(i.le.ia-1)
         call getarg(i,arg1)  

         if(arg1(1:2).eq.'-d') then
            i=i+1
            call getarg(i,arg2)  
            if(arg2(1:1).eq.'-')then
               write(*,*)'ERROR: Argument required for option ',
     +              arg1(1:2)
               STOP
            else
               read(arg2,100) indev
               if(verbose)write(*,*)' Input device : ',indev
            endif        
            
         elseif(arg1(1:2).eq.'-u') then
            i=i+1
            call getarg(i,arg2)  
            if(arg2(1:1).eq.'-')then
               write(*,*)'ERROR: Argument required for option ',
     +              arg1(1:2)
               STOP
            else
               read(arg2,100)intape
            endif
            if(verbose)write(*,*)' Tape unit : ',intape       
            
         elseif(arg1(1:2).eq.'-b') then
            i=i+1
            call getarg(i,arg2)  
            if(arg2(1:1).eq.'-')then
               write(*,*)'ERROR: Argument required for option ',
     +              arg1(1:2)
               STOP
            else
               read(arg2,*)ii
               if(ii.lt.1 .or. ii.gt.nbeammax)STOP 'Illegal beam number'
               wbeam(ii)=1
            endif
            if(verbose)write(*,*)' Write data for beam nr : ',ii  
            
         elseif(arg1(1:2).eq.'-g') then
            gskip=.true.
            if(verbose)write(*,*)' Will skip grid files '
            
         elseif(arg1(1:2).eq.'-G') then
            pmtape=.true.
            
         elseif(arg1(1:2).eq.'-s') then
            wsumm=.true.
            if(verbose)write(*,*)' Write summary file'
            
         elseif(arg1(1:2).eq.'-D') then
            wdat=.false.
            if(verbose)write(*,*)' Will not write data to disk '
            
         elseif(arg1(1:2).eq.'-f') then
            i=i+1
            call getarg(i,arg2)  
            if(arg2(1:1).eq.'-')then
               write(*,*)'ERROR: Argument required for option ',
     +              arg1(1:2)
               STOP
            else
               read(arg2,*)isys
            endif
            if(verbose)write(*,*)' Using Filter system: ',isys
            
         elseif(arg1(1:2).eq.'-c') then
            i=i+1
            call getarg(i,arg2)  
            if(arg2(1:1).eq.'-')then
               write(*,*)'ERROR: Argument required for option ',
     +              arg1(1:2)
               STOP
            else
               read(arg2,*)clip
            endif
            if(verbose)write(*,*)' Clip level: ',clip
            
         elseif(arg1(1:2).eq.'-K') then
            i=i+1
            call getarg(i,arg2)  
            if(arg2(1:1).eq.'-')then
               write(*,*)'ERROR: Argument required for option ',
     +              arg1(1:2)
               STOP
            else
               read(arg2,'(a)')killtlabel
            endif
            ltk=index(killtlabel,' ')-1
            if(verbose)write(*,*)
     +          ' Tape label for kill.chans: ',killtlabel(1:ltk)
            
         elseif(arg1(1:2).eq.'-x') then
            killfile=.false.
            write(*,*) 'WARNING - No kill.chans file in use'

         elseif(arg1(1:2).eq.'-a') then
            i=i+1
            call getarg(i,arg2)  
            if(arg2(1:1).eq.'-')then
                write(*,*)'ERROR: Argument required for option ',
     +              arg1(1:2)
               STOP
            else
               read(arg2,*)naskip
            endif
            if(verbose)write(*,*)' Nr of files already skipped or read:'
     +           ,naskip
            
         elseif(arg1(1:2).eq.'-m') then
            i=i+1
            call getarg(i,arg2)  
            if(arg2(1:1).eq.'-')then
               write(*,*)'ERROR: Argument required for option ',
     +              arg1(1:2)
               STOP
            else
               read(arg2,*)nfskip
            endif
            if(verbose)write(*,*)' Number of files to skip: ',nfskip
            
         elseif(arg1(1:2).eq.'-n') then
            i=i+1
            call getarg(i,arg2)  
            if(arg2(1:1).eq.'-')then
               write(*,*)'ERROR: Argument required for option ',
     +              arg1(1:2)
               STOP
            else
               read(arg2,*)nfread
            endif
            if(verbose)write(*,*)' Number of files to read: ',nfread
            
         elseif(arg1(1:2).eq.'-i') then
            i=i+1
            call getarg(i,arg2)  
            if(arg2(1:1).eq.'-')then
               write(*,*)'ERROR: Argument required for option ',
     +              arg1(1:2)
               STOP
            else
               read(arg2,*)nbskip
            endif
            if(verbose)write(*,*)' Number of blocks to skip: ',nbskip
            
         elseif(arg1(1:2).eq.'-j') then
            i=i+1
            call getarg(i,arg2)  
            if(arg2(1:1).eq.'-')then
               write(*,*)'ERROR: Argument required for option ',
     +              arg1(1:2)
               STOP
            else
               read(arg2,*)nbread
            endif
            if(verbose)write(*,*)' Number of blocks to read: ',nbread
            
         elseif(arg1(1:2).eq.'-P') then
            npsr = npsr + 1
            if (npsr.gt.npsrmax) then
               write(*,*) 'ERROR: Cannot handle more than ',npsrmax,
     +                    'specific source requests'
               STOP
            else
               i=i+1
               call getarg(i,arg2)
               if(arg2(1:1).eq.'-')then
                  write(*,*)'ERROR: Argument required for option ',
     +              arg1(1:2)
                  STOP
               else
                  read(arg2,'(a)') psr(npsr)
               endif
            endif
            if(verbose)write(*,*)' Will extract source ',psr(npsr)

         elseif(arg1(1:2).eq.'-S') then
            singlebeamonly = .true.
            if(verbose)write(*,*)
     +           ' Will skip files with more than one beam'

         elseif(arg1(1:2).eq.'-E') then
            skeof=.false.
            if(verbose)write(*,*)' Will not skip to EOF'
            
         elseif(arg1(1:2).eq.'-O') then
            overwrite=.false.
            if(verbose)write(*,*)' Will not skip to EOF'
            
         elseif(arg1(1:2).eq.'-A') then
            wrascu=.false.
            if(verbose)write(*,*)' Will not write .ascu files '
            
         elseif(arg1(1:2).eq.'-z') then
            dzero=.true.
            if(verbose)write(*,*)' Will write zerodm dedisp data '
            
         elseif(arg1(1:2).eq.'-l') then
            unbnd=.true.
            if(verbose)write(*,*)' Will linearize in frequency '
            
         elseif(arg1(1:2).eq.'-p') then
            f128c=.true.
            if(verbose)write(*,*)' Add 32 channels of fake data '

         elseif(arg1(1:2).eq.'-t') then
            toff=.true.
            if(verbose)write(*,*)
     +           ' Read time.offsets file, correct start times '
            
         elseif(arg1(1:2).eq.'-I') then
            skpinit=.true.
            if(verbose)write(*,*)' Skip INIT file '
            
         elseif(arg1(1:2).eq.'-v') then
            verbose = .true.
            write(*,*)' Will run in verbose mode '
            
         elseif(arg1(1:2).eq.'-w') then
            vverbose = .true.
            verbose = .true.
            write(*,*)' Will run in very verbose mode '
         else if(arg1(1:1).eq.'-')then
            write(*,'('' ERROR: Unrecognised option: '',a)')arg1
            STOP
         endif
         i=i+1
      enddo 

      if(i.gt.ia)then
         write(*,110)
 110     format
     +   (' ERROR: Must have tape/file name as last command-line entry')
         STOP
      else
         call getarg(i,name)
         if(verbose)write(*,*)' Tape label/disk file name : ',name
         if(name(1:1).eq.'-')then
            write(*,110)
            STOP
         endif
      endif

c  Sort out disk/tape input
      if(indev.eq.'d')then
         diskf=.true.
         diskfile=name
         ldfn=index(diskfile,' ')-1
         if(verbose)write(*,*)'Reading from disk file:',diskfile(1:ldfn)
         skeof=.false.
      else                                
         diskf=.false.
         tlabel=name(1:8)
         ltn=index(tlabel,' ')-1
         if(intape(1:1).eq.' ') then
            intape='/dev/rmt/0n'
         else if(intape(5:5).eq.' ') then
            intape = '/dev/rmt/'//intape(1:4)
         endif
         if(indev.eq.'e')then
            dlt=.false.
            if(verbose)write(*,*)
     +          ' Reading Exabyte ',tlabel(1:ltn),' on ',intape
         else
            dlt=.true.
            if(verbose)write(*,*)
     +          ' Reading DLT ',tlabel(1:ltn),' on ',intape
         endif
      endif      
         

c Check beams to write
      ii=0
      do i=1,nbeammax
         if(wbeam(i).ne.0)ii=1
      enddo
      if(ii.eq.0)then           ! If none specified, write all
         do i=1,nbeammax
            wbeam(i)=1
         enddo
      endif

      if(wsumm)then
         if(pmtape)then
            call getenv('PMTAPES',summdir)
         else
            call getenv('TAPES',summdir)
         endif
         summfile=' '
         lsm=index(summdir,' ')-1
         if(diskf)then
            summfile=summdir(1:lsm)//diskfile(1:ldfn)//'.summ'
         else
            summfile=summdir(1:lsm)//tlabel(1:ltn)//'.summ'
         endif
         lsm=index(summfile,' ')-1
         write(*,'(/'' Writing to summary file: '',a)')
     +        summfile(1:lsm)
      endif
      
      if(unbnd .and. .not.pmtape)write(*,*)
     +   ' Warning - linearization is specific to PM survey parameters'

      if(nfread.eq.0)nfread=99999

      if(isys.eq.1)then
         lsys(1)=.true.
         rsys2=.false.
      else if(isys.eq.2)then
         lsys(1)=.false.
         rsys2=.true.
      else if(isys.eq.3)then
         lsys(1)=.true.
         rsys2=.true.
      endif

      if(f128c)then
         write(*,*)' Warning - add 32 channels not implemented'
         write(*,*)' Code exists in previous JB version'
      endif

      if(toff)then
         if(verbose)write(*,'(''  Opening time.offsets file'')')
         call getenv('tapeinfo',dir)
         kfile=dir(1:index(dir,' ')-1)//'time.offsets'
         open(unit=10,file=kfile,status='old',iostat=ii)
         if(ii.eq.0)then
            ntoff=1
            do while(.true.)
               read(10,*,err=40)toffmjd(ntoff),toffset(ntoff)
               ntoff=ntoff+1
            enddo
 40         close(10)
         else
            write(*,'(/''WARNING - Failed to open time.offsets file'')')
            istat=1
            return
         endif
      endif


      if(killfile)then
        if(verbose)write(*,'(''  Opening kill.chans file'')')
        call getenv('tapeinfo',dir)
        kfile=dir(1:index(dir,' ')-1)//'kill.chans'
        open(unit=10,file=kfile,status='old',iostat=ii)

        if(killtlabel.eq.'none')then
           ltk=ltn
           ktlabel(1:ltk)=tlabel(1:ltn)
        else
           ktlabel(1:ltk)=killtlabel(1:ltk)
        endif
        if(ii.eq.0)then
          killed=.false.
          do while(.true.)
            read(10,'(a)',err=45,end=45)line
c            write(*,'(a,i5,x,a)')tlabel(1:ltn),ltn,line
            if(line(1:ltk).eq.ktlabel(1:ltk)) then
              ix=index(line,'-1')
              read(line(ltk+1:ix+1),*,end=47) freq,bw,(ich(j),j=1,42)
 47           do j=1,42
                if(ich(j).lt.0) goto 48 !rte this was .le... wrong I think?
              enddo
 48           j=j-1
              if(j.gt.40) then
                j=40
                write(*,*)
     & 'WARNING - more than 40 kill channels specified in kill.chans,'
     & //' only 40 used.'
              endif
              if(verbose)then
                write(*,'(i3,'' kill channels used for sys freq/bw:'')')
     &                     freq,bw
                write(*,'(20i4)') (ich(i),i=1,j)
              endif

c Now set the masks
c First check if the system is already in the list
              if (nksys.ne.0) then
                 do ksys=1,nksys
                    if(freq.eq.kfreq(ksys).and.bw.eq.kbw(ksys)) goto 44
                 enddo
              endif
              nksys=nksys+1
              ksys=nksys
              kfreq(ksys)=freq
              kbw(ksys)=bw
 44           killed=.true.
              do i=1,nkillmax
                 kmask1(i)=0
              enddo
              do i=1,j
                kch=ich(i)
                ik=kch/32
                kmask=lshift(1,kch-32*ik)
                do ii=1,4
                  km1=iand(x'ff',rshift(kmask,8*(ii-1)))
                  kmask1(ik*4+ii)=ior(kmask1(ik*4+ii),km1)
                  kmask2(ik*4+ii,ksys)=not(kmask1(ik*4+ii))
                  if(and(i,1).eq.0)
     +              kmask3(ik*4+ii,ksys)= ieor(kmask3(ik*4+ii,ksys),km1)
                enddo
                if(verbose)write(*,
     +                '(x,a,i2,2x,z8,2x,4z2,2x,4z2)') 
     &                ' Kill word/mask: ',ik,kmask
              enddo
            endif
          enddo
 45       if(.not.killed)then
             write(*,'(''WARNING - No entry in kill.chans'')')  
          else
             write(*,'('' Using kill channels from file kill.chans'')') 
             if(verbose) then 
               do ksys=1,nksys
                 write 
     +             (*,'(''Killmasks 3 and 2 for kill system:'',2f8.2)')
     +              kfreq(ksys),kbw(ksys)
                 do i=1,12
                     write(*,'(x,i2,2x,4z2,2x,4z2)') i,kmask3(i,ksys),
     +                  kmask2(i,ksys)
                 enddo
               enddo
             endif
          endif
          close(10)
        else
          write(*,'(''WARNING - Failed to open kill.chans file'')')
        endif
      endif


      istat=0                                                 ! Normal return
      return
          
 50   write(*,*)'SC_TD: Usage: sc_td [options] <tape/file>' 
      write(*,*)'Options:'
      write(*,*)'-a  <naskip> Nr of files already skipped/read  (def=0)'  
      write(*,*)'-A  No .ascu files?            (def=write .ascu files)'  
      write(*,*)'-b  <nb> Beam number to write (can repeat)   (def=all)'
      write(*,*)'-c  <clip> Clip level                   (def=0.0=none)' 
      write(*,*)'-d  <e> Input device    (d=disk, e=Exabyte, D=DLT=def)' 
      write(*,*)'-D  No data to disk?                  (def=write data)'  
      write(*,*)
     +         '-E  Don''t skip to EOF after blocks read     (def=skip)'  
      write(*,*)'-f  <nf> Filter system 1,2 or both (3)         (def=1)'  
      write(*,*)'-g  Skip grid files?                           (def=n)'  
      write(*,*)'-G  Write the file names with the grid_id      (def=n)'
      write(*,*)'-h  Print this help page' 
      write(*,*)'-i  <nbskip> Number of blocks/beam to skip     (def=0)'  
      write(*,*)'-I  Skip INIT file (SCAMP tapes)               (def=n)'  
      write(*,*)'-j  <nbread> Number of blocks/beam to read (def=0=all)'  
      write(*,*)'-K  Tape label for kill.chans (def=use tape/file name)' 
      write(*,*)'-l  Linearize in frequency?                    (def=n)' 
      write(*,*)'-m  <nfskip> Number of files to skip           (def=0)'  
      write(*,*)'-n  <nfread> Number of files to read       (def=0=all)' 
      write(*,*)'-p  Add 32 channels of fake data               (def=n)'
      write(*,*)'-O  Do not overwrite tape/file label   (def=overwrite)'
      write(*,*)'-P  <source> Source to extract (9 char max)  (def=any)'
      write(*,*)'-s  Write summary file?          (def=no summary file)'  
      write(*,*)'-S  Single-beamed files only                   (def=n)'
      write(*,*)'-t  Read time.offsets file and fix start UTs   (def=n)'
      write(*,*)'-u  <unit> Tape unit               (def = /dev/rmt/0n)'
      write(*,*)'        If < 5 char entered, "/dev/rmt/" assumed'
      write(*,*)'-v  Verbose mode                               (def=n)' 
      write(*,*)'-w  Very verbose mode                          (def=n)' 
      write(*,*)'-x  Do not use kill.chans file              (def=kill)'
      write(*,*)'-z  Write zerodm file?                         (def=n)' 
      
      istat=1
      return

 100  format(a)
      end




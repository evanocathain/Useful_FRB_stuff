        integer*4 headl, datl, nbeammax, nkillmax, ksysmax, 
     +    ntoffmax, npsrmax
        parameter (headl=640, datl=49152, nbeammax=13, nkillmax=64, 
     +    ksysmax=8, ntoffmax=256, npsrmax=8)

	integer ltn, nfskip, naskip, nfread, nfile, nbskip,
     +    nbread, ldfn, inunit, nbeam, len, nblk, nbblk, ibm,
     +    nbyte_smp1, nbyte_smp2, nbyte_grp1, nbyte_grp2, samp_blk1,
     +    samp_blk2, nbyte_smp, nbyte_grp, imean1, imean2, iclip1, 
     +    iclip2, nclip1(nbeammax), nbclip1(nbeammax), nclip2(nbeammax), 
     +    nbclip2(nbeammax), nerr, infile, nbyte_blk1, nbyte_blk2,
     +    wbeam(nbeammax), ngid(nbeammax), lsm, ltf, ntoff, npsr,
     +    ksys, nksys, nfill, kss(2), scfile, scblk
        byte kmask1(nkillmax), kmask2(nkillmax,ksysmax), 
     +    kmask3(nkillmax,ksysmax)
	real*4 clip, bclip1, bclip2, rmean(nbeammax), 
     +    kfreq(ksysmax), kbw(ksysmax), freq, bw
	real*8 toffmjd(ntoffmax), toffset(ntoffmax)
      	character tlabel*16, diskfile*50, mbfile(nbeammax)*50, indev*1,
     +    intape*16, summfile*80, psr(npsrmax)*12, killtlabel*16,
     +    lastmove*2
      	logical gskip, dlt, wsumm, wdat, skeof, wrascu, dzero, unbnd, 
     +     f128c, verbose, vverbose, lsys(2), rsys2, scamp, treed,
     +     diskf, pmtape, killfile, toff, skpinit, singlebeamonly,
     +     overwrite

      	common/int4/ltn, wbeam, nfskip, naskip, nfread, nfile,
     +    nbskip, nbread, clip, ldfn, inunit, nbeam, len, nblk, nbblk, 
     +    nbyte_smp1, nbyte_smp2, nbyte_grp1, nbyte_grp2, ibm,
     +    samp_blk1, samp_blk2, nbyte_smp, nbyte_grp, imean1, imean2, 
     +    iclip1, iclip2, bclip1, bclip2, nclip1, nbclip1, rmean,
     +    nclip2, nbclip2, nerr, infile, nbyte_blk1, nbyte_blk2,
     +    ngid, lsm, ltf, kmask1, kmask2, kmask3, ntoff, npsr,
     +    ksys, nksys, kfreq, kbw, freq, bw, nfill, kss, scfile, scblk
	common/r8/toffmjd, toffset
      	common/flags/gskip, dlt, wsumm, wdat, skeof, wrascu, dzero, 
     +    unbnd, f128c, verbose, vverbose, lsys, rsys2, scamp, 
     +    treed, diskf, pmtape, killfile, toff, skpinit,
     +	  singlebeamonly, overwrite
      	common/char/intape, indev, tlabel, diskfile, mbfile, summfile,
     +    psr, killtlabel,lastmove




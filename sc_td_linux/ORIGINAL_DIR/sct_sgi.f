c  File sct_sgi.f 
c  Tape handling routines - sgi versions
c  RNM  March 16, 1992.

	integer*4 function sctopen(unit,name)

	implicit none
	integer*4 unit, topen
	character name*(*)

c	default device name is /dev/ndlt.....
	sctopen=topen(unit,name,.false.)

	end
c--------------------------------------------------

	integer*4 function sctclose(unit)

	implicit none
	integer*4 unit, tclose

	sctclose=tclose(unit)

	end
c--------------------------------------------------

	integer*4 function sctread(unit,cblk,len)

	implicit none
	integer*4 unit, len, istat, tread, tskipf
	character cblk*49792

	sctread=tread(unit,cblk)
c        sctread=sctread+tread(unit,cblk(641:49792))

c  Reset EOF flag
	if(sctread.eq.0)istat=tskipf(unit,1,0)

	end
c------------------------------------------------------

	integer*4 function sctskipf(unit,nsk)

	implicit none
	integer*4 unit, nsk, tskipf

	sctskipf=tskipf(unit,nsk,0)

	end
c------------------------------------------------------

	integer*4 function sctskipr(unit,nsk)

	implicit none
	integer*4 unit, nsk, tskipf

	sctskipr=tskipf(unit,0,nsk)

	end
c-------------------------------------------------------

	integer*4 function sctrewind(unit)

	implicit none
	integer*4 unit, trewin

	sctrewind=trewin(unit)

	end
c--------------------------------------------------------

	subroutine scdisk(diskname)

	implicit none
	character diskname*(*)

	call getenv('scratch_disk',diskname)

	end







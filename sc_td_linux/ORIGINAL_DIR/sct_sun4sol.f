c  File sct_sun.f 
c  Tape handling routines - Sun versions
c  RNM  March 16, 1992.

	integer*4 function sctopen(unit,name)

	implicit none
	integer*4 unit, topen
	character name*(*)

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

	end
c--------------------------------------------------

	integer*4 function sctwrite(unit,cblk)

	implicit none
	integer*4 unit, twrite
	character cblk*49792

	sctwrite=twrite(unit,cblk)

	end
c------------------------------------------------------

	integer*4 function sctstate(unit,stfile,strec,eoff,eotf)

	implicit none
	integer*4 unit, sctstate, tstate,stfile, strec, tcsr
	logical errf, eoff, eotf

	sctstate=tstate(unit,stfile,strec,errf,eoff,eotf,tcsr)

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
c-------------------------------------------------------------------

      integer*4 function sctdltskip(unit)

      implicit none
      integer*4 unit, tskipf

      sctdltskip = tskipf(unit,1,0)  ! If DLT skip EOF marker
     
      end
c--------------------------------------------------------

	subroutine scdisk(diskname)

	implicit none
	character diskname*(*)

	call getenv('scratch_disk',diskname)

	end







c  File sct_OSF1.f 
c  Tape handling routines - alpha versions
c  RNM  March 16, 1992.
c  AP   Aug 2, 1999.

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
c------------------------------------------------------

	integer*4 function sctstate(unit,stfile,strec,eoff,eotf)

	implicit none
	integer*4 unit, tstate,stfile, strec, tcsr
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
c--------------------------------------------------------

      integer*4 function sctdltskip(unit)

      implicit none
      integer*4 unit

      sctdltskip = 1  ! Do nothing
     
      end
c--------------------------------------------------------

	subroutine scdisk(diskname)

	implicit none
	character diskname*(*)

	call getenv('scratch_disk',diskname)

	end







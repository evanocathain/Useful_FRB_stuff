c  File sct_sun.f 
c  Tape handling routines - Linux versions
c  RNM  April 7, 2000.

	integer*4 function sctopen(unit,name)

	implicit none
	integer*4 unit, topenc, length, ln
	character name*16

c	ln=length(name)
	ln=len(name)
	sctopen=topenc(unit,name(1:ln))

	end
c--------------------------------------------------

	integer*4 function sctclose(unit)

	implicit none
	integer*4 unit, tclosec

	sctclose=tclosec(unit)

	end
c--------------------------------------------------

	integer*4 function sctread(unit,cblk,len)

	implicit none
	integer*4 unit, len, istat, treadc
	character cblk*49792

	sctread=treadc(unit,cblk,len)

	end
c--------------------------------------------------

c	integer*4 function sctwrite(unit,cblk)

c	implicit none
c	integer*4 unit, twritec
c	character cblk*49792

c	sctwrite=twritec(unit,cblk)

c	end
c------------------------------------------------------

	integer*4 function sctstate(unit,stfile,strec,eoff,eotf)

	implicit none
	integer*4 unit, tstatec, stfile, strec
	logical eoff, eotf

	sctstate=tstatec(unit,stfile,strec,eoff,eotf)

	end
c------------------------------------------------------

	integer*4 function sctskipf(unit,nsk)

	implicit none
	integer*4 unit, nsk, tskipfc

	sctskipf=tskipfc(unit,nsk,0)

	end
c------------------------------------------------------

	integer*4 function sctskipr(unit,nsk)

	implicit none
	integer*4 unit, nsk, tskiprc

	sctskipr=tskiprc(unit,nsk)

	end
c-------------------------------------------------------

	integer*4 function sctrewind(unit)

	implicit none
	integer*4 unit, trewinc

	sctrewind=trewinc(unit)

	end
c-------------------------------------------------------------------

      integer*4 function sctdltskip(unit)

      implicit none
      integer*4 unit, tskipfc

c      sctdltskip = tskipfc(unit,1,0)  ! If DLT skip EOF marker
     
      end
c--------------------------------------------------------

	subroutine scdisk(diskname)

	implicit none
	character diskname*(*)

	call getenv('scratch_disk',diskname)

	end







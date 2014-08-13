c  File sct_hp.f 
c  Tape handling routines - HP-UX 9.0 versions. Skip files by a call to
*  the shell. "tskipf" skips files block by block!
c  RNM  March 16, 1992.
*  LN   March 19, 1994.

	integer*4 function sctopen(unit,name)

	implicit none
	integer*4 unit, topen
	character name*(*)
        COMMON /TPNAME/ TNAME
*
C-- Exabyte driver: Berkeley style, no rewind, low density
*
c       default device name /dev/rmt/c201d5lnb
	sctopen=topen(unit,name,.false.)

	end
c--------------------------------------------------

	integer*4 function sctread(unit,cblk,len)

	implicit none
	integer*4 unit, len, istat, tread, tskipf
	character cblk*49792

	sctread=tread(unit,cblk)

c  Reset EOF flag
	if(sctread.eq.0)istat=tskipf(unit,1,0)

	end
c------------------------------------------------------
*
      INTEGER*4 FUNCTION SCTSKIPF( UNIT,NSK )
*
      IMPLICIT NONE
      INTEGER*4 UNIT,NSK,TOPEN,TCLOSE,SYSTEM  !,TSKIPF
      CHARACTER TNAME*40,NSKS*3
*--
      COMMON /TPNAME/ TNAME
*--
      WRITE (NSKS,11,ERR=20) NSK
   11 FORMAT (I3)
C.      sctskipf = tskipf( unit,nsk,0 )
      SCTSKIPF = TCLOSE( unit )
      IF (SCTSKIPF .LT. 0) THEN
        WRITE (6,12)
   12 FORMAT ('SCTSKIPF: Error closing tape unit.')
        RETURN
      ENDIF
      sctskipf = SYSTEM( 'mt -t '//TNAME//' fsf '//nsks )
      SCTSKIPF = TOPEN( UNIT,TNAME,.FALSE. )
      IF (SCTSKIPF .LT. 0) THEN
        WRITE (6,13)
   13 FORMAT ('SCTSKIPF: Error opening tape unit.')
        RETURN
      ENDIF
*
  20  RETURN
      END
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
*
      INTEGER*4 FUNCTION SCTCLOSE( UNIT )
*
      IMPLICIT NONE
      INTEGER*4 UNIT,TCLOSE
*
      SCTCLOSE = TCLOSE ( UNIT )
*
      END
c--------------------------------------------------------

	subroutine scdisk(diskname)

	implicit none
	character diskname*(*)

	call getenv('scratch_disk',diskname)

	end

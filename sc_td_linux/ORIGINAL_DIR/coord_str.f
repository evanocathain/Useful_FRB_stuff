********************************************************************
* FILE: UTILITY.FOR
* General purpose utility subroutines.
********************************************************************

      subroutine coord_str(turns, type, string, status)

c function  To generate the character string equivalent of an angular
c           coordinate from its value in revolutions.

c  turns      r*8   Coordinate value in revolutions
c  type       ch*2  Coordinate type 
c T6    ' hh:mm:ss.ssssss'
c T4    ' hh:mm:ss.ssss'
c LA    '-dd:mm:ss.sss'
c GL    'ddd.dddd'
c GB    '-dd.dddd'
c  status    i*4   Return status, 0 is normal.

c  RNM, March, 1988
c  Based on DMCC routine 'zed_encode_coord'
c  AGL, Sept, 1990. Normal returned status changed from 1 to 0.
c  DJ, April 1991.  Rewritten to correct 2:59:60.000 bug

	integer    status
	real*8     turns, lturns, sec, secdec, deg
	character  sign, type*2, string*(*)

	status = 0
	if (turns .lt. 0.0d0) then
		isign = -1
		sign = '-'
	else
		isign = 1
		sign = ' '
	        if(type .eq. 'LA')sign='+'
	end if
	lturns = abs(turns)

	lturns = dmod(lturns, 1.d0)	
	string = ' '			!clear string for output

	if (type .eq. 'T6' .or. type .eq. 'T4') then
		if (isign .eq. -1) then
		   status=2
		   return
		end if
		sec = lturns * 86400.d0
		if (type .eq. 'T6') then
		   ndec = 6
		else
		   ndec = 4
		endif

	else if (type .eq. 'LA') then
		if (lturns .gt. 0.25d0) then
			status = 2
			return
		end if
		sec = lturns * 1296000.d0
		lturns = lturns * isign
		ndec = 3

	else if (type .eq. 'GL') then
		if (isign .eq. -1) then
		   status = 2
	 	   return
         endif
		deg = lturns * 360.d0

	else if (type .eq. 'GB') then
		if (lturns .gt. 0.25d0) then
		   status = 2
		   return
		endif
		deg = turns * 360.d0
	end if

      if (type .eq. 'T6' .or. type .eq. 'T4' .or.
     -    type .eq. 'LA') then
		ihord = sec / 3600.d0
		itemp = ihord * 3600
		imin = (sec - itemp) / 60.d0
		sec = sec - imin * 60.d0 - itemp
		isec = sec
		secdec = (sec - int(sec)) * 10.d0
		isecdec = secdec
		
         do loop = 1, ndec - 1
            secdec = (secdec - int (secdec)) * 10d0
            isecdec = secdec + 10 * isecdec
         enddo

         if (isecdec .ge. 10 ** (ndec) - 1 .and. secdec .ge. 5) then
            isecdec = 0
            isec = isec + 1
            if (isec .gt. 59) then
               isec = 0
               imin = imin + 1
               if (imin .gt. 59) then
                  imin = 0
                  ihord = ihord + 1
               endif
            endif
         endif

         write (string, '(a,i2.2, 2('':'', i2.2), ''.'', i<ndec>.
     -                  <ndec>)') sign, ihord, imin, isec, isecdec

      else
		write (string, 100) deg
      endif

	return

100	format(f8.4)
	end

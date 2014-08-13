
********************************************************************
	subroutine str_coord(string,type,turns,istat)
********************************************************************

c  Converts from string value to turns using "type" as in COORD_STR
c  RNM April, 1988
c  Returns istat=0 as normal.

	implicit none
	character string*(*),type*2,sign*1
	integer*4 istat,ih,im,id
	real*8 turns,sec
	
	istat=0
	if(type(1:1).eq.'T')then

	  read(string(1:3),'(i3)',err=90)ih
	  read(string(5:6),'(i2)',err=90)im

	  if(type(2:2).eq.'6')then
	    read(string(8:16),'(f9.6)',err=90)sec
	  else
	    read(string(8:14),'(f7.4)',err=90)sec
	  endif

	  turns=((ih*60+im)*60 + sec)/86400.d0

	  RETURN

	else if(type.eq.'LA')then

	  sign=string(1:1)
	  read(string(2:3),'(i2)',err=90)id
	  read(string(5:6),'(i2)',err=90)im
	  read(string(8:13),'(f6.3)',err=90)sec

	  turns=((id*60+im)*60 + sec)/1.296d6
	  if(sign.eq.'-')turns = -turns

	  RETURN

	else if(type(1:1).eq.'G')then

	  read(string(1:8),'(f8.4)',err=90)turns
	  turns=turns/360.d0

	  RETURN

	else

	  write(*,*),' Illegal type'

	endif

90	istat=2

	RETURN
	end

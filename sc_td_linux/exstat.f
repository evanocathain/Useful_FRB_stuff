      subroutine exstat(id,message,prog,meslen,proglen)

      implicit none

c     Write the exit status to exit_status_program_name
c     JFB 970905

      character prog*(*)     ! Name of program calling exstat
      character message*(*)  ! Message to write to the file
      integer meslen,proglen
      integer   id           ! Error code ID
                             ! id = 0 - successful completion
                             ! id > 0 - failed 
                             ! id < 0 - status message
      character fname*100
      logical first

      first = .true.

      if(first) then
         fname =  'exit_status_'//prog(1:proglen)
c         fname = 'exit_status_'//prog
         open(unit=11,file=fname(1:proglen+12),status='unknown')
        first = .false.
      endif

      write(11,*)id,message(1:meslen)

      if(id.ge.0) close(11)

      return

      end

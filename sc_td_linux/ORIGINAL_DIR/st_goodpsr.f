        logical function st_goodpsr(tapepsr,psr,npsr)

C       Function to check if the name in the header of
C       this tape file is among those requested.
C       VMK  0799

        implicit none
        character tapepsr*16, psr(*)*9
        integer npsr, ipsr
        logical goodpsr

        goodpsr = .false.
        ipsr = 1
        do while ( (.not.goodpsr).and.(ipsr.le.npsr) )
           if (index(tapepsr,psr(ipsr)).ne.0) then
              goodpsr = .true.
           else
              ipsr = ipsr + 1
           endif
        enddo
 
        st_goodpsr = goodpsr
        return
        end


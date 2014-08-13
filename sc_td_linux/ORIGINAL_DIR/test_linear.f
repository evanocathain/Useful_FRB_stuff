      program test_linear

      implicit none

c     AGL,JFB 970731 Simulate how we would linearise the frequency space of the
c     pmsurv data prior to dedisping using tree et al

      integer i,j,m,n,out(128)
      real    f,df

      m  = 96     ! No of input chans
      n  = 128     ! No of output chans
      f  = 1516.5 ! Frequency of channel 1
      df = -3     ! Channel bandwidth

      do i = 1,m ! loop over input chans
        j = nint((n-1)*(((f+(i-1)*df)**-2 - f**-2)/
     &                  ((f+(m-1)*df)**-2 - f**-2))) + 1
        out(j) = i
c        write(*,*)i,j
      enddo

c     Invert

      do j = 1,n ! loop over output chans
c        i = nint(((((f+(m-1)*df)**-2 - f**-2)*(j-1)/(n-1) + f**-2)**-0.5
c     &       - f)/df) + 1
c        write(*,*)i,j,i-j
         if(out(j).ne.0)write(*,'(i2.2,x,i3,x,i3)')out(j),j,out(j)-j
         if(out(j).eq.0)write(*,'(x,x,x,i3)')j
      enddo 

      end

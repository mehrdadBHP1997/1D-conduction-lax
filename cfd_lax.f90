!  cfd_lax.f90 
!
!  FUNCTIONS:
!  cfd_lax - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: cfd_lax
! by mehrdad baba hosein pour
!  PURPOSE:  Entry point for the console application.
!  beyond sky!
!****************************************************************************

 program wave_lax
implicit none
integer::i,n
integer,parameter:: ix=201,nt=400
real*8::u(nt,ix),a,dt,dx,l,pi,u1(nt,ix)
pi=2*asin(1.d0)
a=0.01
dt=0.04
l=1
dx=l/(ix-1)
print*,'qourant number is = ',a*dt/dx

!exact_solution
do n=1,nt
 do i=1,ix
    u1(n,i)=sin(2*pi*(i-1)*(dx-a*dt)*5)  
 end do
end do

!initial condition
do i=1,ix
    u(1,i)=sin(2.d0*pi*(i-1)*dx*5)
end do

!solve_wave_equation
do n=1,nt-1
    do i=2,ix-1
       u(n+1,i)=(u(n,i+1)+u(n,i-1))/2.d0-a*dt/dx*((u(n,i+1)-u(n,i-1))/2.d0)
    end do
end do
open (1,file='analysis_result_lax.plt')        !analysis_result
do n=1,nt
    write(1,*)'variables="x","u1"'
    write(1,*)'zone'
    do i=1,ix
        write(1,*) dx*(i-1),u1(n,i)
    end do
end do
open (2,file='computational_result_lax.plt')   ! computational_result
do n=1,nt
    write(2,*)'variables="x","u"'
    write(2,*)'zone'
    do i=1,ix
        write(2,*) dx*(i-1),u(n,i)
    end do
end do
print*,'>>> please inter <<<'

pause 
    end program wave_lax




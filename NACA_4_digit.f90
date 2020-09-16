program NACA_4_digit
implicit none
!creating all the necessary variables
integer::dig1,dig2,dig34
real::c


!--------------------------------------------------------------------------------------!
write(*,*)"*****************************************************************************"
write(*,*)"*   Welcome to Foil-Gen!                                                    *"
write(*,*)"*   This program generates NACA 4 digit airfoil coordinates                 *" 
write(*,*)"*   Created by Guruprasad S                                                 *"
write(*,*)"*   Email:- guru112358@gmail.com                                            *"
write(*,*)"*   Input the airfoil 4 digit number  as per prompts on the screen          *"
write(*,*)"*   NOTE:- The graphs are plotted using gnuplot                             *"
write(*,*)"*****************************************************************************"
write(*,*)
write(*,*)
write(*,*)
!--------------------------------------------------------------------------------------!
!The NACA four-digit wing sections define the profile by:
!First digit describing maximum camber as percentage of the chord.
!Second digit describing the distance of maximum camber from the airfoil leading edge in tenths of the chord.
!Last two digits describing maximum thickness of the airfoil as percent of the chord.
!taking user input through variables 
!All coordinates are normalised to a chord value of 1.0.
!--------------------------------------------------------------------------------------!
write(*,*)"Enter the first digit of the airfoil: "
write(*,*)
read *,dig1
write(*,*)
write(*,*)"Enter the second digit of the airfoil: "
read *,dig2
write(*,*)
write(*,*)"Enter the  last two digits of the airfoil: "
write(*,*)
read *,dig34
write(*,*)
write(*,*)"Enter the desired chord:"
write(*,*)
read *,c
write(*,*)

!The computation of coordinates is split into separate subroutines for symmetric and cambered airfoils

IF(dig1==0)then
write(*,*)"The airfoil is Symmetric"
call SYMMETRIC_GEN(dig1,dig2,dig34,c)

ELSE
write(*,*)"The airfoil is Cambered"
call CAMBERED_GEN(dig1,dig2,dig34,c)
END IF

contains

!defining the subroutines:
!-------------------------------------------------------------------------------------------------------------------!
SUBROUTINE SYMMETRIC_GEN(dig1,dig2,dig34,c)
implicit none
real,dimension(0:200)::x,yt,xu,u,xl,yl,yu
integer::dig1,dig2,dig34,i
real::t,dx,c
dx=real(c/200)

t=real(dig34/100.0)*c !thickness in 100s of chord(1))
open(1,file='symmetric.dat',status='replace')
open(2,file='symmetric.plt',status='replace')
do i=0,200
x(i)=i*dx
yt(i)=5.0*t*(0.2969*SQRT(x(i)/c)-(0.1260*x(i)/c)-0.3516*((x(i)/c)**2)+(0.2843*(x(i)/c)**3)-(0.1036*(x(i)/c)**4))
xu(i)=x(i)
yu(i)=yt(i)
xl(i)=x(i)
yl(i)=-yt(i)
write(1,*)xu(i),yu(i)
end do
!writing lower surface points
do i=0,200
write(1,*)xl(i),yl(i)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
end do 

print *,"The symmetric airfoil has been generated,dat file can be found in directory"
!plotting the airfoil
 write(2,*)'set xlabel "X"'
 write(2,*)'set ylabel "Y"'
 write(2,*)'set autoscale xy'
 write(2,*)'set title "Airfoil surface plot"'
 write(2,*)'set size ratio 0.1'
 write(2,*)'plot "symmetric.dat" with line'
 CALL SYSTEM('gnuplot -p symmetric.plt')
close(1)
close(2)
END SUBROUTINE SYMMETRIC_GEN
!-------------------------------------------------------------------------------------------------------------------------!

SUBROUTINE CAMBERED_GEN(dig1,dig2,dig34,c)
implicit none
integer::dig1,dig2,dig34,i
real::m,p,dx,t,c
real,dimension(0:200)::x,yc,xu,u,xl,yl,yu,theta,dyc_dx,yt
m=real(dig1/(100.0))*c
p=real(dig2/10.0)*c
t=real(dig34/100.0)*c

dx=real(c/200.0)
open(1,file='cambered.dat',status='replace')
open(2,file='cambered.plt',status='replace')

do i=0,200

x(i)=i*dx
yt(i)=5.0*t*(0.2969*SQRT(x(i)/c)-(0.1260*x(i)/c)-0.3516*((x(i)/c)**2)+(0.2843*(x(i)/c)**3)-(0.1036*(x(i)/c)**4))
if(x(i)<=p*c)then
yc(i)=(m/(p**2))*((2*p*(x(i)/c))-(x(i)/c)**2)
dyc_dx(i)=(2*m/(p**2))*(p-(x(i)/c))
else if(x(i)>=p*c)then
yc(i)=(m/(1-p)**2)*((1-2*p)+(2*p*(x(i)/c))-(x(i)/c)**2)
dyc_dx(i)=((2*m)/(1-p**2))*(p-(x(i)/c))
end if

theta(i)=ATAN(dyc_dx(i))
xu(i)=x(i)-yt(i)*SIN(theta(i))
xl(i)=x(i)+yt(i)*SIN(theta(i))

yu(i)=yc(i)+yt(i)*COS(theta(i))
yl(i)=yc(i)-yt(i)*COS(theta(i))

end do

do i=0,200
write(1,*)xu(i),yu(i)
end do

do i=0,200
write(1,*)xl(i),yl(i)
end do
write(*,*)"The cambered airfoil has been generated,dat file can be found in the directory"

 write(2,*)'set xlabel "X"'
 write(2,*)'set ylabel "Y"'
 write(2,*)'set autoscale xy'
 write(2,*)'set title "Airfoil surface plot"'
 write(2,*)'set size ratio 0.1'
 write(2,*)'plot "cambered.dat" with line'
 CALL SYSTEM('gnuplot -p cambered.plt')

close(1)
close(2)

END SUBROUTINE CAMBERED_GEN



end program NACA_4_digit

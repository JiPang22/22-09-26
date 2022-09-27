program b
implicit none
integer i,n
real t,dt,beta,omzero,x,dx,v,dv

! condition
i=0;n=0;t=0.;dt=0.01;beta=1.5;omzero=1.;x=1.;v=1.

open(1,file='b')

do i=0,1000
t=i*dt
write(1,*) i*dt, x

dv=-2*beta*v-omzero**2*x+(sin(t)+(1/3.)*sin(3*t)+(1/5.)*sin(5*t))
dx=v

v=v+dv*dt
x=x+dx*dt

end do 
end

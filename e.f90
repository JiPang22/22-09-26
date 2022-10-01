program a
implicit none 
integer i,j
real dt,x,dx,v,dv,omzero,gam,sums
open(1,file='aa')
sums=10*sin(i*dt)+(1/3)*sin(3*i*dt)
do j=1,10
dt=0.01;x=0.01;omzero=2.;gam=0.5;sums=sums+10*(1/(2*j+1))*sin((2*j+1)*i*dt)
do i=0,2000
write(1,*) i*dt,x
dv=-2*gam*v-omzero**2*x+sums;dx=v
v=v+dv*dt;x=x+dx*dt
end do
write(1,*) ''
end do
end

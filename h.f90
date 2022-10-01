program a
implicit none 
integer i,j
real dt,x,dx,v,dv,omzero,gam
open(1,file='aa')
dt=0.01;x=0.01;omzero=2;gam=0.5
do i=0,3000
write(1,*) i*dt,x
dv=-2*gam*v-omzero**2*x;dx=v
v=v+dv*dt;x=x+dx*dt
end do
end

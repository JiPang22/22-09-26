program a
implicit none
integer*4 i,j
real*4 t,dt,x,dx,v,dv,w,dw,sumr,sumi,gam,omzero
parameter(dt=1.e-2,dw=1.e-2,omzero=4.)
open(1,file='aa');open(2,file='ab')
do j=0,10
t=0.;w=0.;gam=1.e-1;sumr=0.;sumi=0.;x=1.e-2;v=1.e-2
do i=0,5000
write(1,*) t,x;write(2,*) w,sqrt(sumr**2+sumi**2)
dv=-2*gam*v-omzero**2*x;dx=v
v=v+dv*dt;x=x+dx*dt;t=i*dt;w=i*dw;gam=j*0.1
sumr=sumr+x*cos(w*t)*dt;sumi=sumi-x*sin(w*t)*dt
enddo
write(1,*)'';write(2,*)''
enddo
end

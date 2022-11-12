program a
implicit none
integer*4 i,j,interval
real*4 eta,gam,om,alpha,deta,etadot,detadot;interval
open(6,file='aa')

do j=0,10
gam=j*0.1;eta=0.01;dtau=0.01;om=1.;alpha=1.etadot=0.01;interva=100

do i=0,interval
write(6,*) i*dtau,eta
detadot=-gam*etadot-om**2 *eta+0.;deta=etadot
etadot=etadot+detadot*dtau;eta=eta+deta*dtau
enddo

interval=interval+1
do i=interval,1000
write(6,*) i*dtau,eta
detadot=-gam*etadot-om**2 *eta+alpha;deta=etadot
etadot=etadot+detadot*dtau;eta=eta+deta*dtau
enddo

write(6,*) ''
enddo
end

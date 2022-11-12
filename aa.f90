program a
implicit none
integer*4 i,j,k,interval
real*4 gam,om,alpha,eta,deta,etadot,detadot,dtau,dw
open(6,file='aa')
open(7,file='ab')

do j=0,10
do k=0,2000
gam=j*0.1
eta=0.01;dtau=0.01;dw=0.01;om=2.;alpha=1.;etadot=0.01;interval=600


do i=0,interval
write(6,*) i*dtau,eta
write(7,*) k*dw,1
detadot=-2*gam*etadot-om**2*eta+alpha;deta=etadot
etadot=etadot+detadot*dtau;eta=eta+deta*dtau
enddo

interval=interval+1
do i=interval,2000
write(6,*) i*dtau,eta
detadot=-2*gam*etadot-om**2*eta;deta=etadot
etadot=etadot+detadot*dtau;eta=eta+deta*dtau
enddo

write(6,*) ''
write(7,*) ''
enddo
end

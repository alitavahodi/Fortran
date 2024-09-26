PROGRAM p1_AliTavahodi_RezaBeyk
IMPLICIT NONE

REAL::R,pi,Acircle,Asphere,Vsphere

WRITE(*,*)'plz ENTER Radius R(cm)'

READ(*,*)R
pi=3.1415
Acircle=pi*R*R
Asphere=4*Acircle
Vsphere=(4*Asphere*R)/3

WRITE(*,*)'circle area with radious R=',Acircle,'cm**2'
WRITE(*,*)'sphere area with radious R=',Asphere,'cm**2'
WRITE(*,*)'sphere volume with radious R=',Vsphere,'cm**3'

STOP
END PROGRAM p1_AliTavahodi_RezaBeyk
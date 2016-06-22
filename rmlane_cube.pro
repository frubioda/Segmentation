pro rmlane_cube,sparr,aarr
;
; purpose : lane finding algorithm after Roudier, Muller , SP 107 p.11 ff
; author : Johann Hirzberger 05-Jun-1995
; changes : Johann Hirzberger 06-Oct-1997
; changes : Johann Hirzberger 11-Jan-1999
; changes : Johann Hirzberger 10-Jan-2002
;
;
;
; input parameters
;
; first          number of the first image of the series
; last           number of the last image of the series
; resx           x-dimension of the images
; resy           y-dimension of the images
; apod           apodized edges [%]
; pxlsize        pixelsize [arcsec]
; rma,rmb,rmc    constants in the Fourier filter    
; nsd            cutting level [mean+nsd*(standard deviation of the image)]
;
;
; output
; 
; aim            area image
; uim            perimeter image
;                in uim each granule gets an interger value, if
;                the granule is truncated by the image boundaries
;                it gets the integer value + 10000)
;
;
; --------------------------------------------------------------
;
loadct,3
!order=0
close,/all
;for i=0,3 do wdelete,i
;
;
;
; input/output paths
;
;str1=string('/home/jkh/23.10.99/aim/g2_aim_subsonic.')
;str3=string('/home/jkh/23.10.99/aim/g2_fg_subsonic.')
;str1=string('/home/jkh/23.10.99/aim/g2_aim.')
;str3=string('/home/jkh/23.10.99/aim/g2_fg.')
;
;restore,'/home/jkh/23.10.99/aim/v3d_subsonic.sav'
;restore,'/home/jkh/23.10.99/aim/v3d.sav'
;
;sparr=sparr_sub
;v0arr=v0arr_sub
;v2arr=v2arr_sub
;i0arr=i0arr_sub
;i2arr=i2arr_sub
;
;
s=size(sparr)
first=0
last=s(3)-1
resx=s(1)
resy=s(2)
;
apod=5
x1=rfix(resx*apod/100.)
x2=resx-x1
y1=rfix(resy*apod/100.)
y2=resy-y1
rx=x2-x1
ry=y2-y1
print,x1,x2,y1,y2
stop
mres=resx>resy
im=fltarr(mres,mres)
grad_im=fltarr(mres,mres)
;
aarr=intarr(rx,ry,s(3))
;
pxlsize=0.1
;
rma=4.95     ; passt zu
rmb=1.8      ; Hirzberger et al.
rmc=0.55     ; 1997,
nsd=0.       ; ApJ, 480,406
;
;rma=1.1
;rmb=3.7
;rmc=1.25
;nsd=0.5
;
; -------------------------------------------------------------------
;
;
; Calculating filter
;
       xmid=fix(mres/2)
       kxstep=1./(pxlsize*mres)
       kx=findgen(xmid+1)*kxstep
       karcsec=kx
       karcsec(1:xmid)=1./(karcsec(1:xmid))
;
       filter=dist(mres)/xmid
       filter=(1-exp(-rma*rmb*rmb*filter*filter))*$
               exp(-rma*rmc*rmc*filter*filter)
;       filter=exp(-rma*rmc*rmc*filter*filter)
       window,1,xsize=450,ysize=300,xpos=550,ypos=50
       plot,kx,filter(0:xmid,0),xtitle='k [arcsec!e-1!n]'
;       save,kx,filter,xmid,filename='/das1/jkh/dat/filrm.dat'
       hw=max(filter(*,0))/2.
       hwarr=replicate(hw,xmid+1)
       oplot,kx,hwarr
       maxloc=0
       hwloc1=0
       hwloc2=0
;
;  calculate maximum position and FWHM
;
       fil1d=reform(filter(0:xmid,0),xmid+1)
       for i=1,xmid do begin
         if fil1d(i) gt fil1d(i-1) then maxloc=i 
         if (fil1d(i) le hw) and (fil1d(i-1) ge hw) then hwloc1=i
         if (fil1d(i) ge hw) and (fil1d(i-1) le hw) then hwloc2=i
       endfor
       print,''
       print,'Maximum of filter at : ',karcsec(maxloc),' arcsec'
       print,'Halfwidth : '+ strtrim(abs(karcsec(hwloc1)-karcsec(hwloc2)),1)+ $
             ' arcsec' 
       print,'between ',karcsec(hwloc1),' and ',karcsec(hwloc2), ' arcsec'
       print,''
;
; -----------------------------------------------------------------------
;
;
for picnum=first,last do begin
;
; if (picnum eq 13) or (picnum eq 14) then goto, sprung
;
       
       pic=sparr(*,*,picnum)
;       v0=v0arr(*,*,picnum)
;       v2=v2arr(*,*,picnum)
;       i0=i0arr(*,*,picnum)
;       i2=i2arr(*,*,picnum)
;
       window,3,xsize=rx,ysize=ry,xpos=20,ypos=550
       window,2,xsize=rx,ysize=ry,xpos=20,ypos=50
       window,0,xsize=rx,ysize=ry,xpos=20,ypos=300
       wset,3
       tvscl,pic(x1:x2-1,y1:y2-1)
       wset,0
       im(0,0)=pic
;
; Fourier Transform
;
       fim=fft(im,-1)
;
; Inverse Fourier Transform
;
       im=fft(fim*filter,1)
       im=float(im)
       ims=im(0:resx-1,0:resy-1)
;
; displaying filtered image
;
      imz=im(x1:x2-1,y1:y2-1)
      grad_imz=grad_im(x1:x2-1,y1:y2-1)
;      v0=v0(x1:x2-1,y1:y2-1)
;      v2=v2(x1:x2-1,y1:y2-1)
;      i0=i0(x1:x2-1,y1:y2-1)
;      i2=i2(x1:x2-1,y1:y2-1)
      sim=pic(x1:x2-1,y1:y2-1)
      wset,0
      tvscl,imz
;
; lane finding:
;
      sd=stdev(imz,mean)
      gim=(imz gt mean+nsd*sd)
      for i=1,rx-2 do begin
        for j=1,ry-2 do begin
          if (gim(i,j) eq 1) and (gim(i-1,j) eq 0) and (gim(i+1,j) eq 0) then $
              gim(i,j)=0
          if (gim(i,j) eq 1) and (gim(i,j-1) eq 0) and (gim(i,j+1) eq 0) then $
              gim(i,j)=0
        endfor
      endfor
;      gim=1-gim
      tvscl,gim
;
; calculate Ag(= Area of granules)
;
ag=total(gim)/n_elements(gim)
print,'Image number : ', picnum
print,'Ag       = ',ag*100.,' %'
;
;
;
;strr='y'
;read,'Good filter (Y/N) : ',strr
;if strr ne 'y' then goto,newfil
;
;
; now search for single granules
;
dim=gim
uim=intarr(rx,ry)
aim=intarr(rx,ry)
coord=intarr(2,5000)
newcoord=fltarr(2,5000)
grnum=0
;
;
for j=0,ry-1 do begin
  for i=0,rx-1 do begin
    if dim(i,j) eq 1 then begin
     a=1
     u=1
     np=0
     nnewp=0
     coord(0,np)=i
     coord(1,np)=j
     dim(i,j)=0
     aim(i,j)=grnum+1
     uim(i,j)=grnum+1

repeat begin
  for n=0,np do begin
left:
     l=coord(0,n)-1
     m=coord(1,n)
     if l ge 0 then begin
     if dim(l,m) eq 1 then begin
       nnewp=nnewp+1
       newcoord(0,nnewp-1)=l
       newcoord(1,nnewp-1)=m
       dim(l,m)=0
       aim(l,m)=grnum+1
       a=a+1
     endif
     endif
right:
     l=coord(0,n)+1
     m=coord(1,n)
     if l lt rx then begin
     if dim(l,m) eq 1 then begin
       nnewp=nnewp+1
       newcoord(0,nnewp-1)=l
       newcoord(1,nnewp-1)=m
       dim(l,m)=0
       aim(l,m)=grnum+1
       a=a+1
     endif
     endif
up:
     l=coord(0,n)
     m=coord(1,n)+1
     if m lt ry then begin
     if dim(l,m) eq 1 then begin
       nnewp=nnewp+1
       newcoord(0,nnewp-1)=l
       newcoord(1,nnewp-1)=m
       dim(l,m)=0
       aim(l,m)=grnum+1
       a=a+1
     endif
     endif
down:
     l=coord(0,n)
     m=coord(1,n)-1
     if m ge 0 then begin
     if dim(l,m) eq 1 then begin
       nnewp=nnewp+1
       newcoord(0,nnewp-1)=l
       newcoord(1,nnewp-1)=m
       dim(l,m)=0
       aim(l,m)=grnum+1
       a=a+1
     endif
     endif
  endfor
  np=nnewp
  nnewp=0
  coord=newcoord
  endrep until np eq 0
  grnum=grnum+1
  endif
endfor
endfor
tvscl,aim
;
; calculate perimeters
;
for i=1,grnum do begin
  gran=where(aim eq i)
  nn=n_elements(gran)
  for j=0,nn-1 do begin
    line=fix(gran(j)/rx)
    row=gran(j) mod rx
;
; marking granules which are at the edge of the image
;
    if (line eq 0) or (line eq ry-1) or (row eq 0) or $
       (row eq rx-1) then begin
         uim(gran(j))=10000+i
         goto,fin
    endif
    if (aim(gran(j)-1) eq 0) then uim(gran(j))=i else begin
      if (aim(gran(j)+1) eq 0) then uim(gran(j))=i else begin
        if (aim(gran(j)-rx) eq 0) then uim(gran(j))=i else begin
          if (aim(gran(j)+rx) eq 0) then uim(gran(j))=i
        endelse
      endelse
    endelse
fin :
  endfor
endfor  
;
uim1=uim
if max(uim1) gt 10000 then uim1(where(uim1 ge 10000))=0
wset,2
tvscl,uim1
wset,0
;
   aarr(*,*,picnum)=aim
;
;   s=size(aim)
;   finalim=fltarr(s(1),s(2),7)
;   finalim(*,*,0)=aim
;   finalim(*,*,1)=uim
;   finalim(*,*,2)=sim
;   finalim(*,*,3)=v0
;   finalim(*,*,4)=v2
;   finalim(*,*,5)=i0
;   finalim(*,*,6)=i2
;
; saving output
;
;openw,1,str1+strtrim(picnum,1)
;  writeu,1,finalim
;close,1
;
;openw,1,str3+strtrim(picnum,1)
;  printf,1,s(1),s(2)
;close,1
;
print,'Number of granules : ',max(aim)
print,''
print,'---------------'
print,''
;
;
;
sprung:
;
;
endfor
;
end










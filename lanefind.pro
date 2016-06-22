pro lanefind,im,aim,uim,rma,rmb,rmc,nsd, RESOLUTION=resolution,$
             MINAREA=minarea,SILENT=silent
;
; purpose : lane finding algorithm after Roudier & Muller , Solar Phys. 107, p.11 ff
; author : Johann Hirzberger 05-Jun-1995
; changes : Johann Hirzberger 06-Oct-1997
; changes : Johann Hirzberger 11-Jan-1999
; changes : Johann Hirzberger 10-Aug-2001
; changes : Johann Hirzberger 06-Sep-2005
;
;
;
; input parameters
;
; im             image to be segmented
;
; rma,rmb,rmc    constants in the Fourier filter
;                y = (1 - exp(-rma * rmb * x^2)) * exp(-rma * rmc * x^2) 
;
; nsd            cutting level [mean+nsd*(standard deviation of the image)]
;
; resolution     arcsec/pixel
;
; minarea        minimum size of structures to be kept [pixe]
;
;
; output
; 
; aim            area image
;                in uim each granule gets an interger value
; uim            perimeter image
;                in uim each granule gets an integer value (same as
;                in aim), if the granule is truncated by the image 
;                boundaries it gets the integer value + 10000
;
;
; --------------------------------------------------------------
;
; INPUT PARAMETERS :
;
;---------------------------------------------------------------
;
;
;rma=3.56
;rmb=2.5
;rmc=1.0
;nsd=0.
;
;---------------------------------------------------------------
;

IF KEYWORD_SET(resolution) EQ 0 THEN pxlsize=0.1 ELSE pxlsize=0.1
IF NOT KEYWORD_SET(MINAREA) THEN minarea = 4

!order=0
close,/all
;
;
s=size(im)
;
resx=s(1)
resy=s(2)
;
apod=2.
x1=rfix(resx*apod/100.)
y1=rfix(resy*apod/100.)
x2=resx-x1
y2=resy-y1
rx=x2-x1
ry=y2-y1
;
mres=resx>resy
;
;---------------------------------------------------------------
;
; Calculating filter
;
;---------------------------------------------------------------
;

xmid=fix(resx/2)
ymid=fix(resy/2)

kxstep=1./(pxlsize*resx)
kystep=1./(pxlsize*resy)

kx=findgen(xmid+1)*kxstep
ky=findgen(ymid+1)*kystep
karcsec=kx
karcsec(1:xmid)=1./(karcsec(1:xmid))
;
filter=dist(mres)/xmid
filter=congrid(filter,resx,resy,/interp)
filter=(1-exp(-rma*rmb*rmb*filter*filter))*$
        exp(-rma*rmc*rmc*filter*filter)

IF NOT KEYWORD_SET(SILENT) THEN BEGIN

   window,3,xsize=450,ysize=300,xpos=550,ypos=50
   plot,kx,filter(0:xmid,0),xtitle='k [arcsec!e-1!n]'

   hw=max(filter(*,0))/2. 
   hwarr=replicate(hw,xmid+1)
   oplot,kx,hwarr 
   maxloc=0

;-------------------------------------------------------
;  calculate maximum position and FWHM
;-------------------------------------------------------

   for i=1,xmid do begin
       if filter(i,0) gt filter(i-1,0) then maxloc=i 
       if (filter(i,0) le hw) and (filter(i-1,0) ge hw) then hwloc1=i
       if (filter(i,0) ge hw) and (filter(i-1,0) le hw) then hwloc2=i
   endfor
      
   IF ((n_elements(hwloc1) GT 0) AND (n_elements(hwloc2) GT 0)) THEN BEGIN

      print,''
      print,'Maximum of filter at : ',karcsec(maxloc),' arcsec'
      print,'Halfwidth : between ',karcsec(hwloc1),' and ',karcsec(hwloc2),$
           ' arcsec' 
      print,''

   ENDIF

ENDIF

;-------------------------------------------------------
; Segmentation
;-------------------------------------------------------

apo2d,im,apod
;

IF NOT KEYWORD_SET(SILENT) THEN BEGIN

   window,2,xsize=rx,ysize=ry,xpos=20,ypos=650
   window,1,xsize=rx,ysize=ry,xpos=20,ypos=350
   window,0,xsize=rx,ysize=ry,xpos=20,ypos=50

   wset,0
   tvscl,im(x1:x2-1,y1:y2-1)

ENDIF

pic = im
fim=fft(pic,-1)
pic=fft(fim*filter,1)
pic=float(pic)

imz=pic(x1:x2-1,y1:y2-1)


IF NOT KEYWORD_SET(SILENT) THEN BEGIN

       wset,1
       tvscl,imz

ENDIF

sd=stdev(imz,mean)
gim=(imz gt mean+nsd*sd)
;      
for i=1,rx-2 do begin
    for j=1,ry-2 do begin
        if (gim(i,j) eq 1) and (gim(i-1,j) eq 0) and (gim(i+1,j) eq 0) then $
           gim(i,j)=0
        if (gim(i,j) eq 1) and (gim(i,j-1) eq 0) and (gim(i,j+1) eq 0) then $
           gim(i,j)=0
    endfor
endfor

IF NOT KEYWORD_SET(SILENT) THEN BEGIN

   wset,2
   tvscl,gim

ENDIF

;-------------------------------------------------------
; calculate Ag(= Area of granules)
;-------------------------------------------------------

ag=total(gim)/n_elements(gim)

IF NOT KEYWORD_SET(SILENT) THEN print,'Ag       = ',ag*100.,' %'

;-------------------------------------------------------
; now search for single granules 
;-------------------------------------------------------

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
;     uim(i,j)=grnum+1

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

IF NOT KEYWORD_SET(SILENT) THEN BEGIN

   wset,2
   tvscl,aim

ENDIF

remove_small,aim,minarea

k = 1

FOR i=1,grnum DO BEGIN

    wg = where(aim EQ i,cnt)

    IF cnt NE 0 THEN BEGIN

       aim[wg] = k 
       k = k + 1
   
   ENDIF

ENDFOR

grnum = max(aim)

;-------------------------------------------------------
; calculate perimeters
;-------------------------------------------------------

for i=1,grnum do begin
  gran=where(aim eq i)
  nn=n_elements(gran)
  for j=0,nn-1 do begin
    line=fix(gran(j)/rx)
    row=gran(j) mod rx

;-------------------------------------------------------
; marking granules which are at the edge of the image 
;-------------------------------------------------------

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
wbad=where(uim ge 10000,count)
if count gt 0 then uim1(wbad)=0

IF NOT KEYWORD_SET(SILENT) THEN BEGIN

   wset,1
   tvscl,uim1

   print,'Number of granules : ',max(aim)
   print,''
   print,'---------------'
   print,''

ENDIF

aim1 = intarr(resx,resy)
uim1 = intarr(resx,resy)

aim1(x1,y1) = aim
uim1(x1,y1) = uim

aim = aim1
uim = uim1

end

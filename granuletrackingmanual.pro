; pro granuletrackingmanual
;
; Purpose :
; Author  :
; Changes :
;
;
;
close,/all
loadct,13
;
scan=40
first=14
ngran=40
newscan:
;
flag1=0
moreimforw=0
moreimback=10
nmore=0
;for first=70,70 do begin
;
;print,'Starting Image Nr.: ',first
i=fix(first)
;
openw,10,'/home/jkh/penumbra/t'+strtrim(i,1)+$
         '.'+strtrim(scan,1)
;
sel_gran_num=0
newbegin:
i=rfix(first)
;
;readmode:
;read,'Mode (1/2): ',mode
;mode=fix(mode)
;if (mode ne 1) and (mode ne 2) then goto,readmode
;
im=fltarr(120,210)
im0=fltarr(120,210)
ima=intarr(120,210)
;imp=fltarr(120,210)
imaf=intarr(120,210)
imtv1=intarr(120,210)
imtv2=intarr(120,210)
imarr=intarr(5,120,210)
sel_num=fltarr(5)
imcoll=intarr(120,210)
imcoll0=intarr(120,210)
area=intarr(28)
areasum=intarr(28)
savearr=intarr(28)
primcoord=intarr(2)
header=bytarr(2880)
;
str1=string('/home/jkh/penumbra/aim')
str2=string('/home/jkh/penumbra/spim')
;str4=string('/home/jkh/aim95/grb_rmperi.')
str3=string('.fts')
;
;
dcn=strtrim(i,1)
;if strlen(dcn) eq 1 then str='000'+dcn
;if strlen(dcn) eq 2 then str='00'+dcn
;if strlen(dcn) eq 3 then str='0'+dcn
;print,'Now reading image : '+dcn
;openr,1,str1+dcn+str3
;  readu,1,header,ima
;close,1
ima=readfits(str1+dcn+str3,/silent)
;
;openr,1,str4+dcn
;  readu,1,pim
;close,1
;
dcn=strtrim(i,1)
;if strlen(dcn) eq 1 then str='000'+dcn
;if strlen(dcn) eq 2 then str='00'+dcn
;if strlen(dcn) eq 3 then str='0'+dcn
;openr,1,str2+dcn+str3
;  readu,1,header,im0
;close,1
im0=readfits(str2+dcn+str3,/silent)
;
;window,10,xsize=202,ysize=202,xpos=5,ypos=31,title='Image-4'
;window,11,xsize=202,ysize=202,xpos=217,ypos=31,title='Image-3'
;window,12,xsize=202,ysize=202,xpos=429,ypos=31,title='Image-2'
;window,13,xsize=202,ysize=202,xpos=641,ypos=31,title='Image-1'
;window,14,xsize=202,ysize=202,xpos=853,ypos=31,title='Actual Image'
window,0,xsize=461,ysize=387,xpos=680,ypos=515,title='Actual Image'
;window,2,xsize=303,ysize=303,xpos=5,ypos=596,title='First Image'
window,3,xsize=303,ysize=303,xpos=5,ypos=263,title='Last 5 Images'
imtv1=ima
;
maxgran=max(ima)
print,'Number of Granules : '+strtrim(maxgran,1)
;rdn=randomu(seed)
;ngran=fix(rdn*maxgran)
;ngran=ngran+1
;if ngran gt maxgran-70 then goto, totalend
savearr(i)=ngran
;
imtv1(where(imtv1 gt 0))=1
wset,0
tvscl,congrid(imtv1,461,387)
print,'Selected Granule Nr.: ',ngran
gran0=where(ima eq ngran)
gran=gran0
imtv1(gran)=2
tvscl,congrid(imtv1,461,387)
n10=n_elements(gran)
;pedge=where(pim eq ngran+10000,npe)
;if npe gt 0 then goto, newselect
;if n10 gt 1500 then goto, newselect
;if n10 lt 18 then goto, newselect
;
n1=n10
area(i)=n1
areasum(i)=n1
x=lonarr(n1)
y=lonarr(n1)
r=lonarr(n1)
for k=0,n1-1 do begin 
  x(k)=gran(k) mod 120
  y(k)=gran(k)/120
endfor
xs0=rfix(total(x)/n1)
ys0=rfix(total(y)/n1)
primcoord(0)=xs0
primcoord(1)=ys0
print,'Area : '+strtrim(n1,1)
corrsize=rfix(sqrt(4.*n1))
if corrsize mod 2 ne 0 then corrsize=corrsize+1
if corrsize lt 30 then corrsize=30
print,'Correlation Window : ',corrsize
;read,'Admissible Correlation : ',maxcorr
maxcorr=0.
;print,'Admissible Correlation : '+strtrim(maxcorr,1)
cim0=fltarr(corrsize,corrsize)
cim1=fltarr(corrsize,corrsize)
;
xmin=xs0-corrsize/2
xmax=xs0+corrsize/2-1
ymin=ys0-corrsize/2
ymax=ys0+corrsize/2-1
if xmin lt 0 then goto,stop
if ymin lt 0 then goto,stop;window
if xmax gt 119 then goto,stop;window
if ymax gt 209 then goto,stop;window
;
cim0=im0(xmin:xmax,ymin:ymax)
;
;wset,2
xmin=xs0-corrsize/2
xmax=xs0+corrsize/2-1
ymin=ys0-corrsize/2
ymax=ys0+corrsize/2-1
if xmin lt 0 then goto,stop;window
if ymin lt 0 then goto,stop;window
if xmax gt 119 then goto,stop;window
if ymax gt 209 then goto,stop;window
imtv1(xmin,ymin:ymax)=2
imtv1(xmax,ymin:ymax)=2
imtv1(xmin:xmax,ymin)=2
imtv1(xmin:xmax,ymax)=2
;
  xmin=xs0-50
  xmax=xs0+50
  ymin=ys0-50
  ymax=ys0+50
  if xmin lt 0 then begin
     xmin=0
     xmax=100
  endif
  if ymin lt 0 then begin
     ymin=0
     ymax=100
  endif
  if xmax gt 119 then begin
     xmax=119
     xmin=19
  endif
  if ymax gt 209 then begin
     ymax=209
     ymin=109
  endif
;  tvscl,congrid(imtv1(xmin:xmax,ymin:ymax),303,303)
;
imcoll0(gran)=1
imarr(0,*,*)=imcoll0
sel_num(*)=1.0
;
;
;
;
;
newforward :
;
;  if ((i+1) gt 27) and (flag1 eq 0) then goto, newselect
;  if ((i+1) gt 27) and (flag1 eq 1) then goto, stopforwardtotal
  if ((i+1) gt 27) then goto, stopforwardtotal
  dcn=strtrim(i+1,1)
;  if strlen(dcn) eq 1 then str='000'+dcn
;  if strlen(dcn) eq 2 then str='00'+dcn
;  if strlen(dcn) eq 3 then str='0'+dcn
  ;print,'Now reading image : '+dcn  
;  openr,1,str1+dcn+str3
;    readu,1,header,imaf
;  close,1
imaf=readfits(str1+dcn+str3,/silent)
;
;  openr,1,str4+dcn
;    readu,1,pim
;  close,1
;
  dcn=strtrim(i+1,1)
;  if strlen(dcn) eq 1 then str='000'+dcn
;  if strlen(dcn) eq 2 then str='00'+dcn
;  if strlen(dcn) eq 3 then str='0'+dcn
;  openr,1,str2+dcn+str3
;    readu,1,header,im
;  close,1
im=readfits(str2+dcn+str3,/silent)

;
  x=lonarr(n1)
  y=lonarr(n1)
  r=lonarr(n1)
  for k=0,n1-1 do begin
    x(k)=gran(k) mod 120
    y(k)=gran(k)/120
  endfor
  xs=rfix(total(x)/n1)
  ys=rfix(total(y)/n1)
  print,'center of mass : ',xs,ys
;
  xmin=xs-15
  xmax=xs+15
  ymin=ys-15
  ymax=ys+15
  if xmin lt 0 then goto,stop;window
  if ymin lt 0 then goto,stop;window
  if xmax gt 119 then goto,stop;window
  if ymax gt 209 then goto,stop;window
  imbox=imaf(xmin:xmax,ymin:ymax)
  r=lonarr(961)
  x=lonarr(961)
  y=lonarr(961)
  for k=0,960 do begin
    x(k)=k mod 31
    y(k)=k/31 
    if imbox(k) gt 0 then $
                     r(k)=rfix(sqrt((15-x(k))^2.+(15-y(k))^2.)) else r(k)=15
  endfor
  rmin=min(r)
  print,'Minimum Distance : ',rmin
  min_dist=imbox(where(r eq rmin))
  print,'New Granule Numbers : ',min_dist
  nmin=n_elements(min_dist)
;
  xmin=xs-corrsize/2+15
  xmax=xs+corrsize/2-1+15
  ymin=ys-corrsize/2+15
  ymax=ys+corrsize/2-1+15
  if xmin lt 0 then goto,stop;window
  if ymin lt 0 then goto,stop;window
  if xmax gt 119 then goto,stop;window
  if ymax gt 209 then goto,stop;window
  cim1=im(xmin:xmax,ymin:ymax)
  cim=correl_images1(cim0,cim1,xshift=3,yshift=3)
  print,'Mean correlation : ',mean(cim)
  print,''
  imtv2=imaf
  imtv2(where(imtv2 gt 0))=1
  for k=0,nmin-1 do begin
    imtv2(where(imaf eq min_dist(k)))=2
;    pedge=where(pim eq min_dist(k)+1000,npe)
;    if npe gt 0 then goto, newselect
  endfor
  savearr(i+1)=min_dist(0)
  imtv2(xs,ys)=3
  agran=where(imtv2 ge 2,narea)
;  if flag1 eq 0 then area(i+1)=narea else areasum(i+1)=narea
  area(i+1)=narea
  wset,0
  tvscl,congrid(imtv2,461,387)
;  wset,2
;  tvscl,congrid(imtv2(xs-15:xs+15,ys-15:ys+15),124,124)
 
  imarr=shift(imarr,1,0,0)
  sel_num=shift(sel_num,1)
  imcoll(*,*)=0
  sel_num(0)=mean(cim)
  for k=0,nmin-1 do begin
    gran1=where(imaf eq min_dist(k),n3)
    imcoll(gran1)=imcoll(gran1)+1
  endfor
  imarr(0,*,*)=imcoll
  imcoll=reform(imarr(0,*,*)+imarr(1,*,*)+$
                imarr(2,*,*)+imarr(3,*,*)+imarr(4,*,*),120,210)
;
  xmin=xs-15
  xmax=xs+15
  ymin=ys-15
  ymax=ys+15
  if xmin lt 0 then goto,stop;window
  if ymin lt 0 then goto,stop;window
  if xmax gt 119 then goto,stop;window
  if ymax gt 209 then goto,stop;window
  for k=0,4 do begin
;    wset,10+k
;    tvscl,congrid(reform(imarr(4-k,xmin:xmax,ymin:ymax)),124,124)
  endfor
  gran=where(imcoll eq max(imcoll),n1)
  areasum(i+1)=n1
;
;wset,3
;tvscl,congrid(imcoll(xs-15:xs+15,ys-15:ys+15),124,124)  
;
if flag1 eq 1 then begin
  nmore=nmore+1
  goto,stopforward
endif
;
;if (total(sel_num)/5. lt maxcorr) or (sel_num(0) lt 0.2) then begin 
;   flag1=1    
;   goto,stopforward
;endif
;
  ima=imaf
  i=i+1
  goto,newforward 
;
;
;
;
stopforward:
if nmore eq moreimforw then goto,stopforwardtotal
print,''
print,'Granule evolved, now tracking '+strtrim(moreimforw-nmore,1)+' more frames. '
print,''
  ima=imaf
  i=i+1
  goto,newforward
;
stopforwardtotal:
flag1=0
nmore=0
;
max_im_nr=i
imarr(*,*,*)=0
imarr(0,*,*)=imcoll0
sel_num(*)=1.0
i=rfix(first)
gran=gran0
n1=n10
xs=xs0
ys=ys0
print,''
print,'Now tracking backward !'
print,''
goto,newbackward
;
;
  newbackward:
;
;  if ((i-1) lt 12) and (flag1 eq 0) then goto, newselect
;  if ((i-1) lt 12) and (flag1 eq 1) then goto, stopbackwardtotal
 if ((i-1) lt 0) then goto, stopbackwardtotal
 dcn=strtrim(i-1,1)
;  if strlen(dcn) eq 1 then str='000'+dcn
;  if strlen(dcn) eq 2 then str='00'+dcn
;  if strlen(dcn) eq 3 then str='0'+dcn
  ;print,'Now reading image : '+dcn  
;  openr,1,str1+dcn+str3
;    readu,1,header,imaf
;  close,1
imaf=readfits(str1+dcn+str3,/silent)
;
;  openr,1,str4+dcn
;    readu,1,pim
;  close,1
;
  dcn=strtrim(i-1,1)
;  if strlen(dcn) eq 1 then str='000'+dcn
;  if strlen(dcn) eq 2 then str='00'+dcn
;  if strlen(dcn) eq 3 then str='0'+dcn
;  openr,1,str2+dcn+str3
;    readu,1,header,im
;  close,1
im=readfits(str2+dcn+str3,/silent)
;
  x=lonarr(n1)
  y=lonarr(n1)
  r=lonarr(n1)
  for k=0,n1-1 do begin
    x(k)=gran(k) mod 120
    y(k)=gran(k)/120
  endfor
  xs=rfix(total(x)/n1)
  ys=rfix(total(y)/n1)
  ;print,'center of mass : ',xs,ys
;
  xmin=xs-15
  xmax=xs+15
  ymin=ys-15
  ymax=ys+15
  if xmin lt 0 then goto,stop;window
  if ymin lt 0 then goto,stop;window
  if xmax gt 119 then goto,stop;window
  if ymax gt 209 then goto,stop;window
  imbox=imaf(xmin:xmax,ymin:ymax)
  r=lonarr(961)
  x=lonarr(961)
  y=lonarr(961)
  for k=0,960 do begin
    x(k)=k mod 31
    y(k)=k/31 
    if imbox(k) gt 0 then $
                     r(k)=rfix(sqrt((15-x(k))^2.+(15-y(k))^2.)) else r(k)=15
  endfor
  rmin=min(r)
  print,'Minimum Distance : ',rmin
  min_dist=imbox(where(r eq rmin))
  print,'New Granule Numbers : ',min_dist
  nmin=n_elements(min_dist)
;
  xmin=xs-corrsize/2+15
  xmax=xs+corrsize/2-1+15
  ymin=ys-corrsize/2+15
  ymax=ys+corrsize/2-1+15
  if xmin lt 0 then goto,stop;window
  if ymin lt 0 then goto,stop;window
  if xmax gt 119 then goto,stop;window
  if ymax gt 209 then goto,stop;window
  cim1=im(xmin:xmax,ymin:ymax)
  cim=correl_images1(cim0,cim1,xshift=3,yshift=3)
  print,'Mean correlation : ',mean(cim)
  print,''
  imtv2=imaf
  imtv2(where(imtv2 gt 0))=1
  for k=0,nmin-1 do begin
    imtv2(where(imaf eq min_dist(k)))=2
;    pedge=where(pim eq min_dist(k)+10000,npe)
;    if npe gt 0 then goto, newselect
  endfor
  savearr(i-1)=min_dist(0)
  imtv2(xs,ys)=3
  agran=where(imtv2 ge 2,narea)
;  if flag1 eq 0 then area(i-1)=narea else areasum(i-1)=narea
   area(i-1)=narea
  wset,0
  tvscl,congrid(imtv2,461,387)
;  wset,2
;  tvscl,congrid(imtv2(xs-15:xs+15,ys-15:ys+15),124,124)
; 
  imarr=shift(imarr,1,0,0)
  sel_num=shift(sel_num,1)
  imcoll(*,*)=0
  sel_num(0)=mean(cim)
  for k=0,nmin-1 do begin
    gran1=where(imaf eq min_dist(k),n3)
    imcoll(gran1)=imcoll(gran1)+1
  endfor
  imarr(0,*,*)=imcoll
  imcoll=reform(imarr(0,*,*)+imarr(1,*,*)+$
                imarr(2,*,*)+imarr(3,*,*)+imarr(4,*,*),120,210)
;
  xmin=xs-15
  xmax=xs+15
  ymin=ys-15
  ymax=ys+15
  if xmin lt 0 then goto,stop;window
  if ymin lt 0 then goto,stop;window
  if xmax gt 119 then goto,stop;window
  if ymax gt 209 then goto,stop;window
  for k=0,4 do begin
;    wset,10+k
;    tvscl,congrid(reform(imarr(4-k,xmin:xmax,ymin:ymax)),124,124)
  endfor
  gran=where(imcoll eq max(imcoll),n1)
  areasum(i-1)=n1
;
wset,3
;tvscl,congrid(imcoll(xs-15:xs+15,ys-15:ys+15),124,124)  
;
if flag1 eq 1 then begin
  nmore=nmore+1
  goto,stopbackward
endif
if (total(sel_num)/5. lt maxcorr) or (sel_num(0) lt 0.2) then begin
     flag1=1
     goto,stopbackward
endif
;
  ima=imaf
  i=i-1
  goto,newbackward
;
;
;
;
stopbackward:
if nmore eq moreimback then goto,stopbackwardtotal
print,''
print,'Granule evolved, now tracking '+strtrim(moreimback-nmore,1)+' more frames. '
print,''
  ima=imaf
  i=i-1
  goto,newbackward
;
stopbackwardtotal:
flag1=0
nmore=0
;
print,''
print,'Granule evolved, stop tracking.'
print,''
min_im_nr=i
;for i=0,32 do wdelete,i
;
plot,area,xrange=[min_im_nr,max_im_nr],yticks=10,ticklen=1,psym=4
oplot,area,psym=4,thick=3
oplot,areasum,psym=4,color=90
oplot,[min_im_nr,max_im_nr],[2*n10,2*n10],linestyle=1,color=70
oplot,[min_im_nr,max_im_nr],[n10,n10],linestyle=1,color=90
oplot,[min_im_nr,max_im_nr],[0.5*n10,0.5*n10],linestyle=1,color=70
oplot,[min_im_nr,max_im_nr],[0.25*n10,0.25*n10],linestyle=1,color=70
;
writeu,10,primcoord
writeu,10,savearr
writeu,10,area
;writeu,10,areasum
sel_gran_num=sel_gran_num + 1
print,''
print,'Number of saved granules : ',strtrim(sel_gran_num,1)
print,''
goto,stop
;
stopwindow:
print,''
print,'Window out of Image Border, Stopping !!!'
print,''
;for i=0,32 do wdelete,i 
;
stop:
;
newselect:
if sel_gran_num lt 1 then begin
   print,'Granule out of Series Border, Stopping !!!'
   close,10
   goto, totalend
endif
close,10
;
print,systime()
;
;
;
;endfor
;
;scan=scan+1
;goto,newscan
;
totalend:
;
end

pro lanefind3d,cube,acube,ucube,rma,rmb,rmc,nsd, RESOLUTION=resolution,$
               CADENCE=cadence,MINCUBE=mincube,SILENT=silent
;
; purpose : lane finding algorithm after Roudier, Muller , SP 107 p.11 ff
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
; output
; 
; acube          area image
;                in ucube each granule gets an interger value
; ucube            perimeter image
;                in ucube each granule gets an interger value (same as
;                in acube), if the granule is truncated by the image 
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

IF NOT KEYWORD_SET(resolution) THEN resolution = 0.1
IF NOT KEYWORD_SET(cadence) THEN cadence = 60.
IF NOT KEYWORD_SET(MINCUBE) THEN mincube = 8

!order=0
close,/all
;
;
s=size(cube)
;
resx=s(1)
resy=s(2)
rest=s(3)
;
apod=5.
x1=rfix(resx*apod/100.)
y1=rfix(resy*apod/100.)
t1=rfix(rest*apod/100.)
x2=resx-x1
y2=resy-y1
t2=rest-t1
rx=x2-x1
ry=y2-y1
rt=t2-t1
;
;
;---------------------------------------------------------------
;
; Calculating filter
;
;---------------------------------------------------------------
;

xmid=fix(resx/2)
ymid=fix(resy/2)
tmid=fix(rest/2)

kxstep=1./(resolution*resx)
kystep=1./(resolution*resy)
ktstep=1./(cadence*rest)

kx=findgen(xmid+1)*kxstep
ky=findgen(ymid+1)*kystep
kt=findgen(tmid+1)*ktstep
karcsec=kx
karcsec(1:xmid)=1./(karcsec(1:xmid))
;
f3d = fltarr(xmid+1,ymid+1,tmid+1)

FOR i=0,xmid DO BEGIN

    FOR j=0,ymid DO BEGIN

        FOR k=0,tmid DO BEGIN

            x = float(i)/float(xmid) ; * kxstep
            y = float(j)/float(ymid) ; * kystep
            t = float(k)/float(tmid) ; * ktstep

            r = sqrt(x^2 + y^2 + t^2)

            f3d[i,j,k] = r

        ENDFOR

    ENDFOR

ENDFOR

filter = fltarr(resx,resy,rest)

filter[0:xmid,0:ymid,0:tmid] = f3d
filter[xmid+1:resx-1,0:ymid,0:tmid] = reverse(f3d[1:xmid-1,*,*],1)
filter[0:xmid,ymid+1:resy-1,0:tmid] = reverse(f3d[*,1:ymid-1,*],2)
filter[0:xmid,0:ymid,tmid+1:rest-1] = reverse(f3d[*,*,1:tmid-1],3)
filter[xmid+1:resx-1,ymid+1:resy-1,0:tmid] = $
      reverse(reverse(f3d[1:xmid-1,1:ymid-1,*],1),2)
filter[xmid+1:resx-1,0:ymid,tmid+1:rest-1] = $
      reverse(reverse(f3d[1:xmid-1,*,1:tmid-1],1),3)
filter[0:xmid,ymid+1:resy-1,tmid+1:rest-1] = $
      reverse(reverse(f3d[*,1:ymid-1,1:tmid-1],2),3)
filter[xmid+1:resx-1,ymid+1:resy-1,tmid+1:rest-1] = $
      reverse(reverse(reverse(f3d[1:xmid-1,1:ymid-1,1:tmid-1],1),2),3)

;filter=dist(mres)/xmid
;filter=congrid(filter,resx,resy,/interp)
filter=(1-exp(-rma*rmb*rmb*filter*filter))*$
        exp(-rma*rmc*rmc*filter*filter)

IF NOT KEYWORD_SET(SILENT) THEN BEGIN

   window,3,xsize=450,ysize=300,xpos=550,ypos=50
   plot,kx,filter(0:xmid,0,0),xtitle='k [arcsec!e-1!n]'

   hw=max(filter(*,0,0))/2. 
   hwarr=replicate(hw,xmid+1)
   oplot,kx,hwarr 
   maxloc=0

;-------------------------------------------------------
;  calculate maximum position and FWHM
;-------------------------------------------------------

   for i=1,xmid do begin
       if filter(i,0,0) gt filter(i-1,0,0) then maxloc=i 
       if (filter(i,0,0) le hw) and (filter(i-1,0,0) ge hw) then hwloc1=i
       if (filter(i,0,0) ge hw) and (filter(i-1,0,0) le hw) then hwloc2=i
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

apo3d,cube,apod
;
IF NOT KEYWORD_SET(SILENT) THEN BEGIN

   window,0,xsize=rx,ysize=ry,xpos=20,ypos=50
   window,1,xsize=rx,ysize=ry,xpos=20,ypos=400
   window,2,xsize=rx,ysize=ry,xpos=20,ypos=750

   wset,0
   tvscl,cube(x1:x2-1,y1:y2-1,tmid)

ENDIF

pic = cube
fim=fft(pic,-1)
pic=fft(fim*filter,1)
pic=float(pic)

imz=pic(x1:x2-1,y1:y2-1,t1:t2)


IF NOT KEYWORD_SET(SILENT) THEN BEGIN

       wset,1
       tvscl,imz(*,*,tmid)

ENDIF

sd=stdev(imz,mean)
gim=(imz gt mean+nsd*sd)
;      
for i=1,rx-2 do begin
    for j=1,ry-2 do begin
        for k=1,rt-2 do begin

           if (gim[i,j,k] eq 1) and (gim[i-1,j,k] eq 0) and $
              (gim[i+1,j,k] eq 0) then gim[i,j,k]=0
           if (gim[i,j,k] eq 1) and (gim[i,j-1,k] eq 0) and $
              (gim[i,j+1,k] eq 0) then gim[i,j,k]=0
;          if (gim[i,j,k] eq 1) and (gim[i,j,k-1] eq 0) and $
;             (gim[i,j,k+1] eq 0) then gim[i,j,k]=0

        endfor
    endfor
endfor

;-------------------------------------------------------
; calculate Ag(= Area of granules)
;-------------------------------------------------------

ag=total(gim)/n_elements(gim)

IF NOT KEYWORD_SET(SILENT) THEN print,'Ag       = ',ag*100.,' %'

;-------------------------------------------------------
; now search for single granules 
;-------------------------------------------------------

dim=gim
ucube=intarr(rx,ry,rt)
acube=intarr(rx,ry,rt)
coord=intarr(3,5000)
newcoord=fltarr(3,50000)
grnum=0
;
;
for j=0,ry-1 do begin
  for i=0,rx-1 do begin
      for k=0,rt-1 do begin

          if dim(i,j,k) eq 1 then begin

             a=1
             u=1
             np=0
             nnewp=0
             coord(0,np)=i
             coord(1,np)=j
             coord(2,np)=k

             dim(i,j,k)=0
             acube(i,j,k)=grnum+1

             repeat begin

                for n=0,np do begin

left:

                    l=coord(0,n)-1
                    m=coord(1,n)
                    p=coord(2,n)

                    if l ge 0 then begin

                       if dim(l,m,p) eq 1 then begin

                          nnewp=nnewp+1
                          newcoord(0,nnewp-1)=l
                          newcoord(1,nnewp-1)=m
                          newcoord(2,nnewp-1)=p

                          dim(l,m,p)=0
                          acube(l,m,p)=grnum+1
                          a=a+1

                       endif
                 
                    endif

right:

                    l=coord(0,n)+1
                    m=coord(1,n)
                    p=coord(2,n)

                    if l LT rx then begin

                       if dim(l,m,p) eq 1 then begin

                          nnewp=nnewp+1
                          newcoord(0,nnewp-1)=l
                          newcoord(1,nnewp-1)=m
                          newcoord(2,nnewp-1)=p

                          dim(l,m,p)=0
                          acube(l,m,p)=grnum+1
                          a=a+1

                       endif
                 
                    endif

down:

                    l=coord(0,n)
                    m=coord(1,n)-1
                    p=coord(2,n)

                    if m ge 0 then begin

                       if dim(l,m,p) eq 1 then begin

                          nnewp=nnewp+1
                          newcoord(0,nnewp-1)=l
                          newcoord(1,nnewp-1)=m
                          newcoord(2,nnewp-1)=p

                          dim(l,m,p)=0
                          acube(l,m,p)=grnum+1
                          a=a+1

                       endif
                 
                    endif

up:

                    l=coord(0,n)
                    m=coord(1,n)+1
                    p=coord(2,n)

                    if m LT ry then begin

                       if dim(l,m,p) eq 1 then begin

                          nnewp=nnewp+1
                          newcoord(0,nnewp-1)=l
                          newcoord(1,nnewp-1)=m
                          newcoord(2,nnewp-1)=p

                          dim(l,m,p)=0
                          acube(l,m,p)=grnum+1
                          a=a+1

                       endif
                 
                    endif

backward:

                    l=coord(0,n)
                    m=coord(1,n)
                    p=coord(2,n)-1

                    if p ge 0 then begin

                       if dim(l,m,p) eq 1 then begin

                          nnewp=nnewp+1
                          newcoord(0,nnewp-1)=l
                          newcoord(1,nnewp-1)=m
                          newcoord(2,nnewp-1)=p

                          dim(l,m,p)=0
                          acube(l,m,p)=grnum+1
                          a=a+1

                       endif
                 
                    endif

forward:

                    l=coord(0,n)
                    m=coord(1,n)
                    p=coord(2,n)+1

                    if p LT rt then begin

                       if dim(l,m,p) eq 1 then begin

                          nnewp=nnewp+1
                          newcoord(0,nnewp-1)=l
                          newcoord(1,nnewp-1)=m
                          newcoord(2,nnewp-1)=p

                          dim(l,m,p)=0
                          acube(l,m,p)=grnum+1
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

endfor

IF NOT KEYWORD_SET(SILENT) THEN BEGIN

   wset,2
   tvscl,acube

ENDIF

remove_small,acube,mincube

k = 1

FOR i=1,grnum DO BEGIN

    wg = where(acube EQ i,cnt)

    IF cnt NE 0 THEN BEGIN

       acube[wg] = k 
       k = k + 1
   
   ENDIF

ENDFOR

grnum = max(acube)

;-------------------------------------------------------
; calculate perimeters
;-------------------------------------------------------

for i=1,grnum do begin

    gran=where(acube eq i)
    nn=n_elements(gran)

    for j=long(0),nn-1 do begin

        xc = gran(j) mod rx
        yc = (gran(j) mod (rx*ry)) / rx
        tc = gran(j)/(rx*ry)

;-------------------------------------------------------
; marking granules which are at the edge of the image 
;-------------------------------------------------------

        if (xc eq 0) or (xc eq rx-1) or (yc eq 0) or $
           (yc eq ry-1) or (tc eq 0) or (tc eq rt-1) then begin

           ucube[gran[j]]=-i

        endif else begin

           if (acube(gran(j)-1) eq 0) then ucube(gran(j))=i 
           if (acube(gran(j)+1) eq 0) then ucube(gran(j))=i 
           if (acube(gran(j)-rx) eq 0) then ucube(gran(j))=i 
           if (acube(gran(j)+rx) eq 0) then ucube(gran(j))=i
           if (acube(gran(j)-(rx*ry)) eq 0) then ucube(gran(j))=i 
           if (acube(gran(j)+(rx*ry)) eq 0) then ucube(gran(j))=i 
        
        endelse

    endfor
endfor  


IF NOT KEYWORD_SET(SILENT) THEN BEGIN

 ;  wset,1
 ;  tvscl,cube1

   print,'Number of granules : ',max(acube)
   print,''
   print,'---------------'
   print,''

ENDIF

acube1 = intarr(resx,resy,rest)
ucube1 = intarr(resx,resy,rest)

acube1(x1,y1,t1) = acube
ucube1(x1,y1,t1) = ucube

acube = acube1
ucube = ucube1

acube1 = 0
ucube1 = 0

END


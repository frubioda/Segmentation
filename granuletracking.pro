; granuletracking.pro
;
; Purpose :  Tracking of granules forward and backward in time
; Author  :  J. Hirzberger   09-Jun-1999
; Changes :
;
;
;--------------------------------------------------
;
; Automatical tracking of granules (see Hirzberger et al., ApJ 515, 441) 
;
;description of input parameters:
;
; first,last: length of time series
; startim: the program tracks all granules in startim forward and backward 
;          in time
; xdim, ydim : size of original AND area images 
;              the program loads all images into 3D arrays
;              i.e. it needs a lot of memory but the tracking becomes faster 
; minarea, maxarea : only granules (in startim) with areas between these
;                    two values are tracked
; corrbox, mincorrsize : the box for computing the correlation is
;          either sqrt(corrbox times the area of the granule) or mincorrsize
;          mincorrsize must be an even number
; maxcorr: the tracking is stopped when the averaged mean correlation
;          of 5 cosecutive frames is below maxcorr
; maxcorr1: the tracking is stopped immediately when the mean correlation 
;          is below maxcorr1
; moreimf, morimb: if one of the correlation conditions stops the tracking
;                  moreimf frames are tracked additonally forward and
;                  moreimb frames are tracked additionally backward. It's
;                  only useful when the result is used for tracking
;                  later on manually with the program "areaplot". 
;
; output :
;
; numvec    : a vector of length last+1 containing numbers of tracked granule
; areavec   : a vector of length last+1 containing areas of tracked granule
; primcoord : a vector of lenth 2 containing center-of-mass co-ordinates
;             of the tracked granule in startim
; results are saved in path/t(startim)_(story_nr).sav'
;
;--------------------------------------------------
;
close,/all
loadct,13
;for i=0,32 do wdelete,i
;
;
; input parameters
;
;
first=0                        ; number of first image in time series
last=27                        ; number of last image in time series
startim=14                     ; number of starting image
;
xdim=120                       ; x-dimension of images [pxl] 
ydim=210                       ; y-dimension of images [pxl]
;
minarea=1                      ; minimum area of granules being tracked [pxl]
maxarea=1500                   ; maximum area of granules being tracked [pxl]
;
corrbox=4                      ; area of box = area of granule * corrbox
mincorrsize=32                 ; minimum size of box [pxl]  (even number !!!)
;
maxcorr=0.3                    ; stopping when mean correlation lt maxcorr 
maxcorr1=0.2                   ; stopping when correlation lt maxcorr1 
;
moreimf=0                      ; number of frames additionally tracked forward
moreimb=0                      ; number of frames additionally tracked backward                ;
;
; file paths
;
;
str1=string('/home/jkh/penumbra/aim')        ; area images
str2=string('/home/jkh/penumbra/spim')       ; original images
str3=string('/home/jkh/penumbra/t')             ; output
;
;
; defining arrays
;
imorig=fltarr(xdim,ydim)
imaorig=intarr(xdim,ydim)
im3d=fltarr(last-first+1,xdim,ydim)
ima3d=intarr(last-first+1,xdim,ydim)
;
imcoll0=intarr(xdim,ydim)
imcoll=intarr(xdim,ydim)
imarr=intarr(5,xdim,ydim)
corrarr=fltarr(5)
;
;
;
; loading data
;
;
for i=first,last do begin 
  dcn=strtrim(i,1)
  print,'Now reading image : '+dcn
  imaorig=readfits(str1+dcn+'.fts')
  ima3d(i-first,*,*)=imaorig
;
  imorig=readfits(str2+dcn+'.fts')
  im3d(i-first,*,*)=imorig
endfor

s=size(imorig)
xdim=s(1)
ydim=s(2)

;
;
; opening windows
;
;
window,10,xsize=200,ysize=200,xpos=5,ypos=31,title='Image-4'
window,11,xsize=200,ysize=200,xpos=217,ypos=31,title='Image-3'
window,12,xsize=200,ysize=200,xpos=429,ypos=31,title='Image-2'
window,13,xsize=200,ysize=200,xpos=641,ypos=31,title='Image-1'
window,14,xsize=200,ysize=200,xpos=853,ypos=31,title='Actual Image'
window,0,xsize=461,ysize=387,xpos=680,ypos=515,title='Actual Image'
window,2,xsize=300,ysize=300,xpos=5,ypos=596,title='First Image'
window,3,xsize=300,ysize=300,xpos=5,ypos=263,title='Last 5 Images'
window,4,xsize=461,ysize=225,xpos=680,ypos=260,title='Tracked Areas'
;
;
; preparing starting image
;
;
imastart=reform(ima3d(startim-first,*,*),xdim,ydim)
imstart=reform(im3d(startim-first,*,*),xdim,ydim)
maxgran=max(imaorig)                                  ; number of granules 
;
print,'Number of Granules : '+strtrim(maxgran,1)
;
;
ngran=1
story_nr=0
;
REPEAT begin            ; tracking all granules 
;
  imarr(*,*,*)=0
  corrarr(*)=1.
  imcoll(*,*)=0
  nmore=0
  flag1=0
;
  imtv=imastart
  imtv(where(imastart gt 0))=1
  gran0=where(imastart eq ngran,countnew0)
  if countnew0 lt minarea then goto,nextgrannr  
  if countnew0 gt maxarea then goto,nextgrannr
;
;
; displaying selected granule
;
;
  print,'Granule Nr.: '+strtrim(ngran,1)
  print,'Area : '+strtrim(countnew0,1)+' pxl'  
  imtv(gran0)=2
  wset,0
  tvscl,imtv
;
;
; calculating center of mass
;
;
  x=gran0 mod xdim
  y=gran0/xdim
  xs0=rfix(total(x)/float(countnew0))
  ys0=rfix(total(y)/float(countnew0))
;
;
  numvec=intarr(last+1)
  areavec=intarr(last+1)
  primcoord=intarr(2)
;
  numvec(startim)=ngran           ;  storing number of granule
  areavec(startim)=countnew0      ;  storing area of granule
  primcoord(0)=xs0                ;  storing position
  primcoord(1)=ys0
;
;
; cutting box for correlation
;
;
  corrsize=rfix(sqrt(corrbox*countnew0))          ; boxsize=corrbox*granulesize
  if corrsize lt mincorrsize then corrsize=mincorrsize  ; minimum size
  if corrsize mod 2 ne 0 then corrsize=corrsize+1 ; even number
;
  xmin=xs0-corrsize/2        ; defining box coordinates
  xmax=xs0+corrsize/2-1
  ymin=ys0-corrsize/2
  ymax=ys0+corrsize/2-1
;
  if (xmin lt 0) or (ymin lt 0) or (xmax gt xdim-1) or (ymax gt ydim-1) $
    then begin
      print,''
      print,'Granule out of image border'       ;  taking care for image
      print,''                                  ;  boundaries
      goto,nextgrannr
  endif
;
  cim0=imstart(xmin:xmax,ymin:ymax)
;
;
; displaying box
;
;
  cima0=imtv(xmin:xmax,ymin:ymax)
  wset,2
  tvscl,congrid(cima0,300,300)
;
;
  imcoll0(gran0)=1
  imarr(0,*,*)=imcoll0
;
;
  print,''
  print,''
  print,'Now tracking forward'
  print,''
  print,''
;
;
;--------------------------------------------------------
;
; tracking forward
;
;--------------------------------------------------------
;
;
  iff=startim+1
  gran=gran0
  countnew=countnew0
;
  REPEAT begin
;
    print,'Image Nr.: '+strtrim(iff,1)
    if iff gt last then begin
      print,''
      print,'end of series, stop tracking'
      print,''
      goto,nextgrannr
    endif
;
    imaf=reform(ima3d(iff-first,*,*),xdim,ydim)
    imf=reform(im3d(iff-first,*,*),xdim,ydim)
;
;
; finding closest granule
;
;
    x=gran mod xdim
    y=gran/xdim
    xs=rfix(total(x)/float(countnew))     ; center of mass of previous granule
    ys=rfix(total(y)/float(countnew)) 
;
    xmin=xs-corrsize/2        ; defining box coordinates
    xmax=xs+corrsize/2-1
    ymin=ys-corrsize/2
    ymax=ys+corrsize/2-1
;
    if (xmin lt 0) or (ymin lt 0) or (xmax gt xdim-1) or (ymax gt ydim-1) $
      then begin
        print,''
        print,'Granule out of image border'       ;  taking care for image
        print,''                                  ;  boundaries
        goto,nextgrannr
    endif
;
    imbox=imaf(xmin:xmax,ymin:ymax)
    wgran=where(imbox gt 0,countgr)
    x=(wgran mod corrsize) - corrsize/2
    y=(wgran/corrsize)-corrsize/2
    r=sqrt(x*x+y*y)
    rmin=min(r)                                  ;  shortest discance
    min_dist=imbox(wgran(where(r eq rmin)))      ;  number of closest granule
    nmin=n_elements(min_dist)
;
;
; calculating correlation
;
;
    cima1=imaf(xmin:xmax,ymin:ymax)
    cim1=imf(xmin:xmax,ymin:ymax)
    cim=correl_images1(cim0,cim1,xshift=3,yshift=3)
    print,'Mean correlation : ',mean(cim)
    print,''
;
;
; displaying box
;
;
    imtv=imaf
    imtv(where(imtv gt 0))=1
    for k=0,nmin-1 do imtv(where(imaf eq min_dist(k)))=2
    imtv(xs,ys)=3
    wset,0
    tvscl,congrid(imtv,200,256)
;
    agran=where(imtv ge 2,narea)            
    areavec(iff)=narea                    ; storing area of granule
    numvec(iff)=min_dist(0)               ; storing number of granule
;
;
    imarr=shift(imarr,1,0,0)
    corrarr=shift(corrarr,1)
    corrarr(0)=mean(cim)          ; saving last 5 mean correlation values
    for k=0,nmin-1 do begin
      gran1=where(imaf eq min_dist(k),n3)
      imcoll(gran1)=1    
    endfor
    imarr(0,*,*)=imcoll    
    imcoll=reform(imarr(0,*,*)+imarr(1,*,*)+$      ; superposing last 5 images
                imarr(2,*,*)+imarr(3,*,*)+imarr(4,*,*),xdim,ydim)    
;
;
; displaying superposition and last 5 images
;
;
    wset,3
    tvscl,congrid(imcoll(xmin:xmax,ymin:ymax),300,300)
    for k=0,4 do begin
      wset,10+k
      tvscl,congrid(reform(imarr(4-k,xmin:xmax,ymin:ymax)),200,200)
    endfor
;
;
    gran=where(imcoll eq max(imcoll),countnew)  ; region for new center of mass
    imcoll(*,*)=0
;
;
; stopping condition
;
;
    if (total(corrarr)/5. lt maxcorr) or (corrarr(0) lt maxcorr1) then flag1=1  
;
;
    if flag1 eq 1 then begin
      print,''
      print,'Granule evolved, now tracking '+strtrim(moreimf-nmore,1)+$
            ' more frames.'
      if nmore lt moreimf then begin
        nmore=nmore+1
      endif else begin
        goto,backward
      endelse
    endif
;    
;
    iff=iff+1
;
  ENDREP UNTIL iff eq last+2
;
;
  backward:
;
;
  print,''
  print,''
  print,'Now tracking backward'
  print,''
  print,''
;
;--------------------------------------------------------
;
; tracking backward
;
;--------------------------------------------------------
;
;
  iff=startim-1
  gran=gran0
  countnew=countnew0
  imarr(*,*,*)=0
  imarr(0,*,*)=imcoll0
  flag1=0
  nmore=0
  corrarr(*)=1.
;
  REPEAT begin
;
    print,'Image Nr.: '+strtrim(iff,1)
    if iff lt first then begin
      print,''
      print,'end of series, stop tracking'
      print,''
      goto,nextgrannr
    endif
;
    imaf=reform(ima3d(iff-first,*,*),xdim,ydim)
    imf=reform(im3d(iff-first,*,*),xdim,ydim)
;
;
; finding closest granule
;
;
    x=gran mod xdim
    y=gran/xdim
    xs=rfix(total(x)/float(countnew))     ; center of mass of previous granule
    ys=rfix(total(y)/float(countnew)) 
;
    xmin=xs-corrsize/2        ; defining box coordinates
    xmax=xs+corrsize/2-1
    ymin=ys-corrsize/2
    ymax=ys+corrsize/2-1
;
    if (xmin lt 0) or (ymin lt 0) or (xmax gt xdim-1) or (ymax gt ydim-1) $
      then begin
        print,''
        print,'Granule out of image border'       ;  taking care for image
        print,''                                  ;  boundaries
        goto,nextgrannr
    endif
;
    imbox=imaf(xmin:xmax,ymin:ymax)
    wgran=where(imbox gt 0,countgr)
    x=(wgran mod corrsize) - corrsize/2
    y=(wgran/corrsize)-corrsize/2
    r=sqrt(x*x+y*y)
    rmin=min(r)                                  ;  shortest discance
    min_dist=imbox(wgran(where(r eq rmin)))      ;  number of closest granule
    nmin=n_elements(min_dist)
;
;
; calculating correlation
;
;
    cima1=imaf(xmin:xmax,ymin:ymax)
    cim1=imf(xmin:xmax,ymin:ymax)
    cim=correl_images1(cim0,cim1,xshift=3,yshift=3)
    print,'Mean correlation : ',mean(cim)
    print,''
;
;
; displaying box
;
;
    imtv=imaf
    imtv(where(imtv gt 0))=1
    for k=0,nmin-1 do imtv(where(imaf eq min_dist(k)))=2
    imtv(xs,ys)=3
    wset,0
    tvscl,congrid(imtv,200,256)
;
    agran=where(imtv ge 2,narea)            
    areavec(iff)=narea                    ; storing area of granule
    numvec(iff)=min_dist(0)               ; storing number of granule
;
;
    imarr=shift(imarr,1,0,0)
    corrarr=shift(corrarr,1)
    corrarr(0)=mean(cim)          ; saving last 5 mean correlation values
    for k=0,nmin-1 do begin
      gran1=where(imaf eq min_dist(k),n3)
      imcoll(gran1)=1    
    endfor
    imarr(0,*,*)=imcoll    
    imcoll=reform(imarr(0,*,*)+imarr(1,*,*)+$      ; superposing last 5 images
                imarr(2,*,*)+imarr(3,*,*)+imarr(4,*,*),xdim,ydim)    
;
;
; displaying superposition and last 5 images
;
;
    wset,3
    tvscl,congrid(imcoll(xmin:xmax,ymin:ymax),300,300)
    for k=0,4 do begin
      wset,10+k
      tvscl,congrid(reform(imarr(4-k,xmin:xmax,ymin:ymax)),200,200)
    endfor
;
;
    gran=where(imcoll eq max(imcoll),countnew)  ; region for new center of mass
    imcoll(*,*)=0
;
;
; stopping condition
;
;
    if (total(corrarr)/5. lt maxcorr) or (corrarr(0) lt maxcorr1) then flag1=1  
;
;
    if flag1 eq 1 then begin
      print,''
      print,'Granule evolved, now tracking '+strtrim(moreimb-nmore,1)+$
            ' more frames.'
      if nmore lt moreimb then begin
        nmore=nmore+1
      endif else begin
        goto,endtrack
      endelse
    endif
;    
;
    iff=iff-1
;
  ENDREP UNTIL iff eq first-2
;
;
  endtrack:
;
;
; plotting result
;
;
  wset,4
  plot,areavec,xtitle='image Number',ytitle='Area'
;
;
; saving result
;
;
  story_nr=story_nr+1 
  save,numvec,areavec,primcoord,filename=$
       str3+strtrim(startim,1)+'_'+strtrim(story_nr,1)+'.sav'
;
;
;
  nextgrannr:
;
  ngran=ngran+1
;
ENDREP until ngran eq maxgran
;
end

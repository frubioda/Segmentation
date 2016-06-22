pro view_time_story,nstart,stnr
;
; Anschun der getrackten Time-Stories
;
;
; file paths
;
;
str1=string('/data/hirzberg/gizon/2000/aim/aimc')        ; area images
str2=string('/data/hirzberg/gizon/2000/aim/spimc')       ; original images
str3=string('/data/hirzberg/gizon/2000/time/t')             ; output
str4=string('/data/hirzberg/gizon/2000/aim/uimc')        ; perimeter images

nstart = fix(nstart)
stnr = fix(stnr)

restore,str3+strtrim(nstart,1)+'.sav'

s = size(numvecarr)

IF stnr GE s[2] THEN BEGIN

   print,'No such story !!!'
   goto,ende

ENDIF

numvec = reform(numvecarr[*,stnr])
areavec = reform(areavecarr[*,stnr])
jvec = reform(jvecarr[*,stnr])
xvec = reform(xvecarr[*,stnr])
yvec = reform(yvecarr[*,stnr])
xdrvec = reform(xdrvecarr[*,stnr])
;
primcoord = intarr(2)
primcoord[0] = xvec[nstart]
primcoord[1] = yvec[nstart]
;
; Differential rotation
;
restore,'/data/hirzberg/gizon/2000/diffrot.sav'
sdr = size(diffrot)

IF sdr[1] NE 419 THEN BEGIN

   print,'Wrong differential rotation function'
   goto,ende

ENDIF
;
; Image shifts 
;
restore,'/data/hirzberg/gizon/2000/aim/shiftarr.sav'
refshift = shiftarr[nstart]

wg = where(numvec GT 0)

first = min(wg)
last = max(wg)

nim = last-first+1

aim3d = intarr(100,100,nim)
uim3d = intarr(100,100,nim)
im3d = fltarr(100,100,nim)

x1 = primcoord[0] - 20
x2 = primcoord[0] + 19
y1 = primcoord[1] - 20
y2 = primcoord[1] + 19

IF x1 LT 0 THEN BEGIN

   x1 = 0
   x2 = 39

ENDIF

IF y1 LT 0 THEN BEGIN

   y1 = 0
   y2 = 39

ENDIF

IF x2 GT 499 THEN BEGIN

   x2 = 499
   x1 = 460

ENDIF

IF y2 GT 418 THEN BEGIN

   y2 = 418
   y1 = 379

ENDIF

vdr = diffrot[primcoord[1]]

aimtot = intarr(100*nim,100)
imtot = fltarr(100*nim,100)

FOR i=0,nim-1 DO BEGIN

    ndr = i + first - nstart
    imshift = shiftarr[i+first] - refshift

    dcn = strtrim(i+first,1)
    aim = readfits(str1+dcn+'.fts')
    uim = readfits(str4+dcn+'.fts')
    im = readfits(str2+dcn+'.fts')

    ngran = numvec(i+first)
    wgran = where(aim eq ngran,cnt)
  
    aim[where(aim GT 0)] = 1
    aim[wgran] = 2

    x11 = x1 + nint(ndr*vdr) - imshift
    x22 = x2 + nint(ndr*vdr) - imshift

    print,i+first,ndr,nint(ndr*vdr)

    IF x11 LT 0 THEN BEGIN

       x11 = 0
       x22 = 39
 
    ENDIF

    IF x22 GT 499 THEN BEGIN

       x22 = 499
       x11 = 460

    ENDIF

    aim = congrid(aim[x11:x22,y1:y2],100,100)
    uim = congrid(uim[x11:x22,y1:y2],100,100)
    im = congrid(im[x11:x22,y1:y2],100,100,/interp)

    IF (i+first) EQ nstart THEN BEGIN

       aim[0,*] = 3
       aim[99,*] = 3
       aim[*,0] = 3
       aim[*,99] = 3

    ENDIF

;    im = gconvol(im,5)

    wu = where(uim EQ ngran,cnt)
;    wu = where(aim gt 1,cnt)
 
    
    IF (jvec[i+first] EQ 0) THEN BEGIN

       IF cnt GT 0 THEN im[wu] = min(im)

    ENDIF ELSE BEGIN

       IF cnt GT 0 THEN im[wu] = max(im)

    ENDELSE

    aim3d[*,*,i] = aim
    im3d[*,*,i] = im

    aimtot[i*100:(i+1)*100-1,*] = aim
    imtot[i*100:(i+1)*100-1,*] = im

ENDFOR 

window,0,xsize=nim*100,ysize=200
loadct,13
tvscl,aimtot
loadct,3
tvscl,imtot,0,100
window,1
plot,jvec,psym=2,yra=[-0.1,1.1],xra=[0,90],xticks=6,xminor=10,/ystyle
oplot,jvec

ende:

END

PRO mapstat,im,aim,uim,ahist,ihist,mxihist
;+
; NAME:
;       MAPSTAT
; PURPOSE:
;       statistics of segmented area maps
; CALLING SEQUENCE:
;       mapstat,im,aim,uim,ahist,ihist 
; INPUTS:
;       im  = "intensity" image
;       aim = segmented area image with structures assigned by integer numbers
;       uim = segmented perimeter image with border pixels add by 10000 
; OPTIONAL PARAMETERS:
;	
; KEYWORDS:
;       None
; OUTPUTS:
;       ahist   = histogram of areas
;       ihist   = histogram of mean "intensities"
;       mxihist = histogram of maximum "intensities"
; COMMON BLOCKS:
;       None
; SIDE EFFECTS:
;       None
; RESTRICTIONS:
;       
; PROCEDURE:
;       
; MODIFICATION HISTORY:
;       16-May-07 J. Hirzberger, MPS
;-
;
ON_ERROR,2

s = size(im)
xdim = s[1]
ydim = s[2]

ngran = max(aim)

ahist = intarr(ngran)
ihist = intarr(ngran)
mxihist = intarr(ngran)

FOR i=1,ngran DO BEGIN

    wbad = where(uim EQ i+1000,cnt)

    IF cnt EQ 0 THEN BEGIN

       wgran = where(aim EQ i,area)

       IF area GT 0 THEN BEGIN

          ahist[i-1] = area
          ihist[i-1] = mean(im[wgran])
          mxihist[i-1] = max(im[wgran])

       ENDIF

    ENDIF

ENDFOR

wgood = where(ahist GT 0,cnt)

ahist = ahist[wgood]
ihist = ihist[wgood]
mxihist = mxihist[wgood]

END

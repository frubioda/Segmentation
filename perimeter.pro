FUNCTION perimeter, x, y, RESOLUTION = resolution
;
;+
; NAME:
;      PERIMETER
; PURPOSE:
;       Calculates perimeter of a structure
; CALLING SEQUENCE:
;       Result = perimeter(x,y)
; INPUTS:
;       x = vector containing x-values
;       y = vector containig y-values
; COMMON BLOCKS:
;       none       
; SIDE EFFECTS:
;       
; RESTRICTIONS:
;             
; PROCEDURE:
;             
; MODIFICATION HISTORY:
;       18-Jul-06 J. Hirzberger, MPS
;-
;

; on_error, 2

IF KEYWORD_SET(resolution) EQ 0 THEN resolution = 1.

npix = n_elements(x)

IF npix NE n_elements(y) THEN BEGIN

   print,'x and y coordinates have different dimensions !'
   goto, ende

ENDIF

mnx = min(x) - 1
mny = min(y) - 1 
mxx = max(x) + 2
mxy = max(y) + 2 

xdim = fix(mxx) - fix(mnx) + 1
ydim = fix(mxy) - fix(mny) + 1

xnew = x-mnx+1
ynew = y-mny+1

im = bytarr(xdim,ydim)
im[xnew,ynew] = 1

pm = 0

FOR i=0,npix-1 DO BEGIN

    box = im[xnew[i]-1:xnew[i]+1,ynew[i]-1:ynew[i]+1]
    p1 = box[0,1] + box[2,1] + box[1,0] + box[1,2]
   
    pm = pm + 4 - p1

ENDFOR

RETURN, pm * resolution

ende: 

END

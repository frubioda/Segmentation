FUNCTION frand,im,cnum,WZERO=wzero
;+
; NAME:
;       FRAND
; PURPOSE:
;       finds pixel which form the border of a structure
; CALLING SEQUENCE:
;       Result = frand(im,cnum)
; INPUTS:
;       im   = segmented Image
;       cnum = number of structure to be treated
; OPTIONAL PARAMETERS:
;	None  
; KEYWORDS:
;       WZERO = those border pixels which have a 0 outside
;               (those which don not touch another structure)
; OUTPUTS:
;       position of border pixels
; COMMON BLOCKS:
;       None
; SIDE EFFECTS:
;       None
; RESTRICTIONS:
;       to be called by "cell_segment_seismo.pro"
; PROCEDURE:
;        
; MODIFICATION HISTORY:
;       28-Aug-01 J. Hirzberger, MPS
;-
;

s = size(im)

wg = where(im EQ cnum,count)

IF count EQ 0 THEN BEGIN

print, count
   message,'Error: No such structure !'

ENDIF ELSE BEGIN

   wgood = replicate(1,count)
   wzero = replicate(1,count)

   wx = wg MOD s[1]
   wy = wg / s[1]

   FOR i=0,count-1 DO BEGIN

       IF wx[i] GT 0      THEN i1 = im[wx[i]-1,wy[i]] ELSE i1 = cnum
       IF wx[i] LT s[1]-1 THEN i2 = im[wx[i]+1,wy[i]] ELSE i2 = cnum
       IF wy[i] GT 0      THEN i3 = im[wx[i],wy[i]-1] ELSE i3 = cnum
       IF wy[i] LT s[2]-1 THEN i4 = im[wx[i],wy[i]+1] ELSE i4 = cnum

       IF ((i1 EQ cnum) AND (i2 EQ cnum) AND (i3 EQ cnum) AND (i4 EQ cnum)) $
          THEN  wgood[i] = 0
       IF ((i1 NE 0) AND (i2 NE 0) AND (i3 NE 0) AND (i4 NE 0)) $
          THEN  wzero[i] = 0

   ENDFOR

   wgood = wg[where(wgood EQ 1)] 
   wz = where(wzero EQ 1,cnt)
   IF cnt GT 0 THEN wzero = wg[wz] ELSE wzero = [-1] 
   
   return,wgood

ENDELSE

END

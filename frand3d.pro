FUNCTION frand3d,im,cnum,WZERO=wzero
;+
; NAME:
;       FRAND3D
; PURPOSE:
;       finds pixel which form the border of a structure
; CALLING SEQUENCE:
;       Result = frand3d(im,cnum)
; INPUTS:
;       im   = segmented Image Cube
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
;       to be called by "cell_segment_seismo_3d.pro"
; PROCEDURE:
;        
; MODIFICATION HISTORY:
;       14-Sep-01 J. Hirzberger, MPS
;-
;

s = size(im)

wg = where(im EQ cnum,count)

IF count EQ 0 THEN BEGIN

   message,'Error: No such structure !'

ENDIF ELSE BEGIN

   wgood = replicate(1,count)
   wzero = replicate(1,count)

   wx = wg MOD s[1]
   wy = (wg MOD (s[1]*s[2])) / s[1]
   wt = wg / (s[1]*s[2])

   FOR i=long(0),count-1 DO BEGIN

       i1 = im[wx[i]-1,wy[i],wt[i]]
       i2 = im[wx[i]+1,wy[i],wt[i]]
       i3 = im[wx[i],wy[i]-1,wt[i]]
       i4 = im[wx[i],wy[i]+1,wt[i]]
       i5 = im[wx[i],wy[i],wt[i]-1]
       i6 = im[wx[i],wy[i],wt[i]+1]

       IF ((i1 EQ cnum) AND (i2 EQ cnum) AND (i3 EQ cnum) AND $
           (i4 EQ cnum) AND (i5 EQ cnum) AND (i6 EQ cnum)) $
          THEN  wgood[i] = 0
       IF ((i1 NE 0) AND (i2 NE 0) AND (i3 NE 0) AND $
           (i4 NE 0) AND (i5 NE 0) AND (i6 NE 0)) $
          THEN  wzero[i] = 0

   ENDFOR

   wgood = wg[where(wgood EQ 1)] 
   wz = where(wzero EQ 1,cnt)
   IF cnt GT 0 THEN wzero = wg[wz] ELSE wzero = [-1] 
   
   return,wgood

ENDELSE

END

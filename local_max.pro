PRO local_max,imin,imout,SMOOTH=smooth,SD_REGION=sd_region,MIN=min
;+
; NAME:
;	LOCAL_MAX
;
; PURPOSE:
;	Find the positions of local maxima in a two dimensional array.
;
; CALLING SEQUENCE:
;	LOCAL_MAX,imin,imout
;
; INPUTS:
;	imin = a two dimensional array.
;
; OUTPUTS:
;	imout = array with postion of local maxima set to one
;
; SIDE EFFECTS:
;	None.
;
; COMMON BLOCKS:
;	None.
;
; RESTRICTIONS:
;	None.
;
; PROCEDURE:
;	Straightforward.
;
; MODIFICATION HISTORY:
;       12-Nov-04 J. Hirzberger, IGAM
;
;-
;
ON_ERROR,2
;
im1 = imin - mean(imin)
;im1 = imin/mean(imin)
;im1 = im1-min(im1)

IF KEYWORD_SET(SMOOTH) NE 0 THEN BEGIN

   ims = smooth(im1,smooth,/edge)

ENDIF ELSE ims = im1

a1 = shift(ims,1,0)
a2 = shift(ims,-1,0)
a3 = shift(ims,0,1)
a4 = shift(ims,0,-1)
a5 = shift(ims,1,1)
a6 = shift(ims,-1,-1)
a7 = shift(ims,1,-1)
a8 = shift(ims,-1,1)

IF KEYWORD_SET(MIN) EQ 0 THEN BEGIN

   im2 = (ims GT a1) AND (ims GT a2) AND (ims GT a3) AND (ims GT a4)
   im2 = im2 AND (ims GT a5) AND (ims GT a6) 
   im2 = im2 AND (ims GT a6) AND (ims GT a8)

ENDIF ELSE BEGIN

   im2 = (ims LT a1) AND (ims LT a2) AND (ims LT a3) AND (ims LT a4)
   im2 = im2 AND (ims LT a5) AND (ims LT a6) 
   im2 = im2 AND (ims LT a6) AND (ims LT a8)

ENDELSE

;
;
IF KEYWORD_SET(SD_REGION) EQ 0 THEN BEGIN

   imout = im2

ENDIF ELSE BEGIN

   s = size(im2)
   imout = bytarr(s(1),s(2))

   IF KEYWORD_SET(MIN) EQ 0 THEN BEGIN

      im3 = (ims LT a1) AND (ims LT a2) 
      im3 = im3 OR ((ims LT a3) AND (ims LT a4))
      im3 = im3 OR ((ims LT a5) AND (ims LT a6))
      im3 = im3 OR ((ims LT a7) AND (ims LT a8))

   ENDIF ELSE BEGIN

      im3 = (ims GT a1) AND (ims GT a2) 
      im3 = im3 OR ((ims GT a3) AND (ims GT a4))
      im3 = im3 OR ((ims GT a5) AND (ims GT a6))
      im3 = im3 OR ((ims GT a7) AND (ims GT a8))

   ENDELSE

   w3 = where(im3 EQ 1)
   im3x = w3 MOD s(1)
   im3y = w3 / s(1)

   wg = where(im2 EQ 1,cnt)

;   imp = ims
;   imp(wg) = max(imp)
                    
   IF cnt GT 0 THEN BEGIN

      FOR i = 0,cnt-1 DO BEGIN

          pos = wg(i)
          hel = ims(pos)

          posx = pos MOD s(1)
          posy = pos / s(1)

          dist = abs(sqrt((posx - im3x)^2 + (posy - im3y)^2))
          md = min(dist,index)

          posmin = w3(index)
          helmin = ims(posmin)

          im4 = ims GE helmin + (hel - helmin)*sd_region
          afind,im4,aim4,/silent

          pi = aim4(pos)
          wbad = where(aim4 NE pi,c1)
          IF c1 GT 0 THEN aim4(wbad) = 0
          wgood = where(aim4 EQ pi,c2)
          IF c2 GT 0 THEN imout(wgood) = 1
          aim4(pos) = 2*max(aim4)
          aim4(posmin) = 2*max(aim4)

;          window,1,xsize=6*s(1),ysize=2*s(2),ypos=50
;          
;          tvscl,congrid(imp,2*s(1),2*s(2))
;          tvscl,congrid(im2,2*s(1),2*s(2)),2*s(1),0
;          tvscl,congrid(aim4,2*s(1),2*s(2)),4*s(1),0
;
;          read,aaa

      ENDFOR

   ENDIF

ENDELSE

END

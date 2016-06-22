PRO MLT,imin,imout,uimout,LEVELS=levels,SMOOTH=smooth,MINDIST=mindist,$
        SILENT=silent,MINAREA=minarea
;+
; NAME:
;	MLT
;
; PURPOSE:
;	Multi-Level-Tracking Segmentation of an image
;
; CALLING SEQUENCE:
;	MLT,imin,imout,levels=levels
;
; INPUTS:
;	imin = a two dimensional array.
;	levels = intensity levels defining strucrues
;                must be in descending order
;
; OUTPUTS:
;	imout = segmented array
;       uimout = if stated, optional perimeter image
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
;	see Bovelet & Wiehr, 2001, Solar Physics 201, 13
;
; MODIFICATION HISTORY:
;       15-Nov-04 J. Hirzberger, IGAM
;       03-Jul-06 J. Hirzberger, MPS
;
;-
;
ON_ERROR,2
;
IF NOT KEYWORD_SET(SILENT) THEN print,systime()

IF KEYWORD_SET(SMOOTH) EQ 0 THEN BEGIN
   ims = imin 
ENDIF ELSE BEGIN
   ims = smooth(imin,smooth,/edge)
ENDELSE

mxim = max(ims)
mnim = min(ims)

IF NOT KEYWORD_SET(MINDIST) THEN mindist = 1.5
IF NOT KEYWORD_SET(MINAREA) THEN minarea = 4

s = size(imin)

levels = reverse(levels(sort(levels)))
nlev = n_elements(levels)

ima = ims GT levels(0)
afind,ima,aima,uima,uim1,/silent
remove_small,aima,minarea       

IF nlev GT 1 THEN BEGIN

   FOR i=1,nlev-1 DO BEGIN

       clev = levels(i)
       
       IF ((clev LT mxim) AND (clev GT mnim)) THEN BEGIN

       imb = ims GT clev
       afind,imb,aimb,/silent
       remove_small,aimb,minarea
       
       shape_sep,aimb,aima,uima,imseg,mindist=mindist,SILENT=silent

       IF NOT KEYWORD_SET(SILENT) THEN BEGIN

          print,'Processing Level Number : ', i
          window,0,xsize=4*s(1),ysize=s(2),ypos=50
          tvscl,ims
          tvscl,aima,s(1),0
          tvscl,aimb,2*s(1),0
          tvscl,imseg,3*s(1),0

       ENDIF

       afind,imseg,aima,uima,uim1,/silent
       remove_small,aima,minarea

       ENDIF

   ENDFOR

   imout = aima

   IF N_PARAMS() EQ 3 THEN uimout = uim1

ENDIF ELSE BEGIN

      imout = aima 
 
      IF N_PARAMS() EQ 3 THEN uimout = uim1

ENDELSE

;
; Filling number gaps after removing structures smaller than MINAREA
;

grnum = max(imout)
k = 1

FOR i=1,grnum DO BEGIN

    wg = where(imout EQ i,cnt)

    IF N_PARAMS() EQ 3 THEN BEGIN

       wgu = where(uimout EQ i,cntu)
       wgu1 = where(uimout EQ i+10000,cntu1)

    ENDIF

    IF cnt NE 0 THEN BEGIN

       IF N_PARAMS() EQ 3 THEN BEGIN

          uimout[wgu] = k
          IF cntu1 GT 0 THEN uimout[wgu1] = k+10000

       ENDIF
   
       imout[wg] = k 
       k = k + 1
   
   ENDIF

ENDFOR


IF NOT KEYWORD_SET(SILENT) THEN print,systime()

END

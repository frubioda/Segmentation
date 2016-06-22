PRO REMOVE_SMALL,im,amin
;+
; NAME:
;	REMOVE_SMALL
;
; PURPOSE:
;	removes structures smaller than a minimum area from an image
;
; CALLING SEQUENCE:
;	REMOVE_SMALL,im,amin
;
; INPUTS:
;	im = a two dimensional segmented array
;	amin = minimum permitted area
;
; OUTPUTS:
;	None.
;
; SIDE EFFECTS:
;	None.
;
; COMMON BLOCKS:
;	None.
;
; RESTRICTIONS:
;	Part of the MLT segmentation.
;
; PROCEDURE:
;	see Bovelet & Wiehr, 2001, Solar Physics 201, 13
;
; MODIFICATION HISTORY:
;       16-Nov-04 J. Hirzberger, IGAM
;
;-
;
ON_ERROR,2
;

nmax = max(im)

FOR i=1,nmax DO BEGIN

    wn = where(im EQ i,cnt)
    IF ((cnt GT 0) AND (cnt LT amin)) THEN im(wn) = 0

ENDFOR

END

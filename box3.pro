FUNCTION box3,cim,sim,x0,y0,BOXCHANGE=boxchange 
;+
; NAME:
;       BOX3
; PURPOSE:
;       Dilates segmented area in  a 3x3 box around a certain pixel 
; CALLING SEQUENCE:
;       Result = box3(cim,sim,x0,y0)
; INPUTS:
;       cim = segmented Image
;       sim = "Intensity image"
;       x0,y0 = Position of border pixel     
; OPTIONAL PARAMETERS:
;	None  
; KEYWORDS:
;       BOXCHANGE = 1 if box has been changed 
; OUTPUTS:
;       dilated 3x3 box
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
abox1 = cim[x0-1:x0+1,y0-1:y0+1]
sbox1 = sim[x0-1:x0+1,y0-1:y0+1]

box = abox1

nr = cim[x0,y0]

wg = where(abox1  EQ 0,cnt1)

IF cnt1 GT 0 THEN BEGIN

   wx = wg MOD 3
   wy = wg / 3

   FOR i=0,cnt1-1 DO BEGIN

       abox2 = cim[x0+wx[i]-2:x0+wx[i],y0+wy[i]-2:y0+wy[i]]
       sbox2 = sim[x0+wx[i]-2:x0+wx[i],y0+wy[i]-2:y0+wy[i]]

       wg2 = where(abox2 NE nr,g2)
       wb2 = where(abox2 EQ nr,b2)
       wb3 = where(abox2 EQ 0,b3)

       gd = total(sbox2[wg2])/float(g2)
       bd = total(sbox2[wb2])/float(b2)
       bd2 = sbox2[1,1]
       bd3 = min(sbox2[wb3])

       IF (bd2 GT bd3) THEN abox1[wx[i],wy[i]] = nr
;       IF (bd GE bd2) THEN abox1[wx[i],wy[i]] = nr
;       IF (gd LE bd) THEN abox1[wx[i],wy[i]] = nr
;       abox1[wx[i],wy[i]] = nr

   ENDFOR

ENDIF

IF (total(box - abox1) NE 0) THEN boxchange = 1

RETURN, abox1

END

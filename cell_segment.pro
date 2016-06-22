PRO cell_segment,im,aim,cim,MINDIL=mindil,SILENT=silent
;+
; NAME:
;       CELL_SEGMENT
; PURPOSE:
;       Dilates area maps segmented with R&M or MLT to cell maps
; CALLING SEQUENCE:
;       cell_segment,im,aim,cim
; INPUTS: 
;       im = "Intensity image"
;       aim = R&M or MLT segmented are image
; OPTIONAL PARAMETERS:
;	None  
; KEYWORDS:
;       MINDIL = The code stops if the number of dilations (in pixel)
;                is LE mindil
; OUTPUTS:
;       cim = cell segmented image
; COMMON BLOCKS:
;       None
; SIDE EFFECTS:
;       None
; RESTRICTIONS:
;       
; PROCEDURE:
;        
; MODIFICATION HISTORY:
;       13-Oct-10 J. Hirzberger, MPS
;-
;

cim = aim
sim = im
maxgran = max(aim)
s = size(cim)

IF NOT KEYWORD_SET(SILENT) THEN window,1,xsize=s[1],ysize=s[2],ypos=350
IF NOT KEYWORD_SET(MINDIL) THEN mindil = 0

REPEAT BEGIN

  scx = long(0) 

  FOR j=1,maxgran DO BEGIN


      c = frand(cim,j,wzero=wz)

      IF wz[0] NE -1 THEN BEGIN

         a = where(cim EQ j)

         nc = n_elements(c)

         cx = c MOD s[1]
         cy = c / s[1]

         FOR k=0,nc-1 DO BEGIN

             x0 = cx[k]
             y0 = cy[k]

             IF ((x0 GT 1) AND (y0 GT 1) AND (x0 LT s[1]-2) AND $
                (y0 LT s[2]-2)) THEN BEGIN
;                (y0 LT s[2]-2) AND (feld[x0,y0] NE 3)) THEN BEGIN

                bc = 0
                box = box3(cim,sim,x0,y0,boxchange=bc)
           
                IF bc GT 0 THEN scx = scx + 1

                cim[x0-1:x0+1,y0-1:y0+1] = box

             ENDIF

         ENDFOR

      ENDIF 

  ENDFOR

  IF NOT KEYWORD_SET(SILENT) THEN BEGIN

     tvscl,cim
     print,''
     print,'Number of dilations : ',scx

  ENDIF

ENDREP UNTIL scx LE mindil

END

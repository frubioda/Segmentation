PRO SHAPE_SEP,imlarge,imsmall,uimsmall,imout,MINDIST=mindist,SILENT=silent
;+
; NAME:
;	MLT
;
; PURPOSE:
;	Separation of a large shape with respect to smaller shapes
;
; CALLING SEQUENCE:
;	SHAPE_SEP,imin,imout
;
; INPUTS:
;	imlarge = a two dimensional array with large shapes
; OUTPUTS:
;	imsmall = a two dimensional array with smaller shapes
; OUTPUTS:
;
; SIDE EFFECTS:
;	None.
;
; COMMON BLOCKS:
;	None.
;
; RESTRICTIONS:
;	Part of the MLT Segmentaion
;
; PROCEDURE:
;	see Bovelet & Wiehr, 2001, Solar Physics 201, 13
;
; MODIFICATION HISTORY:
;       15-Nov-04 J. Hirzberger, IGAM
;
;-
;
ON_ERROR,2

s = size(imsmall)
nl = max(imlarge)
imtot = intarr(s(1),s(2))

IF NOT KEYWORD_SET(MINDIST) THEN mindist = 1.5

FOR i=1,nl DO BEGIN

    wlarge = where(imlarge EQ i,cnt)

    IF cnt GT 0 THEN BEGIN

       xlarge = wlarge MOD s(1)
       ylarge = wlarge / s(1)

       x1 = min(xlarge) - nint(mindist) + 1
       x2 = max(xlarge) + nint(mindist) + 1
       y1 = min(ylarge) - nint(mindist) + 1
       y2 = max(ylarge) + nint(mindist) + 1

       IF x1 LE 0 THEN x1 = 0
       IF y1 LE 0 THEN y1 = 0
       IF x2 GE s(1) THEN x2 = s(1) - 1
       IF y2 GE s(2) THEN y2 = s(2) - 1

       dx = x2-x1+1
       dy = y2-y1+1

       imneu = intarr(dx,dy)
       imfin = imtot(x1:x2,y1:y2)

       ims1 = imsmall(x1:x2,y1:y2)
       imu1 = uimsmall(x1:x2,y1:y2)
       iml1 = imlarge(x1:x2,y1:y2)

       wlarge = where(iml1 EQ i,cnt)

       psmall = ims1(wlarge)
       psmall = psmall(uniq(psmall,sort(psmall)))
       wsmall = where(psmall GT 0,c2)
   
       IF c2 EQ 0 THEN BEGIN
    
          imfin(wlarge) = 1
          wfin = where(imfin GT 0,ctot)

          IF ctot GT 0 THEN BEGIN

             xtot = wfin MOD dx
             ytot = wfin / dx
             imtot(xtot+x1,ytot+y1) = imfin(wfin)

          ENDIF

       ENDIF ELSE BEGIN

          psmall = psmall(wsmall)

          FOR j=0,c2-1 DO BEGIN

              nsh = psmall(j)
              asmall = where(ims1 EQ nsh,csa)
              imfin(asmall) = nsh
              usmall = where(imu1 EQ nsh,csu)
 
              IF csu EQ 0 THEN BEGIN
                 imneu(asmall) = nsh
              ENDIF ELSE BEGIN   
                 imneu(usmall) = nsh     
              ENDELSE

          ENDFOR

wneu = where(imneu GT 0,c1) 
xneu = wneu MOD dx
yneu = wneu / dx
cneu = imneu(wneu)

WHILE c1 NE 0 DO BEGIN

imneu = intarr(dx,dy)

FOR k = 0,c1-1 DO BEGIN

    x0 = xneu(k)
    y0 = yneu(k)
    nsh = cneu(k)

    wfin = where((imfin NE nsh) AND (imfin NE 0),c2)

    IF c2 GT 0 THEN BEGIN

       xfin = wfin MOD dx
       yfin = wfin / dx

   ENDIF ELSE BEGIN
 
       xfin = x0 - 3 * nint(mindist)
       yfin = y0 - 3 * nint(mindist)

   ENDELSE   

left:
                     
     xpos = x0 - 1 
     ypos = y0
                     
     IF (xpos GE 0) THEN BEGIN                 
        dist = sqrt((xpos - xfin)^2 + (ypos -yfin)^2)
        dmin = min(dist)
        
        IF ((dmin GT mindist) AND (iml1(xpos,ypos) GT 0) AND $
            (imneu(xpos,ypos) EQ 0) AND (imfin(xpos,ypos) EQ 0)) THEN BEGIN
           imneu(xpos,ypos) = nsh
        ENDIF
     ENDIF
     
     
right:

     xpos = x0 + 1 
     ypos = y0

     IF (xpos LT dx) THEN BEGIN          
        dist = sqrt((xpos - xfin)^2 + (ypos -yfin)^2)
        dmin = min(dist)
        IF ((dmin GT mindist) AND (iml1(xpos,ypos) GT 0) AND $
            (imneu(xpos,ypos) EQ 0) AND (imfin(xpos,ypos) EQ 0)) THEN BEGIN
           imneu(xpos,ypos) = nsh
        ENDIF
     ENDIF                

up:
                     
     xpos = x0 
     ypos = y0 + 1

     IF (ypos LT dy) THEN BEGIN          
        dist = sqrt((xpos - xfin)^2 + (ypos -yfin)^2)
        dmin = min(dist)
        IF ((dmin GT mindist) AND (iml1(xpos,ypos) GT 0) AND $
            (imneu(xpos,ypos) EQ 0) AND (imfin(xpos,ypos) EQ 0)) THEN BEGIN
           imneu(xpos,ypos) = nsh
        ENDIF
     ENDIF                

down:
                     
     xpos = x0
     ypos = y0 - 1

     IF (ypos GE 0) THEN BEGIN                  
        dist = sqrt((xpos - xfin)^2 + (ypos -yfin)^2)
        dmin = min(dist)
        IF ((dmin GT mindist) AND (iml1(xpos,ypos) GT 0) AND $
            (imneu(xpos,ypos) EQ 0) AND (imfin(xpos,ypos) EQ 0)) THEN BEGIN
           imneu(xpos,ypos) = nsh
        ENDIF
     ENDIF                

ENDFOR

     wneu = where(imneu GT 0,c1)

     IF c1 GT 0 THEN BEGIN

        imfin(wneu) = imneu(wneu)
        xneu = wneu MOD dx
        yneu = wneu / dx
        cneu = imneu(wneu)

     ENDIF

ENDWHILE 

wfin = where(imfin GT 0,ctot)

IF ctot GT 0 THEN BEGIN

   xtot = wfin MOD dx
   ytot = wfin / dx
   imtot(xtot+x1,ytot+y1) = imfin(wfin)

ENDIF

ENDELSE

ENDIF

ENDFOR

wg = where(imtot GT 0,cnt)
IF cnt GT 0 THEN imtot(wg) = 1

imout = imtot

END

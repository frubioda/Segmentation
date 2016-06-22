pro clean_story,nstart,numvec,areavec,primcoord,xvec,yvec,xdrvec,jvec,growvec
;+
; NAME:
;       CLEAN_STORY
; PURPOSE:
;       Cleans lifetime stories 
; CALLING SEQUENCE:
;       CLEAN_STORY,nstart,numvec,areavec,primcoord,xvec,yvec,xdrvec,jvec
; INPUTS:
;       nstart = Number of starting image (tracking)
;       numvec = vector containing granule numbers
;       areavec = vector containing granule areas
;       xvec, yvec =  vector containing granule barycenters
; OPTIONAL PARAMETERS:
;	None
; KEYWORDS:
;       None
; OUTPUTS:
;       cleaned vectors
; COMMON BLOCKS:
;       None
; SIDE EFFECTS:
;       None
; RESTRICTIONS:
;       Subroutine of granuletracking_seismo.pro
; PROCEDURE:
;       Uses several criteria 
; MODIFICATION HISTORY:
;       06-Jul-06 J. Hirzberger, MPS
;-
;
ON_ERROR,2

nvec = n_elements(numvec)
jvec = bytarr(nvec)
growvec = bytarr(nvec)

wg = where(numvec GT 0)
wb = where(numvec EQ 0)
first = min(wg)
last = max(wg)   
jvec[wb] = 1

mxjump = 3
;mxainc = 10.
;mxadec = 0.1

IF last GE nstart+2 THEN BEGIN

   FOR i=nstart,last-2 DO BEGIN

       dx1 = xvec[i]-xvec[i+1]-xdrvec[i]+xdrvec[i+1]
       dx2 = xvec[i]-xvec[i+2]-xdrvec[i]+xdrvec[i+2]
       dy1 = yvec[i]-yvec[i+1]
       dy2 = yvec[i]-yvec[i+2]

       r1 = sqrt(dx1^2 + dy1^2)
       r2 = sqrt(dx2^2 + dy2^2)

;       ac1 = float(areavec[i])/float(areavec[i+1])
;       ac2 = float(areavec[i])/float(areavec[i+2])

       IF ((r1 GT mxjump) AND (r2 GT mxjump)) THEN BEGIN

          jvec[i+1:last] = 1

       ENDIF

;       IF ((ac1 GT mxainc) AND (ac2 GT mxainc)) THEN BEGIN
;
;          jvec[i+1:last] = 1
;
;       ENDIF
;
;       IF ((ac1 LT mxadec) AND (ac2 LT mxadec)) THEN BEGIN
;
;          jvec[i+1:last] = 1
;
;       ENDIF

       IF i EQ (last-2) THEN BEGIN

          IF ((r1 LE mxjump) AND (r2 GT mxjump)) THEN jvec[i+2] = 1

;          IF ((ac1 LE mxainc) AND (ac2 GT mxainc)) THEN jvec[i+2] = 1
;
;          IF ((ac1 GE mxadec) AND (ac2 LT mxadec)) THEN jvec[i+2] = 1

       ENDIF

   ENDFOR

ENDIF ELSE BEGIN

   dx = xvec[nstart]-xvec[nstart+1]-xdrvec[nstart]+xdrvec[nstart+1] 
   dy = yvec[nstart]-yvec[nstart+1]

   r1 = sqrt(dx^2 + dy^2)

;   ac1 = float(areavec[nstart])/float(areavec[nstart+1])

   IF r1 GT mxjump THEN jvec[nstart+1] = 1

;   IF ac1 GT mxainc THEN jvec[nstart+1] = 1
; 
;   IF ac1 LT mxadec THEN jvec[nstart+1] = 1

ENDELSE

IF first LE nstart-2 THEN BEGIN

   FOR i=nstart,first+2,-1 DO BEGIN

       dx1 = xvec[i]-xvec[i-1]-xdrvec[i]+xdrvec[i-1]
       dx2 = xvec[i]-xvec[i-2]-xdrvec[i]+xdrvec[i-2]
       dy1 = yvec[i]-yvec[i-1]
       dy2 = yvec[i]-yvec[i-2]

       r1 = sqrt(dx1^2 + dy1^2)
       r2 = sqrt(dx2^2 + dy2^2)

;       ac1 = float(areavec[i])/float(areavec[i-1])
;       ac2 = float(areavec[i])/float(areavec[i-2])

       IF ((r1 GT mxjump) AND (r2 GT mxjump)) THEN BEGIN

          jvec[first:i-1] = 1

       ENDIF

;       IF ((ac1 GT mxainc) AND (ac2 GT mxainc)) THEN BEGIN
;
;          jvec[first:i-1] = 1
;
;       ENDIF
;
;       IF ((ac1 LT mxadec) AND (ac2 LT mxadec)) THEN BEGIN
;
;          jvec[first:i-1] = 1
;
;       ENDIF

       IF i EQ (first+2) THEN BEGIN

          IF ((r1 LE mxjump) AND (r2 GT mxjump)) THEN jvec[i-2] = 1

;          IF ((ac1 LE mxainc) AND (ac2 GT mxainc)) THEN jvec[i-2] = 1
;
;          IF ((ac1 GE mxadec) AND (ac2 LT mxadec)) THEN jvec[i-2] = 1

       ENDIF

   ENDFOR

ENDIF ELSE BEGIN

   dx = xvec[nstart]-xvec[nstart-1]-xdrvec[nstart]+xdrvec[nstart-1] 
   dy = yvec[nstart]-yvec[nstart-1]

   r1 = sqrt(dx^2 + dy^2)

;   ac1 = float(areavec[nstart])/float(areavec[nstart-1])

   IF r1 GT mxjump THEN jvec[nstart-1] = 1

;   IF ac1 GT mxainc THEN jvec[nstart-1] = 1
; 
;   IF ac1 LT mxadec THEN jvec[nstart-1] = 1

ENDELSE

x = findgen(nvec)
wg = where(jvec EQ 0,cnt)
area = median(areavec[wg])
wg = where((jvec EQ 0) AND (areavec LT 2*area) AND (areavec GT 0.5*area),cnt)

IF cnt GT 3 THEN BEGIN

   x1 = x[wg]
   y1 = areavec[wg]

   fit = poly_fit(x1,y1,2,sigma=sigma,yband=yband,/double,yfit=yfit)
   
   yf = fit[0] + fit[1]*x + fit[2]*x^2
   y2 = yf + 1.5*mean(yband)
   y3 = yf - 1.5*mean(yband)

   FOR i=first,last DO BEGIN

       IF ((areavec[i] GT y2[i]) OR (areavec[i] LT y3[i])) THEN jvec[i] = 1

   ENDFOR

   wg = where(jvec EQ 0)
   x5 = x[wg]
   y5 = areavec[wg]

;   window,22
;   plot,x,areavec,psym=2,xrange=[min(x1)-5,max(x1)+5],/xstyle
;   oplot,x,areavec
;   oplot,x1,y1,psym=2,color=80
;   oplot,x5,y5,psym=2,color=180
;   oplot,x,y2,linestyle=1
;   oplot,x,y3,linestyle=1
;
;   stop
;
;   wdelete,22

    FOR i=first,last-1 DO BEGIN

        IF ((jvec[i] GT 0) AND(jvec[i+1] GT 0)) THEN BEGIN

           IF i LE nstart-2 THEN jvec[first:i+1] = 1
           IF i GT nstart  THEN jvec[i:last] = 1

        ENDIF 

    ENDFOR


ENDIF

mx = max(areavec,index)
growvec[0:index] = 0
IF index LT nvec-1 THEN growvec[index+1:nvec-1] = 1

END

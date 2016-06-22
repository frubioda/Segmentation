PRO afind,gim,aim,uim,uim1,SILENT=silent,DIAG=diag
;+
; NAME:
;       AFIND
; PURPOSE:
;       Computes areas and perimeters of structures
; CALLING SEQUENCE:
;       afind,gim, aim, uim, uim1
; INPUTS:
;       gim = image with structures       
; OPTIONAL PARAMETERS:
;	none  
; KEYWORDS:
;       DIAG = diagonal connections of pixels included
;       SILENT = be quiet
; OUTPUTS:
;       aim  = are aimage 
;       uim  = perimeter image
;       uim1 = perimeter image, structures truncated by the edge are
;              marked with number+10000
; COMMON BLOCKS:
;       None
; SIDE EFFECTS:
;       None
; RESTRICTIONS:
;       Images must be 2D, structures should be 1 background should be 0
; PROCEDURE:
;       ---
; MODIFICATION HISTORY:
;       23-Sep-02 J. Hirzberger, IGAM :
;       20-Nov-06 J. Hirzberger, MPS  : keyword diag
;-
;
ON_ERROR,2
;
;
;
dim=gim
s=size(dim)
rx=s(1)
ry=s(2)
;
IF N_PARAMS() EQ 4 THEN uim=intarr(rx,ry)

aim=intarr(rx,ry)
coord=intarr(2,5000)
newcoord=fltarr(2,5000)
grnum=0
;
IF NOT KEYWORD_SET(SILENT) THEN BEGIN

   window,2,xsize=rx,ysize=ry,xpos=200,ypos=500
   tvscl,gim

ENDIF

;
for j=0,ry-1 do begin
  for i=0,rx-1 do begin
    if dim(i,j) eq 1 then begin
     a=1
     u=1
     np=0
     nnewp=0
     coord(0,np)=i
     coord(1,np)=j
     dim(i,j)=0
     aim(i,j)=grnum+1

     IF N_PARAMS() EQ 4 THEN BEGIN
        
        uim=intarr(rx,ry) 
        uim(i,j)=grnum+1

     ENDIF

repeat begin
  for n=0,np do begin
left:
     l=coord(0,n)-1
     m=coord(1,n)
     if l ge 0 then begin
     if dim(l,m) eq 1 then begin
       nnewp=nnewp+1
       newcoord(0,nnewp-1)=l
       newcoord(1,nnewp-1)=m
       dim(l,m)=0
       aim(l,m)=grnum+1
       a=a+1
     endif
     endif
right:
     l=coord(0,n)+1
     m=coord(1,n)
     if l lt rx then begin
     if dim(l,m) eq 1 then begin
       nnewp=nnewp+1
       newcoord(0,nnewp-1)=l
       newcoord(1,nnewp-1)=m
       dim(l,m)=0
       aim(l,m)=grnum+1
       a=a+1
     endif
     endif
up:
     l=coord(0,n)
     m=coord(1,n)+1
     if m lt ry then begin
     if dim(l,m) eq 1 then begin
       nnewp=nnewp+1
       newcoord(0,nnewp-1)=l
       newcoord(1,nnewp-1)=m
       dim(l,m)=0
       aim(l,m)=grnum+1
       a=a+1
     endif
     endif
down:
     l=coord(0,n)
     m=coord(1,n)-1
     if m ge 0 then begin
     if dim(l,m) eq 1 then begin
       nnewp=nnewp+1
       newcoord(0,nnewp-1)=l
       newcoord(1,nnewp-1)=m
       dim(l,m)=0
       aim(l,m)=grnum+1
       a=a+1
     endif
     endif

    IF keyword_set(diag) THEN BEGIN
upleft:
     l=coord(0,n)-1
     m=coord(1,n)+1
     if l ge 0 then begin
     if m lt ry then begin
     if dim(l,m) eq 1 then begin
       nnewp=nnewp+1
       newcoord(0,nnewp-1)=l
       newcoord(1,nnewp-1)=m
       dim(l,m)=0
       aim(l,m)=grnum+1
       a=a+1
     endif
     endif
     endif
upright:
     l=coord(0,n)+1
     m=coord(1,n)+1
     if l lt rx then begin
     if m lt ry then begin
     if dim(l,m) eq 1 then begin
       nnewp=nnewp+1
       newcoord(0,nnewp-1)=l
       newcoord(1,nnewp-1)=m
       dim(l,m)=0
       aim(l,m)=grnum+1
       a=a+1
     endif
     endif
     endif
downleft:
     l=coord(0,n)-1
     m=coord(1,n)-1
     if l ge 0 then begin
     if m ge 0 then begin
     if dim(l,m) eq 1 then begin
       nnewp=nnewp+1
       newcoord(0,nnewp-1)=l
       newcoord(1,nnewp-1)=m
       dim(l,m)=0
       aim(l,m)=grnum+1
       a=a+1
     endif
     endif
     endif
downright:
     l=coord(0,n)+1
     m=coord(1,n)-1
     if l lt rx then begin
     if m ge 0 then begin
     if dim(l,m) eq 1 then begin
       nnewp=nnewp+1
       newcoord(0,nnewp-1)=l
       newcoord(1,nnewp-1)=m
       dim(l,m)=0
       aim(l,m)=grnum+1
       a=a+1
     endif
     endif
     endif 
   ENDIF

  endfor
  np=nnewp
  nnewp=0
  coord=newcoord
  endrep until np eq 0
  grnum=grnum+1
  endif
endfor
endfor
;
;
; calculate perimeters
;

IF N_PARAMS() EQ 4 THEN BEGIN


for i=1,grnum do begin
  gran=where(aim eq i)
  nn=n_elements(gran)
  for j=long(0),nn-1 do begin
    line=fix(gran(j)/rx)
    row=gran(j) mod rx
;
; marking granules which are at the edge of the image
;
    if (line eq 0) or (line eq ry-1) or (row eq 0) or $
       (row eq rx-1) then begin
         uim(gran(j))=10000+i
         goto,fin
    endif
    if (aim(gran(j)-1) eq 0) then uim(gran(j))=i else begin
      if (aim(gran(j)+1) eq 0) then uim(gran(j))=i else begin
        if (aim(gran(j)-rx) eq 0) then uim(gran(j))=i else begin
          if (aim(gran(j)+rx) eq 0) then uim(gran(j))=i
        endelse
      endelse
    endelse
fin :
  endfor
endfor  
;
uim1=uim
if max(uim1) gt 10000 then uim1(where(uim1 ge 10000))=0

ENDIF


IF NOT KEYWORD_SET(SILENT) EQ 0 THEN BEGIN

   window,0,xsize=rx,ysize=ry,xpos=200,ypos=300
   tvscl,aim
   wset,0
   window,1,xsize=rx,ysize=ry,xpos=200,ypos=100
   tvscl,uim1
   wset,0
;
;
;
   print,'Number of granules : ',max(aim)
   print,''
   print,'---------------'
   print,''

ENDIF

;
;
;
end





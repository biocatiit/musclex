C     LAST UPDATE 20/03/98
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      SUBROUTINE BGWSRT2(BUF,B,IWID,JWID,ISEP,JSEP,SMOO,TENS,PC1,PC2,
     &                   NPIX,NRAST,MAXDIM,MAXWIN,
     &                   XB,YB,YS,YSP,WRK,BW,INDEX,IPRINT,ILOG)
      IMPLICIT NONE
C
C Purpose: Fits the background by the following method. 
C          A window is then moved across the original image, the 
C          pixel values in the window are sorted and those between   
C          PC1 and PC2 are taken as background. A smoothing spline
C          is fitted to fill in the gaps between window centres.
C
C Calls   2: SORT , CURVS 
C Called by: BCKWIN
C 
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C
C Scalar arguments:
C
      REAL SMOO,TENS,PC1,PC2
      INTEGER IWID,JWID,ISEP,JSEP,NPIX,NRAST,MAXDIM,MAXWIN
      INTEGER IPRINT,ILOG
C
C Array arguments:
C
      REAL BUF(NPIX*NRAST),B(NPIX*NRAST)
      REAL XB(MAXDIM),YB(MAXDIM),YS(MAXDIM),YSP(MAXDIM),WRK(9*MAXDIM)
      REAL BW(MAXWIN)
      INTEGER INDEX(MAXWIN)
C
C Local arrays:
C
      LOGICAL KNOT(2000)
C
C Local Scalars:
C
      INTEGER I,J,IP,IBAD,IBCK,M,MM,ISTRT,IEND,NIWIND
      INTEGER IFPIX,ILPIX,IFRAST,ILRAST
      INTEGER NB,JW1,JW2,IW1,IW2,IW,IER,IN,JN
      REAL RS,ZS,BCK,S,EPS
      REAL DELR,RMIN,DELZ,ZMIN
      LOGICAL LINE
C
C External function:
C
      REAL CURV2
      EXTERNAL CURV2
C
C-----------------------------------------------------------------------
C
      IFPIX=1
      ILPIX=NPIX
      IFRAST=1
      ILRAST=NRAST
      RMIN=1.0
      DELR=1.0
      ZMIN=1.0
      DELZ=1.0

      WRITE(IPRINT,1000)
      CALL FLUSH(IPRINT)
      WRITE(ILOG,1000)
      IBAD = 0
C
C========Check window size for array bounds
C
      NB = (2*IWID+1)*(2*JWID+1) 
      IF(NB.GT.MAXWIN)THEN
         WRITE(IPRINT,1010)
         CALL FLUSH(IPRINT)
         WRITE(ILOG,1010)
         STOP
      ENDIF
C
C========Initialize B to flag unknown background values
C
      DO 20 J=1,NRAST
         DO 10 I=1,NPIX
            M = (J-1)*NPIX + I
            B(M) = -1.0E+30
 10      CONTINUE
 20   CONTINUE
C
C========Set default window separation if necessary
C
      IF(JSEP.LE.0)JSEP = JWID
      IF(ISEP.LE.0)ISEP = IWID
C
C========Loop over rasters and determine window limits
C
      DO 120 J=IFRAST,ILRAST
         JW1 = J - JWID
         IF(JW1.LT.1)JW1 = 1
         JW2 = J + JWID
         IF(JW2.GT.NRAST)JW2 = NRAST
         ZS = FLOAT(J)*DELZ + ZMIN
C     
C========Find first and last relevant pixels in this raster
C
         ISTRT = 0
         IEND = 0
         DO 30 I=IFPIX,ILPIX
            KNOT(I) = .FALSE.
            M = (J-1)*NPIX + I
            MM = J*NPIX - I + 1
            WRITE(IPRINT,"(I10)")I
            IF(BUF(M).GT.-1.0E+30)THEN
               IF(ISTRT.EQ.0)ISTRT = I
            ENDIF
            IF(BUF(MM).GT.-1.0E+30)THEN
               IF(IEND.EQ.0)IEND = NPIX - I + 1
            ENDIF
 30      CONTINUE
         WRITE(IPRINT,*)"PASS1"
C
C========Determine whether this is a raster for interpolation, if so, 
C========flag the appropriate pixels, if not, flag only those pixels 
C========likely to be on the perimeter of the image or masked areas
C
         LINE = .FALSE.
         IF(MOD(J-IFRAST,JSEP).EQ.0.OR.J.EQ.ILRAST)THEN
            NIWIND = (IEND-ISTRT-1)/ISEP + 2 
            DO 40 IN=1,NIWIND
               I = ISTRT + (IN-1)*ISEP
               IF(I.GT.IEND)I = IEND 
               KNOT(I) = .TRUE.
 40         CONTINUE
            LINE = .TRUE.
         ELSE
            DO 50 I=ISTRT,IEND
               M = (J-1)*NPIX + I 
               IF(BUF(M-NPIX).LT.-0.5E+30.OR.
     &            BUF(M+NPIX).LT.-0.55E+30)KNOT(I) = .TRUE.
 50         CONTINUE
         ENDIF
         WRITE(IPRINT,*)"PASS2"
C
C=========Loop over pixels in this raster
C
         IBCK = 0
         DO 100 I=IFPIX,ILPIX
            IF(KNOT(I))THEN
               M = (J-1)*NPIX + I
               IF(BUF(M).GT.-1.0E+30)THEN
                  IW1 = I - IWID
                  IF(IW1.LT.1)IW1 = 1
                  IW2 = I + IWID
                  IF(IW2.GT.NPIX)IW2 = NPIX
C
C========SPECIFIC WINDOW BACKGROUND METHOD
C
C========Collect pixel values in this window
C
                  NB = 0
                  DO 80 JN=JW1,JW2
                     DO 70 IN=IW1,IW2
                        MM = (JN-1)*NPIX + IN
                        IF(BUF(MM).GT.-1.0E+30)THEN
                           NB = NB + 1
                           BW(NB) = BUF(MM)
                        ENDIF
 70                  CONTINUE
 80               CONTINUE
C
C========Sort pixel values and sum over desired interval
C
                  IF(NB.GT.0)THEN
                     CALL SORT(BW,NB,INDEX)
                     IP = 0
                     BCK = 0.0
                     IW1 = MIN(INT(PC1*FLOAT(NB))+1,NB)
                     IW2 = MIN(INT(PC2*FLOAT(NB))+1,NB)
                     DO 90 IW=IW1,IW2
                        IP = IP + 1
                        BCK = BCK + BW(INDEX(IW))
 90                  CONTINUE 
C
C========Form average
C
                     IBCK = IBCK + 1
                     YB(IBCK) = BCK/FLOAT(IP)
                     B(M) = YB(IBCK)
                     XB(IBCK) = FLOAT(I)*DELR + RMIN 
                  ELSE
                     IBAD = IBAD + 1
                  ENDIF
C
C========END OF SPECIFIC WINDOW BACKGROUND METHOD
C
               ENDIF
            ENDIF
 100     CONTINUE
C
C========Fit splines to fill in gaps in this raster
C
         IF(LINE.AND.IBCK.GT.1)THEN
            EPS = SQRT(2.0/FLOAT(IBCK))
            S = SMOO*FLOAT(IBCK)
            CALL CURVS(IBCK,XB,YB,1.0,1,S,EPS,YS,YSP,TENS,WRK,IER)
            IF(IER.NE.0)THEN
               WRITE(IPRINT,2000)IER
               CALL FLUSH(IPRINT)
               WRITE(ILOG,2000)IER
               STOP 'Fatal error'
            ENDIF
C
C========Interpolate
C
            DO 110 I=ISTRT+1,IEND-1
               M = (J-1)*NPIX + I
               IF(BUF(M).GT.-1.0E+30)THEN
                  RS = DELR*FLOAT(I) + RMIN
                  B(M) = CURV2(RS,IBCK,XB,YS,YSP,TENS)
               ENDIF
 110        CONTINUE 
         ENDIF
 120  CONTINUE
C
C========Transpose the spline fitting
C
      DO 160 I=IFPIX,ILPIX
C
C========Set up arrays for interpolation
C
         IBCK = 0
         DO 140 J=IFRAST,ILRAST
            M = (J-1)*NPIX + I
            IF(B(M).GT.-1.0E+30)THEN
               IBCK = IBCK + 1
               XB(IBCK) = FLOAT(J)*DELZ + ZMIN
               YB(IBCK) = B(M)
            ENDIF
 140     CONTINUE
         IF(IBCK.GT.1)THEN
            EPS = SQRT(2.0/FLOAT(IBCK))
            S = SMOO*FLOAT(IBCK)
            CALL CURVS(IBCK,XB,YB,1.0,1,S,EPS,YS,YSP,TENS,WRK,IER)
            IF(IER.NE.0)THEN
               WRITE(IPRINT,2000)IER
               CALL FLUSH(IPRINT)
               WRITE(ILOG,2000)IER
               STOP 'Fatal error'
            ENDIF
C
C========Interpolate
C
            DO 150 J=IFRAST,ILRAST
               M = (J-1)*NPIX + I
               IF(BUF(M).GT.-1.0E+30)THEN
                  ZS = DELZ*FLOAT(J) + ZMIN 
                  B(M) = CURV2(ZS,IBCK,XB,YS,YSP,TENS)
               ENDIF
 150        CONTINUE
         ENDIF  
 160  CONTINUE
C
C========Warn about omitted knot points
C
      WRITE(IPRINT,1020)IBAD
      CALL FLUSH(IPRINT)
      WRITE(ILOG,1020)IBAD

      RETURN 

 1000 FORMAT(/1X,'BGWSRT Window background fitting...')
 1010 FORMAT(1X,'BGWSRT Error - window size too large for array')
 1020 FORMAT(1X,'Number of bad background points ',I10) 
 2000 FORMAT(1X,'***Error in spline fitting- FITPACK error ',I10)

      END

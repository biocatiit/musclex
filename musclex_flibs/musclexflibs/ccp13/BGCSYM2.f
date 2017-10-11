C     LAST UPDATE 16/03/98
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      SUBROUTINE BGCSYM2(AD,B,SMOO,TENS,PC1,PC2,NPIX,NRAST,DMIN,DMAX,
     &                  XC,YC,DINC,
     &                  CSYB,CSYD,YS,YSP,WRK,PIXBIN,INDEX,
     &                  MAXBIN,MAXNUM,IPRINT,ILOG)
      IMPLICIT NONE
C
C Purpose: Fits the background by the following method. The pixels are
C          assigned to a radial bin. The values held in this bin are  
C          sorted and pixel values in the range PC1 to PC2 are averaged
C          to form a background value for this bin. When the binning is
C          complete, the radial bin values are used to calculate spline
C          coefficients. Background values for the 2D image are then 
C          interpolated using these coefficients.
C
C Calls   2: SORT , CURVS
C Called by: BCKCSYM
C
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C
C Scalar arguments:
C
      INTEGER MAXBIN,MAXNUM
      REAL SMOO,TENS,PC1,PC2
      REAL DMIN,DMAX,XC,YC,DINC
      INTEGER IPRINT,ILOG
      INTEGER NPIX,NRAST
C
C Array arguments:
C
      REAL B(NPIX*NRAST),AD(NPIX*NRAST)
      REAL CSYB(MAXBIN),CSYD(MAXBIN),YS(MAXBIN),YSP(MAXBIN),
     &     WRK(9*MAXBIN),PIXBIN(MAXNUM)
      INTEGER INDEX(MAXNUM)
C
C Local Scalars:
C
      INTEGER IFPIX,ILPIX,IFRAST,ILRAST,NBIN
      INTEGER I,J,M,NB,IER,IBAD,NP,IP,IW,IBCK,IW1,IW2
      REAL DS,S,EPS,D1,D2,BCK
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


      WRITE(IPRINT,1000)
      CALL FLUSH(IPRINT)
      WRITE(ILOG,1000)
C
C========Set B to 0.
C
      DO 20 J=1,NRAST
         DO 10 I=1,NPIX
            M = (J-1)*NPIX + I
            B(M) = 0. 
 10      CONTINUE
 20   CONTINUE
C
C========Initialize limits and increment for binning
C
      IF(DINC.EQ.0.)DINC=1.0
      NBIN=INT( (DMAX-DMIN)/DINC )+1
C
C========Loop over pixels and bin into d*
C

      IBAD = 0
      IBCK = 0
      DO 70 NB=1,NBIN
         D1 = DMIN+(FLOAT(NB-1)*DINC)
         D2 = D1 + DINC
         NP = 0
         DO 50 J=1,NRAST
            DO 40 I=1,NPIX
               DS = SQRT( (FLOAT(I)-0.5-XC)**2 + (FLOAT(J)-0.5-YC)**2 )
               IF(DS.GT.D1.AND.DS.LE.D2)THEN
                  M = (J-1)*NPIX + I
                  IF(AD(M).GT.-0.99E+30)THEN
                     IF(NP.LT.MAXNUM)THEN
                        NP = NP + 1
                        PIXBIN(NP) = AD(M)
                     ENDIF
                  ENDIF
               ENDIF
 40         CONTINUE
 50      CONTINUE
C
C========Sort pixel values and sum over desired interval
C
         IF(NP.GT.0)THEN
            CALL SORT(PIXBIN,NP,INDEX)
            IP = 0
            BCK = 0.0
            IW1 = MIN(INT(PC1*FLOAT(NP))+1,NP)
            IW2 = MIN(INT(PC2*FLOAT(NP))+1,NP)
            DO 60 IW=IW1,IW2
               IP = IP + 1
               BCK = BCK + PIXBIN(INDEX(IW))
 60         CONTINUE
C
C========Form average
C
            IBCK = IBCK + 1
            CSYB(IBCK) = BCK/FLOAT(IP)
            CSYD(IBCK) = DMIN+((FLOAT(NB)-0.5)*DINC)
         ELSE
            IBAD = IBAD + 1
         ENDIF
 70   CONTINUE
      WRITE(IPRINT,1005)IBCK,DINC
      CALL FLUSH(IPRINT)
      WRITE(ILOG,1005)IBCK,DINC
C
C========Calculate spline coefficients for interpolating radial bins
C
      IF(IBCK.GT.1)THEN
         EPS = SQRT(2.0/FLOAT(IBCK))
         S = SMOO*FLOAT(IBCK)
         CALL CURVS(IBCK,CSYD,CSYB,1.0,1,S,EPS,YS,YSP,TENS,WRK,IER)
         IF(IER.NE.0)THEN
            WRITE(IPRINT,2000)IER
            CALL FLUSH(IPRINT)
            WRITE(ILOG,2000)IER
            STOP 'Fatal error'
         ENDIF
C
C========Loop over image pixels to interpolate background values
C
         DO 90 J=IFRAST,ILRAST
            DO 80 I=IFPIX,ILPIX
               M = (J-1)*NPIX + I
               DS = SQRT(  (FLOAT(I)-0.5-XC)**2 + (FLOAT(J)-0.5-YC)**2 )
               IF(AD(M).GT.-0.99E+30)THEN
                  B(M) = CURV2(DS,IBCK,CSYD,YS,YSP,TENS)
               ENDIF
 80         CONTINUE
 90      CONTINUE
      ENDIF

      WRITE(IPRINT,1010)
      CALL FLUSH(IPRINT)
      WRITE(ILOG,1010)

      RETURN
C
 1000 FORMAT(/1X,'BGCSYM Circularly-symmetric background fitting...')
 1005 FORMAT(1X,'Number of radial bins ',I4,1X,'increment ',F10.4)
 1010 FORMAT(1X,'Circularly-symmetric background fitting done')
 2000 FORMAT(1X,'***Error in spline fitting: FITPACK error ',I3)
      END


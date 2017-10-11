C     LAST UPDATE 06/10/98
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      SUBROUTINE BCKSMOOTH(BUF,CBACK,B,SMBUF,VALS,OPTIONS,
     &                     XB,YB,YS,YSP,SIG,WRK,IFLAG,MAXFUNC)
      IMPLICIT NONE
C
C Purpose: Background subtraction by smoothing
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Calls 3: SUBTRACTBLUR,CURVS,CURV2
C
C FIX include file:
C
      INCLUDE 'FIXPAR.COM'
      INCLUDE 'FIT.COM'
C
C Arguments:
C
      INTEGER MAXFUNC
      REAL BUF(NPIX*NRAST),CBACK(NPIX*NRAST),B(NPIX*NRAST)
      REAL SMBUF(MAXFUNC)
      INTEGER IFLAG(NPIX*NRAST)
      REAL XB(NPIX),YB(NPIX),YS(NPIX),YSP(NPIX),SIG(NPIX),WRK(9*NPIX)
      REAL VALS(20)
      CHARACTER*10 OPTIONS(10)
C
C Local variables:
C
      INTEGER PWID,RWID,CYCLES
      INTEGER I,J,M,J1UNIT,I1UNIT,I2UNIT,IRC,IMEM
      INTEGER ISPEC,LSPEC,INCR,MEM,IFFR,ILFR,IFINC,NFRAME
      INTEGER IHFMAX,IFRMAX,JPIX,JRAST
      INTEGER BFRAME,DMBFRAME,NFRAMES
      INTEGER*4 BPTR
      CHARACTER*80 HFNAM,BFNAME
      CHARACTER*80 BHEAD1,BHEAD2
      INTEGER FWHM
      REAL ARG1,ARG2,GTOT,STDEV,XC1,YC1,DMAX,DMIN,SQRAD,MSIG
      REAL PI,LOWVAL
      INTEGER GCEN

      INTEGER IBCK
      REAL XA,S,EPS
      REAL SMOO,TENS

      LOGICAL BOXCA,GAUSS,MERGE,DOEDGE
      CHARACTER*1 YON
C
C External function:
C
      REAL CURV2
      EXTERNAL CURV2
      INTEGER GETMEM
      EXTERNAL GETMEM
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      BOXCA=.TRUE.
      GAUSS=.FALSE.
      MERGE=.FALSE.
      DOEDGE=.FALSE.

      DO 666 I=1,10
        IF(OPTIONS(I)(1:5).EQ.'BOXCA')THEN
          BOXCA=.TRUE.
          GAUSS=.FALSE.
        ELSEIF(OPTIONS(I)(1:5).EQ.'GAUSS')THEN
          GAUSS=.TRUE.
          BOXCA=.FALSE.
        ELSEIF(OPTIONS(I)(1:5).EQ.'MERGE')THEN
          MERGE=.TRUE.
        ELSEIF(OPTIONS(I)(1:4).EQ.'DOED')THEN
          DOEDGE=.TRUE.
        ENDIF
 666  CONTINUE

      PI=ACOS(-1.0)
      J1UNIT=13
      I1UNIT=14
      I2UNIT=15
      ITERM=5
      IPRINT=6

      IF(GAUSS)THEN

        FWHM=NINT(VALS(1))
        CYCLES=NINT(VALS(2))
        DMIN=VALS(3)
        DMAX=VALS(4)
        XC1=VALS(5)
        YC1=VALS(6)
        LOWVAL=VALS(7)
        IF(MERGE.AND..NOT.DOEDGE)THEN
          SMOO=VALS(8)
          TENS=VALS(9)
          MSIG=VALS(10)
        ENDIF

        PWID=2*FWHM+1
        RWID=PWID
        GCEN=FWHM+1
        STDEV=FWHM/2.354

      ELSEIF(BOXCA)THEN

        PWID=NINT(VALS(1))*2+1
        RWID=NINT(VALS(2))*2+1
        CYCLES=NINT(VALS(3))
        DMIN=VALS(4)
        DMAX=VALS(5)
        XC1=VALS(6)
        YC1=VALS(7)
        LOWVAL=VALS(8)
        IF(MERGE.AND..NOT.DOEDGE)THEN
          SMOO=VALS(9)
          TENS=VALS(10)
          MSIG=VALS(11)
        ENDIF

      ENDIF

      IF(PWID*RWID.GT.MAXFUNC)THEN
        WRITE(IPRINT,1400)
        CALL FLUSH(IPRINT)   
        WRITE(ILOG,1400)
      STOP
      ENDIF
      WRITE(IPRINT,1500)
      CALL FLUSH(IPRINT)
      WRITE(ILOG,1500)

      WRITE(ILOG,2000)
      WRITE(IPRINT,2000)
      CALL FLUSH(IPRINT)
      IF(BOXCA)THEN
        WRITE(ILOG,2010)PWID,RWID
        WRITE(IPRINT,2010)PWID,RWID
        CALL FLUSH(IPRINT)
      ELSEIF(GAUSS)THEN
        WRITE(ILOG,2020)FWHM
        WRITE(IPRINT,2020)FWHM
        CALL FLUSH(IPRINT)
      ENDIF
      WRITE(ILOG,2015)CYCLES
      WRITE(IPRINT,2015)CYCLES
      CALL FLUSH(IPRINT)
      WRITE(ILOG,2030)DMIN,DMAX
      WRITE(IPRINT,2030)DMIN,DMAX
      CALL FLUSH(IPRINT)
      WRITE(ILOG,2040)XC1,YC1
      WRITE(IPRINT,2040)XC1,YC1
      CALL FLUSH(IPRINT)

C++++++++  Pad the image +++++++++++++++++++++++++++++++++++++++++++++

      DO 2 J=1,NRAST
        DO 1 I=1,NPIX
          M=(J-1)*NPIX+I
          IF(BUF(M).LT.LOWVAL)THEN
            BUF(M)=-1.0E+30
          ELSE
            SQRAD=(FLOAT(I)-0.5-XC1)**2 + (FLOAT(J)-0.5-YC1)**2
            IF(SQRAD.LT.DMIN**2.OR.SQRAD.GT.DMAX**2)BUF(M)=-1.0E+30
          ENDIF
 1      CONTINUE
 2    CONTINUE


C+++++++++  Define smoothing function (SMBUF)  +++++++++++++++++++++++++

      IF(GAUSS)THEN
        GTOT=0.
        ARG1=1.0/SQRT(2.0*PI)
        DO 15 I=1,PWID
          DO 10 J=1,PWID
            M=(J-1)*PWID+I
            ARG2=( SQRT( FLOAT(I-GCEN)**2.+FLOAT(J-GCEN)**2. )
     &             /STDEV )**2.
            SMBUF(M)=ARG1*EXP(-0.5*ARG2)
            GTOT=GTOT+SMBUF(M)
 10       CONTINUE
 15     CONTINUE
        DO 18 I=1,PWID**2
          SMBUF(I)=SMBUF(I)/GTOT
 18     CONTINUE
      ELSEIF(BOXCA)THEN
        DO 20 I=1,PWID*RWID
          SMBUF(I)=1./FLOAT(PWID*RWID)
 20     CONTINUE
      ENDIF


C++++++++  Load starting background (B) ++++++++++++++++++++++++++++++

      IF(.NOT.DOEDGE)THEN
        WRITE(IPRINT,1005)
        CALL FLUSH(IPRINT)

        CALL GETHDR(ITERM,IPRINT,I1UNIT,HFNAM,ISPEC,LSPEC,INCR,MEM,
     &              IFFR,ILFR,IFINC,IHFMAX,IFRMAX,JPIX,JRAST,IRC)

        CALL FCLOSE(I1UNIT)

      IF(JPIX.NE.NPIX.OR.JRAST.NE.NRAST)THEN
        WRITE(IPRINT,1010)
        CALL FLUSH(IPRINT)
        WRITE(ILOG,1010)
        GOTO 999
      ENDIF

        CALL OPNFIL(I2UNIT,HFNAM,ISPEC,MEM,IFFR,ILFR,JPIX,JRAST,NFRAME,
     &              IRC)

        CALL RFRAME(I2UNIT,IFFR,JPIX,JRAST,BPTR,IRC)

        CALL FCLOSE(I2UNIT)

        CALL GETBUF(%val(BPTR),B,NPIX,NRAST)

        WRITE(ILOG,2050)HFNAM
        WRITE(IPRINT,2050)HFNAM
        CALL FLUSH(IPRINT)

      ENDIF


C++++++++  Find points that will be smoothed (IFLAG) ++++++++++++++++++

        CALL BLURLIMITS(BUF,NPIX,NRAST,PWID,RWID,XC1,YC1,IFLAG,DOEDGE)


C+++++++++  Get current background (CBACK)  ++++++++++++++++++++++++++++
C+++++++++  Combination of data to be smoothed and starting ++++++++++++
C+++++++++  background where no smoothing ++++++++++++++++++++++++++++++

      DO 25 I=1,NPIX*NRAST
        IF(IFLAG(I).EQ.2)THEN
          CBACK(i)=B(i)
        ELSE
          CBACK(i)=BUF(i)
        ENDIF
 25   CONTINUE

C+++++++++  Loop over the number of smoothing cycles +++++++++++++++++++

 26   CONTINUE

      DO 100 I=1,CYCLES 

C+++++++++  Blur background and subtract from pattern  +++++++++++++++++
C+++++++++  Put result in B    +++++++++++++++++++++++++++++++++++++++++

        CALL BLUR(BUF,CBACK,SMBUF,NPIX,NRAST,PWID,RWID,XC1,YC1,B,IFLAG,
     &            DOEDGE)


C++++++++  Subtract positive values in result (B)  ++++++++++++++++++++
C++++++++  from pattern (BUF) and set to current backgrnd (CBACK)  ++++

         DO 30 J=1,NPIX*NRAST
           IF(B(J).GT.0..AND.IFLAG(J).EQ.1)THEN
             CBACK(J)=BUF(J)-B(J)
           ENDIF
 30      CONTINUE

      WRITE(IPRINT,1001)I
      CALL FLUSH(IPRINT)

 100  CONTINUE


C++++++++  Store the current background +++++++++++++++++++++++++++++++

         DO 333 J=1,NPIX*NRAST
             B(J)=CBACK(J)
 333     CONTINUE


C++++++++  Fit splines to smooth out the image ++++++++++++++++++++++++

      IF(MERGE.AND..NOT.DOEDGE)THEN

        WRITE(ILOG,2060)SMOO,TENS
        WRITE(IPRINT,2060)SMOO,TENS
        CALL FLUSH(IPRINT)
        WRITE(ILOG,2065)MSIG
        WRITE(IPRINT,2065)MSIG
        CALL FLUSH(IPRINT)

        DO 200 J=1,NRAST
          XA=0.0
          IBCK=0
          DO 150 I=1,NPIX
            M=(J-1)*NPIX+I
            XA=XA+1.0
            IF(IFLAG(M).EQ.1)THEN
              IBCK=IBCK+1
              XB(IBCK)=XA
              YB(IBCK)=CBACK(M)
              SIG(IBCK)=1.0
            ELSEIF(IFLAG(M).EQ.2)THEN
              IBCK=IBCK+1
              XB(IBCK)=XA
              YB(IBCK)=CBACK(M)
              SIG(IBCK)=MSIG
            ENDIF
 150      CONTINUE
          IF(IBCK.GT.0)THEN
            EPS=SQRT(2.0/FLOAT(IBCK))
            S=SMOO*FLOAT(IBCK)
            CALL CURVS(IBCK,XB,YB,SIG,0,S,EPS,YS,YSP,TENS,WRK,IRC)
          ENDIF
          XA=0.0
          DO 170 I=1,NPIX
            M=(J-1)*NPIX+I
            XA=XA+1.0
          IF(IFLAG(M).GT.0)THEN
            CBACK(M)=CURV2(XA,IBCK,XB,YS,YSP,TENS)
          ELSEIF(IFLAG(M).EQ.0)THEN
            CBACK(M)=0.
          ENDIF
 170      CONTINUE
 200    CONTINUE
      ELSE
        DO 555 I=1,NPIX*NRAST
         IF(IFLAG(I).NE.1)THEN
            CBACK(I)=0.
         ENDIF
 555    CONTINUE
      ENDIF
 

C++++++++  Write background and pattern-background  +++++++++++++++++++
C++++++++  to different frames of output file  ++++++++++++++++++++++++

      IMEM=1
      NFRAMES=2
      BFRAME=1
      DMBFRAME=2

      WRITE(IPRINT,1006)
      CALL FLUSH(IPRINT)

      CALL OUTFIL(5,6,BFNAME,BHEAD1,BHEAD2,IRC)

      CALL OPNNEW(J1UNIT,NPIX,NRAST,NFRAMES,BFNAME,IMEM,BHEAD1,
     &            BHEAD2,IRC)


C++++++++  Write background +++++++++++++++++++++++++++++++++++++++++++

      CALL WFRAME(J1UNIT,BFRAME,NPIX,NRAST,CBACK,IRC)


C++++++++  Write pattern-background  ++++++++++++++++++++++++++++++++++

      DO 300 J=1,NPIX*NRAST
          CBACK(J)=BUF(J)-CBACK(J)
 300  CONTINUE

      CALL WFRAME(J1UNIT,DMBFRAME,NPIX,NRAST,CBACK,IRC)  

      CALL FCLOSE(J1UNIT)

C++++++++  Enquire as whether to perform more cycles ++++++++++++++++++

      WRITE(IPRINT,1700)
      CALL FLUSH(IPRINT)
      READ(5,'(A1)')YON
      CALL UPPER(YON,1)
      IF(YON.EQ.'Y')THEN
        WRITE(IPRINT,1800)
        CALL FLUSH(IPRINT)
        READ(5,*)CYCLES
        DO 777 J=1,NPIX*NRAST
          CBACK(J)=B(J)
 777    CONTINUE
        WRITE(ILOG,2070)CYCLES
        WRITE(IPRINT,2070)CYCLES
        CALL FLUSH(IPRINT)
        WRITE(IPRINT,1500)
        CALL FLUSH(IPRINT)
        WRITE(ILOG,1500)
        GOTO 26
      ENDIF

      WRITE(IPRINT,1600)
      CALL FLUSH(IPRINT)
      WRITE(ILOG,1600)

 999  RETURN
 1001 FORMAT(/1X,'Completed cycle ',I4)
 1005 FORMAT(1X,'208 Import background file')
 1006 FORMAT(1X,'209 New background file')
 1010 FORMAT(1X,'400 Imported Background frame has wrong dimensions')
 1400 FORMAT(/1X,'Smoothing function too big for array')
 1500 FORMAT(/1X,'203 Calculating background...')
 1600 FORMAT(/1X,'Finished calculating background'/)
 1700 FORMAT(1X,' 207 Perform more smoothing cycles?')
 1800 FORMAT(1X,' Enter number of additional cycles')
 2000 FORMAT(/1X,'Background smoothing')
 2010 FORMAT(1X,'Smoothing function = Boxcar, width = ',I4,
     &       ' ,height = ',I4)
 2015 FORMAT(1X,'Cycles = ',I4)
 2020 FORMAT(1X,'Smoothing function = Gaussian, FWHM = ',I4)
 2030 FORMAT(1X,'Pattern limits  Rmin = ',F10.2,' ,Rmax = ',F10.2)
 2040 FORMAT(1X,'Centre = ',F10.2,' , ',F10.2)
 2050 FORMAT(1X,'Imported background file = ',A)
 2060 FORMAT(1X,'Smoothing = ',F10.2,' ,tension = ',F10.2)
 2065 FORMAT(1X,'Weight of imported background = ',F10.2)
 2070 FORMAT(1X,'Additional cycles ',I4)
      END

C     LAST UPDATE 06/10/98
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      SUBROUTINE BCKWIN(BUF,BBUF,VALS)
      IMPLICIT NONE
C
C Purpose: Background subtraction by roving window 
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Calls 1: BGWSRT2
C
C
C FIX include file:
C
      INCLUDE 'FIXPAR.COM'
      INCLUDE 'FIT.COM'
C
C Arguments:
C
      REAL BUF(NPIX*NRAST),BBUF(NPIX*NRAST)
      REAL VALS(20)
C
C Local variables:
C
      INTEGER IWID,JWID,ISEP,JSEP
      REAL SMOO,TENS,LPIX,HPIX,XC1,YC1,DMIN,DMAX,SQRAD
      REAL LOWVAL
      INTEGER BFRAME,DMBFRAME,NFRAMES,IMEM,IRC
      INTEGER J1UNIT,I,J,M
      CHARACTER*80 BFNAME
      CHARACTER*80 BHEAD1,BHEAD2
C+++++++++++++++ These need to be integer*8 on 64 bit machines ++++++++
      INTEGER*4 JBUF1,JBUF2,JBUF3,JBUF4,JBUF5,JBUF6,JBUF7
      INTEGER*4 NBUF1,NBUF2,NBUF3
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C External functions:
C
      INTEGER GETMEM
      EXTERNAL GETMEM
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      NBUF1=MAX(NPIX,NRAST)*4
      NBUF2=NBUF1*9
      NBUF3=(NPIX/2)*(NRAST/2)*4

c Allocate memory

      IRC=GETMEM(NBUF1,JBUF1)
      IF(IRC.NE.1)STOP 'Error allocating memory'
      IRC=GETMEM(NBUF1,JBUF2)
      IF(IRC.NE.1)STOP 'Error allocating memory'
      IRC=GETMEM(NBUF1,JBUF3)
      IF(IRC.NE.1)STOP 'Error allocating memory'
      IRC=GETMEM(NBUF1,JBUF4)
      IF(IRC.NE.1)STOP 'Error allocating memory'
      IRC=GETMEM(NBUF2,JBUF5)
      IF(IRC.NE.1)STOP 'Error allocating memory'
      IRC=GETMEM(NBUF3,JBUF6)
      IF(IRC.NE.1)STOP 'Error allocating memory'
      IRC=GETMEM(NBUF3,JBUF7)
      IF(IRC.NE.1)STOP 'Error allocating memory'
 
      J1UNIT=13

      IWID=NINT(VALS(1))
      JWID=NINT(VALS(2))
      ISEP=NINT(VALS(3))
      JSEP=NINT(VALS(4))
      SMOO=VALS(5)
      TENS=VALS(6)
      LPIX=VALS(7)/100.
      HPIX=VALS(8)/100.
      DMIN=VALS(9)
      DMAX=VALS(10)
      XC1=VALS(11)
      YC1=VALS(12)
      LOWVAL=VALS(13)

      WRITE(IPRINT,1500)
      CALL FLUSH(IPRINT)
      WRITE(ILOG,1500)

      WRITE(IPRINT,2000)
      CALL FLUSH(IPRINT)
      WRITE(ILOG,2000)

      WRITE(IPRINT,2010)(2*IWID+1),(2*JWID+1)
      CALL FLUSH(IPRINT)
      WRITE(ILOG,2010)(2*IWID+1),(2*JWID+1)

      WRITE(IPRINT,2020)ISEP,JSEP
      CALL FLUSH(IPRINT)
      WRITE(ILOG,2020)ISEP,JSEP

      WRITE(IPRINT,2030)LPIX,HPIX
      CALL FLUSH(IPRINT)
      WRITE(ILOG,2030)LPIX,HPIX

      WRITE(IPRINT,2040)DMIN,DMAX
      CALL FLUSH(IPRINT)
      WRITE(ILOG,2040)DMIN,DMAX

      WRITE(IPRINT,2050)XC1,YC1
      CALL FLUSH(IPRINT)
      WRITE(ILOG,2050)XC1,YC1

      WRITE(IPRINT,2060)SMOO,TENS
      CALL FLUSH(IPRINT)
      WRITE(ILOG,2060)SMOO,TENS



C++++++++  Pad the image +++++++++++++++++++++++++++++++++++++++++++++

      DO 20 J=1,NRAST
        DO 10 I=1,NPIX
          M=(J-1)*NPIX+I
          IF(BUF(M).LT.LOWVAL)THEN
            BUF(M)=-1.0E+30
          ELSE
            SQRAD=(FLOAT(I)-0.5-XC1)**2 + (FLOAT(J)-0.5-YC1)**2
            IF(SQRAD.LT.DMIN**2.OR.SQRAD.GT.DMAX**2)BUF(M)=-1.0E+30
          ENDIF
 10     CONTINUE
 20   CONTINUE

C++++++++  Get background by roving window method   +++++++++++++++++++

      CALL BGWSRT2( BUF,BBUF,IWID,JWID,ISEP,JSEP,SMOO,TENS,
     &             LPIX,HPIX,NPIX,NRAST,NBUF1/4,NBUF3/4,
     &             %val(JBUF1),%val(JBUF2),%val(JBUF3),
     &             %val(JBUF4),%val(JBUF5),%val(JBUF6),%val(JBUF7), 
     &             IPRINT,ILOG)

      CALL FREMEM(JBUF1)
      CALL FREMEM(JBUF2)
      CALL FREMEM(JBUF3)
      CALL FREMEM(JBUF4)
      CALL FREMEM(JBUF5)
      CALL FREMEM(JBUF6)
      CALL FREMEM(JBUF7)

      DO 200 J=1,NPIX*NRAST
        IF(BBUF(J).EQ.-1.0E+30)BBUF(J)=0.
 200  CONTINUE

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

      CALL WFRAME(J1UNIT,BFRAME,NPIX,NRAST,BBUF,IRC)


C++++++++  Write pattern-background  ++++++++++++++++++++++++++++++++++

      DO 300 J=1,NPIX*NRAST
        BBUF(J)=BUF(J)-BBUF(J)
 300  CONTINUE

      CALL WFRAME(J1UNIT,DMBFRAME,NPIX,NRAST,BBUF,IRC)

      CALL FCLOSE(J1UNIT)

      WRITE(IPRINT,1600)
      CALL FLUSH(IPRINT)
      WRITE(ILOG,1600)
 
      RETURN
 
 1006 FORMAT(1X,'209 New background file')
 1500 FORMAT(/1X,'203 Calculating background...')
 1600 FORMAT(/1X,'206 Finished calculating background'/)
 2000 FORMAT(/1X,'Roving window background')
 2010 FORMAT(1X,'Window size = ',I4,' , ',I4)
 2020 FORMAT(1X,'Window separation = ',I4,' , ',I4)
 2030 FORMAT(1X,'Pixel range = ',F6.2,' , ',F6.2)
 2040 FORMAT(1X,'Pattern limits  Rmin = ',F10.2,' ,Rmax = ',F10.2)
 2050 FORMAT(1X,'Centre = ',F10.2,' , ',F10.2)
 2060 FORMAT(1X,'Smoothing = ',F10.2,' ,tension = ',F10.2)
      END

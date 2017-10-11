C     LAST UPDATE 06/10/98
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      SUBROUTINE BLURLIMITS(BUF,NPIX,NRAST,PWID,RWID,XC,YC,IFLAG,DOEDGE)
      IMPLICIT NONE
C
C Purpose: Get limits for blurring background
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Arguements:
C
      INTEGER NPIX,NRAST,PWID,RWID
      REAL XC,YC
      REAL BUF(NPIX*NRAST)
      INTEGER IFLAG(NPIX*NRAST)
      LOGICAL DOEDGE
C
C Local variables:
C
      INTEGER I,J,K,L,M,SPIX,SRAST,SPOINT
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C

      DO 100 J=1,NRAST
        DO 90 I=1,NPIX

          M=(J-1)*NPIX+I
          IFLAG(M)=0

          IF(BUF(M).GT.-0.5E+30.AND.DOEDGE)THEN

            IFLAG(M)=1

          ELSEIF(BUF(M).GT.-0.5E+30.AND..NOT.DOEDGE)THEN

            DO 80 L=1,RWID
              DO 70 K=1,PWID

                SPIX=(I+K-(PWID+1)/2)
                SRAST=(J+L-(RWID+1)/2)

                IF(SPIX.GT.0.AND.SPIX.LE.NPIX.AND.
     &             SRAST.GT.0.AND.SRAST.LE.NRAST)THEN

                   SPOINT=(SRAST-1)*NPIX+SPIX

                   IF(BUF(SPOINT).GT.-0.5E+30)THEN
                     IFLAG(M)=1
                   ELSE
                     IFLAG(M)=2
                   ENDIF

                ELSE

                    IFLAG(M)=2
                    GOTO 90

                ENDIF

 70           CONTINUE
 80         CONTINUE

          ENDIF

 90     CONTINUE
 100  CONTINUE

      RETURN
      END

C     LAST UPDATE 25/03/93
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      SUBROUTINE SORT(X,N,IND)
      IMPLICIT NONE
C
C Purpose: Sorts an array X returning it unchanged but with and index
C          array IND. 
C
C Calls   0:
C Called by:
C
C-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
C Arguments:
C
      INTEGER N
      REAL X(N)
      INTEGER IND(N)
C
C Local variables
C
      INTEGER I,J,ITMP,L,IR 
      REAL Q 
C
C-----------------------------------------------------------------------
      DO 10 I=1,N
         IND(I) = I
 10   CONTINUE
      IF(N.EQ.1)RETURN 
      L = N/2 + 1
      IR = N
 20   CONTINUE
         IF(L.GT.1)THEN
            L = L - 1
            ITMP = IND(L)
            Q = X(ITMP)
         ELSE
            ITMP = IND(IR)
            Q = X(ITMP)
            IND(IR) = IND(1)
            IR = IR - 1
            IF(IR.EQ.1)THEN
               IND(1) = ITMP
               RETURN
            ENDIF
         ENDIF
         I = L
         J = L + L
 30      IF(J.LE.IR)THEN
            IF(J.LT.IR)THEN
               IF(X(IND(J)).LT.X(IND(J+1)))J = J + 1
            ENDIF
            IF(Q.LT.X(IND(J)))THEN
               IND(I) = IND(J)
               I = J
               J = J + J
            ELSE
               J = IR + 1
            ENDIF
         GOTO 30
         ENDIF  
         IND(I) = ITMP
      GOTO 20
      END 

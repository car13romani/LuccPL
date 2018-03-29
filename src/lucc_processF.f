CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                             C
C   (c) Carlos Alexandre Romani <car13romani@gmail.com>       C
C                                                             C
C       Aplied Computing                                      C
C       National Institute for Space Research (INPE), Brazil  C
C                                                             C
C                                                             C
C   Lucc Process                      C
C                                                             C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     SB - Size of data block
C     ST - Size of time series
C     NR - Number of relations
C     BI - Input of data
C     QA - Query Array - QA(4,NR) = {relations{}, years{}, patterns{}, union_op{} }
C     L - Location
C     I - Time Instant
C     R - Relation
C     CO - Count
C     KO - Key OR
C     
C     before e after será apenas um operador DURING com data de inicio e final (apenas internamente)
C     
C     

      SUBROUTINE lucc_process(SB, ST, NR, BI, BO, QA)

      INTEGER SB, ST, NR, I, J, R, L, BI(ST,SB), BO(ST,SB), QA(NR,5), CO, KO

      CO = 0
      KO = 0
C     For all block
      DO 50 L = 1, SB
        DO 40 R = 1, NR



C     1. DURING ----------------------------------
          IF (QA(R,1).EQ.1) THEN
            CO = 0
C     Individual processing            
            DO 30 I = (QA(R,2)+1), (QA(R,3)-1)
              IF (BI(I,L).EQ.QA(R,4)) THEN
                BO(I,L) = 1
                CO = CO + 1
              ENDIF
   30       CONTINUE

c            PRINT*, "R = ", R
c            PRINT*, "CO = ", CO
c            PRINT*, "KO = ", KO
c            PRINT*, "QAR5 = ", QA(R,5)
c            PRINT*

c           AND/OR analisys
            IF (CO.EQ.0) THEN
              IF (KO.EQ.0) THEN
                IF (QA(R,5).EQ.0) THEN
C                 limpar TS
                  DO 10 I = 1, ST
                    BO(I,L) = 0
   10             CONTINUE
                  GOTO 50
                ELSE
                  KO = 1
                ENDIF
C             KO = 1
              ELSE 
                IF (KO.EQ.1) THEN
                  IF (QA(R,5).EQ.0) THEN
C                  limpar TS
                    DO 11 I = 1, ST
                      BO(I,L) = 0
   11               CONTINUE
                    GOTO 50
                  ENDIF
C               KO = -1
                ELSE
                  IF (QA(R,5).EQ.0) THEN
                    KO = 0
                  ENDIF
                ENDIF
              ENDIF

C           C > 0            
            ELSE
              IF ((KO.EQ.0).AND.(QA(R,5).EQ.1)) THEN
                KO = -1
              ELSE 
                IF (KO.EQ.1) THEN
                  IF (QA(R,5).EQ.0) THEN
                    KO = 0
                  ELSE
                    KO = -1
                  ENDIF
                ELSE
                  IF (QA(R,5).EQ.0) THEN
                    KO = 0
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
            
            
          ENDIF
          

   40   CONTINUE
   50 CONTINUE
      END

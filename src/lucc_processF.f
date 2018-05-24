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

      INTEGER SB, ST, NR, I, J, R, L, BI(ST,SB), BO(ST,SB), QA(NR,5), CO, KO, TI

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
c *********************************
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
c *********************************
            
          ENDIF
          



C     2. MEETS --------------------------------------------
          IF (QA(R,1).EQ.2) THEN
            CO = 0
C     Individual processing
            IF (BI((QA(R,3)+1),L).NE.(QA(R,4))) THEN
              IF (BI(QA(R,3),L).EQ.(QA(R,4))) THEN
                IF (BI(QA(R,3)-1,L).EQ.(QA(R,4))) THEN
                  BO(QA(R,3),L) = 1
                  BO((QA(R,3)-1),L) = 1
                  CO = CO + 2
                  DO 31 I = (QA(R,3)-2), (QA(R,2)+1), -1
                    IF (BI(I,L).EQ.QA(R,4)) THEN
                      BO(I,L) = 1
                      CO = CO + 1
                    ELSE
c                      PRINT*, "MEETS"
c                      PRINT*, "R = ", R
c                      PRINT*, "CO = ", CO
c                      PRINT*, "KO = ", KO
c                      PRINT*, "QAR5 = ", QA(R,5)
c                      PRINT*
                      GOTO 41
                    ENDIF
   31             CONTINUE
                ENDIF
              ENDIF
            ENDIF
           

           
c *********************************
c           AND/OR analisys
          DO 41
            IF (CO.EQ.0) THEN
              IF (KO.EQ.0) THEN
                IF (QA(R,5).EQ.0) THEN
C                 limpar TS
c                  PRINT*, "CLEAN"
                  DO 12 I = 1, ST
                    BO(I,L) = 0
   12             CONTINUE
                  GOTO 50
                ELSE
                  KO = 1
                ENDIF
C             KO = 1
              ELSE 
                IF (KO.EQ.1) THEN
                  IF (QA(R,5).EQ.0) THEN
C                  limpar TS
                    DO 13 I = 1, ST
                      BO(I,L) = 0
   13               CONTINUE
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
c              PRINT*, "KO = ", KO
c              PRINT*, "CO = ", CO
c              PRINT*, "QA = ", QA(R,5)
              IF ((KO.EQ.0).AND.(QA(R,5).EQ.1)) THEN
                KO = -1
c                PRINT*, "KO = -1"
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
          GOTO 40
   41   CONTINUE
c *********************************


          ENDIF

C     3. MEEBY -------------------------------------------
          IF (QA(R,1).EQ.3) THEN
            CO = 0
C     Individual processing
            IF (BI((QA(R,2)-1),L).NE.(QA(R,4))) THEN
              IF (BI(QA(R,2),L).EQ.(QA(R,4))) THEN
                IF (BI(QA(R,2)+1,L).EQ.(QA(R,4))) THEN
                  BO(QA(R,2),L) = 1
                  BO((QA(R,2)+1),L) = 1
                  CO = CO + 2
                  DO 32 I = (QA(R,2)+2), (QA(R,3)-1) 
                    IF (BI(I,L).EQ.QA(R,4)) THEN
                      BO(I,L) = 1
                      CO = CO + 1
                    ELSE
c                      PRINT*, "METBY"
c                      PRINT*, "R = ", R
c                      PRINT*, "CO = ", CO
c                      PRINT*, "KO = ", KO
c                      PRINT*, "QAR5 = ", QA(R,5)
c                      PRINT*
                      GOTO 40
                    ENDIF
   32             CONTINUE
                ENDIF
              ENDIF
            ENDIF


c            PRINT*, "METBY"
c            PRINT*, "R = ", R
c            PRINT*, "CO = ", CO
c            PRINT*, "KO = ", KO
c            PRINT*, "QAR5 = ", QA(R,5)
c            PRINT*


c *********************************
c           AND/OR analisys
            IF (CO.EQ.0) THEN
              IF (KO.EQ.0) THEN
                IF (QA(R,5).EQ.0) THEN
C                 limpar TS
                  DO 14 I = 1, ST
                    BO(I,L) = 0
   14             CONTINUE
                  GOTO 50
                ELSE
                  KO = 1
                ENDIF
C             KO = 1
              ELSE 
                IF (KO.EQ.1) THEN
                  IF (QA(R,5).EQ.0) THEN
C                  limpar TS
c                    PRINT*, "CLEAN"
                    DO 15 I = 1, ST
                      BO(I,L) = 0
   15               CONTINUE
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
c *********************************

          ENDIF


C     4. HOLDS ----------------------------------
          IF (QA(R,1).EQ.4) THEN
            CO = 0
C     Individual processing
            IF (BI((QA(R,2)-1),L).NE.(QA(R,4))) THEN
              IF (BI((QA(R,3)+1),L).NE.(QA(R,4))) THEN

                DO 33 I = (QA(R,2)), (QA(R,3)) 
                  IF (BI(I,L).EQ.QA(R,4)) THEN
                    BO(I,L) = 1
                    CO = CO + 1
                  ELSE
                    GOTO 40
                  ENDIF
   33           CONTINUE
                TI = (QA(R,3)-QA(R,2)+1)
                IF (CO.NE.TI) THEN
                  DO 110 J = 1, ST
                    BO(J,L) = 0
  110            CONTINUE
                  GOTO 50
                ENDIF
              ENDIF
            ENDIF

c            PRINT*, "R = ", R
c            PRINT*, "CO = ", CO
c            PRINT*, "KO = ", KO
c            PRINT*, "QAR5 = ", QA(R,5)
c            PRINT*
c *********************************
c           AND/OR analisys
            IF (CO.EQ.0) THEN
              IF (KO.EQ.0) THEN
                IF (QA(R,5).EQ.0) THEN
C                 limpar TS
                  DO 16 I = 1, ST
                    BO(I,L) = 0
   16             CONTINUE
                  GOTO 50
                ELSE
                  KO = 1
                ENDIF
C             KO = 1
              ELSE 
                IF (KO.EQ.1) THEN
                  IF (QA(R,5).EQ.0) THEN
C                  limpar TS
                    DO 17 I = 1, ST
                      BO(I,L) = 0
   17               CONTINUE
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
c *********************************

          ENDIF

   40   CONTINUE
   50 CONTINUE
      END










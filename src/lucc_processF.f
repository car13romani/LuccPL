CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                             C
C   (c) Carlos Alexandre Romani <car13romani@gmail.com>       C
C                                                             C
C       Aplied Computing                                      C
C       National Institute for Space Research (INPE), Brazil  C
C                                                             C
C                                                             C
C                           Lucc Process                      C
C                                                             C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     SB - Size of data block
C     ST - Size of time series
C     NR - Number of relations
C     BI - Input of data
C     QA - Query Array - QA(6,NR) = {relations{}, time_1{}, time_2{}, patterns_1{}, patterns_2{}, union_op{} }
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

      INTEGER SB, ST, NR, I, J, R, L, BI(ST,SB), BO(ST,SB), QA(NR,6), CO, KO, TI, STOP_TS
      STOP_TS = 0
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
            call and_or(CO,KO,QA(R,6),ST,BO( : ,L),STOP_TS)
            IF (STOP_TS.EQ.1) THEN
              STOP_TS = 0
              GOTO 50
            ENDIF
          ENDIF
          








C     2. MEETS --------------------------------------------
          IF (QA(R,1).EQ.2) THEN
            CO = 0
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
                    EXIT
                  ENDIF
   31           CONTINUE
              ENDIF
            ENDIF
            call and_or(CO,KO,QA(R,6),ST,BO( : ,L),STOP_TS)
            IF (STOP_TS.EQ.1) THEN
              STOP_TS = 0
              GOTO 50
            ENDIF
          ENDIF







C     3. MEEBY -------------------------------------------
          IF (QA(R,1).EQ.3) THEN
            CO = 0
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
                    EXIT
                  ENDIF
   32           CONTINUE
              ENDIF
            ENDIF
            call and_or(CO,KO,QA(R,6),ST,BO( : ,L),STOP_TS)
            IF (STOP_TS.EQ.1) THEN
              STOP_TS = 0
              GOTO 50
            ENDIF
          ENDIF







C     4. HOLDS ----------------------------------
          IF (QA(R,1).EQ.4) THEN
            CO = 0
            DO 33 I = (QA(R,2)), (QA(R,3)) 
              IF (BI(I,L).EQ.QA(R,4)) THEN
                BO(I,L) = 1
                CO = CO + 1
                
              ENDIF
   33       CONTINUE
            TI = (QA(R,3)-QA(R,2)+1)
            IF (CO.NE.TI) THEN
              CALL clean_ts(QA(R,2),QA(R,3),BO( : ,L))
              CO = 0
            ENDIF
            call and_or(CO,KO,QA(R,6),ST,BO( : ,L),STOP_TS)
            IF (STOP_TS.EQ.1) THEN
              STOP_TS = 0
              GOTO 50
            ENDIF
          ENDIF






C     5. RECUR ----------------------------------
          IF (QA(R,1).EQ.5) THEN
            CO = 0
            DO 34 I = 0, (QA(R,2)-1) 
              IF (BI(I,L).EQ.QA(R,4)) THEN
                BO(I,L) = 1
                CO = CO + 1
              ENDIF
   34       CONTINUE
c            PRINT*, "CO = ", CO
            IF (CO.GT.0) THEN
              CO = 0
              DO 35 J = (QA(R,3)+1), ST
                IF (BI(J,L).EQ.QA(R,4)) THEN
c                  PRINT*, "J = ", J
                  BO(J,L) = 1
                  CO = CO + 1
                ENDIF
   35         CONTINUE
              IF (CO.EQ.0) THEN
                CALL clean_ts(1,ST,BO( : ,L))
              ENDIF
            ENDIF
            call and_or(CO,KO,QA(R,6),ST,BO( : ,L),STOP_TS)
            IF (STOP_TS.EQ.1) THEN
              STOP_TS = 0
              GOTO 50
            ENDIF
          ENDIF





C     6. CONVERT ----------------------------------
          IF (QA(R,1).EQ.6) THEN
            CO = 0
            IF (BI(QA(R,2)+1,L).EQ.(QA(R,5))) THEN
              IF (BI(QA(R,2),L).EQ.(QA(R,4))) THEN
                IF (BI(QA(R,2)-1,L).EQ.(QA(R,4))) THEN
                  BO(QA(R,2),L) = 1
                  BO((QA(R,2)-1),L) = 1
                  BO((QA(R,2)+1),L) = 1
                  CO = CO + 3
                  DO 36 I = (QA(R,2)-2), 0, -1
                    IF (BI(I,L).EQ.QA(R,4)) THEN
                      BO(I,L) = 1
                      CO = CO + 1
                    ELSE
                      EXIT
                    ENDIF
   36             CONTINUE
   				  DO 37 I = (QA(R,2)+2), ST
                    IF (BI(I,L).EQ.QA(R,5)) THEN
                      BO(I,L) = 1
                      CO = CO + 1
                    ELSE
                      EXIT
                    ENDIF
   37             CONTINUE
                ENDIF
              ENDIF
            ENDIF
            call and_or(CO,KO,QA(R,6),ST,BO( : ,L),STOP_TS)
            IF (STOP_TS.EQ.1) THEN
              STOP_TS = 0
              GOTO 50
            ENDIF
          ENDIF







C     7. EVOLVE ----------------------------------
		  IF (QA(R,1).EQ.7) THEN
            CO = 0
            DO 38 I = (QA(R,2)), 0, -1
              IF (BI(I,L).EQ.QA(R,4)) THEN
                BO(I,L) = 1
                CO = CO + 1
              ENDIF
   38       CONTINUE
   			IF (CO.GT.0) THEN
   			  CO = 0
   			  DO 39 I = (QA(R,3)), ST
                IF (BI(I,L).EQ.QA(R,5)) THEN
                  BO(I,L) = 1
                  CO = CO + 1
                ENDIF
   39         CONTINUE
            ENDIF
            call and_or(CO,KO,QA(R,6),ST,BO( : ,L),STOP_TS)
            IF (STOP_TS.EQ.1) THEN
              STOP_TS = 0
              GOTO 50
            ENDIF
          ENDIF




















  



   40   CONTINUE
   50 CONTINUE
      END


      SUBROUTINE clean_ts(S_ST,E_ST,TS)
        INTEGER S_ST, E_ST, TS(E_ST)
c        PRINT*, "CLEAN TS", S_ST
        DO 10 J = S_ST, E_ST
          TS(J) = 0
   10   CONTINUE
      END


      SUBROUTINE and_or(CO,KO,QA,ST,TS,STOP_TS)
        INTEGER CO, KO, QA, ST, TS(ST)
c        PRINT*, "AND OR ANALISYS"
c        PRINT*, "CO = ", CO
c        PRINT*, "KO = ", KO
c        PRINT*, "QAR5 = ", QA
c        PRINT*

c           AND/OR analisys
            IF (CO.EQ.0) THEN
              IF (KO.EQ.0) THEN
                IF (QA.EQ.0) THEN
C                 limpar TS
                  CALL clean_ts(1,ST,TS)
                  STOP_TS = 1
                  return
                ELSE
                  KO = 1
                ENDIF
C             KO = 1
              ELSE 
                IF (KO.EQ.1) THEN
                  IF (QA.EQ.0) THEN
C                  limpar TS
                    CALL clean_ts(1,ST,TS)
                    STOP_TS = 1
                    return
                  ENDIF
C               KO = -1
                ELSE
                  IF (QA.EQ.0) THEN
                    KO = 0
                  ENDIF
                ENDIF
              ENDIF

C           C > 0            
            ELSE
              IF ((KO.EQ.0).AND.(QA.EQ.1)) THEN
                KO = -1
              ELSE 
                IF (KO.EQ.1) THEN
                  IF (QA.EQ.0) THEN
                    KO = 0
                  ELSE
                    KO = -1
                  ENDIF
                ELSE
                  IF (QA.EQ.0) THEN
                    KO = 0
                  ENDIF
                ENDIF
              ENDIF
            ENDIF

            
      END


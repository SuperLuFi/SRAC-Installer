CMOD  SUBROUTINE UTLWOT(X,NCOL,LTBL,LG,TOP1,TOP2,TOP3,TITLE )
      SUBROUTINE UTLWOT(X,NCOL,LTBL,LG0,TOP1,TOP2,TOP3,TITLE )
C***********************************************************************
C     OUTPUT WRITES 1,2, OR 3-D ARRAYS
C***********************************************************************
C       X(LTBL,NCOL,LG) :  1,2 OR 3-D ARRARY
C       TOP1            :  COMMENT FOR FIRST  SUFFIX OF X ARRAY (A4)
C       TOP2            :  COMMENT FOR SECOND SUFFIX OF X ARRAY (A4)
C       TOP3            :  COMMENT FOR THIRD  SUFFIX OF X ARRAY (A4)
C***********************************************************************
      DIMENSION    X(LTBL,NCOL,1 )
      CHARACTER*4  TOP1,TOP2,TOP3,BLK
      CHARACTER*20 TITLE
C
CM    DATA         BLK / 4H     /
      DATA         BLK / '    ' /
C
C *** START OF PROCESS
C
      ISW = 0
      N6  = 6
      LG  = 0
C
*     LG  = LG0
*     IF(LG.GT.3) LG = 3
*     IF(LG .GT.0) ISW = 1
C
      IF(ISW.EQ.0) RETURN
C
      WRITE(N6,7) TITLE
    7 FORMAT(1H1,20X,'**************************',
     +      /1H ,20X,'*  ',A20,'  *',
     +      /1H ,20X,'**************************'/)
C
      DO 170 L=1,LG
         I02=0
         I03=(NCOL+9)/10
         IF (LG.GT.1) WRITE(N6,10000) TOP3,L
            DO 160 I=1,I03
            I01=I02+1
            I02=MIN0(I01+9,NCOL)
            WRITE(N6,10200) TOP1,(TOP2,J,J=I01,I02)
               DO 150 K=1,LTBL
               IF (K.EQ.1) GO TO 140
                  DO 100 J=I01,I02
  100             IF (X(K,J,L).NE.X(K-1,J,L)) GO TO 130
               IF (KE.EQ.KS) GO TO 110
               KE=K
               IF (K.EQ.LTBL) GO TO 130
               GO TO 150
  110          IF (K.EQ.LTBL) GO TO 140
               DO 120 J=I01,I02
  120             IF (X(K,J,L).NE.X(K+1,J,L)) GO TO 140
               KE=KE+1
               GO TO 150
  130          IF (KE.EQ.KS) GO TO 140
               WRITE(N6,10100) TOP1,KS,TOP1,KE
               IF (K.EQ.LTBL.AND.KE.EQ.K) GO TO 150
C 140          WRITE(N6,10300) K,(X(K,J,L),J=I01,I02)
  140          CONTINUE
               IF(LG.EQ.1.AND.K.EQ.LTBL) THEN
               IF(TOP3.EQ.BLK) THEN
                               WRITE(N6,10300) K,(X(K,J,L),J=I01,I02)
                               ELSE
                               WRITE(N6,10400) TOP3,(X(K,J,L),J=I01,I02)
                               ENDIF
               ELSE
               WRITE(N6,10300) K,(X(K,J,L),J=I01,I02)
               ENDIF
C
               KS=K+1
               KE=KS
  150       CONTINUE
  160    CONTINUE
  170 CONTINUE
      RETURN
C
10000 FORMAT(/1H0,2X,A4,I5)
10100 FORMAT(8X,A4,I5,6H THRU ,A4,I5,14H SAME AS ABOVE)
10200 FORMAT(//2X,A4,2X,A4,I3,9(5X,A4,I3))
10300 FORMAT(I6,1P10E12.5)
10400 FORMAT(2H  ,A4,1P10E12.5)
C
      END

      SUBROUTINE  BURNRW( MEMBER  , ARRAY , IMAX  , JMAX , KMAX  ,
     1                    ILAST   , JLAST , KLAST , MODE )
C
C
C
      INCLUDE  'BURNDINC'
C
      CHARACTER*8   MEMDIR , MEMBER
      CHARACTER*4   MODE
C
      COMMON  / BURNDB /  MEMDIR(MAXDIR),IST(MAXDIR),LENG(MAXDIR),
     1                    LAST ,IOWRK  , IOCNT , MREST
      COMMON  /KKSTORE /  STORE(MAXDIM)
C
      REAL*4        ARRAY(IMAX,JMAX,KMAX)
C
C
C ***  BRANCHING ACCORDING TO MODE
C
      IF(MODE.EQ.'READ') GO TO 1001
      IF(MODE.EQ.'OVER') GO TO 2001
C
C *** WRITE MODE ****
C
      IF(MEMBER.EQ.'START$  ')  THEN
                       LAST     = 0
                       IOWRK    = 97
                       MREST    = MAXDIM
                       IOCNT    = 0
                       DO 10  I = 1 , MAXDIR
CKSK                   MEMDIR(I)= 8H
                       MEMDIR(I)= '        '
  10                   CONTINUE
                       CALL  CLEA( STORE , MAXDIM , 0.0 )
                       CALL ICLEA( IST   , MAXDIR  , 0  )
                       CALL ICLEA( LENG  , MAXDIR  , 0  )
                       IST   (1)= 1
                       RETURN
                       ENDIF
C
       IPOS   = 0
       IF(LAST.GT.0) THEN
                     DO 15 I = 1 , LAST
                     IF(MEMDIR(I).EQ.MEMBER) THEN
       WRITE(6,*)  ' ** REQUESTED MEMBER DATA ALREADY EXISTS !!! '
       WRITE(6,*)  ' ** DATA(',MEMBER,') STORAGE WILL BE SKIPPED !! '
                         RETURN
                         ENDIF
  15                 CONTINUE
                     ENDIF
C
  20   KENG   = ILAST * JLAST * KLAST
       LAST   = LAST + 1
       IF(LAST.GT.MAXDIR) THEN
           WRITE(6,*)  ' ** FATAL PROGRAMING ERROR STOP AT BNRITE !'
           STOP 999
           ENDIF
C
       LENG(LAST)   = KENG
       MEMDIR(LAST) = MEMBER
C
       IF(KENG.GT.MREST) GO TO 101
C
       IST(LAST+1) = IST(LAST) + KENG
       ISW         = IST(LAST)
C
CM     WRITE(6,*) ' **  MEMBER(',MEMBER,') WILL STORED ARRAY SOTRE : ',
CM   1             KENG,' WORDS FROM ',ISW,' TO ',ISW+KENG-1,' ADDRESS.'
CM     WRITE(6,*) ' ** ILAST JLAST KLAST : ',ILAST,JLAST,KLAST
C
       DO 30   K = 1 , KLAST
       DO 30   J = 1 , JLAST
       DO 30   I = 1 , ILAST
       STORE(ISW)= ARRAY(I,J,K)
       ISW       = ISW + 1
   30  CONTINUE
       MREST     = MREST - KENG
       RETURN
C
  101  CONTINUE
       WRITE(6,*) '  ***  FILE IO WILL BE USED (FT97F001) (BURNRW) ! '
  102  IOCNT     = IOCNT + 1
       IST(LAST) = - IOCNT
       LENG(LAST)= KENG
       REWIND IOWRK
       IF(IOCNT.GT.1) THEN
                      DO 110 LOP = 1 , IOCNT - 1
                      READ(IOWRK)
  110                 CONTINUE
                      ENDIF
       WRITE(IOWRK) (((ARRAY(I,J,K),I=1,ILAST),J=1,JLAST),K=1,KLAST)
       REWIND IOWRK
       RETURN
C
C *** READ MODE ****
C
 1001  CONTINUE
C
       IPOS  = 0
       DO 210 I = 1 , LAST
       IF(MEMDIR(I).EQ.MEMBER) THEN
                 IPOS = I
                 GO TO 220
                 ENDIF
  210  CONTINUE
       WRITE(6,*)  ' ** NO MEMBER IN SEARCHING DATA AT BNREAD !'
       WRITE(6,*)  ' ** MEMBER(',MEMBER,') WAS NOT FOUND !'
       STOP 999
  220  CONTINUE
       KENG = ILAST*JLAST*KLAST
       IF(KENG.NE.LENG(IPOS)) THEN
            WRITE(6,*)  ' ** DATA LENGTH UNMATCH AT BNREAD !'
            WRITE(6,*)  ' ** MEMBER NAME IS ',MEMBER, ' !!!'
            WRITE(6,*)  ' ** REQUEST LENGTH IS ',KENG,' BUT ',LENG(IPOS)
            STOP 999
            ENDIF
C
       MENG      = IMAX*JMAX*KMAX
       CALL  CLEA( ARRAY , MENG , 0.0 )
C
       ISW       = IST(IPOS)
       IF(ISW.LT.0) GO TO 310
C
CM     WRITE(6,*) ' **  MEMBER(',MEMBER,') WILL BE READ : ',
CM   1             KENG,' WORDS FROM ',ISW,' ADDRESS.'
C
       DO 230  K = 1 , KLAST
       DO 230  J = 1 , JLAST
       DO 230  I = 1 , ILAST
       ARRAY(I,J,K) = STORE(ISW)
       ISW          = ISW + 1
  230  CONTINUE
       RETURN
C
  310  CONTINUE
       REWIND IOWRK
       ISW  = -ISW
C
       IF(ISW.GT.1) THEN
                    DO 320 I = 1 , ISW - 1
                    READ(IOWRK)
  320               CONTINUE
                    ENDIF
C
       READ(IOWRK) (((ARRAY(I,J,K),I=1,ILAST),J=1,JLAST),K=1,KLAST)
       REWIND IOWRK
       RETURN
C
C
C *** OVER-WRITE MODE ****
C
 2001  IPOS  = 0
       DO 410 I = 1 , LAST
       IF(MEMDIR(I).EQ.MEMBER) THEN
                 IPOS = I
                 GO TO 420
                 ENDIF
  410  CONTINUE
       GO TO 20
C
  420  ISW  = IST(IPOS)
       KENG = ILAST*JLAST*KLAST
       IF(KENG.GT.LENG(IPOS)) THEN
CKSK        MEMDIR(IPOS) = 8H
            MEMDIR(IPOS) = '        '
            LENG  (IPOS) = 0
            GO TO 20
            ELSE
            LENG  (IPOS) = KENG
            ENDIF
C
       IF(ISW.LT.0) THEN
CKSK                MEMDIR(IPOS) = 8H
                    MEMDIR(IPOS) = '        '
                    LENG  (IPOS) = 0
                    GO TO 101
                    ENDIF
C
       DO 510 K = 1 , KLAST
       DO 510 J = 1 , JLAST
       DO 510 I = 1 , ILAST
       STORE(ISW) = ARRAY(I,J,K)
       ISW        = ISW + 1
  510  CONTINUE
       RETURN
       END

      SUBROUTINE TSOURC(NR,NRR,MAR,IRR,MATNM,VOLR)
C
C     SUBPROGRAM TO PRODUCE THE SLOWING DOWN SOURCE TO THE THERMAL
C     RANGE
C
      PARAMETER  ( MAXW = 40000 )
      CHARACTER *4   CASENM,FILENM,NODE(2),NUMB,MATNM,LETF,LETA
C
      DIMENSION      MAR(*),VOLR(*)
      DIMENSION      MATNM(2,*),IRR(*)
C
      COMMON /MAINC/ IOPT(54) ,NEF   ,NET, NERF,NERT ,NMAT,DUM2(4),
     &               NOUT1    ,NOUT2 ,DUM3(6)  ,NEF3 ,I73 ,NSOUC , NFIN,
     &               NFOUT    ,ITYPE ,I78(23)  ,CASENM(2) ,DUM5(398)

CKSK  COMMON /WORK/   FLUX(15000),S(15000),AA(10000)
      COMMON /WORK/   FLUX(MAXW),S(MAXW),AA(10000)
      COMMON /PDSPDS/ BUF(540),IFLSW,FILENM(3),ECODE,TEMPRY
      COMMON /TMPSET/ STND(35),NUMB(61),NTDUMY
C
      DIMENSION       II(10000)
C
      EQUIVALENCE (AA(1),II(1))
C
      DATA LETF/'   F'/,LETA/'   A'/
C
C === CLEAR S:SOURCE TERM
C
      REWIND NSOUC
      IJGT  = NR *NET
      IJG   = NRR*NEF
CKSK
      IJGM = MAX0(IJGT,IJG)
      IF(IJGM.GT.MAXW) THEN
        WRITE(6,*) ' ERROR STOP(TSOURC) : WORK DIMENSION OVER'
        WRITE(6,*) ' ENLARGE VALUE OF MAXW IN SUBROUTINE TSOURC'
        WRITE(6,*) ' CURRENT  VALUE OF MAXW = ',MAXW
        WRITE(6,*) ' REQUIRED VALUE OF MAXW = ',IJGM
        WRITE(6,*) '   NRR         = ', NRR
        WRITE(6,*) '   NEF         = ', NEF
        WRITE(6,*) '   NR          = ', NR
        WRITE(6,*) '   NET         = ', NET
        WRITE(6,*) '   IJG =NRR*NEF= ', IJG
        WRITE(6,*) '   IJGT= NR*NET= ', IJGT
        STOP 777
      ENDIF
CKSK
      CALL CLEA ( S , IJGT , 0.0 )
C
      IFLSW      = 1
      FILENM(1)  = 'FLUX'
      FILENM(2)  = '    '
CM    FILENM(3)  = '    '
      NODE(1)    = CASENM(1)
      NODE(2)    = 'X002'
      CALL PACK(NODE(2),1,LETF)
      IF(IOPT(79).GT.0)    CALL PACK(NODE(2),2,NUMB(IOPT(79)))
C
      CALL READ(NODE,FLUX,IJG)
      LTH        = 0
      MATNP      = 0
      IRP        = 0
      DO 500  I  = 1,NR
      IR         = IABS(IRR(I))
      IF(IR.EQ.IRP)     GO TO 450
      IRP        = IR
      MATN       = IABS(MAR(IR))
      IF(MATN.EQ.MATNP) GO TO 200
      MATNP      = MATN
C
      FILENM(1)  = 'MACR'
      FILENM(2)  = 'OWRK'
      NODE(1)    = MATNM(1,MATN)
      NODE(2)    = MATNM(2,MATN)
      CALL PACK(NODE(2),1,LETF)
      CALL PACK(NODE(2),4,NUMB(2))
      ISW        = 0
      JSW        = 0
      CALL SEARCH(NODE,LTH,ISW)
      IF(ISW.EQ.1) THEN
                   CALL PACK(NODE(2),1,LETA)
                   CALL SEARCH(NODE,LTH,JSW)
                   IF(JSW.EQ.1) THEN
                                WRITE(NOUT1,150) NODE
                                STOP
                                ENDIF
                   ENDIF
C
  150 FORMAT(' *** ERROR STOP BY MISSING MEMBER ',2A4,' IN MACROWRK '
     & ,'FILE ENCOUNTERED IN THERMAL SOURCE STEP')
C
      CALL READ(NODE,AA,LTH)
  200 K        = 0
      DO 400 N = 1,NEF
      IJG      = NRR*(N-1) + IR
      MXDOWN   = N   + II(K+2) - 1
      IF(MXDOWN.LE.NEF) GO TO 350
      K1       = NEF -  N  +  2
      DO 300 L = K1 , II(K+2)
      ND       = L   -  K1 +  1
      IJGD     = NR*(ND-1) +  I
      S(IJGD)  = S(IJGD)   + FLUX(IJG)*AA(K+L+10)/VOLR(IR)
  300 CONTINUE
  350 K        = K  +  10  + II(K+2)
  400 CONTINUE
      GO TO 500
C
  450 DO 480 N = 1,NET
      IJGD     = NR*(N-1) + I
      S(IJGD)  = S(IJGD-1)
  480 CONTINUE
  500 CONTINUE
C
      WRITE(NSOUC) (S(K),K=1,IJGT)
      REWIND NSOUC
C
      IF(ITYPE.EQ.0) GO TO 600
C
C *** SET INITIAL FLUX FOR FIXED SOURCE PROBLEM
C
      FILENM(1) = 'MACR'
      FILENM(2) = 'OWRK'
      NODE(1)   = 'CONT'
      NODE(2)   = 'T002'
      CALL READ(NODE,AA,NET+1)
      ICO       = 0
      DO 550  N = 1,NET
      DO 550  I = 1,NR
      ICO       = ICO + 1
      S(ICO)    = AA(N+1)
  550 CONTINUE
      REWIND NFIN
      WRITE(NFIN) (S(I),I=1,ICO)
      REWIND NFIN
C
C **  END OF PROCESS
C
  600 CONTINUE
      RETURN
      END

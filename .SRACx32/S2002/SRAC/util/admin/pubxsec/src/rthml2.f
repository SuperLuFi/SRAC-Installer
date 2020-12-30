C-----------------------------------------------------------------------
C     SUBROUTINE TO READ PUBLIC THERMAL LIBRARY DATA (Kzzmc000-Uzzmc000)
      SUBROUTINE RTHML2(MEMBER,TVEC,UPSC,TTOT,TMRX,
     &                  NTTEMP, ITEMP, IPDS, MSGCLS)
C-----------------------------------------------------------------------
C
      INCLUDE 'INCMAX'
      CHARACTER*8     MEMBER
      CHARACTER*8     SUBNAM
C
      COMMON  /UNITIO/ NIN1, NOUT1, LOUT
      COMMON  /THMLLB/ NGT,WT(MXNGT),ET(MXNGT+1),INTT(8)
      COMMON  /WORK  / WORK(MAXWK)
      DIMENSION        IWORK(MAXWK)
      EQUIVALENCE (IWORK(1),WORK(1))
      DIMENSION  TVEC(MXNGT,11),UPSC(MXNGT),TTOT(MXNGT),
     &           TMRX(MXNGT,MXNGT)
C-----------------------------------------------------------------------
      SUBNAM = 'RTHML2  '
C****************************************
C  READ MEMBER Kzzmc000(P0)-Uzzmc000(P5)
C****************************************
      CALL PONPON('SRCH',SUBNAM,IPDS,MEMBER,LENG,WORK,MSGCLS)
      IF(LENG.GT.MAXWK) THEN
        WRITE(NOUT1,8000) SUBNAM, MEMBER, LENG, MAXWK
        STOP 777
      ENDIF
      LENG = 0
      CALL PONPON('READ',SUBNAM,IPDS,MEMBER,LENG,WORK,MSGCLS)
C
C****************************************
C  SET P0 VECTOR XS IN ARRAY TVEC 
C****************************************
      IF(MEMBER(1:1).NE.'K') GOTO 1000
      LENGW = NGT*(NGT+4)*NTTEMP
C-- SCATTERING MATRIX
      IF(LENG.EQ.LENGW) THEN
        IP = NGT*(NGT+4)*(ITEMP-1)
        DO 100 IG=1, NGT
          DO 100 IGG=1, NGT
            IP = IP + 1
            TMRX(IG,IGG) = WORK(IP)
  100   CONTINUE
      ELSE
        IP = 4*NGT*(ITEMP-1)
      ENDIF
C-- UP-SCATTERING
      DO 110 IG=1,NGT
        IP = IP + 1
        UPSC(IG) = WORK(IP)
  110 CONTINUE
C-- CAPTURE
      DO 120 IG=1,NGT
        IP = IP + 1
        TVEC(IG,1) = WORK(IP)
  120 CONTINUE
C-- TOTAL
      DO 130 IG=1,NGT
        IP = IP + 1
        TTOT(IG) = WORK(IP)
  130 CONTINUE
C-- FISSION
      DO 140 IG=1,NGT
        IP = IP + 1
        TVEC(IG,2) = WORK(IP)
  140 CONTINUE
      GOTO 9999
C
C**************************
C  SET HIGHER ORDER PL XS 
C  (FOR FUTURE USE)
C**************************
 1000 CONTINUE
C-- SCATTERING MATRIX
      IP = NGT*(NGT+4)*(ITEMP-1)
      DO 200 IG=1, NGT
        DO 200 IGG=1, NGT
          IP = IP + 1
          TMRX(IG,IGG) = WORK(IP)
  200 CONTINUE
C-- UP-SCATTERING
      DO 210 IG=1,NGT
        IP = IP + 1
        UPSC(IG) = WORK(IP)
  210 CONTINUE
C-- CAPTURE (SKIP DUMMY ZERO VALUE)
      IP = IP + NGT
C-- TOTAL
      DO 220 IG=1,NGT
        IP = IP + 1
        TTOT(IG) = WORK(IP)
  220 CONTINUE
C-- FISSION (SKIP DUMMY ZERO VALUE)
      IP = IP + NGT
C-----------------------------------------------------------------------
 8000 FORMAT(//1H ,'<<<  ERROR STOP (',A,')  >>>',/1X,
     &'DIMENSION SIZE OF WORK ARRAY IS TOO SMALL FOR MEMBER ',A,/1X,
     &'REQUIRED SIZE :',I8,'  CURRENT SIZE :',I8,/1X,
     &'CHANGE PARAMETER VALUE(MAXWK) IN INCLUDE FILE' )
C
 9999 RETURN
      END

C-----------------------------------------------------------------------
C     SUBROUTINE TO READ PUBLIC THERMAL LIBRARY DATA
      SUBROUTINE RTHML(MEMNAM,IFIG)
C     MEMNAM : INPUT MEMBER NAME = Xzzmc00t
C     IFIG   = 0  NOT PRINT XS TABLE ON LOUT-TH DEVICE
C            = 1  PRINT XS TABLE (FOR BAR GRAPH)
C            =-1  PRINT XS TABLE (FOR HISTGRAM )
C-----------------------------------------------------------------------
C
      INCLUDE 'INCMAX'
      CHARACTER*8     MEMBER, MEMNAM
      CHARACTER*8     SUBNAM
      CHARACTER*1     TTAG
C
      COMMON  /UNITIO/ NIN1, NOUT1, LOUT
      COMMON  /THMLLB/ NGT,WT(MXNGT),ET(MXNGT+1),INTT(8),
     &                 TTEMP(12), TSIG0(8), XNU, LENGT, LSCAT,
     &                 TVEC(MXNGT,11), UPSC(MXNGT,6), TTOT(MXNGT,6),
     &                 TMRX(MXNGT,MXNGT,6)
      COMMON  /WORK  / WORK(MAXWK)
      DIMENSION        IWORK(MAXWK)
      EQUIVALENCE (IWORK(1),WORK(1))
C--- <<THMLLB>>
C    NGT      : NUMBER OF THERMAL ENERGY GROUPS IN PTHERMAL(THERMAL1)
C    WT       : WEIGHTED LETHARGY WIDTH (THERMALt)
C    ET       : ENERGY BOUNDARY (THERMAL1)
C    INTT-LENGT : DATA IN CONTROL MEMBER (Czzmc000)
C    LSCAT    : MAX ORDER OF PL XS
C               LSCAT =-1 (K-MEMBER WITHOUT SCATTERING MATRIX)
C                     = 0 (K-MEMBER WITH K-MATRIX :INTT(2)=2
C                     :
C                     = 5 (K,P,Q,S,T,U-MEMBERS)   :INTT(2)=126
C    TVEC(g,1): =CAPT Capture
C    TVEC(g,2): =FISS Fission
C    TVEC(g,3): =FNU  Fission neutron yield / fission (Nyu-value)
C                     (read from Zzzmc000)
C    TVEC(g,4): =FSPC (=0.0 always)
C    TVEC(g,5): =0.0 (not defined)
C    TVEC(g,6): =WT   Lethargy width (=WT in THERMALt)
C    TVEC(g,7): =0.0 (not defined)
C    TVEC(g,8): =0.0 always
C    TVEC(g,9): =0.0 always
C    TVEC(g,10):=SUM on g' {TMRX(g,g',1(L=0)}
C    TVEC(g,11):=SUM on g' {TMRX(g,g',2(L=1)}
C    UPSC(g,L+1): Up-scattering cross section of the L-th order
C                 read from K/P/Q/S/T/U-matrix
C    TTOT(g,L+1): Total cross section of the L-th order
C                 read from K/P/Q/S/T/U-matrix
C    TMRX(g,g',L+1): scattering matrix(g->g') of the L-th order
C                 read from K/P/Q/S/T/U-matrix
C-----------------------------------------------------------------------
      SUBNAM = 'RTHML   '
      IPDS   = 2
      MSGCLS = 1
      IPRN   = 1
C***************************
C  CHECK TEMPERATURE POINT 
C***************************
      TTAG = MEMNAM(8:8)
      IF(TTAG.EQ.'1') THEN
        ITEMP = 1
      ELSEIF(TTAG.EQ.'2') THEN
        ITEMP = 2
      ELSEIF(TTAG.EQ.'3') THEN
        ITEMP = 3
      ELSEIF(TTAG.EQ.'4') THEN
        ITEMP = 4
      ELSEIF(TTAG.EQ.'5') THEN
        ITEMP = 5
      ELSEIF(TTAG.EQ.'6') THEN
        ITEMP = 6
      ELSEIF(TTAG.EQ.'7') THEN
        ITEMP = 7
      ELSEIF(TTAG.EQ.'8') THEN
        ITEMP = 8
      ELSEIF(TTAG.EQ.'9') THEN
        ITEMP = 9
      ELSEIF(TTAG.EQ.'A') THEN
        ITEMP = 10
      ELSEIF(TTAG.EQ.'B') THEN
        ITEMP = 11
      ELSEIF(TTAG.EQ.'C') THEN
        ITEMP = 12
      ELSE
        WRITE(NOUT1,8000) SUBNAM, MEMBER
        STOP 888
      ENDIF
C***************************
C  READ MEMBER THERMALt
C***************************
      MEMBER = 'THERMAL1'
      MEMBER(8:8) = TTAG
      CALL PONPON('SRCH',SUBNAM,IPDS,MEMBER,LENG,WORK,MSGCLS)
      IF(LENG.GT.MAXWK) THEN
        WRITE(NOUT1,8100) SUBNAM, MEMBER, LENG, MAXWK
        STOP 777
      ENDIF
C
      LENG = 0
      CALL PONPON('READ',SUBNAM,IPDS,MEMBER,LENG,WORK,MSGCLS)
      NGT  = IWORK(1)
      IP = 1
      DO 100 I = 1,NGT
        WT(I)  = WORK(I+IP)
  100 CONTINUE
      IP = IP + NGT
      DO 110 I = 1,NGT+1
        ET(I) = WORK(I+IP)
  110 CONTINUE
C
C***************************
C  READ MEMBER Czzmc000
C***************************
      MEMBER = 'C'//MEMNAM(2:5)//'000'
      CALL PONPON('SRCH',SUBNAM,IPDS,MEMBER,LENG,WORK,MSGCLS)
      IF(LENG.GT.MAXWK) THEN
        WRITE(NOUT1,8100) SUBNAM, MEMBER, LENG, MAXWK
        STOP 777
      ENDIF
      LENG = 0
      CALL PONPON('READ',SUBNAM,IPDS,MEMBER,LENG,WORK,MSGCLS)
C
      DO 200 I=1,8
        INTT(I)=IWORK(I)
  200 CONTINUE
      IP = 8
      DO 210 I=1,12
        TTEMP(I) = WORK(I+IP)
  210 CONTINUE
      IP = IP+12
      DO 220 I=1,8
        TSIG0(I) = WORK(I+IP)
  220 CONTINUE
      IP = IP+8
      XNU   = WORK(IP+1)
      LENGT = WORK(IP+2)
      IF(INTT(2).EQ.0) THEN 
        LSCAT = -1
      ELSEIF(INTT(2).EQ.2) THEN 
        LSCAT = 0
      ELSEIF(INTT(2).EQ.6) THEN 
        LSCAT = 1
      ELSEIF(INTT(2).EQ.14) THEN 
        LSCAT = 2
      ELSEIF(INTT(2).EQ.30) THEN 
        LSCAT = 3
      ELSEIF(INTT(2).EQ.62) THEN 
        LSCAT = 4
      ELSEIF(INTT(2).EQ.126) THEN 
        LSCAT = 5
      ELSE
        WRITE(NOUT1,8200) SUBNAM, MEMBER, INTT(2)
        STOP 999
      ENDIF
C*****************************
C  PRINT CONTENTS OF Czzmc000
C*****************************
      IF(IPRN.EQ.0) GOTO 1000
      MEMBER = 'C'//MEMNAM(2:5)//'000'
      WRITE(NOUT1,*)
      WRITE(NOUT1,*) ' MEMBER NAME : PTHERMAL/',MEMBER
      WRITE(NOUT1,*) ' INT(1-8)            = ', (INTT(I),I=1,8)
      WRITE(NOUT1,*) ' TTEMP(1-12)         = ', (TTEMP(I),I=1,12)
      WRITE(NOUT1,*) ' TSIG0(1-8)          = ', (TSIG0(I),I=1,8)
      WRITE(NOUT1,*) ' XNU                 = ', XNU
      WRITE(NOUT1,*) ' LENGTH              = ', LENGTH
      WRITE(NOUT1,*)
C-----------------------------------------------------------------------
 1000 CONTINUE
      NTTEMP = INTT(7)
      IF(ITEMP.GT.NTTEMP) THEN
        WRITE(NOUT1,8300) SUBNAM, MEMBER, NTTEMP, TTAG, ITEMP,
     &                    (TTEMP(I),I=1,NTTEMP)
        STOP 888
      ENDIF
C
C****************************************
C  READ MEMBER Kzzmc000(P0)-Uzzmc000(P5)
C****************************************
      MEMBER = 'K'//MEMNAM(2:5)//'000'
      LS=1
      CALL RTHML2(MEMBER,TVEC,UPSC(1,LS),TTOT(1,LS),
     &            TMRX(1,1,LS), NTTEMP, ITEMP, IPDS, MSGCLS)
C----- FOR FUTURE USE
      IF(LSCAT.GE.1) THEN
        MEMBER = 'P'//MEMNAM(2:5)//'000'
        LS=2
        CALL RTHML2(MEMBER,TVEC,UPSC(1,LS),TTOT(1,LS),
     &            TMRX(1,1,LS), NTTEMP, ITEMP, IPDS, MSGCLS)
      ENDIF
C
      IF(LSCAT.GE.2) THEN
        MEMBER = 'Q'//MEMNAM(2:5)//'000'
        LS=3
        CALL RTHML2(MEMBER,TVEC,UPSC(1,LS),TTOT(1,LS),
     &            TMRX(1,1,LS), NTTEMP, ITEMP, IPDS, MSGCLS)
      ENDIF
C
      IF(LSCAT.GE.3) THEN
        MEMBER = 'S'//MEMNAM(2:5)//'000'
        LS=4
        CALL RTHML2(MEMBER,TVEC,UPSC(1,LS),TTOT(1,LS),
     &            TMRX(1,1,LS), NTTEMP, ITEMP, IPDS, MSGCLS)
      ENDIF
C
      IF(LSCAT.GE.4) THEN
        MEMBER = 'T'//MEMNAM(2:5)//'000'
        LS=5
        CALL RTHML2(MEMBER,TVEC,UPSC(1,LS),TTOT(1,LS),
     &            TMRX(1,1,LS), NTTEMP, ITEMP, IPDS, MSGCLS)
      ENDIF
C
      IF(LSCAT.GE.5) THEN
        MEMBER = 'U'//MEMNAM(2:5)//'000'
        LS=6
        CALL RTHML2(MEMBER,TVEC,UPSC(1,LS),TTOT(1,LS),
     &            TMRX(1,1,LS), NTTEMP, ITEMP, IPDS, MSGCLS)
      ENDIF
C
C***************************
C  SET REMAINING VECTOR XS
C***************************
C TVEC(g,3): =FNU  Fission neutron yield / fission (Nyu-value)
C (FNU(g,t),g=1,NGT),t=1,NTTEMP) in a MEMBER Zzzmc000
      IF(INTT(1).EQ.0) GOTO  2000
      MEMBER = 'Z'//MEMNAM(2:5)//'000'
      CALL PONPON('SRCH',SUBNAM,IPDS,MEMBER,LENG,WORK,MSGCLS)
      IF(LENG.GT.MAXWK) THEN
        WRITE(NOUT1,8100) SUBNAM, MEMBER, LENG, MAXWK
        STOP 777
      ENDIF
      LENG = 0
      CALL PONPON('READ',SUBNAM,IPDS,MEMBER,LENG,WORK,MSGCLS)
      IP = NGT*(ITEMP-1)
      DO 300 IG=1,NGT
        IP = IP + 1
        TVEC(IG,3) = WORK(IP)
  300 CONTINUE
C
 2000 CONTINUE
C TVEC(g,6): =WT   Lethargy width (=WT in THERMALt)
C USE WEIGHT IN a MEMBER THERMALt(Valid?)
      DO 400 IG=1,NGT
        TVEC(IG,6) = WT(IG)
  400 CONTINUE
C
C TVEC(g,10):=SUM on g' {TMRX(g,g',1(L=0)}
C TVEC(g,11):=SUM on g' {TMRX(g,g',2(L=1)}
      DO 500 IG=1,NGT
        DO 500 IGG=1,NGT
          TVEC(IG,10) = TVEC(IG,10) + TMRX(IG,IGG,1)
          TVEC(IG,11) = TVEC(IG,11) + TMRX(IG,IGG,2)
  500 CONTINUE
C
C***************************
C  PRINT VECTOR XS
C***************************
      IF (IFIG.EQ.0) GOTO 9999
C
      MEMBER = MEMNAM
      WRITE(LOUT,*)
      WRITE(LOUT,*) '========(MEMBER: PTHERMAL/',MEMBER, ')========'
      WRITE(LOUT,7100)
      DO 600 IG=1,NGT
        WRITE(LOUT,7200) IG, ET(IG), (TVEC(IG,IX),IX=1,11)
        IF(IFIG.LT.0) THEN
          WRITE(LOUT,7200) IG, ET(IG+1), (TVEC(IG,IX),IX=1,11)
        ENDIF
  600 CONTINUE
      WRITE(LOUT,*)
C-----------------------------------------------------------------------
 7100 FORMAT(1X,'GRP',2X,'ENERGY-BOUND',2X,'  CAPTURE   ',2X,
     &                   '  FISSION   ',2X,'  NYU-VALUE ',2X,
     &                   '  X-VALUE   ',2X,'  TRANSPORT ',2X,
     &                   'WT-LETHARGY ',2X,' ELASTIC(P0)',2X,
     &                   ' INELASTIC  ',2X,'    N2N     ',2X,
     &                   ' SUM-P0SCAT ',2X,' SUM-P1SCAT '   )
 7200 FORMAT(1X,I3,12(2X,1PE12.5))
C
 8000 FORMAT(//1H ,'<<<  ERROR STOP (',A,')  >>>',/1X,
     &'THE LAST TAG FOR TEMPERATURE INDEX OF INPUT MEMBER NAME (',A,
     &') IS INVALID')
 8100 FORMAT(//1H ,'<<<  ERROR STOP (',A,')  >>>',/1X,
     &'DIMENSION SIZE OF WORK ARRAY IS TOO SMALL FOR MEMBER ',A,/1X,
     &'REQUIRED SIZE :',I8,'  CURRENT SIZE :',I8,/1X,
     &'CHANGE PARAMETER VALUE(MAXWK) IN INCLUDE FILE' )
 8200 FORMAT(//1H ,'<<<  ERROR STOP (',A,')  >>>',/1X,
     &'CONTROL INTEGER(INT(2) IN A MEMBER PTHERMAL/',A,' IS INVALID',/
     &1X,'INT(2)=',I3)
 8300 FORMAT(//1H ,'<<<  ERROR STOP (',A,')  >>>',/1X,
     &'TEMPERATURE OF INPUT MEMBER IS OUT OF TABULATION',/1X,
     &'INPUT MEMBER = ',A,/1X,
     &'NUMBER OF TABULATED TEMPERATURES IN PTHERMAL = ',I2,/1X,
     &'TEMPERATURE POINT SPECIFIED BY T-TAG(',A,') = ',I2,/1X,
     &'TABULATED TEMPERATURES(K) :',/1X,12F10.3 )
C
 9999 RETURN
      END

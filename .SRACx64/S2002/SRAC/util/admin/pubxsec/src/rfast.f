C-----------------------------------------------------------------------
C     SUBROUTINE TO READ PUBLIC FAST LIBRARY DATA
      SUBROUTINE RFAST(MEMNAM,IFIG)
C     MEMNAM : INPUT MEMBER NAME = Xzzmc00t
C     IFIG   = 0  NOT PRINT XS TABLE ON LOUT-TH DEVICE
C            = 1  PRINT XS TABLE (FOR BAR GRAPH)
C            =-1  PRINT XS TABLE (FOR HISTGRAM )
C-----------------------------------------------------------------------
C
      INCLUDE 'INCMAX'
      CHARACTER*8     MEMBER, MEMNAM
      CHARACTER*8     SUBNAM
C
      COMMON  /UNITIO/ NIN1, NOUT1, LOUT
      COMMON  /FASTLB/ NGF,WF(MXNGF),EF(MXNGF+1),ICAP,IFISS,IRP,LTOT,
     &                 LTH(4),LA(4),LD(4),IFS,IFTR,IFC,IFF,IFE,IFER,
     &                 NGMIN, NGMAX, NSIG, NFTEMP, AMASS, SIGP, SIGC0,
     &                 FTEMP(4),FSIG0(8),IPL,
     &                 FVEC(MXNGF,11), FMRX(MXLTH,4)
      COMMON  /WORK  / WORK(MAXWK)
      DIMENSION        IWORK(MAXWK)
      EQUIVALENCE (IWORK(1),WORK(1))
C--- <<FASTLB>>
C    NGF      : NUMBER OF FAST ENERGY GROUPS IN PFAST(FASTLIB)
C    WF       : WEIGHTED LETHARGY WIDTH (FASTLIB)
C    EF       : ENERGY BOUNDARY (FASTLIB)
C    ICAP-IPL : DATA IN CONTROL MEMBER (Czzm0000)
C    Vector XSs ---------------
C    FVEC(g,1): =CAPT Capture
C    FVEC(g,2): =FISS Fission
C    FVEC(g,3): =FNU  Fission neutron yield / fission (Nyu-value)
C    FVEC(g,4): =FSPC Fission neutron spectrum (X-value)
C    FVEC(g,5): =TR   Transport(?)
C    FVEC(g,6): =WT   Lethargy width
C    FVEC(g,7): =ELAS Total elastic cross-section
C    FVEC(g,8): =SUM on g' {FMRX(1)(g->g')}
C    FVEC(g,9): =SUM on g' {FMRX(2)(g->g')}
C    FVEC(g,10):=SUM on g' {FMRX(3)(g->g')}
C    FVEC(g,11):=SUM on g' {FMRX(4)(g->g')}
C    Matrix XSs ---------------
C    FMRX(i,1): =N-N  Inelastic matrix of length i=LTH(1)
C    FMRX(i,2): =N2N  (n,2n) matrix of length i=LTH(2)
C    FMRX(i,3): =ELP0 Elastic P0 scattering matrix of length i=LTH(3)
C    FMRX(i,4): =ELP1 Elastic P1 scattering matrix of length i=LTH(4)
C-----------------------------------------------------------------------
      SUBNAM = 'RFAST   '
      IPDS   = 1
      MSGCLS = 1
      IPRN   = 1
C***************************
C  READ MEMBER FASTLIB
C***************************
      MEMBER = 'FASTLIB '
      CALL PONPON('SRCH',SUBNAM,IPDS,MEMBER,LENG,WORK,MSGCLS)
      IF(LENG.GT.MAXWK) THEN
        WRITE(NOUT1,8000) SUBNAM, MEMBER, LENG, MAXWK
        STOP 777
      ENDIF
C
      LENG = 0
      CALL PONPON('READ',SUBNAM,IPDS,MEMBER,LENG,WORK,MSGCLS)
C
      NGF  = IWORK(1)
      NGF1 = IWORK(2)
      NGF2 = IWORK(3)
      NGF3 = IWORK(4)
C
      IP = 4
      DO 100 I = 1,NGF
        WF(I)  = WORK(I+IP)
  100 CONTINUE
      IP = IP + NGF
      DO 110 I = 1,NGF+1
        EF(I) = WORK(I+IP)
  110 CONTINUE
C
C***************************
C  READ MEMBER Czzm0000
C***************************
      MEMBER = 'C'//MEMNAM(2:4)//'0000'
      CALL PONPON('SRCH',SUBNAM,IPDS,MEMBER,LENG,WORK,MSGCLS)
      IF(LENG.GT.MAXWK) THEN
        WRITE(NOUT1,8000) SUBNAM, MEMBER, LENG, MAXWK
        STOP 777
      ENDIF
      LENG = 0
      CALL PONPON('READ',SUBNAM,IPDS,MEMBER,LENG,WORK,MSGCLS)
C
      ICAP = IWORK(1)
      IFISS= IWORK(2)
      IRP  = IWORK(3)
      LTOT = IWORK(4)
      IP = 4
      DO 200 I=1,4
        LTH(I) = IWORK(I+IP)
        LA(I)  = IWORK(I+IP+4)
        LD(I)  = IWORK(I+IP+8)
  200 CONTINUE
      IP = IP + 12
      IFS  = IWORK(1+IP)
      IFTR = IWORK(2+IP)
      IFC  = IWORK(3+IP)
      IFF  = IWORK(4+IP)
      IFE  = IWORK(5+IP)
      IFER = IWORK(6+IP)
      NGMIN= IWORK(7+IP)
      NGMAX= IWORK(8+IP)
      NSIG  =IWORK(9+IP)
      NFTEMP=IWORK(10+IP)
      AMASS = WORK(11+IP)
      SIGP  = WORK(12+IP)
      SIGC0 = WORK(13+IP)
      IP = IP + 13
      DO 210 I=1,4
        FTEMP(I) = WORK(I+IP)
  210 CONTINUE
      IP = IP + 4
      DO 220 I=1,8
        FSIG0(I) = WORK(I+IP)
  220 CONTINUE
      IP = IP + 8
      IPL = IWORK(1+IP)
      IP = IP + 1
C
C*****************************
C  PRINT CONTENTS OF Czzm0000
C*****************************
      IF(IPRN.EQ.0) GOTO 1000
      MEMBER = 'C'//MEMNAM(2:4)//'0000'
      WRITE(NOUT1,*)
      WRITE(NOUT1,*) ' MEMBER NAME : PFAST/',MEMBER
      WRITE(NOUT1,*) ' ICAP,IFISS,IRP,LTOT = ', ICAP,IFISS,IRP,LTOT
      WRITE(NOUT1,*) ' LTH(1-4)            = ', (LTH(I),I=1,4)
      WRITE(NOUT1,*) ' LA (1-4)            = ', (LA(I) ,I=1,4)
      WRITE(NOUT1,*) ' LD (1-4)            = ', (LD(I) ,I=1,4)
      WRITE(NOUT1,*) ' IFS,IFTR,IFC,IFF,IFE,IFER =',
     &                 IFS,IFTR,IFC,IFF,IFE,IFER
      WRITE(NOUT1,*) ' NGMIN, NGMAX, NFTEMP= ', NGMIN, NGMAX, NFTEMP
      WRITE(NOUT1,*) ' AMASS, SIGP, SIGC0  = ', AMASS, SIGP, SIGC0
      WRITE(NOUT1,*) ' FTEMP(1-4)          = ', (FTEMP(I),I=1,4)
      WRITE(NOUT1,*) ' FSIG0(1-8)          = ', (FSIG0(I),I=1,8)
      WRITE(NOUT1,*) ' IPL                 = ', IPL
      WRITE(NOUT1,*)
C-----------------------------------------------------------------------
 1000 CONTINUE
C
C***************************
C  READ MEMBER Mzzm0000
C***************************
      MEMBER = 'M'//MEMNAM(2:4)//'0000'
      CALL PONPON('SRCH',SUBNAM,IPDS,MEMBER,LENG,WORK,MSGCLS)
      IF(LENG.GT.MAXWK) THEN
        WRITE(NOUT1,8000) SUBNAM, MEMBER, LENG, MAXWK
        STOP 777
      ENDIF
      LENG = 0
      CALL PONPON('READ',SUBNAM,IPDS,MEMBER,LENG,WORK,MSGCLS)
C
      IP = 0
      IF(ICAP.EQ.1) THEN
        DO 300 I=1,NGF
          IP = IP + 1
          FVEC(I, 1) = WORK(IP)
  300   CONTINUE
      ENDIF
C
      IF(IFISS.EQ.1) THEN
        DO 310 I=1,NGF
          IP = IP + 1
          FVEC(I, 2) = WORK(IP)
  310   CONTINUE
      ENDIF
C
      IF(IFISS.EQ.1) THEN
        DO 320 I=1,NGF
          IP = IP + 1
          FVEC(I, 3) = WORK(IP)
  320   CONTINUE
      ENDIF
C
      IF(IFISS.EQ.1) THEN
        DO 330 I=1,NGF
          IP = IP + 1
          FVEC(I, 4) = WORK(IP)
  330   CONTINUE
      ENDIF
C
      DO 340 I=1,NGF
        IP = IP + 1
        FVEC(I, 5) = WORK(IP)
  340 CONTINUE
C
      DO 350 I=1,NGF
        IP = IP + 1
        FVEC(I, 6) = WORK(IP)
  350 CONTINUE
C
      DO 360 I=1,NGF
        IP = IP + 1
        FVEC(I, 7) = WORK(IP)
  360 CONTINUE
C
C---- MATRIX DATA (ISC=1,4)
      DO 400 ISC=1,4
        IF(LTH(ISC).NE.LA(ISC)*(LD(ISC)+1)) THEN
          WRITE(NOUT1, 8100) SUBNAM, ISC, LTH(ISC), LA(ISC), LD(ISC)
          STOP 999
        ENDIF
        IF(LTH(ISC).GT.0) THEN
          DO 410 I=1,LTH(ISC)
            IP = IP + 1
            FMRX(I,ISC) = WORK(IP)
  410     CONTINUE
          IPP = 0
          DO 420 IG=1,LA(ISC)
            DO 420 IGG=IG,IG+LD(ISC)
              IPP=IPP+1
              FVEC(IG, 7+ISC) = FVEC(IG,7+ISC) + FMRX(IPP,ISC)
  420     CONTINUE
        ENDIF
  400 CONTINUE
C
C***************************
C  PRINT VECTOR XS 
C***************************
      IF (IFIG.EQ.0) GOTO 9999
C
      MEMBER = MEMNAM
      WRITE(LOUT,*)
      WRITE(LOUT,*) '========(MEMBER: PFAST/',MEMBER, ')========'
      WRITE(LOUT,7100)
      DO 500 IG=1,NGF
        WRITE(LOUT,7200) IG, EF(IG), (FVEC(IG,IX),IX=1,11)
        IF(IFIG.LT.0) THEN
          WRITE(LOUT,7200) IG, EF(IG+1), (FVEC(IG,IX),IX=1,11)
        ENDIF
  500 CONTINUE
      WRITE(LOUT,*)
C
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
     &'DIMENSION SIZE OF WORK ARRAY IS TOO SMALL FOR MEMBER ',A,/1X,
     &'REQUIRED SIZE :',I8,'  CURRENT SIZE :',I8,/1X,
     &'CHANGE PARAMETER VALUE(MAXWK) IN INCLUDE FILE' )
 8100 FORMAT(//1H ,'<<<  ERROR STOP (',A,')  >>>',/1X,
     &'CONTROL INTEGERS FOR THE ',I1,'-TH SCATTERING MATRIX ARE ',
     &'NOT CONSISTENT',/1X,
     &'LTH, LD, LA =', I5,2X,I5,2X,I5)
C
 9999 RETURN
      END

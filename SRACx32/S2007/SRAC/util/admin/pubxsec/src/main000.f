C-----------------------------------------------------------------------
C     MAIN PROGRAM TO MAKE A PLOT DATA OF INFINITE DILUTION XS
C     IN PUBLIC LIBRARIES OF SRAC (PFAST AND PTHERMAL)
C     PROGRAMED BY KEISUKE OKUMURA (10 Oct. 2001)
C-----------------------------------------------------------------------
C
      INCLUDE 'INCMAX'
      CHARACTER*8     MEMNAM
      CHARACTER*30    PREFIX
C
      COMMON /WORK  / WORK(MAXWK)
C
C-- FOR PONPON (PDS ACCESS PACKAGE)
      INCLUDE 'INCPDS'
      CHARACTER        PDSDIR*120, MOACS*4, MOSTY*4
      COMMON  /USPDSC/ PDSDIR(MXPDS), MOACS(MXPDS), MOSTY(MXPDS)
      COMMON  /USPDSI/ IOMSG, IOVPDS, IOTMP1, IOTMP2
      COMMON  /UNITIO/ NIN1, NOUT1, LOUT, IOMSG2
C     PDSDIR(1) : FOR PREFIX=PFAST
C     PDSDIR(2) : FOR PREFIX=PTHERMAL
C
C-----------------------------------------------------------------------
C-- DATA FOR THIS PROGRAM
      DIMENSION  IOPT(2)
      COMMON  /FASTLB/ NGF,WF(MXNGF),EF(MXNGF+1),ICAP,IFISS,IRP,LTOT,
     &                 LTH(4),LA(4),LD(4),IFS,IFTR,IFC,IFF,IFE,IFER,
     &                 NGMIN, NGMAX, NSIG, NFTEMP, AMASS, SIGP, SIGC0,
     &                 FTEMP(4),FSIG0(8),IPL,
     &                 FVEC(MXNGF,11), FMRX(MXLTH,4)
      COMMON  /THMLLB/ NGT,WT(MXNGT),ET(MXNGT+1),INTT(8),
     &                 TTEMP(12), TSIG0(8), XNU, LENGT, LSCAT,
     &                 TVEC(MXNGT,11), UPSC(MXNGT,6), TTOT(MXNGT,6),
     &                 TMRX(MXNGT,MXNGT,6)
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
C
C=======================================================================
C *** IO DEVICE CHANGABLE ONLY IN THIS ROUTINE
C
C     NIN1  : STANDARD INPUT (READ)
C     NOUT1 : STANDARD OUTPUT (WRITE)
C     LOUT  : LIBRARY INFORMATION TABLE (WRITE)
C     IOVPDS: FOR PONPON
C     IOMSG : MESSAGE PRINT FROM PONPON (=IOMSG2)
C     IOTMP1: FOR PONPON (NOT USED IN THIS PROGRAM)
C     IOTMP2: FOR PONPON (NOT USED IN THIS PROGRAM)
      NIN1   = 5
      NOUT1  = 6
      LOUT   = 10
C--- FOR PONPON & PONTXT(PDS ACCESS)
      IOMSG  = 6
      IOMSG2 = IOMSG
      IOVPDS = 41
      IOTMP1 = 42
      IOTMP2 = 43
C--- FILE ACCESS FOR ALL PDS
      MOACS(1) = 'FILE'
      MOACS(2) = 'FILE'
C
C *** START OF PROCESS
C
cksk  CALL USYSTM
      CALL UIOSET
      CALL LOGOPR
C
C***************************
C  SET PDS DIRECTORY NAME 
C***************************
      PREFIX = 'PFAST'
      CALL GETDIR (PREFIX,PDSDIR(1),LENDIR)
      PREFIX = 'PTHERMAL'
      CALL GETDIR (PREFIX,PDSDIR(2),LENDIR)
C
C***************************
C  READ STANDARD INPUT DATA 
C***************************
C--- READ CONTROL OPTION IOPT(10) [NORMAL FREE FORMAT(*)]
C IOPT( 1) = 0  PRINT ONLY CONTENTS OF CONTROL MEMBER(Czzmc000)
C               ON STANDARD OUTPUT(DEVICE=6)
C          = 1  PRINT CONTENTS OF CONTROL MEMBER(Czzmc000)
C               AND MAKE A PLOT TABLE (BAR GRAPH TYPE) ON DEVICE 10
C          =-1  PRINT CONTENTS OF CONTROL MEMBER(Czzmc000) 
C               AND MAKE A PLOT TABLE (HISTGRAM TYPE) ON DEVICE 10
C IOPT( 2)      ENERGY RANGE
C          = 1  PFAST
C          = 2  PTHERMAL
C          = 3  PFAST AND PTHERMAL
C          =-N  PFAST AND PTHERMAL CONCATINATED AFTER N-TH FAST GROUP
C
      READ(NIN1,*) (IOPT(I),I=1,2)
      IF(IOPT(1).NE.0.AND.IOPT(1).NE.1.AND.IOPT(1).NE.-1) THEN
        WRITE(NOUT1,8000) 1, IOPT(1)
        STOP 888
      ENDIF
      IF(IOPT(2).GE.4.OR.IOPT(2).EQ.0) THEN
        WRITE(NOUT1,8000) 2, IOPT(2)
        STOP 888
      ENDIF
      IF(IOPT(2).LT.0) THEN
        IF(IABS(IOPT(2)).LT.58.OR.IABS(IOPT(2)).GT.74) THEN
          WRITE(NOUT1,8000) 2, IOPT(2)
          STOP 888
        ENDIF
      ENDIF
C
C---- LOOP ON MEMBER (Xzzmc00t: t=1,2,3,...9,A,B,C)
      REWIND LOUT
 1000 CONTINUE
      READ(NIN1,'(A8)',END=9999) MEMNAM
      IF(MEMNAM.EQ.'        ') GOTO 9999
      WRITE(NOUT1,6000) MEMNAM
 6000 FORMAT(/,1X,'**********************************',/,
     &         1X,'*  INPUT MEMBER NAME : ',A8,'  *',/,
     &         1X,'**********************************',/)
C
C************************************************
C  ZERO CLEA OF COMMON DATA
C************************************************
C
      CALL ZEROCL
C************************************************
C  READ PUBLIC LIBRARY DATA AND SET IN EACH ARRAY 
C************************************************
      IFIG = IOPT(1)
      IF(IOPT(2).GT.3) IFIG=0
C
      IOPT(2)= IABS(IOPT(2))
      IF(IOPT(2).NE.2) THEN
        CALL RFAST(MEMNAM,IFIG)
      ENDIF
      IF(IOPT(2).NE.1) THEN
        CALL RTHML(MEMNAM,IFIG)
      ENDIF
C
C************************************************
C  CONCATINATE THERMAL XS DATA IF IOPT(2)<0
C************************************************
      IF(IOPT(2).GT.3) THEN
        IFIG = IOPT(1)
        CALL CONCAT(IFIG)
      ENDIF
C
C************************************************
C  MAKE A PLOT TABLE AVAILABLE ON EXCEL 
C************************************************
C 
C *** END OF PROCESS
      GOTO 1000
C
C-----------------------------------------------------------------------
 8000 FORMAT(//1H ,'<<<  ERROR STOP (MAIN)  >>>',/,1X,
     &'INPUT DATA FOR IOPT(',I1,') IS INVALID : ',I3)
C
 9999 WRITE(NOUT1,*)
      WRITE(NOUT1,'(A)') ' ========================== END OF MORE-'
     &               //'MOSRA(PUBXSEC)========================='
      STOP
      END

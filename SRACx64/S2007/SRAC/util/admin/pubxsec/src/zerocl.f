C-----------------------------------------------------------------------
C     ZERO CLEAR OF COMMON DATA
      SUBROUTINE ZEROCL
C-----------------------------------------------------------------------
      INCLUDE 'INCMAX'
      COMMON  /FASTLB/ NGF,WF(MXNGF),EF(MXNGF+1),ICAP,IFISS,IRP,LTOT,
     &                 LTH(4),LA(4),LD(4),IFS,IFTR,IFC,IFF,IFE,IFER,
     &                 NGMIN, NGMAX, NFTEMP, AMASS, SIGP, SIGC0,
     &                 FTEMP(4),FSIG0(8),IPL,
     &                 FVEC(MXNGF,11), FMRX(MXLTH,4)
      COMMON  /THMLLB/ NGT,WT(MXNGT),ET(MXNGT+1),INTT(8),
     &                 TTEMP(12), TSIG0(8), XNU, LENGT, LSCAT,
     &                 TVEC(MXNGT,11), UPSC(MXNGT,6), TTOT(MXNGT,6),
     &                 TMRX(MXNGT,MXNGT,6)
C-----------------------------------------------------------------------
CC   << COMMON FASTLB>>
      NGF   = 0
      DO 100 IG=1,MXNGF
        WF(IG) = 0.0
        EF(IG) = 0.0
  100 CONTINUE
      EF(MXNGF+1) = 0.0
      ICAP  = 0
      IFISS = 0
      IRP   = 0
      LTOT  = 0
      DO 110 I=1,4
        LTH(I)  = 0
        LA(I)   = 0
        LD(I)   = 0
        FTEMP(I)= 0.0
  110 CONTINUE
      IFS   = 0
      IFTR  = 0
      IFC   = 0
      IFF   = 0
      IFE   = 0
      IFER  = 0
      NGMIN = 0
      NGMAX = 0
      NFTEMP= 0
      AMASS = 0.0
      SIGP  = 0.0
      SIGC0 = 0.0
      DO 120 I=1,8
        FSIG0(I)= 0.0
  120 CONTINUE
      IPL = 0
      DO 130 IX=1,11
        DO 130 IG=1,MXNGF
          FVEC(IG,IX) = 0.0 
  130 CONTINUE
      DO 140 IX=1,4
        DO 140 L=1,MXLTH
          FMRX(L,IX) = 0.0
  140 CONTINUE
C
C-----------------------------------------------------------------------
C    << COMMON THMLLB>>
      NGT   = 0
      DO 200 IG=1,MXNGT
        WT(IG) = 0.0
        ET(IG) = 0.0
  200 CONTINUE
      ET(MXNGT+1) = 0.0
      DO 210 I=1,8
        INTT(I)  = 0
        TSIG0(I) = 0.0
  210 CONTINUE
      DO 220 I=1,12
        TTEMP(I) = 0.0
  220 CONTINUE
      XNU   = 0.0
      LENGT = 0
      LSCAT = 0
      DO 230 IX=1,11
        DO 230 IG=1,MXNGT
          TVEC(IG,IX) = 0.0
  230 CONTINUE
      DO 240 IX=1,6
        DO 240 IG=1,MXNGT
          UPSC(IG,IX) = 0.0
          TTOT(IG,IX) = 0.0
          DO 250 IGG=1,MXNGT
            TMRX(IG,IGG,IX) = 0.0
  250     CONTINUE
  240 CONTINUE
      RETURN
      END

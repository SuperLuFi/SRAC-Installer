C-----------------------------------------------------------------------
C     SUBROUTINE TO CONCATINATE FAST AND THERMAL DATA
      SUBROUTINE CONCAT(IFIG)
C-----------------------------------------------------------------------
      INCLUDE 'INCMAX'
      COMMON  /UNITIO/ NIN1, NOUT1, LOUT
      COMMON  /FASTLB/ NGF,WF(MXNGF),EF(MXNGF+1),ICAP,IFISS,IRP,LTOT,
     &                 LTH(4),LA(4),LD(4),IFS,IFTR,IFC,IFF,IFE,IFER,
     &                 NGMIN, NGMAX, NSIG, NFTEMP, AMASS, SIGP, SIGC0,
     &                 FTEMP(4),FSIG0(8),IPL,
     &                 FVEC(MXNGF,11), FMRX(MXLTH,4)
      COMMON  /THMLLB/ NGT,WT(MXNGT),ET(MXNGT+1),INTT(8),
     &                 TTEMP(12), TSIG0(8), XNU, LENGT, LSCAT,
     &                 TVEC(MXNGT,11), UPSC(MXNGT,6), TTOT(MXNGT,6),
     &                 TMRX(MXNGT,MXNGT,6)
C-----------------------------------------------------------------------
C
      WRITE(NOUT1,*) ' !!! CONCATINATION OPTION (IOPT(2)<0) IS NOT ',
     &               'AVAILABLE YET !!!'
      STOP 888
      RETURN
      END

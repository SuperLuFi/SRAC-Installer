C***********************************************************************
C
C  UIOUNT   : SET UNFORMATED(0) OR FORMATED(1)
C             FOR EACH I/O DEVICE
C             IOFORM < 0 : NOT OPENED AT INITIALIZATION STEP,
C             EVEN IF "SETENV FU??" IS SET.
C
C***********************************************************************
      SUBROUTINE UIOUNT(IOFORM)
C
      COMMON  /UNITIO/ NIN1, NOUT1, LOUT, IOMSG2
      DIMENSION IOFORM(100)
      DO 100 I=1,100
        IOFORM(I) = -1
  100 CONTINUE
      IOFORM(NIN1)  = 1
      IOFORM(NOUT1) = 1
      IOFORM(LOUT)  = 1
      IOFORM(IOMSG2)= 1
      RETURN
      END

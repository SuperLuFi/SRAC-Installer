C             S966W               LEVEL=1        DATE=81.11.14
      SUBROUTINE S966W(CRX,MTT,IH1,IHM,IGM,MCR,MTP,NT6,NOU)
C   *** S966  READS CROSS SECTION LIB. TAPE
      DIMENSION       NAME(12),MTT(1),CRX(IH1,1)
      REWIND NT6
      NERR = 0
      WRITE (NOU,10)
   10 FORMAT('0 ELEMENTS FROM LIBRARY TAPE')
      DO 1 M=1,MTP
    5 READ(NT6,END=4)   NN1,NN2,NN3,NN4,NAME
C      IF(NN4-MTT(M))2,3,4
      IF(NN4.EQ.MTT(M)) GO TO 3
    2 READ (NT6)
      GO TO 5
    4 NERR=NERR + 1
      WRITE (NOU,30) MTT(M)
   30 FORMAT('  ELEMENT ',I4,' CAN NOT BE FOUND,13& MAY BE OUT OF ORDE',
     &'R'/'0 DATA EDIT CONTINUES')
      GO TO 1
    3 READ(NT6) (CRX(I,M),I=1,IH1)
      I01=MCR+M
      WRITE (NOU,20) I01,NN4,NAME
   20 FORMAT(4X,2I6,6X,12A4)
    1 CONTINUE
      IF(NERR.GT.0) STOP 7
      REWIND NT6
      RETURN
      END

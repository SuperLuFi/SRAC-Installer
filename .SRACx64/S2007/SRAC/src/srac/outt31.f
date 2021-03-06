C             OUTT31              LEVEL=1        DATE=81.11.14
      SUBROUTINE OUTT31
C
C     FINAL PRINT OVERLAY
C
      COMMON /TW1C/ DD(1),LIM1,IA(210)
      COMMON /WORK/AAA(1),LIM2,AA(130)
      DIMENSION D(212),A(132)
      EQUIVALENCE (D(1),DD(1)),(AAA(1),A(1))
C
C     FOR FINAL PRINT
C
      EQUIVALENCE (IA(58),NM),(IA(64),IT),(IA(65),JT),( A(76),LFL),
     &(D(182),LXR),(D(183),LYR),( A(95),LQG),( A(96),LFG),
     &( A(97),LSIN),( A(98),LSS),( A(99),LSOUT),( A(100),LHL),
     &( A(101),LVL),( A(102),LTL),( A(103),LNL),( A(104),LRL),
     &( A(105),LABG),( A(106),LBAL),( D(202),LXRA),( D(203),LYRA),
     &(A(123),LVMESH),(A(124),LZMESH),(A(125),LFLUXM)
CKH
CKSK  CALL FINAL ( A(LFL),A(LQG),A(LFG),A(LSIN),A(LSS),A(LSOUT),A(LNL),
CKSK &A(LABG),A(LBAL),A(LRL),A(LHL),A(LTL),A(LVL),NM,IT,JT,D(LXRA),
CKSK &D(LYRA),D(LXR),D(LYR) )
      CALL FINAL ( AAA(LFL),AAA(LQG),AAA(LFG),AAA(LSIN),AAA(LSS),
     &AAA(LSOUT),AAA(LNL),AAA(LABG),AAA(LBAL),AAA(LRL),AAA(LHL),
     &AAA(LTL),AAA(LVL),NM,IT,JT,DD(LXRA),DD(LYRA),DD(LXR),DD(LYR) )
CKH
CKSK  CALL PDSFLX(A(LFL),A(LVMESH),A(LZMESH),A(LFLUXM),NM,IT,JT)
      CALL PDSFLX(AAA(LFL),AAA(LVMESH),AAA(LZMESH),AAA(LFLUXM),NM,IT,JT)
      RETURN
      END

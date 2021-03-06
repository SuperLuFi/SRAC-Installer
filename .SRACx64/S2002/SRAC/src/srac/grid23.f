C             GRID23              LEVEL=1        DATE=81.11.14
      SUBROUTINE GRID23
C
      COMMON /TW1C/ DD(1),LIM1,IA(210)
      COMMON /WORK/AAA(1),LIM2,AA(130)
      DIMENSION D(212),A(132)
      EQUIVALENCE (D(1),DD(1)),(AAA(1),A(1))
C
C     EQUIVALENCE
C
      EQUIVALENCE (IA(5),IM),(IA(6),JM),(IA(57),MM),(IA(58),NM),
     &(IA(64),IT),(IA(65),JT),(A(72),LBR1),(A(73),LBR2),(A(74),LBT1),
     &(A(75),LBT2),(A(76),LFL),(A(78),LFISA),(D(184),LDX),(D(186),LDYA),
     &(A(79),LXH),(A(80),LYH),(D(198),LF),(A(85),LAB),(A(86),LQQ),
     &(A(87),LQQG),(A(95),LQG),(A(96),LFG),(A(108),LCHIA),(A(120),LBT3),
     &(A(121),LBT4)
C
C     COMPUTE CONVERGENCE NUMBERS AND NEW PARAMETERS
C
CKSK  CALL TESTS ( A(LFG),A(LFISA),A(LAB),A(LCHIA),D(LDX),D(LDYA),
CKSK &A(LXH),A(LYH),A(LQQ),A(LQQG),D(LF),A(LQG),A(LFL),A(LBT1),A(LBT2),
CKSK &A(LBR1),A(LBR2),A(LBT3),A(LBT4),IT,JT,IM,JM,NM,MM )
      CALL TESTS ( AAA(LFG),AAA(LFISA),AAA(LAB),AAA(LCHIA),DD(LDX),
     &DD(LDYA),AAA(LXH),AAA(LYH),AAA(LQQ),AAA(LQQG),DD(LF),AAA(LQG),
     &AAA(LFL),AAA(LBT1),AAA(LBT2),AAA(LBR1),AAA(LBR2),AAA(LBT3),
     &AAA(LBT4),IT,JT,IM,JM,NM,MM )
      RETURN
      END

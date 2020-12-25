      SUBROUTINE UCLCKM(ICPU)
C
C     CLOCKM IS ORIGINALLY A FACOM BUILTIN SERVICE ROUTINE TO RETURN
C     CURRENT CPU TIME(MILI-SEC) IN 4-BYTE INTEGER.
C     ICPU(OUTPUT)  : CURRENT USED CPU TIME (MILI-SEC) IN 4-BYTE INTEGER
C
C     THIS ROUTINE IS REPLACED BY A CLOCK(ICPU,1,0) WHICH IS ALSO
C     A FACOM BUILTIN SERVICE ROUTINE.
C     GENERALLY;
C     ICPU(OUTPUT)  : CURRENT USED CPU TIME (UNIT IS DEPEND ON IC1)
C     IC1  (INPUT)  : =0(SEC.), =1(MILI-SEC.), =2(MICRO-SEC.)
C     IC2  (INPUT)  : =0(INTEGER*4), =1(REAL*4), =2(REAL*8), =3(REAL*16)  
C
      CALL CLOCK(ICPU,1,0)
      RETURN
      END
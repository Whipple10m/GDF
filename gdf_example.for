CDOC gdf_example.tex 

      PROGRAM GDF_EXAMPLE
C-------------------------------------------------------------------------------
C-    main program calling example and test subroutines
C-------------------------------------------------------------------------------

      USE GDF 

      IMPLICIT NONE 
      INTEGER      IERR                         ! return code           
      INTEGER      IARGC                        ! external function
      CHARACTER(64) DATA,TEMP,TEST              ! file names 

      CALL GDF$INIT('Z',IERR)                   ! Z=initialise Zebra
      CALL GDF$OPTION('AXP',.TRUE.,IERR)        ! set DEC Alpha option  

      IF (IARGC(IERR).EQ.0) THEN                ! no input file?
        CALL GDF$MANUAL                         !   generate manual 
      ELSE IF (IARGC(IERR).EQ.1) THEN           ! one input file only?
        CALL GETARG(1,DATA)                     !   get data file name
        CALL GDF$EXAMPLE_1(DATA,IERR)           !   execute GDF example 1  
        CALL GDF$EXAMPLE_2(DATA,IERR)           !                       2 
        CALL GDF$EXAMPLE_3(DATA,IERR)           !                       3
      ELSE IF (IARGC(IERR).GE.2) THEN           ! two file names? 
        CALL GETARG(1,TEMP)                     !   test data output file 
        CALL GETARG(2,TEST)                     !   test data input file 
        CALL GDF$TEST(10,TEMP,'W',IERR)         !   make test data file
        CALL GDF$TEST(10,TEST,'R',IERR)         !   verify test file 
      ENDIF 

      CALL GDF$EXIT(IERR)                       ! terminate GDF 
      END       
CEND 
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
CDOC gdf$example_1.tex 

      SUBROUTINE GDF$EXAMPLE_1(FILE,IERR)
C-------------------------------------------------------------------------------
C-    print out a single variable per record read from file 
C-------------------------------------------------------------------------------

      USE GDF                                                ! GDF definitions  

      IMPLICIT NONE 
      CHARACTER(*) FILE                                      ! input file name 
      INTEGER      IERR                                      ! return code 
      INTEGER      I                                         ! guess ..

      CALL GDF$OPEN(10,FILE,'R',IERR)                        ! R=readonly
      DO I=1,5                                               ! first few events 
        CALL GDF$READ(10,' ',IERR)                           ! read from unit 10
        IF (GDF_RUN%NEW ) PRINT*,'Run number',GDF_RUN%RUN    ! run number 
        IF (GDF_EV10%NEW) PRINT*,'10m event ',GDF_EV10%EVENT ! 10m event number
        IF (GDF_EV11%NEW) PRINT*,'11m event ',GDF_EV11%EVENT ! 11m event number 
        IF (GDF_FR10%NEW) PRINT*,'10m frame ',GDF_FR10%FRAME ! 10m frame number
        IF (GDF_FR11%NEW) PRINT*,'11m frame ',GDF_FR11%FRAME ! 11m frame number
      ENDDO                                         
      CALL GDF$CLOSE(10,IERR)                                ! close file 
      END 
CEND 
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
CDOC gdf$example_2.tex 

      SUBROUTINE GDF$EXAMPLE_2(FILE,IERR)

C-------------------------------------------------------------------------------
C-    print out contents of all records read from file  
C-------------------------------------------------------------------------------

      USE GDF                                            ! GDF definitions  

      IMPLICIT NONE 

      INTEGER       I,J 
      INTEGER       IERR                                 
      CHARACTER*(*) FILE                                 ! file name      

      CALL GDF$OPEN(10,FILE,'R',IERR)                    ! R=readonly, unit 10

      DO I=1,10000                                       ! read a few events 

        CALL GDF$READ(10,' ',IERR)                       ! read from unit 10 

        IF (GDF_RUN%NEW)  CALL GDF$PRINT(GDF_RUN,IERR)    
    
        DO J=1,GDF_TELE_MAX 
          IF (GDF_CCD  (J)%NEW) CALL GDF$PRINT(GDF_CCD  (J),IERR)
          IF (GDF_TRACK(J)%NEW) CALL GDF$PRINT(GDF_TRACK(J),IERR)
*         IF (GDF_HV   (J)%NEW) CALL GDF$PRINT(GDF_HV   (J),IERR)
        ENDDO

        IF (I.LE.100) THEN
*           IF (GDF_FR10%NEW) CALL GDF$PRINT(GDF_FR10,IERR)
           IF (GDF_EV10%NEW) CALL GDF$PRINT(GDF_EV10,IERR) 
*           IF (GDF_FR11%NEW) CALL GDF$PRINT(GDF_FR11,IERR)
*           IF (GDF_EV11%NEW) CALL GDF$PRINT(GDF_EV11,IERR)
        ENDIF

      ENDDO 

      CALL GDF$CLOSE(10,IERR)                              ! close file 
             
      E N D 
CEND
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
CDOC gdf$example_3.tex 

      SUBROUTINE GDF$EXAMPLE_3(FILE,IERR)

C-------------------------------------------------------------------------------
C-    enter sum of total ADC signal per event into histogram
C-------------------------------------------------------------------------------

      USE GDF                                            ! GDF definitions  

      IMPLICIT NONE 

      CHARACTER*(*) FILE                                 ! file name      

      REAL          A,P,S,H 
      INTEGER       I,J,IERR

      COMMON /PAWC/ H(999999)

      CALL HLIMIT(-999999)                           ! init HBOOK 
      CALL HBOOK1(100,'ADC signal',50,0.0,1E3,0.0)   ! book histogram 

      CALL GDF$OPEN(10,FILE,'R',IERR)                ! open file on unit 10

      DO I=1,999                                     ! for a few events 
        CALL GDF$READ(10,' ',IERR)                   ! read from unit 10 
        IF (IERR.NE.0) GOTO 999                      ! end of file? error? 
        IF (GDF_EV10%NEW.AND.GDF_FR10%VALID) THEN    ! new event? pedestals?
          S = 0.0                                    ! reset sum 
          DO J=1,GDF_EV10%NADC                       ! all ADC channels
            A = GDF_EV10%ADC(J)                      ! ADC value 
            P = GDF_FR10%PED_ADC1(J)                 ! pedestal value   
            S = S + ( A - P )                        ! add up signal         
          ENDDO                                      ! next ADC 
          CALL HFILL(100,S,0.0,1.0)                  ! enter total signal
        ENDIF                                            
      ENDDO                                          ! next event 

  999 CALL GDF$CLOSE(10,IERR)                        ! close file 
      CALL HPRINT(100)                               ! show histogram
             
      E N D 
CEND

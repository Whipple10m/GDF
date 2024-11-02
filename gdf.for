C------------------------------------------------------------------------------
C  
C   Granite Data Format software  
C 
C------------------------------------------------------------------------------
CDOC gdf_vers.tex 
CVER 
C
C   0.00 Patchy, Zebra, VMS Fortran records, LaTeX documentation  
C   0.01 self describing Zebra sector words 
C   0.02 now running both on Sun and under Ultrix 
C   0.03 conversion of existing 10m data into this format 
C   0.04 c-library i/o for binary files on SUN 
C   0.05 several words added to CCD record   
C   0.05 modified copy of datman.f no longer needed, removed from card file
C   0.06 structures modified for HYTEC readout, rewriting old stereo data  
C   0.07 calibration new, CCD and tracking revised, swap integer*2 on SUNs 
C   0.08 optional checksum calculation 
C   0.09 valid flags set/reset by GDF$READ 
C   0.10 event selection and non-stereo mode in GDF$CONVERT now working 
C   0.11 new variable GDF_CAL(i).WIDTH_ERROR(j), more parameters
C   0.12 tracking added in GDF$PRINT, new tracking modes defined 
C   0.13 Monte Carlo interface 
C   0.14 CCD and HV cycle numbers 
C   0.15 UTC time of nominal run start and end of run 
C   0.16 long observer comments, GDF$MOVE for shorter than expected records  
C   0.17 routines for MOCCA/GDF (Pascal/Fortran) interface 
C   0.18 c-library IO while writting binary FZ file on Sun.
C   0.19 routine GDF$RESIZE to change length of last sector at run time   
C   0.20 64 bit memory addresses for AXP/OSF-1 
C   0.21 coordinate systems for Monte Carlo revised 
C   0.22 changes in 10m part for new Hytec Camac readout
C   0.23 all sectors in a structures may increase in length 
C   0.24 GDF$SWAP (only used on Sun) revised to swap all integer*2 arrays
C   0.25 GDF_DUMP program to dump file contents on terminal screen 
C   0.26 print out of calibration record in gdf$print 
C   0.27 64-bit alignment for Alpha under VMS and OSF 
C   0.28 changes by Glenn Sembroski to 11m frame and event format (March 95)
C   0.29 file generation by ypatchy and DCL script revised 
C   0.30 merged with additional 11m changes to Version 22 by GHS 
C   0.31 run time option to set IERR=0 on entry or to return 
C   0.32 GDF$ADJUST to set sector length for variable length ZEBRA banks 
C   0.33 in 10m frame variable number of scalers and ADCs 
C   0.34 in 10m event variable number ADCs 
C   0.35 in 11m frame variable number of scalers, ADCs and TDCs  
C   0.36 in 11m event variable number ADCs and TDCs  
C   0.37 backwards compatibility for fixed length 10m records 
C   0.38 revised trigger word bits assignments 
C   0.39 UTC time start of year table updated to year 2004 
C   0.40 Leeds GPS clock added in frame and event stuctures 
C   0.41 increased number of HV channels, variable length record  
C   0.42 added 2 times 16 bit in event records for tracking 
C   0.43 change from LaTeX to LaTeX2e
C   0.44 manual revised, example programs combined into one program 
C   0.45 GDF$PRINT shows bank version numbers 
C   0.46 phase delays in frame records 
C   0.47 new routine GDF$RUN to move run crecords
C   0.48 new routine GDF$EVENT11 to move 11m event records
C   0.49 new routine GDF$FRAME11 to move 11m frame records
C   0.50 GDF$READ and GDF$WRITE revised to use new data record routines  
C   0.51 phase delay added to event records, replaces unused GPS/GEOS 16 bit
C   0.52 GDF$SECTOR removed, as sector descriptors are now set in GDF$MOVE  
C   0.53 new GDF$EVENT10 and GDF$FRAME10 routines, GDF$FIXIT removed   
C   0.54 obsolete GDF_RUN.CHSKY and GDF_RUN.CHTRIG removed
C   0.55 GDF$TRACK, GDF$CCD routines  
C   0.56 true variable length run record, calling UHTOC(.) for ZEBRA conversion
C   0.57 obsolete routine GDF$ADJUST removed
C   0.58 GDF$MOVE now swaps words in 16 bit arrays
C   0.59 sector descriptor words in removed from Fortran structures
C   0.60 obsolete GDF$RESIZE removed, as variable sectors now handled localy  
C   0.61 test routines GDF$TEST to verify data write / read match 
C   0.62 new routine GDF$HV to pack/unpack HV data  
C   0.63 check file name length and specify READONLY in GDF$OPEN for read access
C   0.64 for old format records on SUN swap 16 bit half-words after UCOPY  
C   0.65 bug in GDF$TRACK fixed, 32-bit patterns now only 1 word, was 2 words 
C   0.66 record identifier now in ZEBRA bank, GDF$TEST includes tracking   
C   0.67 GDF$HV ignores zero length arrays and pre version 66 HV records 
C   0.68 increase number of ADC in calibration record to 541 
C   0.69 conversion from VMS FORTRAN 77 Patchy into into single FORTRAN90 file
C   0.70 GDF$MANUAL routine to extract documentation  
C   0.71 GDF$TEST produces 10m frame records 
C   0.72 backwards compatibilty checked for version 67 files 
C   0.73 variable length trigger information in 10m event  
C   0.74 elapsed time counter, Wisconsin TrueTime clock interface 
C   0.75 LaTeX documentation from card file included in GDF.FOR 
C   0.76 document GDF$TEST and GDF$MANUAL 
C   0.77 fix UNIX read in GDF$MANUAL, tested on Sun Solaris 
C   0.78 revised CCD structure  
C   0.79 allow zero length sectors for ADC and trigger data in events 
C   0.80 ignore ADC and current monitor data fields in 10m frames 
C   0.81 revised HV data structure, GDF$PRINT routine for GDF_HV
C   0.82 fix for SUN Solaris F90 32-bit representaion of 16-bit integers 
C   0.83 fix for 16-bit half-word swap in version 82, HV status bits added   
C  
C   - extend GDF$TEST to run HV, CCD, ....  
C   - make PRIVATE the default, declare PUBLIC as needed  
C   - detection of hardware an operating system type at run time 
C
C   - revise MC data structures. Dynamic memory allocation? Linked lists? 
C   - write Monte Carlo interface routines GDF$MCE, GDF$MCP
C   - include C structures (Rod Lessard?) 
C   - add Monte Carlo example to manual 
C   - revise calibration data structures, use RZ direct access file? 
C   - verify that checksum calculation works with new bank header format 
C   - simple interactive KUIP interface 
C   - read/write multiple runs to/from tape or CD   
C   - revise strategy for setting valid flags 
C   - spell check documentation
C
C
CEND 
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
CDOC gdf.tex  
C
C   \documentclass{cernman}
C   \usepackage[dvips]{graphics}
C   \usepackage{draftcopy}
C   \input{gdf_title.tex}   
C   \input{gdf_intro.tex}   
C   \input{gdf_calls.tex}
C   \input{gdf_types.tex}   
C   \input{gdf_inter.tex}   
C   \input{gdf_appen.tex}   
C
C   \end{document}
CEND
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      MODULE GDF 
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

      INTEGER, PARAMETER ::  GDF_VERSION=83 

CDOC gdf_para.tex 
CVER 
C-------------------------------------------------------------------------------
C     global parameter 
C-------------------------------------------------------------------------------
      INTEGER, PARAMETER ::
     .  GDF_TELE_10 =1,
     .  GDF_TELE_11 =2,
     .  GDF_TELE_MAX=2 
C-------------------------------------------------------------------------------
C     sky quality 
C-------------------------------------------------------------------------------
      INTEGER, PARAMETER ::   
     +  SKY_GOOD     =1,      ! definitly good data (A)     
     +  SKY_UNCERTAIN=2,      ! probably good data  (B)
     +  SKY_BAD      =3       ! definitly not good  (C)
C-------------------------------------------------------------------------------
C      trigger mode 
C-------------------------------------------------------------------------------
      INTEGER, PARAMETER ::    
     +  TRIG_MODE_SINGLE =1,  
     +  TRIG_MODE_DUAL   =2

C---- position of trigger bits (LSB=0, MSB=31) 
C
C     The old trigger bit position are arranged according to the GRALP 
C     trigger numbers. When old GRALP files are converted to this format 
C     the trigger bit corresponding to the event code is set.
C

      INTEGER, PARAMETER :: 
     +  GDF_TRIG_SHORT= 1, ! GRALP only, obsolete 
     +  GDF_TRIG_LONG = 2, ! GRALP only, obsolete 
     +  GDF_TRIG_TEST1= 3, ! fixed period trigger,  no  TDC time delay   
     +  GDF_TRIG_TEST2= 4, ! fixed period trigger, with TDC time delay  
     +  GDF_TRIG_WWVB = 6, ! WWVB time marker 
     +  GDF_TRIG_STIME= 7, ! siderial time marker, obsolete?
     +  GDF_TRIG_HIG  = 8, ! high level trigger 
     +  GDF_TRIG_LOW  = 9, ! low level trigger 
     +  GDF_TRIG_EAS  =12  ! GRALP, EAS trigger, obsolete 

C----
C     The Hytec based system has its own trigger bits conventions. 
C     Note that a given event may have more than one trigger bit set. 
C
      INTEGER, PARAMETER :: 
     +  GDF_TRIG_PED = 0, ! pedestal trigger  
     +  GDF_TRIG_PST = 1, ! pattern selection trigger 
     +  GDF_TRIG_MUL = 2  ! multiplicity trigger 
C-------------------------------------------------------------------------------
C     type of run 
C-------------------------------------------------------------------------------
      INTEGER, PARAMETER :: 
     .  GDF_RUN_TYPE_STEREO=1, 
     .  GDF_RUN_TYPE_10    =2, 
     .  GDF_RUN_TYPE_11    =3, 
     .  GDF_RUN_TYPE_MC    =4  
C-------------------------------------------------------------------------------
C     telescope tracking status/mode 
C-------------------------------------------------------------------------------
      INTEGER, PARAMETER :: 
     .  GDF_TRACK_MODE_ON     =1,
     .  GDF_TRACK_MODE_OFF    =2,
     .  GDF_TRACK_MODE_SLEWING=3,
     .  GDF_TRACK_MODE_STANDBY=4,
     .  GDF_TRACK_MODE_ZENITH =5,
     .  GDF_TRACK_MODE_CHECK  =6,      ! pointing check 
     .  GDF_TRACK_MODE_STOWING=7,      ! stowing telescope 
     .  GDF_TRACK_MODE_DRIFT  =8,      ! drift scan 
     .  GDF_TRACK_MODE_MAX    =8 

C-------------------------------------------------------------------------------
C     position of HV status bits (March 1998) 
C-------------------------------------------------------------------------------
      INTEGER, PARAMETER :: 
     .  GDF_HV_HWENABLED    =  0,  ! hardware channel enabled
     .  GDF_HV_RAMPUP       =  1,  ! output ramping to higher absolute value
     .  GDF_HV_RAMPDOWN     =  2,  ! output ramping to lower absolute value
     .  GDF_HV_SWENABLE     =  3,  ! software channel enabled
     .  GDF_HV_TRIP_SUPPLY  =  4,  ! trip condition: violation of supply limit
     .  GDF_HV_TRIP_CURRENT =  5,  ! trip condition: violation of current limit
     .  GDF_HV_TRIP_ERROR   =  6,  ! trip condition: voltage error
     .  GDF_HV_TRIP_VOLTAGE =  7,  ! trip condition: violation of voltage limit
     .  GDF_HV_CRATE_ID     =  8,  ! crate: id
     .  GDF_HV_CRATE_STATE  =  9,  ! crate: HV state        [0 off, 1 on]
     .  GDF_HV_CRATE_EEPROM = 10,  ! crate: eeprom status   [0 bad, 1 ok]
     .  GDF_HV_BATTERY      = 11,  ! crate: battery status  [0 bad, 1 ok]
     .  GDF_HV_24V          = 13,  ! crate: 24 V status     [0 bad, 1 ok]
     .  GDF_HV_PANIC        = 14   ! crate: panic condition [0 not active, 1 active]

CEND 
C-------------------------------------------------------------------------------
C     c library i/o  
C-------------------------------------------------------------------------------
      TYPE GDF_CFILE_T 
        sequence 
        INTEGER LUNDES                ! file descriptor 
        INTEGER MEDIUM                ! physical medium (disk, tape)   
        LOGICAL OPEN                  ! .true. means file is open 
      END TYPE GDF_CFILE_T 

      TYPE(GDF_CFILE_T),PRIVATE :: GDF_CFILE 

C-------------------------------------------------------------------------------
C     error codes and messages 
C-------------------------------------------------------------------------------
      CHARACTER(80)  SRN                 ! subroutine name 
      CHARACTER(80)  MSG                 ! error message 
 
      PRIVATE GDF$ERRROR,SRN,MSG 

C-------------------------------------------------------------------------------
C     run time options 
C-------------------------------------------------------------------------------
      TYPE GDF_OPTION_T  
        LOGICAL     ABORT     ! if on entry IERR.NE.0 return from subroutine  
        LOGICAL     RESET     ! if on entry IERR.NE.0 reset to IERR=0 
        LOGICAL     VERBOSE   ! extra output 
        LOGICAL     SHIFT16   ! swap 16 bit half words (eg on SUNs) 
        LOGICAL     CIO       ! use C-library IO 
        LOGICAL     AXP       ! DEC Alpha number representation, Unix or VMS 
        LOGICAL     SOLARIS   ! Solaris 32-bit represntion of 16-bit integers
        LOGICAL     TIME      ! do time measurements 
      END TYPE GDF_OPTION_T  
      
      TYPE(GDF_OPTION_T),PRIVATE :: GDF_OPTION 

CDOC gdf_run.tex 
CVER-7
C-------------------------------------------------------------------------------
C-    run information  
C-------------------------------------------------------------------------------
      integer, parameter :: gdf_run_mcl=16000 ! maximum comment length 
      type gdf_run_t
        sequence 
        integer*4  version                  ! # software version number
        integer*4  reserved                 ! # for future use 
        integer*4  checksum(2)              ! # data part check sum 
        real   *8  utc                      ! current UTC time            [mjd]
        logical*4  status(gdf_tele_max)     ! detector status             [bits]
        integer*4  idate                    ! VAX date, local time      [yymmdd]
        integer*4  itime                    ! VAX time, local time      [hhmmss]
        integer*4  year                     ! Gregorian year, UTC time     
        integer*4  run                      ! run number
        integer*4  type                     ! type of run  
        integer*4  sky_quality              ! sky quality 
        integer*4  trig_mode(gdf_tele_max)  ! trigger setup
        integer*4  trig_nhig(gdf_tele_max)  ! min tubes above high threhold 
        integer*4  trig_nlow(gdf_tele_max)  !                 low  threhold 
        integer*4  clen                     ! actual length of comment   [bytes]
        real   *4  sid_length(1)            ! siderial nominal run length [min] 
        real   *4  sid_cycle                !          nominal cycle time [min]
        real   *4  sid_actual               !          actual logged time [min]
        real   *4  trig_thrlow(gdf_tele_max)! low  level trigger thresholds  [V]
        real   *4  trig_thrhig(gdf_tele_max)! high level trigger thresholds  [V]
        real   *8  utc_start                ! nominal UTC start of run     [mjd]
        real   *8  utc_end                  ! nominal UTC end   of run     [mjd]
        character  file*80                  ! file name
        character  observer*80              ! observer names
        character  comment*(gdf_run_mcl)    ! any observer comments
        logical*4  new                      ! .TRUE. if just read from file 
        logical*4  valid                    ! .TRUE. if record contents valid 
      end type gdf_run_t 

      type(gdf_run_t), target ::  gdf_run

CEND 
CDOC gdf_hv.tex 
CVER-7
C-------------------------------------------------------------------------------
C-    high voltage 
C-------------------------------------------------------------------------------
      integer, parameter :: gdf_hvc_max  = 640 !  max number of HV channels   

      type gdf_hv_t
        sequence   
        integer*4 version               ! # software id 
        integer*4 reserved              ! # for future use 
        integer*4 checksum(2)           ! # data part check sum 
        real   *8 utc                   ! UTC time, end of measurement  
        integer*4 telescope             ! telescope identifier 
        integer*4 mode                  ! current operation mode 
        integer*4 nch                   ! channels voltages values 
        integer*4 cycle                 ! read cycle number 
        integer*2 status  (gdf_hvc_max) ! status of each HV channel  [bits]
        real   *4 v_set   (gdf_hvc_max) ! presently set  voltage    [V]
        real   *4 v_actual(gdf_hvc_max) ! actual measured voltage   [V]
        real   *4 i_supply(gdf_hvc_max) ! HV supply current         [uA]
        real   *4 i_anode (gdf_hvc_max) ! measured anode current    [uA]
        logical*4 new                   ! .TRUE. if just read from file 
        logical*4 valid                 ! .TRUE. if contents valid 
      end type gdf_hv_t 

      type(gdf_hv_t), target :: gdf_hv(2) 
       
      INTEGER, PARAMETER ::             ! status bit positions, LSB=0,MSB=15   
     .  GDF_HV_ON = 0,                  ! 
     .  GDF_HV_XX = 1, 
     .  GDF_HV_YY = 2 
       
CEND
CDOC gdf_ccd.tex 
CVER-7       
C-------------------------------------------------------------------------------
C-    CCD  camera results 
C-------------------------------------------------------------------------------
      integer, parameter :: gdf_ccd_nstar_max= 100 ! max number of stars 

      type gdf_ccd_t
        sequence  
        integer*4  version                      ! # software version number 
        integer*4  reserved                     ! # for future use 
        integer*4  checksum(2)                  ! # data part check sum 
        real   *8  utc                          ! UTC time picture [mjd] 
        integer*4  telescope                    ! telescope identifier 
        integer*4  nstar                        ! actual number of stars   
        integer*4  cycle                        ! number of updates sofar 
        integer*4  interlace                    ! interlace: on=1,off=0 
        integer*4  bias                         ! pedestal 
        integer*4  gain                         ! gain 
        integer*4  noise_range                  !                           
        integer*4  exposure                     ! exposure time          [msec]
        integer*4  intensity(gdf_ccd_nstar_max) ! intensity 
        integer*4  status   (gdf_ccd_nstar_max) ! status 
        real   *4  marker(2)                    ! marker position center tube
        real   *4  dark_mean                    ! dark file mean        
        real   *4  dark_sigma                   ! dark file sigma         
        real   *4  low_mean                     ! mean,  low live pixel
        real   *4  low_sigma                    ! sigma, low live pixel
        real   *4  noise_threshold              !                           
        real   *4  noise_level                  !
        real   *4  star(2,gdf_ccd_nstar_max)    ! 1=x,2=y
        logical*4  new                          ! .TRUE. if just read from file 
        logical*4  valid                        ! .TRUE. if contents valid 
      end type gdf_ccd_t

      type(gdf_ccd_t), target :: gdf_ccd(2)

CEND
CDOC gdf_track.tex 
CVER-7
C-------------------------------------------------------------------------------
C-    tracking 
C-------------------------------------------------------------------------------
      type gdf_track_t 
        sequence 
        integer*4    version       ! software version number  
        integer*4    reserved      ! # for future use 
        integer*4    checksum(2)   ! # data part check sum 
        real   *8    utc           ! current UTC time               [mjd]
        integer*4    telescope     ! telescope id 
        integer*4    mode          ! tracking mode
        integer*4    cycle         ! number of updates sofar 
        logical*4    status        ! telescope status       [bit-pattern] 
        real   *8    rasc_2000     ! source    right ascension, FK5 J2000 [rad]
        real   *8    decl_2000     !           declination    , FK5 J2000 [rad]
        real   *8    rasc_today    !           right ascension, FK5 today [rad]
        real   *8    decl_today    !           declination    , FK5 today [rad]
        real   *8    rasc_tele     ! telescope right ascencion, FK5 today [rad]
        real   *8    decl_tele     !           declination    , FK5 today [rad]
        real   *8    azimuth       !           pointing, +west, north  =0 [rad]
        real   *8    elevation     !           pointing, +up  , horizon=0 [rad]
        real   *8    deviation     ! angle nominal/actual position        [rad] 
        real   *8    rasc_offset   ! RA offset for off-source runs        [rad]
        real   *8    decl_offset   ! DE offset for off-source runs        [rad]
        real   *8    stl           ! local siderial time                  [rad] 
        real   *8    height        ! height of interaction point        [meter]
        real   *8    azi_incl      ! azimuth change for inclination       [rad]
        real   *8    ele_incl      ! elevation change for incl.           [rad] 
        character*80 source        ! source name  
        logical*4    new           ! .TRUE. if just read from file 
        logical*4    valid         ! .TRUE. if contents valid 
      end type gdf_track_t 

      type(gdf_track_t), target :: gdf_track(2)

CEND
CDOC gdf_fr10.tex 
CVER-7
C-------------------------------------------------------------------------------
C-    10 meter frame 
C-------------------------------------------------------------------------------
C---- maximum number of adcs and scalers 
      integer, parameter :: 
     .  gdf_fr10_nadc= 636,                   ! max ADCs   (12 per module)
     .  gdf_fr10_nsca= 640,                   !     TDCs   (32 per module)
     .  gdf_fr10_nphs=   8                    !     scaler ( 8 per module) 

      type gdf_fr10_t   
        sequence 
        integer*4 version                 ! software version number 
        integer*4 reserved                ! # for future use 
        integer*4 checksum(2)             ! # data part check sum 
        real   *8 utc                     ! current UTC time              [mjd]
        logical*4 status                  ! detector status bits         [bits]
        logical*4 mark_gps                ! Last recorded GPS (perhaps!) [50ns]
        integer*4 nphs                    ! number of phase TDCs
        integer*4 nadc                    ! number of ADCs 
        integer*4 nsca                    ! number of scalers  
        integer*4 run                     ! run number 
        integer*4 frame                   ! frame number 
        integer*4 gps_mjd                 ! soon: modified Julian days     [mjd]
        integer*4 gps_sec                 ! soon: seconds since midnight   [sec]
        integer*4 gps_ns                  ! soon: time from last GPS second [ns]
        integer*2 cal_adc (gdf_fr10_nadc) ! ADC with internal test voltage 
        integer*2 ped_adc1(gdf_fr10_nadc) ! ADC first  random event 
        integer*2 ped_adc2(gdf_fr10_nadc) ! ADC second random event  
        integer*2 scalc   (gdf_fr10_nsca) ! current monitor scaler 
        integer*2 scals   (gdf_fr10_nsca) ! single rates scaler 
        integer*2 gps_clock(3)            ! GPS time last event          [bits]
        integer*2 phase_delay             ! phase delay module settings 
        integer*2 phs1    (gdf_fr10_nphs) ! phase TDC first  random event  
        integer*2 phs2    (gdf_fr10_nphs) ! phase TDC second random event  
        integer*2 gps_status(2)           ! soon: GPS status flags 
        integer*4 align                   ! # 64 bit alignment 
        logical*4 new                     ! .TRUE. if just read from file 
        logical*4 valid                   ! .TRUE. if contents valid 
      end type gdf_fr10_t 

      type(gdf_fr10_t), target :: gdf_fr10 

CEND
CDOC gdf_ev10.tex 
CVER-7
C-------------------------------------------------------------------------------
C-      10 meter event 
C-------------------------------------------------------------------------------
C---- maximum number of adcs and scalers 
      integer, parameter ::
     .  gdf_ev10_nadc = 636,             ! max ADC (12 per module)   
     .  gdf_ev10_nphs =   8,             !     phase TDC  ( 8 per module)
     .  gdf_ev10_nbrst=  12,             !     burst TDC    
     .  gdf_ev10_ntrg =  65              !     trigger pattern data words

      type :: gdf_ev10_t           
        sequence
        integer*4  version                ! software version number 
        integer*4  reserved               ! # for future use 
        integer*4  checksum(2)            ! # data part check sum 
        real   *8  utc                    ! GPS UTC time of event         [mjd]

        integer*4  nadc                   ! number of ADCs 
        integer*4  run                    ! run number 
        integer*4  event                  ! event number
        integer*4  live_sec               ! live time from start of run  [sec]
        integer*4  live_ns                ! live time last incomplete sec [ns]
        integer*4  frame                  ! frame number 
        integer*4  frame_event            ! events within frame 
        integer*4  abort_cnt              ! number of aborts in frame      
        integer*4  nphs                   ! number of phase TDCs
        integer*4  nbrst                  ! number of burst scalers 
        integer*4  gps_mjd                ! soon: modified Julian days     [mjd]
        integer*4  gps_sec                ! soon: seconds since midnight   [sec]
        integer*4  gps_ns                 ! soon: time from last GPS second [ns]
        integer*4  ntrg                   ! number of trigger patterns 
        integer*4  elapsed_sec            ! sec from start of run          [sec]
        integer*4  elapsed_ns             ! ns since last elapsed sec      [ns]
        integer*4  grs_clock(3)		  ! Wisconsin TrueTime interface [bits]
        integer*4  align                  ! 64bit alignment 

        logical*4  trigger                ! trigger information         [32bit]
        logical*4  status                 ! detector status flags       [32bit]
        logical*4  mark_gps               ! last recorded GPS            [50ns]
        logical*4  mark_open              ! last calibration mark open   [50ns]
        logical*4  mark_close             ! last calibration mark close  [50ns]
        logical*4  gate_open              ! last event gate open         [50ns]
        logical*4  gate_close             ! last event gate close        [50ns]

        logical*4  pattern(gdf_ev10_ntrg) ! pattern trigger output      [32bit]

        integer*2  adc  (gdf_ev10_nadc)   ! event ADC's                  
        integer*2  gps_clock(3)           ! GPS satellite time           [bits] 
        integer*2  phase_delay            ! phase delay module settings 
        integer*2  phs  (gdf_ev10_nphs)   ! phase TDCs                      
        integer*2  burst(gdf_ev10_nbrst)  ! burst scalers       
        integer*2  gps_status(2)          ! soon: GPS status flags 
        integer*2  track(2)               ! telescope angle encoders     [bits] 
        logical*4  new                    ! .TRUE. if just read from file 
        logical*4  valid                  ! .TRUE. if contents valid 
      end type gdf_ev10_t 

      type(gdf_ev10_t), target :: gdf_ev10

CEND
CDOC gdf_fr11.tex 
CVER-7 
C-------------------------------------------------------------------------------
C-    11 meter frame 
C-------------------------------------------------------------------------------
C---- maximum number of adcs and scalers for 169 pmt channels   
      integer, parameter ::
     .  gdf_fr11_nadc= 180,                   ! ADCs (12 per module)
     .  gdf_fr11_ntdc= 176,                   ! TDC  ( 8 per module)
     .  gdf_fr11_nphs=   8,                   ! phase TDCs ( 8 per module)
     .  gdf_fr11_nsca= 192                    ! scaler     (32 per module) 

      type :: gdf_fr11_t 
        sequence
        integer*4 version                 ! software version number 
        integer*4 reserved                ! # for future use 
        integer*4 checksum(2)             ! # data part check sum 
        real   *8 utc                     ! UTC time of last event        [mjd]
        logical*4 status                  ! detector status flags        [bits]
        logical*4 mark_gps                ! Last recorded GPS (perhaps!)  [50ns]
        integer*4 nphs                    ! number of phase TDCs
        integer*4 ntdc                    ! number of TDCs
        integer*4 nadc                    ! number of ADCs 
        integer*4 nsca                    ! number of scalers  
        integer*4 run                     ! run number 
        integer*4 frame                   ! frame number 
        integer*4 gps_mjd                 ! soon: modified Julian days     [mjd]
        integer*4 gps_sec                 ! soon: seconds since midnight   [sec]
        integer*4 gps_ns                  ! soon: time from last GPS second [ns]
        integer*4 align_1                 ! # 64 bit alignment 
        integer*2 cal_adc (gdf_fr11_nadc) ! ADC value with internal signal  
        integer*2 ped_adc1(gdf_fr11_nadc) ! ADC value first  random event 
        integer*2 ped_adc2(gdf_fr11_nadc) ! ADC value second random event  
        integer*2 tdc1    (gdf_fr11_ntdc) ! TDC value first  random event 
        integer*2 tdc2    (gdf_fr11_ntdc) ! TDC value second random event  
        integer*2 scalc   (gdf_fr11_nsca) ! current monitor scaler 
        integer*2 scals   (gdf_fr11_nsca) ! single rates scaler  
        integer*2 geos_clock(3)           ! Geos time last event [bits]
        integer*2 phase_delay             ! phase delay module settings 
        integer*2 gps_status(2)           ! soon: GPS status flags 
        integer*2 phs1    (gdf_fr11_nphs) ! phase TDC first  random event 
        integer*2 phs2    (gdf_fr11_nphs) ! phase TDC second random event 
        integer*4 align_2                 ! # 64 bit alignment 
        logical*4 new                     ! .TRUE. if just read from file 
        logical*4 valid                   ! .TRUE. if contents valid 
      end type gdf_fr11_t 

      type(gdf_fr11_t), target :: gdf_fr11 
CEND
CDOC gdf_ev11.tex 
CVER-7
C-------------------------------------------------------------------------------
C-      11 meter event 
C-------------------------------------------------------------------------------
C---- maximum number of adcs and scalers for 169 pmt channel 
      integer, parameter :: 
     .  gdf_ev11_nadc  = 180,            ! ADC, 12 per module 
     .  gdf_ev11_ntdc  = 176,            ! TDC,  8 per module 
     .  gdf_ev11_nphs  =   8,            ! phase TDC 
     .  gdf_ev11_nbrst =  12             ! burst TDC 

      type :: gdf_ev11_t
        sequence            
        integer*4  version                ! software version number 
        integer*4  reserved               ! # for future use 
        integer*4  checksum(2)            ! # data part check sum 
        real   *8  utc                    ! UTC time of event             [mjd]
        logical*4  trigger                ! trigger bit pattern          [bits]
        logical*4  status                 ! detector status flags        [bits]
        logical*4  mark_gps               ! last one second GPS marker   [50ns]
        logical*4  mark_open              ! last opening time of cal mrk [50ns]
        logical*4  mark_close             ! last closeing time: Cal time [50ns]
        logical*4  gate_open              ! last event gate open         [50ns]
        logical*4  gate_close             ! event gate close             [50ns]
        integer*4  nbrst                  ! number of burst scalers 
        integer*4  nphs                   ! number of phase TDCs
        integer*4  ntdc                   ! number of TDCs
        integer*4  nadc                   ! number of ADCs 
        integer*4  run                    ! run number 
        integer*4  event                  ! event number
        integer*4  live_sec               ! live time from start of run   [sec]
        integer*4  live_ns                ! live time from start of run    [ns]
        integer*4  frame                  ! frame number 
        integer*4  frame_event            ! number of event within frame
        integer*4  abort_cnt              ! number of aborts in frame.
        integer*4  gps_mjd                ! soon: modified Julian days     [mjd]
        integer*4  gps_sec                ! soon: seconds since midnight   [sec]
        integer*4  gps_ns                 ! soon: time from last GPS second [ns]
        integer*4  align                  ! # 64 bit alignment 
        integer*2  adc  (gdf_ev11_nadc)   ! event ADC's                     [?]
        integer*2  tdc  (gdf_ev11_ntdc)   ! event TDC's                     [?]
        integer*2  geos_clock(3)          ! GPS satellite time
        integer*2  phase_delay            ! phase delay module settings 
        integer*2  gps_status(2)          ! soon: GPS status flags 
        integer*2  phs  (gdf_ev11_nphs)   ! phase TDC                       [?]
        integer*2  burst(gdf_ev11_nbrst)  ! burst scalers 
        integer*2  track(2)               ! telescope angle encoders     [bits] 
        logical*4  new                    ! .TRUE. if just read from file 
        logical*4  valid                  ! .TRUE. if contents valid 
      end type gdf_ev11_t 

      type (gdf_ev11_t),target :: gdf_ev11

CEND
CDOC gdf_cal.tex 
CVER-7
C-------------------------------------------------------------------------------
C-    detector calibration: pedestals and gains  
C-------------------------------------------------------------------------------
C---- maximum number of adcs and scalers 
      integer, parameter :: gdf_cal_npm_max = 541 

      type :: gdf_cal_t           
        sequence 
        integer*4  version                         ! software version number 
        integer*4  reserved                        ! # for future use 
        integer*4  checksum(2)                     ! # data part check sum 
        real   *8  utc                             ! UTC time first event  [mjd]
        integer*4  npm                             ! number of photomultipliers 
        integer*4  run                             ! run number 
        integer*4  method                          ! method used in calculation
        integer*4  status        (gdf_cal_npm_max) ! tube status 
        real   *4  peak          (gdf_cal_npm_max) ! pedestal maximum   
        real   *4  width         (gdf_cal_npm_max) !   width 
        real   *4  pedestal      (gdf_cal_npm_max) !   value itself    
        real   *4  asycor        (gdf_cal_npm_max) !   linear width correction 
        real   *4  symcor        (gdf_cal_npm_max) !   quadr. width correction
        real   *4  gain          (gdf_cal_npm_max) ! gain correction factor  
        real   *4  exponent      (gdf_cal_npm_max) !   power law gain corr.
        real   *4  peak    _error(gdf_cal_npm_max) ! error of peak 
        real   *4  width   _error(gdf_cal_npm_max) !   pedestal 
        real   *4  pedestal_error(gdf_cal_npm_max) !   pedestal 
        real   *4  asycor  _error(gdf_cal_npm_max) !   linear correction
        real   *4  symcor  _error(gdf_cal_npm_max) !   quadr. correction 
        real   *4  gain    _error(gdf_cal_npm_max) !   gain correction 
        real   *4  exponent_error(gdf_cal_npm_max) !   power law corr.
        real   *4  pedestal_pro  (gdf_cal_npm_max) ! probability pedestal ok 
        real   *4  gain    _pro  (gdf_cal_npm_max) ! probability of gain ok 
        logical*4  new                             ! .TRUE. if just read 
        logical*4  valid                           ! .TRUE. if contents valid 
      end type gdf_cal_t

      type(gdf_cal_t), target :: gdf_cal(2)

CEND
C-------------------------------------------------------------------------------
C     Monte Carlo event header        
C-------------------------------------------------------------------------------
C---- maximum number of mirrors 
      integer, parameter  :: gdf_mirror_max = 10

C---- sector header words 
      integer 
     .  gdf_mce_r,                                 ! # real 
     .  gdf_mce_i                                  ! # integer 
      parameter 
     . (gdf_mce_r = 3 + 16*(5+8*gdf_mirror_max),   ! # 
     .  gdf_mce_i = 2 + 16* 3                  )   ! #
C---- 
      type :: gdf_mce_t 
        sequence  
        integer*4  version                   ! software version number 
        integer*4  reserved                 ! # for future use 
        integer*4  checksum(2)               ! # data part check sum 
        real   *8  utc                       ! UTC time of event          [mjd]
        integer*4  charge                    ! incident particle: charge  
        integer*4  nucleons                  !    nucleons 
        integer*4  mirrors                   ! total number of mirrors   
        real   *4  momentum                  ! incident particle: momentum  [eV]
        real   *4  elevation                 !    elevation (up=90)        [deg]
        real   *4  azimuth                   !    azimuth   (E=0,N=90)     [deg]
        real   *4  height                    ! height above sea level        [m]
        real   *4  radius  (  gdf_mirror_max)! mirror: radius                [m]
        real   *4  position(3,gdf_mirror_max)! geographic         {x ,y ,z } [m]
        real   *4  location(3,gdf_mirror_max)! shower coordinates {x',y',z'} [m]
        real   *4  time    (  gdf_mirror_max)! relative arrival time        [ns]
        real   *4  align                     ! # dummy word for 64-bit alignment
        logical*4  new                       ! .TRUE. if just read 
        logical*4  valid                     ! .TRUE. if contents valid 
      end type gdf_mce_t

      type(gdf_mce_t), target :: gdf_mce

C-------------------------------------------------------------------------------
C     Monte Carlo photons hitting individual mirrors        
C-------------------------------------------------------------------------------
      integer, parameter :: gdf_photon_max=20000  ! max. possible # of photons

C---- a sub-structure for photons to be used below 
      type :: gdf_photon_t
        sequence  
        real   *4  xu                        ! x unit velocity    (shower) 
        real   *4  yu                        ! y unit velocity    (shower) 
        real   *4  xi                        ! horiz impact point (shower)   [m]
        real   *4  yi                        ! vert. impact point (shower)   [m]
        real   *4  xf                        ! x-pos focal plane             [m]
        real   *4  yf                        ! y-pos focal plane             [m]
        real   *4  energy                    ! photon energy                [eV]
        real   *4  height                    ! emission height    (geo)      [m]
        real   *4  time                      ! rel. arrival time            [ns]
        real   *4  weight                    ! Monte Carlo weight  
      end type gdf_photon_t 

C---- the final structure including all photons 
      type :: gdf_mcp_t 
        sequence  
        integer*4  version                   ! software version number 
        integer*4  reserved                  ! # for future use 
        integer*4  checksum(2)               ! # data part check sum 
        real   *8  dummy                     ! # would normally be UTC time 
        integer*4  photons                   ! actual number of photons  
        integer*4  mirror(gdf_photon_max)    ! mirror number
        type(gdf_photon_t) ::                   ! use this structure for 
     .             photon(gdf_photon_max)    !   this many photons  
        real   *4  align                     ! # dummy word for 64-bit alignment
        logical*4  new                       ! .TRUE. if just read 
        logical*4  valid                     ! .TRUE. if contents valid 
      end type gdf_mcp_t

      type(gdf_mcp_t), target :: gdf_mcp

C-------------------------------------------------------------------------------
C     force all data structure into a sequence in memory 
C-------------------------------------------------------------------------------
      INTEGER*4 GDF_DATA_FIRST    ! first 32-bit word of COMMON 
      INTEGER*4 GDF_DATA_ALIGN    ! to get 64-bit alignment 
      INTEGER*4 GDF_DATA_LAST     ! last  word of COMMON 

      COMMON / GDF_DATA /         ! COMMON containing all records 
     +  GDF_DATA_FIRST,   
     +  GDF_DATA_ALIGN,   
     +  GDF_RUN, 
     +  GDF_CCD,  GDF_TRACK, GDF_HV, 
     +  GDF_FR10, GDF_EV10,
     +  GDF_FR11, GDF_EV11,
     +  GDF_MCE,  GDF_MCP,           
     +  GDF_CAL,                     
     +  GDF_DATA_LAST   


C-------------------------------------------------------------------------------
C     flags for movement of data between Fortran structure and ZEBRA 
C-------------------------------------------------------------------------------
      INTEGER, PARAMETER,PRIVATE :: 
     .  GDF_MOVE_ZEBRA    =1, 
     .  GDF_MOVE_FORTRAN  =2

C-------------------------------------------------------------------------------
C
C-------------------------------------------------------------------------------

      INTEGER, PARAMETER, PRIVATE ::
     .  GDF_TYPE_CHARACTER=1,
     .  GDF_TYPE_BINARY16 =2,
     .  GDF_TYPE_BINARY32 =3,
     .  GDF_TYPE_DOUBLE   =4,
     .  GDF_TYPE_INTEGER  =5,
     .  GDF_TYPE_REAL     =6 

C-------------------------------------------------------------------------------
C     cross reference between fortran records and Zebra banks 
C-------------------------------------------------------------------------------
      INTEGER, PARAMETER  :: GDF_DATA_MAX=1024*256 ! total size of structures
      INTEGER*4  GDF_DATA_I(0:GDF_DATA_MAX)    
      LOGICAL*4  GDF_DATA_L(0:GDF_DATA_MAX)      

      EQUIVALENCE (GDF_DATA_ALIGN,GDF_DATA_I(0))  ! first data word within a  
      EQUIVALENCE (GDF_DATA_ALIGN,GDF_DATA_L(0))  !   structure gets index 1


      INTEGER, PARAMETER :: 
     .  GDF_XREF_RUN     =  1,
     .  GDF_XREF_HV   _10=  2,
     .  GDF_XREF_HV   _11=  3,
     .  GDF_XREF_TRACK_10=  4,
     .  GDF_XREF_TRACK_11=  5,
     .  GDF_XREF_CCD  _10=  6,
     .  GDF_XREF_CCD  _11=  7,
     .  GDF_XREF_FR10    =  8,
     .  GDF_XREF_EV10    =  9,
     .  GDF_XREF_FR11    = 10,
     .  GDF_XREF_EV11    = 11,
     .  GDF_XREF_MAX     = 15 

      TYPE :: GDF_XREF_T  
        INTEGER FIRST             ! adsress of first word       [32bit] 
        INTEGER NEW               ! address of first word after [32bit] 
        INTEGER BANK              ! corresponding ZEBRA bank number index 
      END TYPE GDF_XREF_T 
      
      TYPE(GDF_XREF_T),PRIVATE :: GDF_XREF(GDF_XREF_MAX) 

C-------------------------------------------------------------------------------
C     calibration method  
C-------------------------------------------------------------------------------
      INTEGER,PARAMETER ::
     .  GDF_CAL_MEDIAN   =1,   ! Tucson classic 
     .  GDF_CAL_FIT      =2,   ! chi^2 or log-likelihood 
     .  GDF_CAL_MUON     =3,   ! muon rings 
     .  GDF_CAL_NIGHT_SKY=4    ! night sky pedestals 
C-------------------------------------------------------------------------------
C     tube calibration status 
C-------------------------------------------------------------------------------
      INTEGER, PARAMETER :: 
     .  GDF_CAL_NOPED  =1,        ! pedestal invalid 
     .  GDF_CAL_NOGAIN =2         ! gain invalid 
C-------------------------------------------------------------------------------
C     high voltage status/mode 
C-------------------------------------------------------------------------------
*
*     to be defined 
*
C-------------------------------------------------------------------------------
C     definition of ZEBRA store
C-------------------------------------------------------------------------------
      INTEGER, PARAMETER ::
     .  GDF_STORE_SIZE  =  4*1024*256,     ! total size of store, four  Mbyte   
     .  GDF_STORE_RL_MAX=   100,           ! max reference links
     .  GDF_STORE_SL_MAX=   100            ! max structural links 

      INTEGER      GDF_STORE_INDEX                    ! returned by MZSTOR
      CHARACTER(8) GDF_STORE_NAME                     ! name of COMMON 
      INTEGER      GDF_STORE_FENCE(100)               ! to detect errors
      INTEGER      GDF_STORE_LQ(GDF_STORE_SIZE)       ! to address as link 
      INTEGER      GDF_STORE_IQ(GDF_STORE_SIZE-8)     ! to address as INTEGER
      REAL         GDF_STORE_Q (GDF_STORE_SIZE-8)     ! to address as REAL 
      INTEGER      GDF_STORE_RL(GDF_STORE_RL_MAX)     ! reference links 
      INTEGER      GDF_STORE_SL(GDF_STORE_SL_MAX)     ! structural links 

      COMMON / GDFC / 
     .  GDF_STORE_FENCE, 
     .  GDF_STORE_LQ                                 !   

      EQUIVALENCE                                    ! shift links by +8 
     . (GDF_STORE_LQ(9),
     .  GDF_STORE_IQ(1),
     .  GDF_STORE_Q(1))                 
      EQUIVALENCE                                    ! structural links 
     .  (GDF_STORE_SL(1),
     .   GDF_STORE_LQ(1))  
      EQUIVALENCE                                    ! reference links 
     . (GDF_STORE_RL(1),
     .  GDF_STORE_LQ(1+GDF_STORE_SL_MAX+1)) 

C-------------------------------------------------------------------------------
C     user divisons inside the store 
C-------------------------------------------------------------------------------
      INTEGER, PARAMETER ::
     +             GDF_DIV_RUN  =1,
     +             GDF_DIV_FRAME=2,
     +             GDF_DIV_EVENT=3,                      ! always last 
     +             GDF_DIV_MAX  =3                       ! 
      INTEGER      GDF_DIV_INDEX(GDF_DIV_MAX)            ! returned index 
      CHARACTER*8  GDF_DIV_NAME (GDF_DIV_MAX)            ! a name 
      CHARACTER*4  GDF_DIV_TYPE (GDF_DIV_MAX)            ! divison type 
      INTEGER      GDF_DIV_SIZE (GDF_DIV_MAX)            ! division size 
      INTEGER      GDF_DIV_SMALL                         ! smallest div size  
      PARAMETER   (GDF_DIV_SMALL=GDF_STORE_SIZE/10) 
      INTEGER      GDF_DIV_LIMIT                         ! largest div size  
      PARAMETER   (GDF_DIV_LIMIT=3*GDF_STORE_SIZE/4) 
C-------------------------------------------------------------------------------
C     bank characteristics 
C-------------------------------------------------------------------------------
      INTEGER, PARAMETER ::
     +  GDF_BANK_RUN     =  1, 
     +  GDF_BANK_CCD     =  2,
     +  GDF_BANK_TRACK   =  3, 
     +  GDF_BANK_HV      =  4,
     +  GDF_BANK_FRAME10 =  5,
     +  GDF_BANK_EVENT10 =  6,
     +  GDF_BANK_FRAME11 =  7,
     +  GDF_BANK_EVENT11 =  8, 
     +  GDF_BANK_MCE     =  9, 
     +  GDF_BANK_MCP     = 10, 
     +  GDF_BANK_CAL     = 11, 
     +  GDF_BANK_MAX     = 11 
                    
      CHARACTER* 4 GDF_BANK_NAME    (GDF_BANK_MAX)   ! bank identifier  
      CHARACTER*40 GDF_BANK_FORMAT  (GDF_BANK_MAX)   ! bank format 
      INTEGER*4    GDF_BANK_IDH     (GDF_BANK_MAX)   ! Hollerith ID 
      INTEGER      GDF_BANK_LOC     (GDF_BANK_MAX)   ! location in memory   
      INTEGER      GDF_BANK_NS      (GDF_BANK_MAX)   ! sector descriptors 
      INTEGER      GDF_BANK_ND      (GDF_BANK_MAX)   ! actual data words
      INTEGER      GDF_BANK_MD      (GDF_BANK_MAX)   ! maximum data words 
      INTEGER      GDF_BANK_IXIO    (GDF_BANK_MAX)   ! IO characteristic
      INTEGER      GDF_DIV_CHOICE   (GDF_BANK_MAX)

C---- everything appart fron the Zebra COMMON block themselfs 
       COMMON / GDF_ZEBRA / 
     +  GDF_STORE_INDEX,                            ! store 
     +  GDF_STORE_NAME,                             ! store name  
     +  GDF_DIV_INDEX,                              ! division 
     +  GDF_DIV_NAME,                               ! division 
     +  GDF_DIV_SIZE,                               ! division 
     +  GDF_DIV_TYPE,                               ! division 
     +  GDF_DIV_CHOICE,                             ! division choice  
     +  GDF_BANK_LOC,                               ! bank location 
     +  GDF_BANK_IDH,                               ! bank ID 
     +  GDF_BANK_NS,    
     +  GDF_BANK_ND,                                ! actual bank data words 
     +  GDF_BANK_MD,                                ! max bank data words 
     +  GDF_BANK_NAME,
     +  GDF_BANK_FORMAT,
     +  GDF_BANK_IXIO,
     +  GDF_AREA_NAME 
C-------------------------------------------------------------------------------
C     permanent link area 
C-------------------------------------------------------------------------------
      INTEGER     GDF_AREA_SIZE                      ! # of links 
      PARAMETER  (GDF_AREA_SIZE=100) 
      INTEGER     GDF_AREA(GDF_AREA_SIZE)            ! the array it self 
      CHARACTER*8 GDF_AREA_NAME                      ! name of COMMON 

      COMMON   / GDFPLA / GDF_AREA           

C-------------------------------------------------------------------------------
C     error diagnostics, ZEBRA return codes 
C-------------------------------------------------------------------------------
      INTEGER IQUEST(100) 
      COMMON / QUEST / IQUEST

C-------------------------------------------------------------------------------
C     FZ file header format 
C-------------------------------------------------------------------------------
      INTEGER  
     + GDF_HEADER_LEN,          ! actual header length 
     + GDF_HEADER_TYPE,         ! record type 
     + GDF_HEADER_RUN,          ! run number 
     + GDF_HEADER_MAX           ! max header length 

      PARAMETER 
     + (GDF_HEADER_TYPE     = 1, 
     +  GDF_HEADER_RUN      = 2,
     +  GDF_HEADER_MAX      = 2)

      INTEGER GDF_HEADER(GDF_HEADER_MAX) 

C-------------------------------------------------------------------------------
C     modified Julian days for begining of each Gregorian year  
C-------------------------------------------------------------------------------
      INTEGER      YEAR_MIN
      INTEGER      YEAR_MAX
      PARAMETER   (YEAR_MIN=1985)
      PARAMETER   (YEAR_MAX=2004)
      REAL*8       MJD_YEAR(YEAR_MIN:YEAR_MAX)
     
      DATA MJD_YEAR(1985) / 46066D0 /
      DATA MJD_YEAR(1986) / 46431D0 /
      DATA MJD_YEAR(1987) / 46796D0 /
      DATA MJD_YEAR(1988) / 47161D0 /
      DATA MJD_YEAR(1989) / 47527D0 /
      DATA MJD_YEAR(1990) / 47892D0 /
      DATA MJD_YEAR(1991) / 48257D0 /
      DATA MJD_YEAR(1992) / 48622D0 /
      DATA MJD_YEAR(1993) / 48988D0 /
      DATA MJD_YEAR(1994) / 49353D0 /
      DATA MJD_YEAR(1995) / 49718D0 /
      DATA MJD_YEAR(1996) / 50083D0 /
      DATA MJD_YEAR(1997) / 50449D0 /
      DATA MJD_YEAR(1998) / 50814D0 /
      DATA MJD_YEAR(1999) / 51179D0 /
      DATA MJD_YEAR(2000) / 51544D0 /
      DATA MJD_YEAR(2001) / 51910D0 /
      DATA MJD_YEAR(2002) / 52275D0 /
      DATA MJD_YEAR(2003) / 52640D0 /
      DATA MJD_YEAR(2004) / 53005D0 /

C-------------------------------------------------------------------------------
C     time and performance mesurements 
C-------------------------------------------------------------------------------
      TYPE, PRIVATE :: GDF_TIME_T 
        INTEGER FZOUT  
      END TYPE 

      TYPE(GDF_TIME_T), PRIVATE :: GDF_TIME 
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

C-------------------------------------------------------------------------------
C    old BLOCK DATA 
C-------------------------------------------------------------------------------
      
      DATA  GDF_AREA_NAME / '/GDFPLA/' /       
      DATA  GDF_STORE_NAME / '/GDF/' / 
      DATA  GDF_DIV_NAME  / 'RUN', 'FRAME',  'EVENT'       /    
      DATA  GDF_DIV_SIZE(GDF_DIV_RUN)    /  GDF_DIV_SMALL  /     
      DATA  GDF_DIV_SIZE(GDF_DIV_FRAME)  /  GDF_DIV_SMALL  /     
      DATA  GDF_DIV_SIZE(GDF_DIV_EVENT)  /  GDF_DIV_LIMIT  /     
      DATA  GDF_DIV_TYPE  / 'L',    'L',  ' ' /               ! L=long term

      DATA         
     .  GDF_BANK_NS(GDF_BANK_RUN    )  /  5 / 
     .  GDF_BANK_NS(GDF_BANK_CCD    )  /  3 /
     .  GDF_BANK_NS(GDF_BANK_TRACK  )  /  4 /
     .  GDF_BANK_NS(GDF_BANK_HV     )  /  0 / ! <<< 
     .  GDF_BANK_NS(GDF_BANK_FRAME10)  /  8 / 
     .  GDF_BANK_NS(GDF_BANK_EVENT10)  /  4 / 
     .  GDF_BANK_NS(GDF_BANK_EVENT11)  /  5 / 
     .  GDF_BANK_NS(GDF_BANK_FRAME11)  / 10/ 
     .  GDF_BANK_NS(GDF_BANK_MCE    )  /  0 / ! <<<
     .  GDF_BANK_NS(GDF_BANK_MCP    )  /  0 / ! <<<
     .  GDF_BANK_NS(GDF_BANK_CAL    )  /  0 / ! <<<  

      DATA         
     .  GDF_BANK_FORMAT(GDF_BANK_RUN    )  / '2I 2B 2D -S' / 
     .  GDF_BANK_FORMAT(GDF_BANK_CCD    )  / '2I 2B 2D -S' / 
     .  GDF_BANK_FORMAT(GDF_BANK_TRACK  )  / '2I 2B 2D -S' / 
     .  GDF_BANK_FORMAT(GDF_BANK_HV     )  / '2I 2B 2D -S' / 
     .  GDF_BANK_FORMAT(GDF_BANK_FRAME10)  / '2I 2B 2D -S' / 
     .  GDF_BANK_FORMAT(GDF_BANK_EVENT10)  / '2I 2B 2D -S' / 
     .  GDF_BANK_FORMAT(GDF_BANK_EVENT11)  / '2I 2B 2D -S' / 
     .  GDF_BANK_FORMAT(GDF_BANK_FRAME11)  / '2I 2B 2D -S' / 
     .  GDF_BANK_FORMAT(GDF_BANK_MCE    )  / '2I 2B 2D -S' / 
     .  GDF_BANK_FORMAT(GDF_BANK_MCP    )  / '2I 2B 2D -S' / 
     .  GDF_BANK_FORMAT(GDF_BANK_CAL    )  / '2I 2B 2D -S' / 

      DATA         
     .  GDF_DIV_CHOICE(GDF_BANK_RUN    )  / GDF_DIV_RUN   / 
     .  GDF_DIV_CHOICE(GDF_BANK_CCD    )  / GDF_DIV_RUN   / 
     .  GDF_DIV_CHOICE(GDF_BANK_TRACK  )  / GDF_DIV_RUN   / 
     .  GDF_DIV_CHOICE(GDF_BANK_HV     )  / GDF_DIV_FRAME / 
     .  GDF_DIV_CHOICE(GDF_BANK_FRAME10)  / GDF_DIV_FRAME / 
     .  GDF_DIV_CHOICE(GDF_BANK_FRAME11)  / GDF_DIV_FRAME / 
     .  GDF_DIV_CHOICE(GDF_BANK_EVENT10)  / GDF_DIV_EVENT / 
     .  GDF_DIV_CHOICE(GDF_BANK_EVENT11)  / GDF_DIV_EVENT / 
     .  GDF_DIV_CHOICE(GDF_BANK_MCE    )  / GDF_DIV_EVENT / 
     .  GDF_DIV_CHOICE(GDF_BANK_MCP    )  / GDF_DIV_EVENT / 
     .  GDF_DIV_CHOICE(GDF_BANK_CAL    )  / GDF_DIV_RUN   / 

      DATA         
     .  GDF_BANK_IXIO  / GDF_BANK_MAX * 0 /    ! IO characteristic
      DATA 
     .  GDF_BANK_NAME(GDF_BANK_RUN    ) / 'RUUR'  / ! RUn 
     .  GDF_BANK_NAME(GDF_BANK_CCD    ) / 'CCCC'  / ! CCd camera  
     .  GDF_BANK_NAME(GDF_BANK_TRACK  ) / 'TRRT'  / ! TRacking  
     .  GDF_BANK_NAME(GDF_BANK_HV     ) / 'HVVH'  / ! High Voltage  
     .  GDF_BANK_NAME(GDF_BANK_FRAME10) / 'FTTF'  / ! Frame Ten  
     .  GDF_BANK_NAME(GDF_BANK_EVENT10) / 'ETTE'  / ! Event Ten  
     .  GDF_BANK_NAME(GDF_BANK_FRAME11) / 'FEEF'  / ! Frame Eleven 
     .  GDF_BANK_NAME(GDF_BANK_EVENT11) / 'EEEE'  / ! Event Eleven 
     .  GDF_BANK_NAME(GDF_BANK_MCE    ) / 'MEEM'  / ! MC Event  
     .  GDF_BANK_NAME(GDF_BANK_MCP    ) / 'MPPM'  / ! MC Photons  
     .  GDF_BANK_NAME(GDF_BANK_CAL    ) / 'CAAC'  / ! CAlibration  

CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

      INTERFACE GDF$PRINT
 
       MODULE PROCEDURE  
     .    GDF$PRINT_CCD, 
     .    GDF$PRINT_EVENT10,
     .    GDF$PRINT_EVENT11,
     .    GDF$PRINT_FRAME10,
     .    GDF$PRINT_FRAME11,
     .    GDF$PRINT_HV, 
     .    GDF$PRINT_RUN, 
     .    GDF$PRINT_TRACK 

      END INTERFACE 

CDOC gdf$print.tex 
C
C     \subsection*{Print content of derived data type structures}
C     \begin{description}
C     \item{\sf Fortran binding:} call as subroutine 
CVER-7
C     SUBROUTINE GDF$PRINT(DATA,IERR) 
CTEX
C     \item{\sf Action:} \\
C     Prints data in derived type data structures. 
C     \Rind{GDF\$PRINT} is defined as a Fortran90 INTERFACE with 
C     module procedures for run, event, frame, CCD and tracking data.   
C     \verb!DATA! can be of type 
C     \verb!GDF_RUN!, \verb!GDF_EV10!, \verb!GDF_EV11!, 
C     \verb!GDF_FR10!, \verb!GDF_FR11!, \verb!GDF_CCD!, or
C     \verb!GDF_TRACK!.
C
C     \end{description}
CEND 
C-------------------------------------------------------------------------------
C
C-------------------------------------------------------------------------------

      INTERFACE GDF$MOVE 

        MODULE PROCEDURE  
     .    GDF$MOVE_I2,             ! 16-bit integer 
     .    GDF$MOVE_I4,             ! 32-bit integer 
     .    GDF$MOVE_L4,             ! 32-bit logical, bit pattern 
     .    GDF$MOVE_R4,             ! 32-bit real 
     .    GDF$MOVE_R8,             ! 64-bit real 
     .    GDF$MOVE_C1              ! character string 

      END INTERFACE 

CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      CONTAINS 
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

      SUBROUTINE GDF$CCD(IREC,FLAG,STORE,NW,IERR)

C-------------------------------------------------------------------------------
C     
C-------------------------------------------------------------------------------
      IMPLICIT NONE 
 
C---- 
      INTEGER IREC 
      INTEGER FLAG       
      INTEGER STORE(*)
      INTEGER NW 
      INTEGER IERR
C----
      INTEGER N_B 
      INTEGER IT 

      CHARACTER(*),PARAMETER  :: SRN='GDF$CCD' 

C---- how many header words?
      IF (STORE(1).GE.27) THEN 
        NW = 7
      ELSE 
        NW = 6
      ENDIF 

C---- which telescope?
      IF (IREC.EQ.GDF_XREF_CCD_10) THEN 
        IT = GDF_TELE_10 
      ELSE IF (IREC.EQ.GDF_XREF_CCD_11) THEN 
        IT = GDF_TELE_11 
      ELSE 
        MSG  = 'Unknown CCD data structure.'        ! error message  
        IERR = GDF$ERROR(SRN,MSG,1)                 ! complain 
        RETURN                                      ! refuse to continue 
      ENDIF                                         

C---- actual/max array length 
      IF (FLAG.EQ.GDF_MOVE_ZEBRA) THEN                ! into ZEBRA 
        N_B = GDF_CCD(IT)%NSTAR*3
      ELSE IF (FLAG.EQ.GDF_MOVE_FORTRAN) THEN         ! into FORTRAN  
        N_B = GDF_CCD_NSTAR_MAX*3 
      ENDIF 

      CALL GDF$MOVE(FLAG,  3,GDF_CCD(IT)%TELESCOPE  ,NW,STORE)
      CALL GDF$MOVE(FLAG,  8,GDF_CCD(IT)%EXPOSURE   ,NW,STORE)
      CALL GDF$MOVE(FLAG,N_B,GDF_CCD(IT)%STAR(1:3,1),NW,STORE)

      RETURN 
      END SUBROUTINE GDF$CCD
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
CDOC gdf$close.tex 
C     \subsection*{Close file}
C     \begin{description}
C       \item{\sf Fortran binding}
CVER-7 
      SUBROUTINE GDF$CLOSE(UNIT,IERR)
C-------------------------------------------------------------------------------
C-    de-initialize package   
C-------------------------------------------------------------------------------
      IMPLICIT NONE 

      INTEGER   UNIT          ! unit number 
      INTEGER   IERR          ! return code 
CTEX
C      \item{\sf Action:} \\ 
C        Closes the currently open file 
C      \end{description}
CEND
C---- 
      CHARACTER(*),PARAMETER  :: SRN='GDF$CLOSE' 

C---- comunication area       
      INTEGER           INFLUN, INFSTA, INFOFZ
      COMMON / FZSTAT / INFLUN, INFSTA, INFOFZ(40)

C---- misc 
      INTEGER DUMMY 

C-------------------------------------------------------------------------------
C     reaction to IERR.NE.0 condition 
C-------------------------------------------------------------------------------
      IF (IERR.NE.0) THEN 
        IF (GDF_OPTION%RESET) IERR = 0 
        IF (GDF_OPTION%ABORT) RETURN 
      ENDIF  
 

      CALL FZINFO(UNIT)
      IF (INFLUN.NE.UNIT) THEN 
        MSG  = 'Can not obtain status information.'
        IERR = GDF$ERROR(SRN,MSG,1)
        RETURN 
      END IF 

      IF (BTEST(INFSTA,11)) THEN                  ! write permission?
        CALL FZRUN (UNIT,-1,0,DUMMY)              ! write end of run 
        CALL FZENDO(UNIT,'QT')                    ! close output. T=terminate
        CLOSE(UNIT) 
      ELSE IF (BTEST(INFSTA,10)) THEN             ! read permission 
        CALL FZENDI(UNIT,'QT')                    ! close input file. Q=quiet
        IF (GDF_CFILE%OPEN) THEN                  ! is it a c-io file?
          CALL CFCLOS(                            ! use c-io routine to close it
     .      GDF_CFILE%LUNDES,                     ! file descriptor 
     .      GDF_CFILE%MEDIUM)                     ! physical medium 
          GDF_CFILE%OPEN = .FALSE.                ! reset file is open flag 
        ELSE                                      ! use normal fortran io 
          CLOSE(UNIT)                             ! commnd to close file 
        END IF  
      ELSE                                        ! whoops, no read, no write?
        MSG = 'Neither input nor output file.'    ! error message 
        IERR = GDF$ERROR(SRN,MSG,2)               ! set error code 
      END IF 

      RETURN 
      END SUBROUTINE GDF$CLOSE 

C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C\Shubrz{CALL GDF\$CLOSE}{(NUNIT,IERR*)}
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C\Action  Closes the current file on logical unit {\tt LUNIT} 
C\Pdesc 
C\begin{DLtt}{MMMMMM}
C\item[NUNIT]  logical unit  
C\item[IERR]   return code  
C\end{DLtt}
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      INTEGER FUNCTION GDF$ERROR(SRN,MSG,IERR)
C-------------------------------------------------------------------------------
C     output error messages and return error code  
C-------------------------------------------------------------------------------
      IMPLICIT NONE 

      CHARACTER(*) SRN              ! subroutine name 
      CHARACTER(*) MSG              ! error message 
      INTEGER      IERR             ! erro code 

C----
      INTEGER LNBLNK                ! external, Cernlib 
      INTEGER L1,L2
      

      L1 = MIN(18,LNBLNK(SRN))    
      L2 = MIN(50,LNBLNK(MSG))

      IF (L1.LT.1.OR.L2.LT.1) THEN 
        PRINT*,'Error: Can not format error message.'
        PRINT*,SRN,MSG,IERR
        RETURN 
      ELSE
        PRINT 1000,SRN(1:L1),MSG(1:L2),IERR 
 1000   FORMAT(1X,'Error in ',A,':',1X,A,T75,I5)
      ENDIF 

      GDF$ERROR = IERR 

      RETURN 
      END FUNCTION GDF$ERROR
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

      SUBROUTINE GDF$EVENT10(FLAG,STORE,NW)
C-------------------------------------------------------------------------------
C     move 10m event record into Zebra bank format and back           
C-------------------------------------------------------------------------------
      IMPLICIT NONE 
 
C---- 
      INTEGER STORE(*)
      INTEGER FLAG 
      INTEGER NW 
C----
      INTEGER  N_TRG                   ! number of 32bit trigger patterns
      INTEGER  N_B2                    ! number of 16bit data words 
      INTEGER  N_ADC                   ! number of ADC data words 
C-------------------------------------------------------------------------------
C
C-------------------------------------------------------------------------------
      IF (FLAG.EQ.GDF_MOVE_ZEBRA) THEN                 ! into ZEBRA 
        N_ADC = GDF_EV10%NADC                          ! actual ADCs
        N_TRG = GDF_EV10%NTRG                          ! actual trigger words
      ELSE IF (FLAG.EQ.GDF_MOVE_FORTRAN) THEN          ! into FORTRAN  
        N_ADC = GDF_EV10_NADC                          ! max ADCs 
        N_TRG = GDF_EV10_NTRG                          ! max trigger words 
      ENDIF 

      N_B2 =  8 + GDF_EV10_NPHS   + GDF_EV10_NBRST     ! 16 bit binary 

      IF (STORE(1).GE.74) THEN                         ! variable trigger?
        NW = 7                                         ! pointer first word 
        CALL GDF$MOVE(FLAG,   20,GDF_EV10%NADC      ,NW,STORE)
        CALL GDF$MOVE(FLAG,    7,GDF_EV10%TRIGGER   ,NW,STORE)
        IF (GDF_EV10%NTRG.GT.0) THEN   
          CALL GDF$MOVE(FLAG,N_TRG,GDF_EV10%PATTERN(1),NW,STORE)
        ENDIF 
        IF (GDF_EV10%NADC.GT.0) THEN   
          CALL GDF$MOVE(FLAG,N_ADC,GDF_EV10%ADC,NW,STORE)
        ENDIF 
        CALL GDF$MOVE(FLAG,N_B2,GDF_EV10%GPS_CLOCK,NW,STORE)

      ELSE IF (STORE(1).GE.27) THEN                    ! variable ADC length?
        NW = 7                                         ! pointer first word 
        CALL GDF$MOVE(FLAG,    7,GDF_EV10%TRIGGER   ,NW,STORE)
        CALL GDF$MOVE(FLAG,   13,GDF_EV10%NADC      ,NW,STORE)
        CALL GDF$MOVE(FLAG,N_ADC,GDF_EV10%ADC       ,NW,STORE)
C    
C       For some reason the Fortran77 version used a mixture 
C       of 16-bit and 32-bit integers for this ZEBRA sector. 
C       In the Fortran90 version the whole sector now consists 
C       of 16-bit integers. So they can all be moved as one block.  
C
        CALL GDF$MOVE(FLAG, N_B2 ,GDF_EV10%GPS_CLOCK ,NW,STORE)
 
      ELSE 
        NW = 6                                         ! pointer first word 
        CALL GDF$MOVE(FLAG,   7,GDF_EV10%TRIGGER   ,NW,STORE)
        CALL GDF$MOVE(FLAG,  13,GDF_EV10%NADC      ,NW,STORE)
        CALL UCOPY(STORE(NW+ 1),GDF_EV10%GPS_CLOCK, 2)
        CALL UCOPY(STORE(NW+ 3),GDF_EV10%ADC      ,60)
        CALL UCOPY(STORE(NW+33),GDF_EV10%PHS      , 4)
        CALL UCOPY(STORE(NW+37),GDF_EV10%BURST    , 6)

        IF (GDF_OPTION%SHIFT16) THEN 
          CALL GDF$SHIFTC(GDF_EV10%ADC,120)                  ! swap 16-bit
        ENDIF 

      ENDIF 

      RETURN 
      END SUBROUTINE GDF$EVENT10 
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

      SUBROUTINE GDF$EVENT11(FLAG,STORE,NW)
C-------------------------------------------------------------------------------
C     move 11m event record into Zebra bank format and back           
C-------------------------------------------------------------------------------
      IMPLICIT NONE 
 
C---- 
      INTEGER STORE(*)
      INTEGER FLAG 
      INTEGER NW 
C----
      INTEGER N_B1,N_B2,N_I,N_ADC,N_TDC 
C-------------------------------------------------------------------------------
C
C-------------------------------------------------------------------------------
      IF (FLAG.EQ.GDF_MOVE_ZEBRA) THEN                 ! into ZEBRA 
        N_ADC = GDF_EV11%NADC                          ! actual ADCs
        N_TDC = GDF_EV11%NTDC                          !        TDCs
      ELSE IF (FLAG.EQ.GDF_MOVE_FORTRAN) THEN          ! into FORTRAN  
        N_ADC = GDF_EV11_NADC                          ! max ADCs 
        N_TDC = GDF_EV11_NTDC                          !     TDCs 
      ENDIF 

      NW   =  7                                        ! pointer first word 
      N_B1 =  7                                        ! 32 bit binary 
      N_I  = 14                                        ! integer 
***   N_B2 =  3 + GDF_EV11_NPHS/2 + GDF_EV11_NBRST/2   ! 32 bit binary 
      N_B2 =  6 + GDF_EV11_NPHS   + GDF_EV11_NBRST     ! 32 bit binary 

      CALL GDF$MOVE(FLAG,N_B1 ,GDF_EV11%TRIGGER    ,NW,STORE)
      CALL GDF$MOVE(FLAG,N_I  ,GDF_EV11%NBRST      ,NW,STORE)
      CALL GDF$MOVE(FLAG,N_ADC,GDF_EV11%ADC        ,NW,STORE)
      CALL GDF$MOVE(FLAG,N_TDC,GDF_EV11%TDC        ,NW,STORE)
      CALL GDF$MOVE(FLAG,N_B2 ,GDF_EV11%GEOS_CLOCK ,NW,STORE)

      RETURN 
      END SUBROUTINE GDF$EVENT11
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
CDOC gdf$exit.tex 
C
C    \subsection*{Terminate} 
C    \begin{description} 
C    \item{\sf Fortran binding:} 
CVER-7
      SUBROUTINE GDF$EXIT(IERR)
C-------------------------------------------------------------------------------
C-      
C-------------------------------------------------------------------------------
      IMPLICIT NONE 
      INTEGER       IERR 
CTEX
C     \item{\sf Action:} \\
C     Last call to terminate all operations.
C     Resources like Fortran units and files are released. 
C     However files should normaly be closed by calling the 
C     appropriate routine first. 
C     \end{description}
CEND

      RETURN
      END SUBROUTINE GDF$EXIT 
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

      SUBROUTINE GDF$FRAME10(FLAG,STORE,NW)

C-------------------------------------------------------------------------------
C     
C-------------------------------------------------------------------------------
      IMPLICIT NONE 

      INTEGER FLAG       
      INTEGER STORE(*)
      INTEGER NW 
C----
      INTEGER N_ADC,N_TDC,N_SCA,N_PHS,N_B
      INTEGER B16,B32,INT 

      PARAMETER 
     . (B16 = GDF_TYPE_BINARY16,
     .  B32 = GDF_TYPE_BINARY32,
     .  INT = GDF_TYPE_INTEGER)  

C---- 
      IF (FLAG.EQ.GDF_MOVE_ZEBRA) THEN                ! into ZEBRA 
        N_ADC = GDF_FR10%NADC                         ! actual # of ADCs
        N_SCA = GDF_FR10%NSCA                         !             scalers 
      ELSE IF (FLAG.EQ.GDF_MOVE_FORTRAN) THEN         ! into FORTRAN  
        N_ADC = GDF_FR10_NADC                         ! max # of ADCs 
        N_SCA = GDF_FR10_NSCA                         ! max # of scalers 
      ENDIF 

      N_B = 6 + 2 * GDF_FR10_NPHS                         

      IF (STORE(1).GE.80) THEN 
        NW  = 7
        CALL GDF$MOVE(FLAG,    2,GDF_FR10%STATUS   ,NW,STORE)
        CALL GDF$MOVE(FLAG,    8,GDF_FR10%NPHS     ,NW,STORE)
        CALL GDF$MOVE(FLAG,N_SCA,GDF_FR10%SCALS    ,NW,STORE)
        CALL GDF$MOVE(FLAG,N_B  ,GDF_FR10%GPS_CLOCK,NW,STORE)
      ELSE IF (STORE(1).GE.27) THEN 
        NW  = 7
        CALL GDF$MOVE(FLAG,    2,GDF_FR10%STATUS   ,NW,STORE)
        CALL GDF$MOVE(FLAG,    8,GDF_FR10%NPHS     ,NW,STORE)
        CALL GDF$MOVE(FLAG,N_ADC,GDF_FR10%CAL_ADC  ,NW,STORE)
        CALL GDF$MOVE(FLAG,N_ADC,GDF_FR10%PED_ADC1 ,NW,STORE)
        CALL GDF$MOVE(FLAG,N_ADC,GDF_FR10%PED_ADC2 ,NW,STORE)
        CALL GDF$MOVE(FLAG,N_SCA,GDF_FR10%SCALC    ,NW,STORE)
        CALL GDF$MOVE(FLAG,N_SCA,GDF_FR10%SCALS    ,NW,STORE)
        CALL GDF$MOVE(FLAG,N_B  ,GDF_FR10%GPS_CLOCK,NW,STORE)
      ELSE
        NW  = 6
        CALL GDF$MOVE(FLAG,    2,GDF_FR10%STATUS    ,NW,STORE)
        CALL GDF$MOVE(FLAG,    8,GDF_FR10%NPHS      ,NW,STORE)
        CALL UCOPY(STORE(NW+  1),GDF_FR10%GPS_CLOCK, 2) 
        CALL UCOPY(STORE(NW+  3),GDF_FR10%PHS1     , 8) 
        CALL UCOPY(STORE(NW+ 11),GDF_FR10%CAL_ADC  ,60) 
        CALL UCOPY(STORE(NW+ 71),GDF_FR10%PED_ADC1 ,60) 
        CALL UCOPY(STORE(NW+131),GDF_FR10%PED_ADC2 ,60) 
        CALL UCOPY(STORE(NW+191),GDF_FR10%SCALC    ,64) 
        CALL UCOPY(STORE(NW+255),GDF_FR10%SCALS    ,64) 

        IF (GDF_OPTION%SHIFT16) THEN 
          CALL GDF$SHIFTC(GDF_FR10%CAL_ADC  ,120) 
          CALL GDF$SHIFTC(GDF_FR10%PED_ADC1 ,120) 
          CALL GDF$SHIFTC(GDF_FR10%PED_ADC2 ,120) 
          CALL GDF$SHIFTC(GDF_FR10%SCALC    ,128) 
          CALL GDF$SHIFTC(GDF_FR10%SCALS    ,128) 
        ENDIF 

      ENDIF 

      RETURN 
      END SUBROUTINE GDF$FRAME10
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

      SUBROUTINE GDF$FRAME11(FLAG,STORE,NW)

C-------------------------------------------------------------------------------
C     
C-------------------------------------------------------------------------------
      IMPLICIT NONE 
 
C---- 
      INTEGER FLAG       
      INTEGER STORE(*)
      INTEGER NW 
C----
      INTEGER N_ADC,N_TDC,N_SCA,N_PHS,N_B
      INTEGER B16,B32,INT 

      PARAMETER 
     . (B16 = GDF_TYPE_BINARY16,
     .  B32 = GDF_TYPE_BINARY32,
     .  INT = GDF_TYPE_INTEGER)  

C---- 
      IF (FLAG.EQ.GDF_MOVE_ZEBRA) THEN                ! into ZEBRA 
        N_ADC = GDF_FR11%NADC                         ! actual # of ADCs
        N_TDC = GDF_FR11%NTDC                         !             TDCs
        N_SCA = GDF_FR11%NSCA                         !             scalers 
      ELSE IF (FLAG.EQ.GDF_MOVE_FORTRAN) THEN         ! into FORTRAN  
        N_ADC = GDF_FR11_NADC                         ! max # of ADCs 
        N_TDC = GDF_FR11_NTDC                         ! max # of TDCs 
        N_SCA = GDF_FR11_NSCA                         ! max # of scalers 
      ENDIF 

      NW  = 7
      N_B = 6 + 2*GDF_FR11_NPHS                         

      CALL GDF$MOVE(FLAG,    2,GDF_FR11%STATUS    ,NW,STORE)
      CALL GDF$MOVE(FLAG,    9,GDF_FR11%NPHS      ,NW,STORE)
      CALL GDF$MOVE(FLAG,N_ADC,GDF_FR11%CAL_ADC   ,NW,STORE)
      CALL GDF$MOVE(FLAG,N_ADC,GDF_FR11%PED_ADC1  ,NW,STORE)
      CALL GDF$MOVE(FLAG,N_ADC,GDF_FR11%PED_ADC2  ,NW,STORE)
      CALL GDF$MOVE(FLAG,N_TDC,GDF_FR11%TDC1      ,NW,STORE)
      CALL GDF$MOVE(FLAG,N_TDC,GDF_FR11%TDC2      ,NW,STORE)
      CALL GDF$MOVE(FLAG,N_SCA,GDF_FR11%SCALC     ,NW,STORE)
      CALL GDF$MOVE(FLAG,N_SCA,GDF_FR11%SCALS     ,NW,STORE)
      CALL GDF$MOVE(FLAG,N_B ,GDF_FR11%GEOS_CLOCK,NW,STORE)

      RETURN 
      END SUBROUTINE GDF$FRAME11 
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

      SUBROUTINE GDF$GPS(GPS,GYEAR,UTC,STATUS,IERR)

C-------------------------------------------------------------------------------
C     decode GPS time and return UTC in modified Julian days  
C-------------------------------------------------------------------------------
      IMPLICIT NONE 
 

      CHARACTER(*),PARAMETER  :: SRN='GDF$GPS' 

C---- arguments 
      INTEGER    GPS(3)   ! bcd encoded GPS time 
      INTEGER    GYEAR    ! Gregorian year 
      REAL*8     UTC      ! returned UTC time [mjd]
      INTEGER*4  STATUS   ! returned status bits 
      INTEGER    IERR       

C---- misc  
      INTEGER D1,D2,D3                          ! decimal digits 
      INTEGER YEAR,DAY,HOUR,MIN,SEC,SEC03,SEC06 ! decoded time   


c---- day of the year
      d1   = iand(ishft(gps(1), -6),15)
      d2   = iand(ishft(gps(1),-10),15)
      d3   = iand(ishft(gps(1),-14), 3)
      DAY  = d3*100 + d2*10 + d1      

c---- hour of the day
      d1   = iand(      gps(1),    15)
      d2   = iand(ishft(gps(1),-4), 3)
      HOUR = d2*10 + d1

c---- minutes
      d1   = iand(ishft(gps(2), -9),15)
      d2   = iand(ishft(gps(2),-13), 7)
      MIN  =  d2*10 + d1

c---- seconds
      d1  = iand(ishft(gps(2),-2),15)
      d2  = iand(ishft(gps(2),-6), 7)
      SEC = d2*10 + d1

c---- msecs
      d1 = iand(ishft(gps(3), -6),15)
      d2 = iand(ishft(gps(3),-10),15)
      d3 = iand(ishft(gps(3),-14), 3)
      d3 = ior(d3,ishft(iand(gps(2),3),2))         ! this bcd digit spans words
      SEC03 = d3*100 + d2*10 + d1

c---- error code
      STATUS = iand(ishft(gps(3),-2),15)

c---- quarter msecs
      SEC06  = iand(gps(3),3) * 250

C---- check Gregorian year 
      IF (GYEAR.LT.100) THEN 
        YEAR = GYEAR + 1900 
      ELSE
        YEAR = GYEAR  
      END IF 
      IF (YEAR.LT.YEAR_MIN.OR.YEAR.GT.YEAR_MAX) THEN
        WRITE(MSG,*) 'Illegal Gregorian year: ',GYEAR 
        IERR = GDF$ERROR(SRN,MSG,1)
        RETURN  
      ENDIF 

C---- 
      UTC =   MJD_YEAR(YEAR)
     .      + DFLOAT(DAY) - 1D0               ! -1D0 because 1. January DAY=1 
     .      + DFLOAT(HOUR ) /    24D0  
     .      + DFLOAT(MIN  ) /  1440D0 
     .      + DFLOAT(SEC  ) / 86400D0 
     .      + DFLOAT(SEC03) / 86400D3
     .      + DFLOAT(SEC06) / 86400D6
  
      RETURN 
      END SUBROUTINE GDF$GPS
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

      SUBROUTINE GDF$HV(IREC,FLAG,STORE,NW,IERR)

C-------------------------------------------------------------------------------
C     
C-------------------------------------------------------------------------------
      IMPLICIT NONE 
 
C---- 
      INTEGER IREC 
      INTEGER FLAG       
      INTEGER STORE(*)
      INTEGER NW 
      INTEGER IERR
C----
      INTEGER NCH               ! number of channels 
      INTEGER IT 

      CHARACTER(*),PARAMETER  :: SRN='GDF$HV' 

C---- 
      IF (STORE(1).LE.66) THEN                       ! ignore any previous HV
        NW = 0                                       ! no data returned       
        RETURN                                       ! finished 
      ELSE                                           ! try to deal with data  
        NW = 7                                       ! pointer first word 
      ENDIF 

C---- which telescope?
      IF (IREC.EQ.GDF_XREF_HV_10) THEN 
        IT = GDF_TELE_10 
      ELSE IF (IREC.EQ.GDF_XREF_HV_11) THEN 
        IT = GDF_TELE_11 
      ELSE 
        MSG  = 'Unknown HV data structure.'         ! error message  
        IERR = GDF$ERROR(SRN,MSG,1)                 ! complain 
        RETURN                                      ! refuse to continue 
      ENDIF                                         

C---- actual/max array length 
      IF (FLAG.EQ.GDF_MOVE_ZEBRA) THEN                ! into ZEBRA 
        NCH = GDF_HV(IT)%NCH                          ! status 
      ELSE IF (FLAG.EQ.GDF_MOVE_FORTRAN) THEN         ! into FORTRAN  
        NCH = GDF_HVC_MAX 
      ENDIF 

      CALL GDF$MOVE(FLAG,4,GDF_HV(IT)%TELESCOPE,NW,STORE)  ! first block 

      IF (GDF_HV(IT)%NCH.GT.0) THEN                          ! status values?
        CALL GDF$MOVE(FLAG,NCH,GDF_HV(IT)%STATUS  ,NW,STORE) !   get them ...
        CALL GDF$MOVE(FLAG,NCH,GDF_HV(IT)%V_SET   ,NW,STORE) !  set values 
        CALL GDF$MOVE(FLAG,NCH,GDF_HV(IT)%V_ACTUAL,NW,STORE) !  actual 
        CALL GDF$MOVE(FLAG,NCH,GDF_HV(IT)%I_SUPPLY,NW,STORE) !  actual 
        CALL GDF$MOVE(FLAG,NCH,GDF_HV(IT)%I_ANODE ,NW,STORE) !  anode current 
      ENDIF 

      RETURN 
      END SUBROUTINE GDF$HV 
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
CDOC gdf$init.tex 
C     \subsection*{Initialise package} 
C     \begin{description}
C     \item{\sf Fortran binding:} 
CVER-7  
      SUBROUTINE GDF$INIT(CHOPT,IERR)
C-------------------------------------------------------------------------------
C-    prefrobnications 
C-------------------------------------------------------------------------------
      IMPLICIT NONE 
      CHARACTER(*) CHOPT                        ! selected options 
      INTEGER       IERR                        ! return code 
CTEX 
C     \item{\sf Action:} \\ 
C        Initialize package. 
C        This routine must be called before any other call to GDF. 
C     \item{\sf Options:} \\
C        If Zebra has not already been initialized by some other package
C        such as HBOOK or HIGZ the option {\tt CHOPT='Z'} must be set. 
C        This instructs the routine to call \Rind{MZEBRA} internaly.  
C     \end{description}
CEND 

C---- ZEBRA store 
      INTEGER    IREF                            ! index, first reference link 
      INTEGER    IDAT                            ! index first data word  
      PARAMETER (IREF = GDF_STORE_SL_MAX + 1)    ! start reference links 
      PARAMETER (IDAT = GDF_STORE_RL_MAX + IREF) ! start data region 
C---- 
      CHARACTER(*),PARAMETER  :: SRN='GDF$INIT' 
      INTEGER       I 
C-------------------------------------------------------------------------------
C
C-------------------------------------------------------------------------------
      IF (INDEX(CHOPT,'Z').GE.1) THEN 
        CALL MZEBRA(0) 
      END IF 

C---- create an additional store for the data 
      CALL MZSTOR(
     +  GDF_STORE_INDEX,                           ! returned store index 
     +  GDF_STORE_NAME,                            ! store name 
     +  'Q',                                       ! log level 'QUIET'
     +  GDF_STORE_FENCE,                           ! to detect overflow 
     +  GDF_STORE_LQ(1),                           ! start structural links 
     +  GDF_STORE_LQ(IREF),                        ! start reference links 
     +  GDF_STORE_LQ(IDAT),                        ! start data region 
     +  GDF_STORE_LQ(GDF_STORE_SIZE/2),            ! lowest postion divison 2 
     +  GDF_STORE_LQ(GDF_STORE_SIZE))              ! last word of store 

C---- create a permanent link area 
      CALL MZLINK(
     +  GDF_STORE_INDEX,      
     +  GDF_AREA_NAME,                             ! common name 
     +  GDF_AREA,                                  ! first structural link 
     +  GDF_AREA(GDF_AREA_SIZE),                   ! last sturctural link 
     +  GDF_AREA(1))                               ! no reference links 

C---- ask for extra division inside store 
      DO I=1,GDF_DIV_MAX
        CALL MZDIV(
     +    GDF_STORE_INDEX,                    ! guess ... 
     +    GDF_DIV_INDEX(I),                   ! returned divison index 
     +    GDF_DIV_NAME(I),                    ! division name 
     +    GDF_DIV_SIZE(I)/2,                  ! initial size 
     +    GDF_DIV_SIZE(I),                    ! maximum size 
     +    GDF_DIV_TYPE(I))                    ! type: short or long term 
      END DO 

C---- verfify structure is correct  
*      DO I=1,GDF_DIV_MAX
*        CALL DZVERI(
*     +   'Check of division: '//GDF_DIV_NAME(I),
*     +    GDF_DIV_INDEX(I),
*     +   'CLSU')
*        IF (IQUEST(1).NE.0) THEN 
*          IERR = GDF$ERROR(SRN,'Faulty ZEBRA division.',8)
*          RETURN  
*        END IF
*      END DO 

C---- tell ZEBRA about bank formats 
      DO I=1,GDF_BANK_MAX
        CALL MZFORM(
     +    GDF_BANK_NAME  (I),
     +    GDF_BANK_FORMAT(I),
     +    GDF_BANK_IXIO  (I))

        IF (GDF_BANK_IXIO(I).EQ.0) THEN 
          MSG  = 'No IO characteristic for '//GDF_BANK_NAME(I)
          IERR = GDF$ERROR(SRN,MSG,9)
        END IF 

      END DO 

C---- convert bank names to Hollerith
      DO I=1,GDF_BANK_MAX
        CALL UCTOH(GDF_BANK_NAME(I),GDF_BANK_IDH(I),4,4)
      END DO      

C---- event divison is always written as the last record
      IF (GDF_DIV_EVENT.NE.GDF_DIV_MAX) THEN                     ! make sure 
        MSG  = 'Illegal event divison index.'        
        IERR = GDF$ERROR(SRN,MSG,19)        
      END IF 

C---- generate structure to bank cross reference table 
      CALL GDF$XREF(IERR) 

      RETURN 
      END SUBROUTINE GDF$INIT 
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
CDOC gdf$manual.tex 
C
C     \subsection*{Generate GDF manual \LaTeX\ files}
C     \begin{description}
C     \item{\sf Fortran binding:}
C
CVER-7 

      SUBROUTINE GDF$MANUAL 
C-------------------------------------------------------------------------------
C-     extract documentation from source code 
C-------------------------------------------------------------------------------
C- 
C-     This routine is a hot entry to win the price for the ugliest, 
C-     most uncomprehensible code with most the GOTO statements, 
C-     that still works.  
C-
C-------------------------------------------------------------------------------
      IMPLICIT NONE 
CTEX  
C     \item{\sf Action:} \\
C     Read the \verb!GDF.FOR! file and use the information in it to 
C     generate the GDF manual as a \LaTeX\ file \verb!gdf.tex!. 
C     The main file \verb!gdf.tex! includes references to other input 
C     file which are also generated.  
C
C     \end{description}
CEND 

      LOGICAL        LATEX
      LOGICAL        CODE 
      CHARACTER(132) LINE
      CHARACTER( 64) FILE   
      CHARACTER( 32) SOURCE 
      INTEGER        L,L1
      INTEGER        STATUS 
      CHARACTER(*),PARAMETER :: BLANK = ' '
C-------------------------------------------------------------------------------
C     open input file and then read one input lines at a time  
C-------------------------------------------------------------------------------
      SOURCE = 'gdf.for'    
 200  CONTINUE 
      OPEN(10,
     +  FILE=SOURCE,  
     +  ACTION="READ",
     +  STATUS="OLD",
     +  FORM='FORMATTED',
     +  PAD='YES') 
      DO 
 100    CONTINUE 
        READ(10,"(A132)",END=999) LINE
        L = LEN_TRIM(LINE) 
C-------------------------------------------------------------------------------
C       ignore special lines 
C-------------------------------------------------------------------------------
        IF (L.GE.2) THEN 
          IF (LINE(1:2).EQ.'C-') GOTO 100
        ENDIF  
        IF (L.GE.7) THEN 
          IF ( INDEX(LINE(7:L),'IMPLICIT NONE').GE.1) GOTO 100 
        ENDIF 
C-------------------------------------------------------------------------------
C       look for command tags 
C-------------------------------------------------------------------------------
        IF (L.GE.4) THEN                                     ! four charcters?
          IF (LINE(1:4).EQ.'CDOC') THEN                      ! open file? 
            LATEX = .TRUE.                                   ! LaTeX on 
            FILE  = TRIM(LINE(6:L))                          ! get file name 
            OPEN(20,FILE=FILE,FORM='FORMATTED')              ! open file 
            GOTO 100                                         ! next line 
          ELSE IF (LINE(1:4).EQ.'CVER') THEN                 ! verbatime on? 
            WRITE(20,*) '\begin{verbatim}'                   ! 
            CODE = .TRUE.
            L1 = 1                                           ! no shift 
            IF (L.GE.6) THEN                                 ! siz characters? 
              IF (LINE(1:5).EQ.'CVER-') THEN                 ! shift on? 
                READ(LINE(6:6),*) L1                         ! read shift 
              ENDIF 
            ENDIF 
            GOTO 100                                         ! next line 
          ELSE IF (LINE(1:4).EQ.'CTEX') THEN                 ! verbatim off?
            IF (CODE) WRITE(20,*) '\end{verbatim}'
            CODE = .FALSE.
            GOTO 100                                         ! next line 
          ELSE IF (LINE(1:4).EQ.'CEND') THEN                 ! close file? 
            IF (CODE) WRITE(20,*) '\end{verbatim}'           ! end verbatim?
            LATEX = .FALSE.
            CODE  = .FALSE.
            CLOSE(20)                                       
            GOTO 100                                         ! next line 
          ENDIF 
        ENDIF 
C-------------------------------------------------------------------------------
C         output line 
C-------------------------------------------------------------------------------
        IF (CODE) THEN 
          WRITE(20,*) LINE(L1:L)
        ELSE IF (LATEX) THEN 
          WRITE(20,*) LINE(2:L) 
        ENDIF
      ENDDO 
C-------------------------------------------------------------------------------
C     prepare to open next file or terminate  
C-------------------------------------------------------------------------------
  999 CONTINUE 
      IF (SOURCE(1:7).EQ.'gdf.for') THEN 
        SOURCE = 'gdf_example.for'
        GOTO 200 
      ENDIF 

      END SUBROUTINE GDF$MANUAL 
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

      SUBROUTINE GDF$MC_EVENT(A,N,P,EL,AZ,H,M,RM,XM,SM,TM,CHOPT,IERR)

C-------------------------------------------------------------------------------
C     fill monte carlo event header record 
C-------------------------------------------------------------------------------
      IMPLICIT NONE 

C---- arguments 
      INTEGER   A       ! incident particle: charge  
      INTEGER   N       !                    nucleons 
      REAL*8    P       !                    momentum                      [eV]
      REAL*8    EL      !                    elevation (up=90)            [deg]
      REAL*8    AZ      !                    azimuth   (east=0,north=90)  [deg]
      REAL*8    H       ! coordinate system height above sea level          [m]
      INTEGER   M       ! mirror: total number  
      REAL*8    RM(M)   !         radius                                    [m]
      REAL*8    XM(3,M) !         position {east,north,up}                  [m]
      REAL*8    SM(3,M) !         position {shower coordinates}             [m]
      REAL*8    TM(M)   !         relative arrival time                    [ns]
      CHARACTER CHOPT   ! 
      INTEGER IERR 
C---- misc 
      INTEGER I,J,K 
      CHARACTER(*),PARAMETER  :: SRN='GDF$MC_EVENT' 
C-------------------------------------------------------------------------------
C
C-------------------------------------------------------------------------------
      IF (CHOPT.EQ.'P') THEN                          ! print data?
        PRINT*
        PRINT*,'momentum ',P
        PRINT*,'direction',EL,AZ
        PRINT*,'(A,N)    ',A,N
        PRINT*,'height   ',H
        PRINT*
        PRINT*,'mirror   ',(I,(XM(K,I),SM(K,I),K=1,3),RM(I),TM(I),I=1,M)
        PRINT*
      ELSE IF (CHOPT.EQ.'E') THEN                     ! enter data?
        GDF_MCE%CHARGE    = A                         ! charge  
        GDF_MCE%NUCLEONS  = N                         ! nucleons 
        GDF_MCE%MIRRORS   = M                         ! total number of mirrors
        GDF_MCE%MOMENTUM  = P                         ! momentum  [eV]
        GDF_MCE%ELEVATION = EL                        ! elevation 
        GDF_MCE%AZIMUTH   = AZ                        ! azimuth   
        GDF_MCE%HEIGHT    = H                         ! height    
        CALL VZERO(GDF_MCE%RADIUS  ,GDF_MIRROR_MAX  ) ! clear 
        CALL VZERO(GDF_MCE%TIME    ,GDF_MIRROR_MAX  ) ! clear 
        CALL VZERO(GDF_MCE%POSITION,GDF_MIRROR_MAX*3) ! clear 
        CALL VZERO(GDF_MCE%LOCATION,GDF_MIRROR_MAX*3) ! clear 
        IF (M.LT.1.OR.M.GT.GDF_MIRROR_MAX) THEN       ! too few/many mirrors?
                ! no mirror at all then 
          MSG  = 'Invalid number of mirrors.'         ! the message 
          IERR = GDF$ERROR(SRN,MSG,1)                 ! complain 
          RETURN                                      ! reject event 
        ENDIF                                         ! 
        DO I=1,GDF_MCE%MIRRORS                        ! all mirrors 
          GDF_MCP%NEW     = .FALSE.                   ! no photons yet 
          GDF_MCP%VALID   = .FALSE.                   ! no photons yet 
          GDF_MCP%PHOTONS = 0                         ! no photons yet  
          GDF_MCE%RADIUS(I)  = RM(I)                  ! mirror radius      [m]
          GDF_MCE%TIME  (I)  = TM(I)                  ! arrival time [ns]
          DO J=1,3                                    ! cartesian coordinates 
            GDF_MCE%POSITION(J,I) = XM(J,I)           ! position (geographic)
            GDF_MCE%LOCATION(J,I) = SM(J,I)           ! position (shower)
          ENDDO 
        ENDDO 
        GDF_MCE%VALID = .TRUE.
        GDF_MCE%NEW   = .TRUE. 
      ELSE                                         
        IERR = GDF$ERROR(SRN,'Illegal option: '//CHOPT,9)
        RETURN  
      ENDIF 

      RETURN
      END SUBROUTINE GDF$MC_EVENT 
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

      SUBROUTINE GDF$MC_FILE(FILE,LEN,RUN,CHOPT,IERR)
C-------------------------------------------------------------------------------
C     init GDF and open a file for MC output      
C-------------------------------------------------------------------------------
      IMPLICIT NONE 

C---- arguments 
      CHARACTER*(*) FILE                  ! file name       
      INTEGER       LEN                   ! name length 
      INTEGER       RUN                   ! run  number 
      CHARACTER*1   CHOPT                 ! option 
      INTEGER       IERR 
C---- 
      CHARACTER(*),PARAMETER  :: SRN='GDF$MC_FILE' 
C-------------------------------------------------------------------------------
C
C-------------------------------------------------------------------------------
      IF (INDEX(CHOPT,'I').GE.1) THEN 
        CALL GDF$INIT('Z',IERR)                                  ! Z=init Zebra
        IF (IERR.NE.0) RETURN 
        GDF_RUN%TYPE  = GDF_RUN_TYPE_MC
        GDF_RUN%RUN   = RUN 
        GDF_RUN%NEW   = .TRUE.        
        GDF_RUN%VALID = .TRUE.        
        CALL GDF$OPEN(10,FILE(1:LEN),'W',IERR)                   ! W=writeonly
      ELSE IF (INDEX(CHOPT,'E').GE.1) THEN 
        CALL GDF$CLOSE(10,IERR) 
        CALL GDF$EXIT(IERR) 
      ELSE IF (INDEX(CHOPT,'W').GE.1) THEN 
*        IF (GDF_MCP%NEW) THEN                            ! any photons? 
*          CALL GDF$RESIZE(                               ! adjust bank length 
*     .      GDF_MCP%VERSION,                             ! for this structure 
*     .      GDF_MCP%NW_R,                                ! first/sector word 
*     .      GDF_MCP%PHOTON(GDF_MCP%PHOTONS)%WEIGHT)      ! last word 
*        ENDIF 
*
*       Note: no routines GDF$MCE and GDF$MCP yet !
*
        CALL GDF$WRITE(10,' ',IERR) 
      ELSE
        IERR = GDF$ERROR(SRN,'Unknown option '//CHOPT,1)
      ENDIF
       
      RETURN 
      END SUBROUTINE GDF$MC_FILE 
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

      SUBROUTINE GDF$MC_PHOTON(M,XD,YD,XI,YI,E,H,G,T,W,CHOPT,IERR)

C-------------------------------------------------------------------------------
C     enter photon into mirror photon record  
C-------------------------------------------------------------------------------
      IMPLICIT NONE 


C---- arguments 
      INTEGER   M      ! mirror number
      REAL*8    XD     ! rel. horizontal arrival direction  
      REAL*8    YD     ! rel. vertical   arrival direction  
      REAL*8    XI     ! rel. horizontal impact point         [m]
      REAL*8    YI     ! rel. vertical   impact point         [m]
      REAL*8    E      ! photon energy                       [eV]
      REAL*8    H      ! emission height                      [m]
      REAL*8    G      ! emission height                 [g/cm^2]
      REAL*8    T      ! arrival time                        [ns]
      REAL*8    W      ! Monte Carlo weight  
      CHARACTER CHOPT  ! 
      INTEGER IERR 

C---- misc 
      INTEGER I,J,K
      CHARACTER(*),PARAMETER  :: SRN='GDF$MC_PHOTON' 
C-------------------------------------------------------------------------------
C
C-------------------------------------------------------------------------------
      IF (CHOPT.EQ.'P') THEN 
        PRINT*,M,XD,YD,XI,YI,E,H,T,W
      ENDIF

C---- check mirror number 
      IF (M.LT.1.OR.M.GT.GDF_MIRROR_MAX) THEN         ! too few/many mirrors?
        MSG  = 'Invalid mirror number.'               ! the message 
        IERR = GDF$ERROR(SRN,MSG,1)                   ! complain 
        RETURN                                        ! reject event 
      ELSE IF (M.GT.GDF_MCE%MIRRORS) THEN 
        WRITE(MSG,*) 'Unknown mirror index.'          ! the message 
        IERR = GDF$ERROR(SRN,MSG,2)                   ! complain 
        RETURN                                        ! reject event 
      ENDIF         

C---- assign photon index 
      IF (GDF_MCP%PHOTONS.GE.GDF_PHOTON_MAX) THEN     ! too many photons?
        WRITE(MSG,*) 'Too many photons to store.'     ! the message 
        IERR = GDF$ERROR(SRN,MSG,3)                   ! complain 
        RETURN                                        ! reject event 
      ELSE IF (GDF_MCP%PHOTONS.LE.0) THEN             ! first photon?
        GDF_MCP%NEW   = .TRUE.                        ! 
        GDF_MCP%VALID = .TRUE.                        ! 
        I = 1
      ELSE                                            ! another one 
        I = GDF_MCP%PHOTONS+1  
      END IF


      GDF_MCP%PHOTONS   = I                 ! index of this photon 
      GDF_MCP%MIRROR(I) = M                 ! mirror number
      GDF_MCP%PHOTON(I)%XU   = XD           ! horiz. arrival direc.  
      GDF_MCP%PHOTON(I)%YU   = YD           ! verti. arrival direc.  
      GDF_MCP%PHOTON(I)%XI   = XI           ! horiz impact point            [m]
      GDF_MCP%PHOTON(I)%YI   = YI           ! vert. impact point            [m]
      GDF_MCP%PHOTON(I)%ENERGY = E          ! photon energy                [eV]
      GDF_MCP%PHOTON(I)%HEIGHT = H          ! emission height               [m]
      GDF_MCP%PHOTON(I)%TIME   = T          ! rel. arrival time            [ns]
      GDF_MCP%PHOTON(I)%WEIGHT = W          ! Monte Carlo weight 

      RETURN 
      END SUBROUTINE GDF$MC_PHOTON 
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

      REAL*8 FUNCTION GDF$MJD_YDHMSN(IYDHMSN,IERR)

C-------------------------------------------------------------------------------
C     year, day, hour, minute, second, nanosecond into modified Julian days  
C-------------------------------------------------------------------------------
      IMPLICIT NONE 
       
      INTEGER      IYDHMSN(6) 
      CHARACTER(*),PARAMETER  :: SRN='GDF$MJD_YDHMSN' 
      INTEGER      IERR 
C-----
      INTEGER      YEAR, DAY, HOUR, MINUTE, SECOND, NANOSEC 
C-----

C---- copy argument vector 
      IF (IYDHMSN(1).LT.100) THEN 
        YEAR = IYDHMSN(1) + 1900 
      ELSE
        YEAR = IYDHMSN(1)  
      END IF 
      DAY     = IYDHMSN(2) 
      HOUR    = IYDHMSN(3)
      MINUTE  = IYDHMSN(4)
      SECOND  = IYDHMSN(5)
      NANOSEC = IYDHMSN(6)

C---- check input 
      IF (YEAR.LT.YEAR_MIN.OR.YEAR.GT.YEAR_MAX) THEN
        IERR = GDF$ERROR(SRN,'Illegal Gregorian year.',1)
        RETURN  
      ELSE IF (DAY.LT.0.OR.DAY.GT.365) THEN
        IERR = GDF$ERROR(SRN,'Illegal number of days.',2)
        RETURN  
      ELSE IF (HOUR.LT.0.OR.HOUR.GT.23) THEN
        IERR = GDF$ERROR(SRN,'Illegal number of hours.',3)
        RETURN  
      ELSE IF (MINUTE.LT.0.OR.MINUTE.GT.59) THEN
        IERR = GDF$ERROR(SRN,'Illegal number of minutes.',4)
        RETURN  
      ELSE IF (SECOND.LT.0.OR.SECOND.GT.59) THEN
        IERR = GDF$ERROR(SRN,'Illegal number of seconds.',5)
        RETURN  
      ELSE IF (NANOSEC.LT.0.OR.NANOSEC.GT.999999999) THEN
        IERR = GDF$ERROR(SRN,'Illegal number of nano-seconds.',6)
        RETURN  
      END IF 

C---- convert into modified Julian days
      GDF$MJD_YDHMSN =  
     .        MJD_YEAR(YEAR)
     .      + DFLOAT(DAY)  
     .      + DFLOAT(HOUR   ) /    24D0  
     .      + DFLOAT(MINUTE ) /  1440D0 
     .      + DFLOAT(SECOND ) / 86400D0 
     .      + DFLOAT(NANOSEC) / 86400D9
  
      RETURN 
      END FUNCTION GDF$MJD_YDHMSN
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


      SUBROUTINE GDF$MOVE_CHECK(IDESC,NW,NT,IERR)

C-------------------------------------------------------------------------------
C     check ZEBRA sector descriptor word against expectation 
C-------------------------------------------------------------------------------
      IMPLICIT NONE 

C---- arguments 
      INTEGER IDESC       ! ZEBRA data desciptor word  
      INTEGER NW          ! expected number of ZEBRA words 
      INTEGER NT          ! expected ZEBRA type index 
      INTEGER IERR        ! return code 

C---- misc 
      CHARACTER(*),PARAMETER  :: SRN='GDF$MOVE_CHECK' 
C-------------------------------------------------------------------------------
C
C-------------------------------------------------------------------------------
      IF (IDESC/16.LE.0) THEN                        ! descriptor ok? 
        MSG  = 'Zero or negative length ZEBRA sector descriptor.' 
        IERR =  GDF$ERROR(SRN,MSG,1) 
        PRINT*,'Descriptor:',IDESC
        RETURN 
      ELSE IF (IDESC/16.GT.NW) THEN                  ! enough space?
        MSG  = 'Not enough space to store ZEBRA sector.' 
        IERR =  GDF$ERROR(SRN,MSG,2) 
        RETURN 
      ENDIF 

      IF(MOD(IDESC,16).NE.NT) THEN              ! matching data type?
        MSG  = 'ZEBRA sector data type mismatch.' 
        IERR =  GDF$ERROR(SRN,MSG,3) 
        PRINT*,'actual    :',MOD(IDESC,16)
        PRINT*,'expected  :',NT
        PRINT*,'descriptor:',IDESC
        RETURN 
      ENDIF 

      IERR = 0 
      RETURN 
      END SUBROUTINE GDF$MOVE_CHECK
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

      SUBROUTINE GDF$MOVE_I2(FLAG,NITEM,FIRST,NWORD,STORE)

C-------------------------------------------------------------------------------
C     Move 16-bit INTEGER numbers from/to ZEBRA data integer array 
C-------------------------------------------------------------------------------
      IMPLICIT NONE 

      INTEGER   FLAG        ! move from Fortran to Zebra or reverse 
      INTEGER   NITEM       ! number of items to move  
      INTEGER*2 FIRST(*)    ! first Fortran address
      INTEGER   NWORD       ! index of first ZEBRA word 
      INTEGER   STORE(*)    ! ZEBRA memory 

C---- misc 
      INTEGER   NW          ! number of 32-bits words 
      INTEGER   NT          ! ZEBRA data type 
      INTEGER*2 J(2)
      CHARACTER(*),PARAMETER  :: SRN='GDF$MOVE_I2' 
      INTEGER   I,K 
      INTEGER*4 I32,J32  
      INTEGER*2 I16,J16  
      INTEGER   IERR 
C-------------------------------------------------------------------------------
C     work out ZEBRA data type and ZEBRA storage words needed
C-------------------------------------------------------------------------------
      NW = (NITEM+1)/2 
      NT =  1 
C-------------------------------------------------------------------------------
C     copy FORTRAN to Zebra      
C-------------------------------------------------------------------------------
      IF (FLAG.EQ.GDF_MOVE_ZEBRA) THEN 
        STORE(NWORD) = NW * 16 + NT                  ! set descriptor 
        IF (NW.GT.0) THEN                            ! any data at all? 
C
C         CALL UCOPY(FIRST,STORE(NWORD+1),NW)        ! copy data 
C
C         The SUN Solaris F90 compiler allocates 32-bits of memory 
C         for each 16-bit INTEGER variable! A simple UCOPY doesn't
C         work because the memory isn't continous. Instead we better 
C         copy 16 bits at a time.
C
C         The SUN F90 implementation of TRANSFER does not allow 
C         certain operations, such as a copy of 16-bit array elements 
C         into a 32-bit variable. Some of lines of code below may 
C         look unnecessary. Under SUN Solaris they are needed to 
C         work around the restrictions of TRANSFER.  
C
          DO I=1,NW                                  ! all 32-bits words 
            K   = (I-1)*2                            ! 16-bit index 
            I32 = 0                                  ! clear bits 
            I16 = FIRST(K+2)                         ! get 16 bits    
            I32 = TRANSFER(I16,I32)                  ! put into lower 16 bits 
            I32 = ISHFTC(I32,16)                     ! shift bits +16 left 
            J16 = FIRST(K+1)                         ! get 16 bits    
            J32 = TRANSFER(J16,J32)                  ! put into lower 16 bits 
            STORE(NWORD+I) = IOR(I32,J32)            ! store results 
          ENDDO 

!          IF (GDF_OPTION%SHIFT16) THEN               ! 
!            DO I=NWORD+1,NWORD+NW
!              STORE(I) = ISHFTC(STORE(I),16,32)      ! circular shift 16 bits 
!            ENDDO 
!          ENDIF 

        ENDIF 
        NWORD = NWORD + NW + 1                       ! increment pointer 
C-------------------------------------------------------------------------------
C     copy from ZEBRA into Fortran data structure      
C-------------------------------------------------------------------------------
      ELSE IF (FLAG.EQ.GDF_MOVE_FORTRAN) THEN 

        CALL GDF$MOVE_CHECK(STORE(NWORD),NW,NT,IERR)
        IF (IERR.EQ.0) THEN 
          NW = STORE(NWORD)/16                          ! words to copy 
          IF (NW.GT.0) THEN                             ! zero sector?

C           CALL UCOPY(STORE(NWORD+1),FIRST,NW)       !   copy 
C
C           Here we have to extract 16 bits at a time from a 32-bit 
C           variable and then store the 16 bits. See comments above. 
C           A block move of array contents with UCOPY doesn't work.  
C
          DO I=1,NW                                  ! all 32-bits words 
            K   = (I-1)*2                            ! 16-bit index 
            I32 = STORE(NWORD+I)                     ! get 32 bit 
            FIRST(K+2) = IBITS(I32,16,16)            ! get upper 16 bits 
            FIRST(K+1) = IBITS(I32, 0,16)            ! get lower 16 bits 
          ENDDO 

          ENDIF 
        ENDIF 
        NWORD = NWORD + NW + 1                        ! advance pointer 

!        IF (GDF_OPTION%SHIFT16) THEN                  ! 
!          DO I=1,NW*2,2
!            J(1) = FIRST(I)
!            J(2) = FIRST(I+1)
!            FIRST(I)   = J(2)
!            FIRST(I+1) = J(1)
!          ENDDO 
!        ENDIF 

C-------------------------------------------------------------------------------
C     
C-------------------------------------------------------------------------------
      ELSE 
        IERR = GDF$ERROR(SRN,'Unknown operation requested.',9)
      ENDIF 

      RETURN                                          ! give up 

      END SUBROUTINE GDF$MOVE_I2
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

      SUBROUTINE GDF$MOVE_I4(FLAG,NITEM,FIRST,NWORD,STORE)

C-------------------------------------------------------------------------------
C     Move 32-bit INTEGER numbers from/to ZEBRA data integer array 
C-------------------------------------------------------------------------------
      INTEGER   FLAG        ! move from Fortran to Zebra or reverse 
      INTEGER   NITEM       ! number of items to move  
      INTEGER   FIRST       ! first INTEGER work (in array) 
      INTEGER   NWORD       ! index of first ZEBRA word 
      INTEGER   STORE(*)    ! ZEBRA memory 

      CHARACTER(*),PARAMETER  :: SRN='GDF$MOVE_I4' 
C-------------------------------------------------------------------------------
C     work out ZEBRA data type and ZEBRA storage words needed
C-------------------------------------------------------------------------------
      NW =  NITEM   
      NT =  2 
C-------------------------------------------------------------------------------
C     copy FORTRAN to Zebra      
C-------------------------------------------------------------------------------
      IF (FLAG.EQ.GDF_MOVE_ZEBRA) THEN 
        STORE(NWORD) = NW * 16 + NT                  ! set descriptor 
        IF (NW.GT.0) THEN                            ! any data at all? 
          CALL UCOPY(FIRST,STORE(NWORD+1),NW)        ! copy data 
        ENDIF 
        NWORD = NWORD + NW + 1                       ! increment pointer 
C-------------------------------------------------------------------------------
C     copy to Fortran structure     
C-------------------------------------------------------------------------------
      ELSE IF (FLAG.EQ.GDF_MOVE_FORTRAN) THEN 
        CALL GDF$MOVE_CHECK(STORE(NWORD),NW,NT,IERR)
        IF (IERR.EQ.0) THEN 
          NW = STORE(NWORD)/16                          ! words to copy 
          IF (NW.GT.0) THEN                             ! zero sector?
            CALL UCOPY(STORE(NWORD+1),FIRST,NW)         !   copy 
          ENDIF
        ENDIF  
        NWORD = NWORD + NW + 1                          ! advance pointer 
C-------------------------------------------------------------------------------
C     illegal flag set?      
C-------------------------------------------------------------------------------
      ELSE 
        IERR = GDF$ERROR(SRN,'Unknown operation requested.',9)
      ENDIF 
 
      END SUBROUTINE GDF$MOVE_I4

CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

      SUBROUTINE GDF$MOVE_L4(FLAG,NITEM,FIRST,NWORD,STORE)

C-------------------------------------------------------------------------------
C     Move 32-bit pattern in a LOGICAL*4  from/to ZEBRA data integer array 
C-------------------------------------------------------------------------------
      INTEGER   FLAG        ! move from Fortran to Zebra or reverse 
      INTEGER   NITEM       ! number of items to move  
      LOGICAL*4 FIRST       ! first INTEGER work (in array) 
      INTEGER   NWORD       ! index of first ZEBRA word 
      INTEGER   STORE(*)    ! ZEBRA memory 

      CHARACTER(*),PARAMETER  :: SRN='GDF$MOVE_L4' 
C-------------------------------------------------------------------------------
C     work out ZEBRA data type and ZEBRA storage words needed
C-------------------------------------------------------------------------------
      NW =  NITEM   
      NT =  1 
C-------------------------------------------------------------------------------
C     copy FORTRAN to Zebra      
C-------------------------------------------------------------------------------
      IF (FLAG.EQ.GDF_MOVE_ZEBRA) THEN 
        STORE(NWORD) = NW * 16 + NT                  ! set descriptor 
        IF (NW.GT.0) THEN                            ! any data at all? 
          CALL UCOPY(FIRST,STORE(NWORD+1),NW)        ! copy data 
        ENDIF 
        NWORD = NWORD + NW + 1                       ! increment pointer 
C-------------------------------------------------------------------------------
C     copy to Fortran structure     
C-------------------------------------------------------------------------------
      ELSE IF (FLAG.EQ.GDF_MOVE_FORTRAN) THEN 
        CALL GDF$MOVE_CHECK(STORE(NWORD),NW,NT,IERR)
        IF (IERR.EQ.0) THEN 
          NW = STORE(NWORD)/16                          ! words to copy 
          IF (NW.GT.0) THEN                             ! zero sector?
            CALL UCOPY(STORE(NWORD+1),FIRST,NW)         !   copy 
          ENDIF
        ENDIF  
        NWORD = NWORD + NW + 1                          ! advance pointer 
C-------------------------------------------------------------------------------
C     illegal flag set?      
C-------------------------------------------------------------------------------
      ELSE 
        IERR = GDF$ERROR(SRN,'Unknown operation requested.',9)
      ENDIF 
 
      END SUBROUTINE GDF$MOVE_L4
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

      SUBROUTINE GDF$MOVE_R4(FLAG,NITEM,FIRST,NWORD,STORE)

C-------------------------------------------------------------------------------
C     Move REAL*4 numbers from/to ZEBRA data integer array 
C-------------------------------------------------------------------------------
      INTEGER   FLAG         ! move from Fortran to Zebra or reverse 
      INTEGER   NITEM        ! number of items to move  
      REAL      FIRST(*)     ! first Fortran address
      INTEGER   NWORD        ! index of first ZEBRA word 
      INTEGER   STORE(*)     ! ZEBRA memory 

C---- misc 
      CHARACTER(*),PARAMETER  :: SRN='GDF$MOVE_R4' 

C-------------------------------------------------------------------------------
C     work out ZEBRA data type and ZEBRA storage words needed
C-------------------------------------------------------------------------------
      NW =  NITEM 
      NT =  3
C-------------------------------------------------------------------------------
C     copy FORTRAN to Zebra      
C-------------------------------------------------------------------------------
      IF (FLAG.EQ.GDF_MOVE_ZEBRA) THEN 
        STORE(NWORD) = NW * 16 + NT                  ! set descriptor 
        IF (NW.GT.0) THEN                            ! any data at all? 
            CALL UCOPY(FIRST,STORE(NWORD+1),NW)      ! copy data 
        ENDIF 
        NWORD = NWORD + NW + 1                       ! increment pointer 
C-------------------------------------------------------------------------------
C     copy from ZEBRa in to Fortran data structure      
C-------------------------------------------------------------------------------
      ELSE IF (FLAG.EQ.GDF_MOVE_FORTRAN) THEN 
        CALL GDF$MOVE_CHECK(STORE(NWORD),NW,NT,IERR)    ! check descriptor 
        IF (IERR.EQ.0) THEN                             ! descriptor ok?
          NW = STORE(NWORD)/16                          ! words to copy 
          IF (NW.GT.0) THEN                             ! zero sector?
            CALL UCOPY(STORE(NWORD+1),FIRST,NW)         !   copy 
          ENDIF 
        ENDIF 
        NWORD = NWORD + NW + 1                        ! advance pointer 
C-------------------------------------------------------------------------------
C     
C-------------------------------------------------------------------------------
      ELSE 
        IERR = GDF$ERROR(SRN,'Unknown operation requested.',9)
      ENDIF 

      RETURN                                          ! give up 
 
      END SUBROUTINE GDF$MOVE_R4 
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

      SUBROUTINE GDF$MOVE_R8(FLAG,NITEM,FIRST,NWORD,STORE)

C-------------------------------------------------------------------------------
C     Move REAL*8 numbers from/to ZEBRA data integer array 
C-------------------------------------------------------------------------------
      IMPLICIT NONE 

      INTEGER   FLAG         ! move from Fortran to Zebra or reverse 
      INTEGER   NITEM        ! number of items to move  
      REAL*8    FIRST        ! first Fortran address
      INTEGER   NWORD        ! index of first ZEBRA word 
      INTEGER   STORE(*)     ! ZEBRA memory 

C----
      INTEGER    NW          ! number of ZEBRA words
      INTEGER    NT          ! ZEBRA data type 
      INTEGER    IERR        ! return code 
 
      CHARACTER(*),PARAMETER  :: SRN='GDF$MOVE_R8' 

C-------------------------------------------------------------------------------
C     work out ZEBRA data type and ZEBRA storage words needed
C-------------------------------------------------------------------------------
      NW =  NITEM * 2  
      NT =  4 
C-------------------------------------------------------------------------------
C     copy FORTRAN to Zebra      
C-------------------------------------------------------------------------------
      IF (FLAG.EQ.GDF_MOVE_ZEBRA) THEN 
        STORE(NWORD) = NW * 16 + NT                  ! set descriptor 
        IF (NW.GT.0) THEN                            ! any data at all? 
          CALL UCOPY(FIRST,STORE(NWORD+1),NW)        ! copy data 
        ENDIF 
        NWORD = NWORD + NW + 1                       ! increment pointer 
C-------------------------------------------------------------------------------
C     
C-------------------------------------------------------------------------------
      ELSE IF (FLAG.EQ.GDF_MOVE_FORTRAN) THEN 
        CALL GDF$MOVE_CHECK(STORE(NWORD),NW,NT,IERR)    ! check descriptor 
        IF (IERR.EQ.0) THEN                             ! descriptor ok?
          NW = STORE(NWORD)/16                          ! words to copy 
          IF (NW.GT.0) THEN                             ! zero sector?
            CALL UCOPY(STORE(NWORD+1),FIRST,NW)         !   copy 
          ENDIF 
        ENDIF 
        NWORD = NWORD + NW + 1                        ! advance pointer 
C-------------------------------------------------------------------------------
C     
C-------------------------------------------------------------------------------
      ELSE 
        IERR = GDF$ERROR(SRN,'Unknown operation requested.',9)
      ENDIF 

      RETURN                                      
      END SUBROUTINE GDF$MOVE_R8 
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

      SUBROUTINE GDF$MOVE_C1(FLAG,NITEM,FIRST,NWORD,STORE)

C-------------------------------------------------------------------------------
C     Move character strings from/to ZEBRA data integer array 
C-------------------------------------------------------------------------------
      INTEGER          FLAG         ! move from Fortran to Zebra or reverse 
      INTEGER          NITEM        ! number of items to move  
      CHARACTER(NITEM) FIRST        ! first Fortran address
      INTEGER          NWORD        ! index of first ZEBRA word 
      INTEGER          STORE(*)     ! ZEBRA memory 
C---- 
      CHARACTER(*),PARAMETER  :: SRN='GDF$MOVE_C1' 
C-------------------------------------------------------------------------------
C     work out ZEBRA data type and ZEBRA storage words needed
C-------------------------------------------------------------------------------
      NW = (NITEM+3)/4 
      NT =  5 
C-------------------------------------------------------------------------------
C     copy FORTRAN to Zebra      
C-------------------------------------------------------------------------------
      IF (FLAG.EQ.GDF_MOVE_ZEBRA) THEN 
        STORE(NWORD) = NW * 16 + NT                  ! set descriptor 
        IF (NW.GT.0) THEN                            ! any data at all? 
          CALL UCTOH(FIRST,STORE(NWORD+1),4,NW*4)  ! convert to Hollerith 
        ENDIF 
        NWORD = NWORD + NW + 1                       ! increment pointer 
C-------------------------------------------------------------------------------
C     copy from ZEBRA into data structure     
C-------------------------------------------------------------------------------
      ELSE IF (FLAG.EQ.GDF_MOVE_FORTRAN) THEN 
        CALL GDF$MOVE_CHECK(STORE(NWORD),NW,NT,IERR)    ! check descriptor 
        IF (IERR.EQ.0) THEN                             ! descriptor ok?
          NW = STORE(NWORD)/16                          ! words to copy 
          IF (NW.GT.0) THEN                             ! zero sector?
            CALL UHTOC(STORE(NWORD+1),4,FIRST,NW*4)     !   unpack 
          ENDIF 
        ENDIF 
        NWORD = NWORD + NW + 1                          ! advance pointer 
C-------------------------------------------------------------------------------
C     
C-------------------------------------------------------------------------------
      ELSE 
        IERR = GDF$ERROR(SRN,'Unknown operation requested.',9)
      ENDIF 

      RETURN                                            ! give up 
      END SUBROUTINE GDF$MOVE_C1 
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
CDOC gdf$open.tex 
C
C     \subsection*{Open file for later read or write}
C     \begin{description}
C     \item{\sf Fortran binding:}
C
CVER-7 
      SUBROUTINE GDF$OPEN(UNIT,FILE,CHOPT,IERR)
C-------------------------------------------------------------------------------
C-    open a file 
C-------------------------------------------------------------------------------
      IMPLICIT NONE 

      INTEGER      UNIT        ! Fortran unit number 
      CHARACTER(*) FILE        ! file name 
      CHARACTER(*) CHOPT       ! setected options
      INTEGER      IERR        ! return code 
CTEX  
C     \item{\sf Action:} \\
C     Opens a sequential Zebra file in data exchange format. 
C
C     On SUNs c-library routines are used to open and read an input file.
C     While no Fortran unit gets assigned to the file the calling program 
C     must still provide a unit number which identifies the file in all 
C     further calls.    
C
C    \item{\sf Options:} \\
C     {\tt CHOPT='R'} open file in read only mode \\
C     {\tt CHOPT='W'} create a new file, ready for to write new data \\
C     {\tt CHOPT='X'} (default) selects a binary number representation. \\
C     {\tt CHOPT='A'} selects ASCII representation. 
C
C     An ASCII representation is sometimes desirable when moving files 
C     via a network which is not be fully transparent.
C     Transfer of ASCII files via a network will work as long as the 
C     program used does translate upper case characters and a few special
C     characters correctly.  
C     Tape files should always be in binary since, since the character 
C     representation on tape files may differ (eg. ASCII, EBCDIC).     
C     The default value is to use a binary number representation.  
C
C     \end{description}
CEND 
C---- 
      INTEGER       NW                        ! record length words  
      INTEGER       RECL                      ! record length bytes 
      INTEGER       LIMIT                     ! maximum file size 
      PARAMETER    (LIMIT=256*1024*1024)      ! in bytes 

      CHARACTER(*),PARAMETER  :: SRN='GDF$OPEN' 

      LOGICAL       BINARY                    ! use binary exhange mode 
      INTEGER       NL                        ! length in characters
C-------------------------------------------------------------------------------
C     rection to IERR.NE.0 condition 
C-------------------------------------------------------------------------------
      IF (IERR.NE.0) THEN 
        IF (GDF_OPTION%RESET) IERR = 0 
        IF (GDF_OPTION%ABORT) RETURN 
      ENDIF  

C---- set record length 
      IF (GDF_OPTION%AXP) THEN 
        NW   =  900                      ! on AXP words  
        RECL =  900                      !        words  
      ELSE
        NW   =  900                      ! else  words  
        RECL = 3600                      !       bytes 
      ENDIF 

      BINARY = .TRUE.                                ! default: binary 
      IF (INDEX(CHOPT,'A').GE.1) BINARY = .FALSE.    ! selected: ASCII 

C-------------------------------------------------------------------------------
C     write access 
C-------------------------------------------------------------------------------
      IF (INDEX(CHOPT,'W').GE.1) THEN 

        IF (BINARY) THEN                                  ! binary mode? 
          IF (GDF_OPTION%CIO) THEN                        ! use C-library IO?
            GDF_CFILE%OPEN   = .TRUE.                     ! c-io active  
            GDF_CFILE%MEDIUM = 0                          ! 0 = disk 
            CALL CFOPEN(                                  ! init c-library I/O
     +        GDF_CFILE%LUNDES,                           ! file descriptor 
     +        GDF_CFILE%MEDIUM,                           ! medium 
     +        NW,'w',0,FILE,IERR)   
            IF (IERR.NE.0) THEN                           ! worked?
              IERR = GDF$ERROR(SRN,'CFOPEN failed.',IERR) ! complain 
              RETURN                                      ! give up 
            END IF                                        ! ok, file open now
            IQUEST(1) = GDF_CFILE%LUNDES                  ! set file descriptor 
            CALL FZFILE(UNIT,NW,'XLO')                    ! L=c-lib, X=binary  
          ELSE                                            ! normal FORTRAN IO
            OPEN(UNIT= UNIT,                              ! open file   
     +        FILE  =FILE,
     +        STATUS='NEW',
     +        FORM  ='UNFORMATTED',                  
     +        ACCESS='DIRECT',
     +        RECL  = RECL,
     +        ERR   = 999)
            CALL FZFILE(UNIT,NW,'XDO')                    ! X=binary
          ENDIF 
        ELSE                                              ! ASCII mode?  
          OPEN(UNIT= UNIT,                                ! open unit  
     +       FILE  =FILE,
     +       STATUS='NEW',
     +       ERR   = 999)
          CALL FZFILE(UNIT,NW,'AO')                       ! A=ASCII  
        END IF 

C----   check status of FZFILE call 
        IF (IQUEST(1).NE.0) THEN 
          MSG  = 'Failed to open FZ file.'//FILE 
          IERR = GDF$ERROR(SRN,MSG,IQUEST(1))
          RETURN 
        ENDIF 

C----   limit file and write start of run record 
        CALL FZLIMI(UNIT,FLOAT(LIMIT)/4E6)                ! limit file size 
        IF (.NOT.GDF_RUN%NEW) THEN  
           MSG  = 'Please update run info first.'
           IERR = GDF$ERROR(SRN,MSG,2)
           RETURN 
        END IF 
        IF (GDF_RUN%RUN.LT.1) THEN                        ! run number ok?
           MSG  = 'Zero or negative run number.'
           IERR = GDF$ERROR(SRN,MSG,3)
           RETURN 
        END IF 
        GDF_HEADER_LEN = 0                                ! 
        CALL FZRUN(                                       ! Zebra run header 
     +    UNIT,                         
     +    GDF_RUN%RUN, 
     +    GDF_HEADER_LEN,      
     +    GDF_HEADER) 
C-------------------------------------------------------------------------------
C     read access
C-------------------------------------------------------------------------------
      ELSE IF (INDEX(CHOPT,'R').GE.1) THEN 
        IF (BINARY) THEN 
          GDF_CFILE%OPEN   = .FALSE.                      ! c-io inactive  
          IF (GDF_OPTION%CIO) THEN                        ! but on SUNs .... 
            GDF_CFILE%OPEN   = .TRUE.                     ! c-io active  
            GDF_CFILE%MEDIUM = 0                          ! 0 = disk 
            CALL CFOPEN(                                  ! init c-library I/O
     +        GDF_CFILE%LUNDES,                           ! file descriptor 
     +        GDF_CFILE%MEDIUM,                           ! medium 
     +        NW,'r',0,FILE,IERR)   
            IF (IERR.NE.0) THEN                           ! worked?
              IERR = GDF$ERROR(SRN,'CFOPEN failed.',IERR) ! complain 
              RETURN                                      ! give up 
            END IF                                        ! ok, file open now
            IQUEST(1) = GDF_CFILE%LUNDES                  ! set file descriptor 
            CALL FZFILE(UNIT,NW,'XLI')                    ! L=c-lib, X=binary  
          ENDIF                                           ! otherwise 

          IF (.NOT.GDF_CFILE%OPEN) THEN                 ! use default method 
            NL = LEN(FILE)
            IF (NL.LE.0) THEN 
              MSG = 'Zero file name length.'
              IERR = GDF$ERROR(SRN,MSG,IERR)
              RETURN                                    ! give up 
            ENDIF                   
            OPEN(UNIT= UNIT,                            ! init FORTRAN i/o  
     +         FILE  = FILE(1:NL),                      ! file name 
     +         ACTION="READ",                           ! read-only acess 
     +         STATUS='OLD',
     +         FORM  ='UNFORMATTED',                    
     +         ACCESS='DIRECT',
     +         RECL  = RECL,  
     +         ERR   = 999)
            CALL FZFILE(UNIT,NW,'XDI')                  ! X=binary  
          ENDIF 
        ELSE 
          OPEN(UNIT= UNIT,                              ! open unit  
     +       FILE  = FILE,
     +       STATUS='OLD',
     +       ERR   = 999)
          CALL FZFILE(UNIT,NW,'AI')                     ! open file 
        ENDIF 
      ELSE
        MSG = 'Unknown option: '//CHOPT 
        IERR = GDF$ERROR(SRN,MSG,4)
        RETURN 
      END IF 
      RETURN 

C---- failed 
  999 CONTINUE
      MSG = 'Can not open '//FILE  
      IERR = GDF$ERROR(SRN,MSG,5)

      RETURN 
      END SUBROUTINE GDF$OPEN 
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
CDOC gdf$option.tex 
C
C     \subsection*{Set run time option}
C     \begin{description}
C     \item{\sf Fortran binding:}
C
CVER-7 

       SUBROUTINE GDF$OPTION(CHOPT,VALUE,IERR)

C-------------------------------------------------------------------------------
C-     set run time options
C-------------------------------------------------------------------------------
       IMPLICIT NONE 

       CHARACTER(*)  CHOPT           ! option to set 
       LOGICAL       VALUE           ! new state, true or false 
       INTEGER       IERR            ! return code 
CTEX  
C     \item{\sf Action:} \\
C     Set run time options. The default is for all option to be \verb!.FALSE.!.
C     
C
C    \item{\sf Options:} \\
C     {\tt CHOPT='ABORT'}   on entry RETURN from subroutine if \verb!IERR.NE.0!\\
C     {\tt CHOPT='AXP'}     DEC Alpha 16-bit integer array representation.
C                           ZEBRA record length in words. \\
C     {\tt CHOPT='CIO'}     use C library IO routines \\
C     {\tt CHOPT='RESET'}   on entry set IERR=0 if \verb!IERR.NE.0! \\
C     {\tt CHOPT='SUN'}     set options for Sun Sparc Solaris\\
C     {\tt CHOPT='VERBOSE'} print extra information \\
C
C     \end{description}
CEND 

       INTEGER        I       

       IF (INDEX(CHOPT,'ABORT'  ).GE.1) GDF_OPTION%ABORT   = VALUE  
       IF (INDEX(CHOPT,'RESET'  ).GE.1) GDF_OPTION%RESET   = VALUE  
       IF (INDEX(CHOPT,'VERBOSE').GE.1) GDF_OPTION%VERBOSE = VALUE  
       IF (INDEX(CHOPT,'CIO'    ).GE.1) GDF_OPTION%CIO     = VALUE  
       IF (INDEX(CHOPT,'AXP'    ).GE.1) GDF_OPTION%AXP     = VALUE  
       IF (INDEX(CHOPT,'SUN'    ).GE.1) GDF_OPTION%SHIFT16 = VALUE  
       IF (INDEX(CHOPT,'SUN'    ).GE.1) GDF_OPTION%CIO     = VALUE  
       IF (INDEX(CHOPT,'SUN'    ).GE.1) GDF_OPTION%SOLARIS = VALUE  

       IF (GDF_OPTION%VERBOSE) THEN 
         PRINT* 
         PRINT*,'Abort GDF if IERR.NE.0 detected: ',GDF_OPTION%ABORT
         PRINT*,'Reset IERR=0 on routine entry  : ',GDF_OPTION%RESET  
         PRINT*,'Verbose mode, more information : ',GDF_OPTION%VERBOSE
         PRINT*,'Use C I/O library routines     : ',GDF_OPTION%CIO 
         PRINT*,'DEC Alpha (VMS or Unix)        : ',GDF_OPTION%AXP 
         PRINT*,'Swap order of 16-bit half words: ',GDF_OPTION%SHIFT16 
         PRINT*,'Solaris 16 bit representation  : ',GDF_OPTION%SOLARIS 
         PRINT* 
       ENDIF

       RETURN 
       END SUBROUTINE GDF$OPTION 
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

      REAL*8 FUNCTION GDF$RAD_DMS(DMS,IERR)

C-------------------------------------------------------------------------------
C     convert degrees, minutes seconds into radians 
C-------------------------------------------------------------------------------
      IMPLICIT NONE 

      INTEGER DMS(3)
      REAL*8  DEGRAD 
      INTEGER IERR       

      DEGRAD = ATAN(1D0)/45D0 

      GDF$RAD_DMS =  DEGRAD * 
     .   (  DFLOAT(DMS(1)) 
     .    + DFLOAT(DMS(2)) /  60D0
     .    + DFLOAT(DMS(3)) /3600D0)

      RETURN 
      END FUNCTION GDF$RAD_DMS
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

      REAL*8 FUNCTION GDF$RAD_HMS(HMS,IERR)

C-------------------------------------------------------------------------------
C     convert hour, minutes seconds into radians 
C-------------------------------------------------------------------------------
      IMPLICIT NONE 

      INTEGER HMS(3)
      REAL*8  TWOPI 
      INTEGER IERR 
      
      TWOPI = 8D0 * ATAN(1D0)

      GDF$RAD_HMS =  TWOPI * 
     .   (  DFLOAT(HMS(1)) /    24D0
     .    + DFLOAT(HMS(2)) /  1440D0
     .    + DFLOAT(HMS(3)) / 86400D0)

      RETURN 
      END FUNCTION GDF$RAD_HMS 
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

      SUBROUTINE GDF$RUN(FLAG,STORE,NW)
C-------------------------------------------------------------------------------
C     move run records to/from Zebra or Fortran structures      
C-------------------------------------------------------------------------------
      IMPLICIT NONE 
 
C---- 
      INTEGER FLAG 
      INTEGER STORE(*)
      INTEGER NW
C----
      INTEGER N_B,N_I,N_R,N_D,N_C1,N_C2

      IF (FLAG.EQ.GDF_MOVE_ZEBRA) THEN                 ! into ZEBRA 
        N_C2 = GDF_RUN%CLEN                            ! actual length 
      ELSE IF (FLAG.EQ.GDF_MOVE_FORTRAN) THEN          ! into FORTRAN  
        N_C2 = GDF_RUN_MCL                             ! max length 
      ENDIF 

      N_B  =           GDF_TELE_MAX                  ! 32-bit binary 
      N_I  =   7 + 3 * GDF_TELE_MAX                  ! integer 
      N_R  =   3 + 2 * GDF_TELE_MAX                  ! real  
      N_D  =   2                                     ! real*8 
      N_C1 = 160 

      IF (STORE(1).GE.27) THEN 
        NW  =   7                               
        CALL GDF$MOVE(FLAG,N_B ,GDF_RUN%STATUS(1) ,NW,STORE)
        CALL GDF$MOVE(FLAG,N_I ,GDF_RUN%IDATE     ,NW,STORE)
        CALL GDF$MOVE(FLAG,N_R ,GDF_RUN%SID_LENGTH,NW,STORE)
        CALL GDF$MOVE(FLAG,N_D ,GDF_RUN%UTC_START ,NW,STORE)
        CALL GDF$MOVE(FLAG,N_C1,GDF_RUN%FILE      ,NW,STORE)
        CALL GDF$MOVE(FLAG,N_C2,GDF_RUN%COMMENT   ,NW,STORE)
      ELSE 
        NW  =   6         
        CALL GDF$MOVE(FLAG,N_B,GDF_RUN%STATUS(1)  ,NW,STORE)
        CALL GDF$MOVE(FLAG,N_I,GDF_RUN%IDATE      ,NW,STORE)
        CALL GDF$MOVE(FLAG,N_R,GDF_RUN%SID_LENGTH ,NW,STORE)
        CALL GDF$MOVE(FLAG,N_D,GDF_RUN%UTC_START  ,NW,STORE)

        CALL UHTOC(STORE(NW+ 1),4,GDF_RUN%FILE    ,80) 
        CALL UHTOC(STORE(NW+21),4,GDF_RUN%OBSERVER,80) 
        CALL UHTOC(STORE(NW+41),4,GDF_RUN%COMMENT ,GDF_RUN%CLEN)

      ENDIF 

      RETURN 
      END SUBROUTINE GDF$RUN 
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

      SUBROUTINE GDF$PRINT_CCD(CCD,IERR)

C-------------------------------------------------------------------------------
C     print one CCD record 
C-------------------------------------------------------------------------------
      IMPLICIT NONE 

      TYPE(GDF_CCD_T) :: CCD  
      INTEGER            IERR 

      PRINT*  
      PRINT*,' --- CCD information ---'
      PRINT*  
      PRINT2,'software version number:',CCD%VERSION  
      PRINT4,'UTC time         [mjd] :',CCD%UTC 
      PRINT*  
      PRINT2,'telescope              :',CCD%TELESCOPE 
      PRINT2,'stars  visible         :',CCD%NSTAR
      PRINT2,'update cycle           :',CCD%CYCLE
      PRINT3,'exposure time    [sec] :',CCD%EXPOSURE 
      PRINT*  

      RETURN 

    1 FORMAT(1X,A,1X,Z8.8)
    2 FORMAT(1X,A,5I12,/,(15X,5I12))
    3 FORMAT(1X,A,4F12.3,/,(15X,4F12.3))
    4 FORMAT(1X,A,3F16.6)
    5 FORMAT(1X,A,1X,A)
    6 FORMAT(1X,A,1X,5Z4.4,/,(15X,5Z4.4))


      END SUBROUTINE GDF$PRINT_CCD
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

      SUBROUTINE GDF$PRINT_EVENT10(EVENT,IERR)

C-------------------------------------------------------------------------------
C     print one run record 
C-------------------------------------------------------------------------------
      IMPLICIT NONE 

      TYPE(GDF_EV10_T) :: EVENT 
      INTEGER            IERR 
      INTEGER I 

      PRINT*
      PRINT*,'--- 10 meter event --- '
      PRINT*  
      PRINT2,'version      :', EVENT%VERSION 
      PRINT1,'trigger bits :', EVENT%TRIGGER  
      PRINT1,'status  bits :', EVENT%STATUS   
      PRINT6,'GPS pattern  :', EVENT%GPS_CLOCK  
      PRINT2,'number of ADC:', EVENT%NADC 
      PRINT2,'phase TDCs   :', EVENT%NPHS 
      PRINT2,'burst scalers:', EVENT%NBRST  
      PRINT2,'trig patterns:', EVENT%NTRG   
      PRINT2,'run number   :', EVENT%RUN 
      PRINT2,'event        :', EVENT%EVENT  
      PRINT2,'live time sec:', EVENT%LIVE_SEC 
      PRINT2,'live time  ns:', EVENT%LIVE_NS  
      PRINT2,'elapsed   sec:', EVENT%ELAPSED_SEC
      PRINT2,'elapsed    ns:', EVENT%ELAPSED_NS
      PRINT4,'UTC  [mjd]   :', EVENT%UTC 
      PRINT1,'trig pattern :',(EVENT%PATTERN(I),I=1,EVENT%NTRG ) 
      PRINT2,'ADC values   :',(EVENT%ADC    (I),I=1,EVENT%NADC ) 
      PRINT2,'Phase TDCs   :',(EVENT%PHS    (I),I=1,EVENT%NPHS ) 
      PRINT2,'Burst scaler :',(EVENT%BURST  (I),I=1,EVENT%NBRST) 
      RETURN 

    1 FORMAT(1X,A,5Z10.8, /,(15X,5Z10.8))
    2 FORMAT(1X,A,5I10,   /,(15X,5I10))
    3 FORMAT(1X,A,4F12.3, /,(15X,4F12.3))
    4 FORMAT(1X,A,3F16.6)
    5 FORMAT(1X,A,1X,A)
    6 FORMAT(1X,A,1X,5Z4.4,/,(15X,5Z4.4))
 
      END SUBROUTINE GDF$PRINT_EVENT10 
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

      SUBROUTINE GDF$PRINT_EVENT11(EVENT,IERR)

C-------------------------------------------------------------------------------
C     print one 11, event record 
C-------------------------------------------------------------------------------
      IMPLICIT NONE 

      TYPE(GDF_EV11_T) :: EVENT   
      INTEGER            IERR 
      INTEGER I 

      PRINT*
      PRINT*,'--- 11 meter event --- '
      PRINT* 
      PRINT2,'version number:',EVENT%VERSION 
      PRINT1,'status  bits :', EVENT%STATUS   
      PRINT1,'gate open    :', EVENT%GATE_OPEN    
      PRINT1,'gate closed  :', EVENT%GATE_CLOSE     
      PRINT2,'number of ADC:', EVENT%NADC 
      PRINT2,'number of TDC:', EVENT%NTDC 
      PRINT2,'phase TDCs   :', EVENT%NPHS  
      PRINT2,'run number   :', EVENT%RUN 
      PRINT2,'event        :', EVENT%EVENT  
      PRINT4,'UTC  [mjd]   :', EVENT%UTC 
      PRINT2,'ADC values   :',(EVENT%ADC  (I),I=1,EVENT%NADC ) 
      PRINT2,'TDC values   :',(EVENT%TDC  (I),I=1,EVENT%NTDC ) 
      PRINT2,'phase TDCs   :',(EVENT%PHS  (I),I=1,EVENT%NPHS ) 
      PRINT2,'burst scalers:',(EVENT%burst(I),I=1,EVENT%Nbrst)

      RETURN 

    1 FORMAT(1X,A,1X,Z8.8)
    2 FORMAT(1X,A,5I12,  /,(15X,5I12))
    3 FORMAT(1X,A,4F12.3,/,(15X,4F12.3))
    4 FORMAT(1X,A,3F16.6)
    5 FORMAT(1X,A,1X,A)
    6 FORMAT(1X,A,1X,5Z4.4,/,(15X,5Z4.4))
 
      END SUBROUTINE GDF$PRINT_EVENT11
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

      SUBROUTINE GDF$PRINT_FRAME10(FRAME,IERR)

C-------------------------------------------------------------------------------
C     print one run record 
C-------------------------------------------------------------------------------
      IMPLICIT NONE 

      TYPE(GDF_FR10_T) :: FRAME   
      INTEGER            IERR 
      INTEGER I 

      PRINT*
      PRINT*,'--- 10 meter frame --- '
      PRINT* 
      PRINT2,'version      :', FRAME%VERSION 
      PRINT1,'status  bits :', FRAME%STATUS   
      PRINT2,'number of ADC:', FRAME%NADC 
      PRINT2,'phase TDCs   :', FRAME%NPHS  
      PRINT2,'scalers      :', FRAME%NSCA   
      PRINT2,'run number   :', FRAME%RUN 
      PRINT2,'frame number :', FRAME%FRAME   
      PRINT4,'UTC  [mjd]   :', FRAME%UTC 
      PRINT2,'ADC values 1 :',(FRAME%PED_ADC1(I),I=1,FRAME%NADC) 
      PRINT2,'ADC values 2 :',(FRAME%PED_ADC2(I),I=1,FRAME%NADC) 
      PRINT2,'ADC calibr.  :',(FRAME%CAL_ADC (I),I=1,FRAME%NADC) 
      PRINT2,'phase TDCs 1 :',(FRAME%PHS1 (I),I=1,FRAME%NPHS) 
      PRINT2,'phase TDCs 2 :',(FRAME%PHS2 (I),I=1,FRAME%NPHS) 
      PRINT2,'current scal :',(FRAME%SCALC(I),I=1,FRAME%NSCA) 
      PRINT2,'single rates :',(FRAME%SCALS(I),I=1,FRAME%NSCA) 

      RETURN

    1 FORMAT(1X,A,1X,Z8.8)
    2 FORMAT(1X,A,5I12,  /,(15X,5I12))
    3 FORMAT(1X,A,4F12.3,/,(15X,4F12.3))
    4 FORMAT(1X,A,3F16.6)
    5 FORMAT(1X,A,1X,A)
    6 FORMAT(1X,A,1X,5Z4.4,/,(15X,5Z4.4))
 
      END SUBROUTINE GDF$PRINT_FRAME10 
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

      SUBROUTINE GDF$PRINT_FRAME11(FRAME,IERR)

C-------------------------------------------------------------------------------
C     print one run record 
C-------------------------------------------------------------------------------
      IMPLICIT NONE 

      TYPE(GDF_FR11_T) :: FRAME   
      INTEGER            IERR 
      INTEGER I 

      PRINT*
      PRINT*,'--- 11 meter frame --- '
      PRINT* 
      PRINT2,'version number:',FRAME%VERSION 
      PRINT1,'status  bits :', FRAME%STATUS   
      PRINT2,'number of ADC:', FRAME%NADC 
      PRINT2,'number of TDC:', FRAME%NTDC 
      PRINT2,'phase TDCs   :', FRAME%NPHS  
      PRINT2,'scalers      :', FRAME%NSCA   
      PRINT2,'run number   :', FRAME%RUN 
      PRINT2,'frame number :', FRAME%FRAME   
      PRINT4,'UTC  [mjd]   :', FRAME%UTC 
      PRINT2,'ADC values 1 :',(FRAME%PED_ADC1(I),I=1,FRAME%NADC) 
      PRINT2,'ADC values 2 :',(FRAME%PED_ADC2(I),I=1,FRAME%NADC) 
      PRINT2,'ADC calibr.  :',(FRAME%CAL_ADC (I),I=1,FRAME%NADC) 
      PRINT2,'TDC values 1 :',(FRAME%TDC1 (I),I=1,FRAME%NTDC) 
      PRINT2,'TDC values 2 :',(FRAME%TDC2 (I),I=1,FRAME%NTDC) 
      PRINT2,'phase TDCs 1 :',(FRAME%PHS1 (I),I=1,FRAME%NPHS) 
      PRINT2,'phase TDCs 2 :',(FRAME%PHS2 (I),I=1,FRAME%NPHS) 
      PRINT2,'current scal :',(FRAME%SCALC(I),I=1,FRAME%NSCA) 
      PRINT2,'single rates :',(FRAME%SCALS(I),I=1,FRAME%NSCA) 

      RETURN

    1 FORMAT(1X,A,1X,Z8.8)
    2 FORMAT(1X,A,5I12,  /,(15X,5I12))
    3 FORMAT(1X,A,4F12.3,/,(15X,4F12.3))
    4 FORMAT(1X,A,3F16.6)
    5 FORMAT(1X,A,1X,A)
    6 FORMAT(1X,A,1X,5Z4.4,/,(15X,5Z4.4))

      END SUBROUTINE GDF$PRINT_FRAME11 
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

      SUBROUTINE GDF$PRINT_HV(HV,IERR)

C-------------------------------------------------------------------------------
C     print one HV record 
C-------------------------------------------------------------------------------
      IMPLICIT NONE 

      TYPE(GDF_HV_T) :: HV  
      INTEGER        :: IERR 
      INTEGER        :: I 

      PRINT*  
      PRINT*,' --- HV information ---'
      PRINT*  
      PRINT2,'software version number:',HV%VERSION  
      PRINT4,'UTC time         [mjd] :',HV%UTC 
      PRINT*  
      PRINT2,'telescope              :',HV%TELESCOPE 
      PRINT2,'update cycle           :',HV%CYCLE 
      PRINT2,'operation mode         :',HV%MODE  
      PRINT2,'number of channels     :',HV%NCH 
      PRINT*  

      DO I=1,HV%NCH 
        PRINT 9, I,  
     .    HV%status  (I),     ! status of each HV channel  [bits]
     .    HV%v_set   (I),     ! presently set  voltage    [V]
     .    HV%v_actual(I),     ! actual measured voltage   [V]
     .    HV%i_supply(I),     ! HV supply current         [uA]
     .    HV%i_anode (I)      ! measured anode current    [uA]
      ENDDO 

      RETURN 

    1 FORMAT(1X,A,1X,Z8.8)
    2 FORMAT(1X,A,5I12,/,(15X,5I12))
    3 FORMAT(1X,A,4F12.3,/,(15X,4F12.3))
    4 FORMAT(1X,A,3F16.6)
    5 FORMAT(1X,A,1X,A)
    6 FORMAT(1X,A,1X,5Z4.4,/,(15X,5Z4.4))

    9 FORMAT(1X,I5,1X,Z4.4,4(1X,F8.1)) 

      END SUBROUTINE GDF$PRINT_HV
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

      SUBROUTINE GDF$PRINT_RUN(RUN,IERR)

C-------------------------------------------------------------------------------
C     print one run record 
C-------------------------------------------------------------------------------
      IMPLICIT NONE 

      TYPE(GDF_RUN_T) :: RUN  
      INTEGER            IERR 

      INTEGER I,J,L 
      INTEGER LNBLNK                                       ! Kernlib 
      
      IF (RUN%VERSION.GE.16) THEN                    ! version 16+ ?
        L = RUN%CLEN                                 ! length of comment
      ELSE                                           ! no length info
        L = LNBLNK(RUN%COMMENT(1:400))               ! find last non blank
      ENDIF 

      PRINT*
      PRINT*,'--- run information ---'
      PRINT* 
      PRINT2,'version number :', GDF_RUN%VERSION   
      PRINT2,'run number     :', GDF_RUN%RUN 
      PRINT4,'UTC now   [mjd]:', GDF_RUN%UTC 
      PRINT4,'UTC start [mjd]:', GDF_RUN%UTC_START 
      PRINT4,'UTC end   [mjd]:', GDF_RUN%UTC_END  
      PRINT2,'VAX date       :', GDF_RUN%IDATE 
      PRINT2,'VAX time       :', GDF_RUN%ITIME 
      PRINT2,'run type       :', GDF_RUN%TYPE
      PRINT2,'sky quality    :', GDF_RUN%SKY_QUALITY 
      PRINT3,'length   [smin]:', GDF_RUN%SID_LENGTH 
      PRINT3,'cycle    [smin]:', GDF_RUN%SID_CYCLE  
      PRINT3,'actual   [smin]:', GDF_RUN%SID_ACTUAL
      PRINT5,'observers      :', GDF_RUN%OBSERVER
      PRINT2,'comment length :', GDF_RUN%CLEN  
      IF (L.GT.0) THEN 
        PRINT*,GDF_RUN%COMMENT(1:L)   
      ELSE
        PRINT5,'no comment' 
      ENDIF 
      PRINT* 
*     PRINT*,'words binary   :',GDF_RUN%NW_B/16
*     PRINT*,'      integer  :',GDF_RUN%NW_I/16
*     PRINT*,'      real*4   :',GDF_RUN%NW_R/16
*     PRINT*,'      real*8   :',GDF_RUN%NW_D/16
*     PRINT*,'      character:',GDF_RUN%NW_C1/16+GDF_RUN%NW_C2/16
*     PRINT* 

      RETURN 

    1 FORMAT(1X,A,1X,Z8.8)
    2 FORMAT(1X,A,5I12,  /,(15X,5I12))
    3 FORMAT(1X,A,4F12.3,/,(15X,4F12.3))
    4 FORMAT(1X,A,3F16.6)
    5 FORMAT(1X,A,1X,A)
    6 FORMAT(1X,A,1X,5Z4.4,/,(15X,5Z4.4))

      END SUBROUTINE GDF$PRINT_RUN 
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

      SUBROUTINE GDF$PRINT_TRACK(TRACK,IERR)

C-------------------------------------------------------------------------------
C     print one run record 
C-------------------------------------------------------------------------------
      IMPLICIT NONE 

      TYPE(GDF_TRACK_T) :: TRACK 
      INTEGER            IERR 

      PRINT*  
      PRINT2,'software version      :', TRACK%VERSION  
      PRINT4,'UTC time         [mjd]:', TRACK%UTC 
      PRINT4,'Siderial time    [mjd]:', TRACK%STL  
      PRINT2,'telescope identifier  :', TRACK%TELESCOPE
      PRINT2,'tracking mode         :', TRACK%MODE
      PRINT1,'status bits           :', TRACK%STATUS 
      PRINT2,'update cycle          :', TRACK%CYCLE 
      PRINT*,'telescope'
      PRINT4,'  right ascension [rad]:', TRACK%RASC_TODAY  
      PRINT4,'  declination     [rad]:', TRACK%DECL_TODAY  
      PRINT4,'  azimuth         [rad]:', TRACK%AZIMUTH   
      PRINT4,'  elevation       [rad]:', TRACK%ELEVATION   
      PRINT4,'  deviation       [rad]:', TRACK%DEVIATION
      PRINT*,'source'
      PRINT4,'  right ascension [rad]:', TRACK%RASC_TODAY  
      PRINT4,'  declination     [rad]:', TRACK%DECL_TODAY  
      PRINT*,'offset'
      PRINT4,'  right ascension [rad]:', TRACK%RASC_OFFSET   
      PRINT4,'  declination     [rad]:', TRACK%DECL_OFFSET 
      PRINT*,'stereo inclination'
      PRINT4,'  intearction height[m]:', TRACK%HEIGHT 
      PRINT4,'  azimuth   shift [rad]:', TRACK%AZI_INCL  
      PRINT4,'  elevation shift [rad]:', TRACK%ELE_INCL  
      PRINT*
      PRINT5,'source name          :', TRACK%SOURCE
      RETURN 

    1 FORMAT(1X,A,1X,Z8.8)
    2 FORMAT(1X,A,5I12,  /,(15X,5I12))
    3 FORMAT(1X,A,4F12.3,/,(15X,4F12.3))
    4 FORMAT(1X,A,3F16.6)
    5 FORMAT(1X,A,1X,A)
    6 FORMAT(1X,A,1X,5Z4.4,/,(15X,5Z4.4))

      END SUBROUTINE GDF$PRINT_TRACK  
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
CDOC gdf$read.tex
C
C     \subsection*{Read data from file}
C     \begin{description}
C       \item{\sf Fortran binding:} 
C
CVER-7          
      SUBROUTINE GDF$READ(UNIT,CHOPT,IERR)   
C-------------------------------------------------------------------------------
C-    read Zebra FZ file until next event record 
C-------------------------------------------------------------------------------
      IMPLICIT NONE  
      INTEGER       UNIT         ! IO unit number to read from             
      CHARACTER(*) CHOPT         ! requested options 
      INTEGER       IERR         ! return code 
CTEX
C     \item{\sf Action:} \\
C     This routine reads a ZEBRA FZ data file and copies the information 
C     up to and including the next event into derived type data structures 
C     in memory. 
C     The routine also updates other information that is not part of 
C     the event but that was found in the data stream while searching 
C     for the next event. 
C     Examples are tracking information or new run information. 
C     The calling program can determine which derived type data structures 
C     were updated by checking the \verb!NEW! field 
C     in each data structure. 
C     A value of \verb!NEW=.TRUE.! indicates that the stucture was updated
C     during the last call to \verb!GDF$READ!. 
C     The next call will rest the value to \verb!NEW=.FALSE.! unless there
C     is new data. 
C     At the same time the {\tt VALID} flags are set, indicating 
C     whether teh information is still valid or not. 
C
C     \item{\sf Return code:}\\
C     A return value ot {\tt IERR=1} indicates that the end of file has been 
C     reached. 
C     The calling program should has to make sure the file will be closed. 
C     \item{\sf Options:}\\
C     If {\tt CHOPT='C'} the routine verifies that that the checksum 
C     of each record is correct, provided a checksum was calculated.
C     If the data file was written on a computer with a different 
C     number representation this option can not be used.
C
C     \end{description}
CEND 
C----
      CHARACTER(*),PARAMETER  :: SRN='GDF$READ' 
      INTEGER   DUMMY 
      INTEGER   DIV_NOW
      INTEGER   I,J,K 
      INTEGER   NEXT      
      INTEGER   IBANK     
      INTEGER   IDH        ! Hollerith ID 
      INTEGER   ND         ! number of data word in bank 
      LOGICAL   CHECKSUM   ! do or do not calculate it
      INTEGER   ICHECK(2)  ! Zebra checksum 
      INTEGER   VERSION    ! bank version number 
      INTEGER   RECORD     ! Fortran record ID 
      INTEGER   FLAG
      PARAMETER (FLAG=GDF_MOVE_FORTRAN) 
      LOGICAL   LTEST(2)    ! local temp variable   
      INTEGER   FALSE
      INTEGER   TRUE 
C-------------------------------------------------------------------------------
C     reaction to IERR.NE.0 condition 
C-------------------------------------------------------------------------------
      IF (IERR.NE.0) THEN 
        IF (GDF_OPTION%RESET) IERR = 0 
        IF (GDF_OPTION%ABORT) RETURN 
      ENDIF  

      FALSE = TRANSFER(.FALSE.,FALSE) 
      TRUE  = TRANSFER( .TRUE.,TRUE ) 

      IF (INDEX(CHOPT,'C').GT.0) THEN
        CHECKSUM = .TRUE.
      ELSE
        CHECKSUM = .FALSE.
      END IF 
C-------------------------------------------------------------------------------
C     flag all Fortran records as obsolete 
C-------------------------------------------------------------------------------
      DO I=1,GDF_XREF_MAX
        GDF_DATA_I(GDF_XREF(I)%NEW) = FALSE        ! set update flag 
      END DO 

      GDF_EV10%VALID = .FALSE.                     ! 10 m event now invalid 
      GDF_EV11%VALID = .FALSE.                     ! 11 m event now invalid 
C-------------------------------------------------------------------------------
C     wipe all Zebra divisons 
C-------------------------------------------------------------------------------
      DO I=1,GDF_DIV_MAX                           ! all division 
        IF (GDF_AREA(I).NE.0) THEN                 ! division not empty?
          CALL MZWIPE(GDF_DIV_INDEX(I))
        END IF          
      END DO 
C-------------------------------------------------------------------------------
C     read next header
C-------------------------------------------------------------------------------
  100 CONTINUE                            
      GDF_HEADER_LEN = GDF_HEADER_MAX   ! max length 
      CALL FZIN(UNIT,                   ! read header only 
     +          0,                      ! dummy division 
     +          DUMMY,                  ! pointer to bank 
     +          2,                      ! stand alone structure     
     +         'S',                     ! S=read header only 
     +          GDF_HEADER_LEN,         ! header length 
     +          GDF_HEADER)             ! header itself 
    
      IF (IQUEST(1).GE.3) THEN                           ! end of file?         
        IERR = 1 
        RETURN 
      ELSE IF (IQUEST(1).EQ.2) THEN                      ! end of run 
        GOTO 100                                         ! 
      ELSE IF (IQUEST(1).EQ.1) THEN                      ! start of run  
        GOTO 100  
      ELSE IF (IQUEST(1).LT.0) THEN                      ! error return?  
        MSG  = 'Error reading ZEBRA FZ file.'
        IERR = GDF$ERROR(SRN,MSG,2)
        PRINT'(1X,I4,I12,1X,Z8.8)',(I,IQUEST(I),IQUEST(I),I=1,20)
        RETURN 
      END IF 
C-------------------------------------------------------------------------------
C     read data part of record 
C-------------------------------------------------------------------------------
      GDF_RUN%RUN   = GDF_HEADER(GDF_HEADER_RUN )  
      DIV_NOW       = GDF_HEADER(GDF_HEADER_TYPE)     

      IF (GDF_AREA(DIV_NOW).NE.0) THEN                 ! any old data?
        CALL MZWIPE(GDF_DIV_INDEX(DIV_NOW))            ! reset division 
      END IF 

      CALL FZIN(UNIT,                                  !
     +          GDF_DIV_INDEX(DIV_NOW),                ! target division 
     +          GDF_AREA(DIV_NOW),                     ! supporting link 
     +          1,                                     ! 1=supporting link
     +         'A',                                    ! get pending structure 
     +          DUMMY,                                 ! header not used 
     +          DUMMY)  

      IF (IQUEST(1).NE.0) THEN 
        MSG  = 'Error reading structure.'
        IERR = GDF$ERROR(SRN,MSG,3)
        PRINT'(1X,I4,I12,1X,Z8.8)',(I,IQUEST(I),IQUEST(I),I=1,20)
        RETURN 
      END IF

C---- the event record should always be last 
      IF (DIV_NOW.NE.GDF_DIV_EVENT) GOTO 100            ! event record?
 
C-------------------------------------------------------------------------------
C     update Fortran records 
C-------------------------------------------------------------------------------
      DO I=1,GDF_DIV_MAX                                ! all divisions 
        NEXT = GDF_AREA(I)                              ! entry pointer 
        DO WHILE (NEXT.NE.0)                            ! any banks?
          DO J=1,GDF_XREF_MAX                           ! try all records

C----       get bank properties 
            IBANK    = GDF_XREF(J)%BANK                 ! bank format ID  
            IDH      = GDF_BANK_IDH(IBANK)              ! Hollerith ID
            ND       = GDF_STORE_IQ(NEXT-1)             ! actual length 
            VERSION  = GDF_STORE_IQ(NEXT+1)             ! version number  
            RECORD   = GDF_STORE_IQ(NEXT+2)             ! record ID 

C----       check version number is set 
            IF (VERSION.LE.0) THEN                      ! value set?
              WRITE(MSG,'(A,1X,A4))')                   ! prepare error 
     .        'No version number set for bank:',IDH     ! message 
              IERR = GDF$ERROR(SRN,MSG,1)               ! send it 
              GOTO 200                                  ! try next record 
            END IF 

C----       check that Hollerith and record IDs match 
*
*           Before version 66 the FORTRAN record IDs were not entered into 
*           the ZEBRA bank. When copying the bank some other bank specific 
*           information had to be used to decide to which telescope the 
*           data refers. 
*
*           However there is no usefull 11m data before version 66 either. 
*           So we can use a little trick: if it is old pre version 66 data
*           we ignore the record ID and the data gets copied into the 10m  
*           structures, which are always defined first. (Better do not 
*           change the record ID order if you want to read old data!) 
* 
*           For post version 66 data we look for a match bewtween record ID  
*           read from the ZEBRA bank and the prededined record IDS. Eventualy 
*           (when nobody wants pre version 66 data) the search loop over 
*           the record ID can go. We know the record ID from the ZEBRA bank.  
* 
            LTEST(1) = IDH.EQ.GDF_STORE_IQ(NEXT-4)      ! Hollerith IDs match?
            LTEST(2) = (VERSION.LT.66).OR.(RECORD.EQ.J) ! record IDs match?
            IF (LTEST(1).AND.LTEST(2)) THEN             ! match? 

C----         check total length 
              IF (ND.GT.GDF_BANK_MD(IBANK)) THEN            ! > max length?
                WRITE(MSG,'(A,1X,A4,2(2X,A,I12))')  
     .          'Unexpected length. Bank:',IDH,
     .          'ND:',ND,
     .          'MD:',GDF_BANK_MD(IBANK)
                IERR = GDF$ERROR(SRN,MSG,82)
                GOTO 200 
              END IF 

C----         verify checksum 
              IF (CHECKSUM) THEN                          ! perform check?
                CALL UCOPY(GDF_STORE_IQ(NEXT+3),ICHECK,2) ! previous checksum 
                IF ((GDF_STORE_IQ(NEXT+3).NE.0).OR.       ! zero if no checksum 
     .              (GDF_STORE_IQ(NEXT+4).NE.0)) THEN     ! zero if no checksum
                  CALL DZCHVC(                            ! calculate checksum
     .              ' ',                                  ! no printout 
     .              GDF_STORE_INDEX,                      ! store index
     .              NEXT+5,                               ! pointer first word
     .              NEXT+ND,                              !         last word
     .              'V',                                  ! V=verify 
     .              ICHECK)                               ! previous checksum 
                  IF (IQUEST(1).NE.0) THEN                !   
                    IERR = GDF$ERROR(SRN,'Checksum error.',8)
                    PRINT 1000,'Expected  :',ICHECK
                    PRINT 1000,'Calculated:',
     .                GDF_STORE_IQ(NEXT+3),GDF_STORE_IQ(NEXT+4)
 1000               FORMAT(1X,A,2(2X,Z8.8))
                  END IF  
                END IF  
              END IF 

C----         copy header of bank 
              IF (VERSION.GE.27) THEN                     ! 64-bit header?
                CALL UCOPY(                               ! copy header words 
     .            GDF_STORE_IQ(NEXT+1),                   ! from Zebra memory 
     .            GDF_DATA_I(GDF_XREF(J)%FIRST),          ! into Fortran 
     .            6)                                      ! number of words 
               ELSE                                       ! short header?
                 GDF_DATA_I(GDF_XREF(J)%FIRST)=VERSION    ! set version number
                 CALL UCOPY(                              ! copy remaining 
     .             GDF_STORE_IQ(NEXT+2),                  ! from Zebra memory 
     .             GDF_DATA_I(GDF_XREF(J)%FIRST+2),       ! into Fortran 
     .             4)                                     ! number of words 
               ENDIF 

C----         copy remaining bank contents into Fortran record 
              IF (IBANK.EQ.GDF_BANK_EVENT10) THEN 
                CALL GDF$EVENT10(FLAG,
     .            GDF_STORE_IQ(NEXT+1),
     .            GDF_BANK_ND(IBANK))               
              ELSE IF (IBANK.EQ.GDF_BANK_EVENT11) THEN 
                CALL GDF$EVENT11(FLAG,
     .            GDF_STORE_IQ(NEXT+1),
     .            GDF_BANK_ND(IBANK))               
              ELSE IF (IBANK.EQ.GDF_BANK_FRAME10) THEN 
                CALL GDF$FRAME10(FLAG,
     .            GDF_STORE_IQ(NEXT+1),               
     .            GDF_BANK_ND(IBANK))               
              ELSE IF (IBANK.EQ.GDF_BANK_FRAME11) THEN 
                CALL GDF$FRAME11(FLAG,
     .            GDF_STORE_IQ(NEXT+1),               
     .            GDF_BANK_ND(IBANK))               
              ELSE IF (IBANK.EQ.GDF_BANK_TRACK) THEN 
                CALL GDF$TRACK(J,FLAG,
     .            GDF_STORE_IQ(NEXT+1),                
     .            GDF_BANK_ND(IBANK),
     .            IERR)               
              ELSE IF (IBANK.EQ.GDF_BANK_HV) THEN 
                CALL GDF$HV(J,FLAG,
     .            GDF_STORE_IQ(NEXT+1),                
     .            GDF_BANK_ND(IBANK),
     .            IERR)               
              ELSE IF (IBANK.EQ.GDF_BANK_CCD) THEN 
                CALL GDF$CCD(J,FLAG,
     .            GDF_STORE_IQ(NEXT+1),                
     .            GDF_BANK_ND(IBANK),
     .            IERR)               
              ELSE IF (IBANK.EQ.GDF_BANK_RUN) THEN 
                CALL GDF$RUN(FLAG,
     .            GDF_STORE_IQ(NEXT+1),                
     .            GDF_BANK_ND(IBANK))               
              ELSE
                MSG = 'No routine to decode Zebra bank.'
                IERR = GDF$ERROR(SRN,MSG,88) 
              ENDIF 

              IF (IERR.NE.0) THEN                       ! problems?
                GDF_DATA_I(GDF_XREF(J)%NEW  ) = FALSE   ! no new data
                GDF_DATA_I(GDF_XREF(J)%NEW+1) = FALSE   ! data invalid 
                RETURN                                  ! abort  
              ELSE IF (GDF_BANK_ND(IBANK).LE.0) THEN    ! any data returned?
                GDF_DATA_I(GDF_XREF(J)%NEW  ) = FALSE   ! no new data
                GDF_DATA_I(GDF_XREF(J)%NEW+1) = FALSE   ! data invalid 
                RETURN                                  ! abort  
              ELSE                                      ! record entered ok
                GDF_DATA_I(GDF_XREF(J)%NEW  ) = TRUE    ! set update flag 
                GDF_DATA_I(GDF_XREF(J)%NEW+1) = TRUE    ! set valid  flag 
                GDF_BANK_ND(IBANK)=GDF_BANK_ND(IBANK)-1 ! 
              ENDIF 
              GOTO 200                                  ! proceed with next bank
            END IF 
          END DO                                        ! no match 
          MSG   = 'Unknown bank format. Bank skipped.'  ! therefore complain 
          IERR  = GDF$ERROR(SRN,MSG,8)                  ! a bit, an then get  
  200     CONTINUE                                      ! ready for next bank 
          NEXT = GDF_STORE_LQ(NEXT)                     ! pointer next bank 
        END DO 
      END DO 


      RETURN 
      END SUBROUTINE GDF$READ 
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

      SUBROUTINE GDF$SHIFTC(I2,N2)

C-------------------------------------------------------------------------------
C     swap elements of integer*2 arrays
C-------------------------------------------------------------------------------
      IMPLICIT NONE 

      INTEGER   N2            ! array length in 2-byte words
      INTEGER*2 I2(N2)        ! declare array as 2-byte worda 
    
C----
      INTEGER   IERR 
      INTEGER   I  
      INTEGER,ALLOCATABLE :: I4(:) 
      INTEGER   N4            ! length of I4 array 

      CHARACTER(*),PARAMETER  :: SRN='GDF$SHIFTC' 

      IF (.NOT.GDF_OPTION%SHIFT16) THEN 
        IERR = GDF$ERROR(SRN,'No SHIFT16 flag set.',1)
        RETURN 
      ENDIF 

      IF (MOD(N2,2).NE.0) THEN 
        IERR = GDF$ERROR(SRN,'Odd number of elements.',1)
      ELSE IF (N2.LT.2) THEN  
        IERR = GDF$ERROR(SRN,'Too few elements in array.',2) 
      END IF 
        
      N4 = N2/2                        ! lenght of 32-bit array needed 
      ALLOCATE(I4(N4))                  ! create 32 bit word array  
      CALL UCOPY(I2,I4,N4)             ! copy two 16-bit into one 32-bit word  
      DO I=1,N4                        ! for each 32-bit word 
        I4(I) = ISHFTC(I4(I),16,32)    !   circular shift 16 bits within 32-bit 
      END DO                           !   next                    
      CALL UCOPY(I4,I2,N4)             ! copy 32-bit back into two 16-bit 
      DEALLOCATE(I4)                   ! release memory 

      RETURN 
      END SUBROUTINE GDF$SHIFTC
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
CDOC gdf$test.tex 
C
C     \subsection*{Write or read a test data file.}
C     \begin{description}
C     \item{\sf Fortran binding:}
C
CVER-7 

      SUBROUTINE GDF$TEST(UNIT,FILE,CHOPT,IERR)
C-------------------------------------------------------------------------------
C-     write or read a test data file.
C-------------------------------------------------------------------------------
      IMPLICIT NONE 

      INTEGER      UNIT          ! logical unit 
      CHARACTER(*) FILE          ! file name 
      CHARACTER(*) CHOPT         ! option  
      INTEGER      IERR          ! return code 
CTEX  
C     \item{\sf Action:} \\
C     Opens a new Zebra file an writes test data into it or reads the 
C     a test file to check that the contents are as expected. 
C     When reading a file the contents are checked against data stored
C     in memory when writting a test data file. 
C
C    \item{\sf Options:} \\
C     {\tt CHOPT='W'} create a new test file \\
C     {\tt CHOPT='R'} read file and check contents \\
C
C     \end{description}
CEND

C---- control flags 
      LOGICAL,PARAMETER :: EV11 = .FALSE.            ! write/read 11m events 

C---- misc
      REAL          RN32                               ! Cernlib 
      CHARACTER(*),PARAMETER  :: SRN='GDF$TEST' 
      INTEGER       NERR 
      INTEGER       A,B
      INTEGER       I,J,N,M  
      REAL*8        U,V 
      REAL*4        START,TIME             ! time [sec]
      REAL          AVG_RATE               ! average rate [Hz]
      REAL          AVG_TIME               ! average time per event [s]
      INTEGER       NCOUNT(3)              ! sys clock ticks 
      INTEGER       NWRITE                 ! sys clock ticks during write 
      INTEGER       NRATE                  ! sys clock ticks per second 
      LOGICAL*4     L4
 
      INTEGER 
     .  RUN, 
     .  NEV,
     .  NADC_10,
     .  NADC_11,
     .  NTDC_11,
     .  MODADC,
     .  MODTDC,
     .  MODPHS,
     .  MODBRS

      REAL*8  UTC_START, UTC_STEP

      PARAMETER 
     . (RUN     =  123,
     .  NEV     = 1234,
     .  NADC_10 =  541,
     .  NADC_11 =  120,
     .  NTDC_11 =   72,
     .  MODADC  = 2046,
     .  MODTDC  = 2047,
     .  MODPHS  = 2048,
     .  MODBRS  = 2049,
     .  UTC_START = 49934D0,   
     .  UTC_STEP  =    1D-8)   


       INTEGER, PARAMETER :: RFRAME = 128                  ! events per frame 
       INTEGER, PARAMETER :: NFRAME = NEV/RFRAME+1         ! number of frames
C----   
       TYPE(GDF_EV10_T) :: EVENT10(NEV)
       TYPE(GDF_EV11_T) :: EVENT11(NEV)
       TYPE(GDF_FR10_T) :: FRAME10(NFRAME) 

C----   
       NWRITE = 0                                      ! reset clock tick count
       CALL SYSTEM_CLOCK(N,M) 
       START = REAL(N)/REAL(M)                         ! start time [sec]

       CALL GDF$OPTION('RESET',.TRUE.,IERR)            ! set run time options 

C-------------------------------------------------------------------------------
C     write test data 
C-------------------------------------------------------------------------------
      IF (INDEX(CHOPT,'W').GE.1) THEN 
        GDF_RUN%NEW     = .TRUE.
        GDF_RUN%RUN     =  RUN  
        GDF_RUN%UTC     =  UTC_START   
        GDF_RUN%COMMENT = "Hello world!" 
        GDF_RUN%CLEN    =  LEN_TRIM(GDF_RUN%COMMENT) 
 
        CALL GDF$OPEN(UNIT,FILE,'W',IERR)                   ! open file 

        GDF_FR10%FRAME = 0 

        DO I=1,NEV                                          ! for a few events 

C-------------------------------------------------------------------------------
C         generate dummy tracking data 
C-------------------------------------------------------------------------------
          IF (MOD(I,777).EQ.1) THEN                         ! do it now? 
            DO J=1,GDF_TELE_MAX                             ! all telescopes 
              GDF_TRACK(J)%NEW       =  .TRUE.              ! flag as new data 
              GDF_TRACK(J)%UTC       =  UTC_START           ! set UTC time 
              GDF_TRACK(J)%CYCLE     =  J*  123             ! update cycle 
              GDF_TRACK(J)%AZIMUTH   =  J*0.456             ! 
              GDF_TRACK(J)%ELEVATION =  J*0.789             ! 
              GDF_TRACK(J)%SOURCE    = 'DARK_STAR'          ! 
            ENDDO          
          ENDIF 
C-------------------------------------------------------------------------------
C         generate 10m event 
C-------------------------------------------------------------------------------
          GDF_EV10%NEW   = .TRUE.
          GDF_EV10%EVENT = I 
          GDF_EV10%NADC  = NADC_10                        ! ADC values 
          GDF_EV10%NTRG  = GDF_EV10_NTRG*RN32(I)   

          GDF_EV10%UTC   = UTC_START+I*UTC_STEP  

          DO J=1,GDF_EV10%NADC                         ! all ADC channels
            GDF_EV10%ADC(J) = MOD(I+J+10,MODADC)
          ENDDO                                        ! next ADC 
          DO J=1,GDF_EV10%NTRG 
            N = 2**24 - 1                              ! excerise bits 0...23 
            M = RN32(J) * N                            ! random bits 
            GDF_EV10%PATTERN(J) = TRANSFER(M,L4)       ! copy 
          ENDDO
          DO J=1,GDF_EV10_NPHS
            GDF_EV10%PHS(J) = MOD(I+J+10,MODPHS)
          ENDDO
          DO J=1,GDF_EV10_NBRST
            GDF_EV10%BURST(J) = MOD(I+J+10,MODBRS)
          ENDDO
          EVENT10(I) = GDF_EV10                               ! store in memory
C-------------------------------------------------------------------------------
C         generate 11m event 
C-------------------------------------------------------------------------------
          IF (EV11) THEN 
            GDF_EV11%NEW  = .TRUE.
            GDF_EV11%NADC = NADC_11                          ! ADC values 
            GDF_EV11%NTDC = NTDC_11                        ! TDC values 
            GDF_EV11%UTC  = UTC_START+I*UTC_STEP  

            DO J=1,GDF_EV11%NADC                         ! all ADC channels
              GDF_EV11%ADC(J) = MOD(I+J+11,MODADC)
            ENDDO                                        ! next ADC 
            DO J=1,GDF_EV11%NTDC                         ! all TDC channels
              GDF_EV11%TDC(J) = MOD(I+J+11,MODTDC)
            ENDDO                                        ! next ADC 
            DO J=1,GDF_EV11_NPHS
              GDF_EV11%PHS(J) = MOD(I+J+11,MODPHS)
            ENDDO
            DO J=1,GDF_EV11_NBRST
              GDF_EV11%BURST(J) = MOD(I+J+11,MODBRS)
            ENDDO
            EVENT11(I) = GDF_EV11                          ! store in memory 
          ENDIF 
C-------------------------------------------------------------------------------
C         generate 10m frame  
C-------------------------------------------------------------------------------
          IF (MOD(I,RFRAME).NE.1) THEN 
            GDF_FR10%NEW  = .FALSE.
          ELSE 
            GDF_FR10%NEW   = .TRUE.
            GDF_FR10%FRAME = GDF_FR10%FRAME+1 
            GDF_FR10%NADC  = NADC_10                       ! ADC values 
            GDF_FR10%NSCA  = GDF_FR10_NSCA                 ! scaler channles
            GDF_FR10%NPHS  = GDF_FR10_NPHS                 ! phase TDCs 
            GDF_FR10%UTC   = UTC_START+I*UTC_STEP  

            DO J=1,GDF_FR10%NADC                         ! all ADC channels
              GDF_FR10%PED_ADC1(J) = 1023 * RN32(J)
              GDF_FR10%PED_ADC2(J) = 1023 * RN32(J)
            ENDDO                                        ! next ADC 
            DO J=1,GDF_FR10_NPHS
              GDF_FR10%PHS1(J) = 2047 * RN32(J)
              GDF_FR10%PHS2(J) = 2047 * RN32(J)
            ENDDO
            DO J=1,GDF_FR10_NSCA 
              GDF_FR10%SCALS(J) = 4097 * RN32(J)
              GDF_FR10%SCALC(J) = 4097 * RN32(J)
            ENDDO
            N = GDF_FR10%FRAME  
            FRAME10(N) = GDF_FR10
          ENDIF 

          CALL SYSTEM_CLOCK(NCOUNT(1)) 
            IF (IERR.NE.0) GOTO 999                      ! end of file? error? 
            CALL GDF$WRITE(UNIT,' ',IERR)                ! write data to file
          CALL SYSTEM_CLOCK(NCOUNT(2))
          CALL SYSTEM_CLOCK(NCOUNT(3))
          NWRITE = NWRITE+(NCOUNT(2)-NCOUNT(1))-(NCOUNT(3)-NCOUNT(2)) 
C-------------------------------------------------------------------------------
C         store copy of data in memory 
C-------------------------------------------------------------------------------
        ENDDO                                          ! next event 
        CALL GDF$CLOSE(UNIT,IERR) 

        CALL SYSTEM_CLOCK(N,M) 
        TIME = (REAL(N)/REAL(M)) - START 

        CALL SYSTEM_CLOCK(COUNT_RATE=NRATE)

        IF (GDF_OPTION%TIME) THEN 
        PRINT*
        PRINT*,'Write data: '
        PRINT*,'  Avg time per event  [ms]',NINT(1E3*TIME/NEV) 
        PRINT*,'  Avg event rate      [Hz]',NINT(NEV/TIME) 
        PRINT*
        PRINT*,'  Time per GDF$WRITE  [ms]',NINT(1E3*NWRITE/(NRATE*NEV)) 
        PRINT*,'  True GDF$WRITE rate [Hz]',NINT((1E0*NEV*NRATE)/NWRITE)
        PRINT*
        AVG_TIME = REAL(GDF_TIME%FZOUT)/REAL(NRATE*NEV)
        AVG_RATE = 1.0/AVG_TIME 
        GDF_TIME%FZOUT = 0 
        PRINT*,'  Time per FZOUT      [ms]',NINT(1E3*AVG_TIME) 
        PRINT*,'  True FZOUT rate     [Hz]',NINT(AVG_RATE)
        PRINT*
        ENDIF 

      ENDIF 
C-------------------------------------------------------------------------------
C     read test data and compare 
C-------------------------------------------------------------------------------
      IF (INDEX(CHOPT,'R').GE.1) THEN 

        NERR = 0 

        CALL GDF$OPEN(10,FILE,'R',IERR)                ! open file on unit 10

        DO I=1,NEV                                     ! all events 

          CALL GDF$READ(UNIT,' ',IERR)                 ! read from unit 10 
          IF (IERR.EQ.1) GOTO 998                      ! end of file? 
          IF (IERR.GT.0) GOTO 999                      ! read error? 

C-------------------------------------------------------------------------------
C         check tracking data 
C-------------------------------------------------------------------------------
          IF (MOD(I,777).EQ.1) THEN  
            DO J=1,GDF_TELE_MAX
              IF(.NOT.GDF_TRACK(J)%NEW) THEN 
                PRINT*,'No tracking record.',J
                NERR = NERR + 1
              ELSE IF (ABS(GDF_TRACK(J)%UTC-UTC_START).GT.1D-6) THEN 
                PRINT*,'Invalid tracking UTC.',J 
                NERR = NERR + 1
              ELSE IF (GDF_TRACK(J)%CYCLE.NE.J*123) THEN  
                PRINT*,'Invalid tracking cycle number.',J
                NERR = NERR + 1
              ELSE IF (ABS(GDF_TRACK(J)%AZIMUTH-J*0.456).GT.1D-6) THEN 
                PRINT*,'Invalid azimuth value',J
                NERR = NERR + 1
              ELSE IF (ABS(GDF_TRACK(J)%ELEVATION-J*0.789).GT.1D-6) THEN  
                PRINT*,'Invalid elevation value',J
                NERR = NERR + 1
              ELSE IF (GDF_TRACK(J)%SOURCE.NE.'DARK_STAR') THEN 
                PRINT*,'Invalid tracking object.',J
                NERR = NERR + 1
              ENDIF 
            ENDDO          
          ENDIF 
C-------------------------------------------------------------------------------
C         check 10m event 
C-------------------------------------------------------------------------------
          IF (.NOT.GDF_EV10%NEW) THEN                 ! new event? pedestals?
            PRINT*,'No 10m event.'
            NERR = NERR + 1
          ELSE 
            U = GDF_EV10%UTC 
            V = UTC_START + I * UTC_STEP 
            IF (ABS(U-V).GT.1D-10) THEN 
              PRINT*, ' Wrong 10m event UTC time ',U,V
              NERR = NERR + 1
            ENDIF 

            IF (GDF_EV10%NADC.NE.NADC_10) THEN 
              PRINT*,'Wrong 10m ADC # ',GDF_EV10%NADC
              NERR = NERR + 1
            ENDIF 

            DO J=1,GDF_EV10%NADC                       ! all ADC channels
              A = EVENT10(I)%ADC(J)
              B = GDF_EV10%ADC(J)
              IF (A.NE.B) THEN 
                PRINT*,'10m ADC counts do not match'
                PRINT*,I,J,A,B
                NERR = NERR + 1
              ENDIF 
            ENDDO                                      ! next ADC 

            IF (GDF_EV10%NTRG.NE.EVENT10(I)%NTRG) THEN 
              PRINT*,'Wrong 10m pattern # ',GDF_EV10%NTRG
              NERR = NERR + 1
            ENDIF 

            DO J=1,GDF_EV10%NTRG                       ! all ADC channels
              A = TRANSFER(EVENT10(I)%PATTERN(J),A)
              B = TRANSFER(  GDF_EV10%PATTERN(J),B)
              IF (A.NE.B) THEN 
                PRINT*,'10m trigger patterns do not match'
                PRINT*,I,J,A,B
                NERR = NERR + 1
              ENDIF 
            ENDDO                                      ! next ADC 

            DO J=1,GDF_EV10_NPHS
              A = MOD(I+J+10,MODPHS)
              B = GDF_EV10%PHS(J)
              IF (A.NE.B) THEN 
                PRINT*,'10m phase scaler counts do not match'
                PRINT*,I,J,A,B
                NERR = NERR + 1
              ENDIF 
            ENDDO                                  

            DO J=1,GDF_EV10_NBRST
              A = MOD(I+J+10,MODBRS)
              B = GDF_EV10%BURST(J)
              IF (A.NE.B) THEN 
                PRINT*,'10m burst scaler counts do not match'
                PRINT*,I,J,A,B
                NERR = NERR + 1
              ENDIF 
            ENDDO                                     

          ENDIF                                            
C-------------------------------------------------------------------------------
C         check 10m frame 
C-------------------------------------------------------------------------------
          IF (MOD(I,RFRAME).EQ.1) THEN                      ! time for a frame?
            N = GDF_FR10%FRAME  
            IF (GDF_FR10%NEW) THEN                          ! is there one?
              IF (GDF_FR10%NADC.NE.FRAME10(N)%NADC) THEN    ! 
                PRINT*,'Wrong number of 10m frame ADCs.'
              ENDIF 
              IF (GDF_FR10%NSCA.NE.FRAME10(N)%NSCA) THEN    !       
                PRINT*,'Wrong number of 10m frame scalers.'
              ENDIF 
              IF (GDF_FR10%NPHS.NE.FRAME10(N)%NPHS) THEN 
                PRINT*,'Wrong number of 10m frame TDCs.'
              ENDIF 


              DO J=1,GDF_FR10%NPHS                       ! all PHS channels
                A = FRAME10(N)%PHS1(J)
                B =   GDF_FR10%PHS1(J)
                IF (A.NE.B) THEN 
                  PRINT*,'10m PHS1 frame counts do not match'
                  PRINT*,I,N,J,A,B
                  NERR = NERR + 1
                ENDIF 
                A = FRAME10(N)%PHS2(J)
                B =   GDF_FR10%PHS2(J)
                IF (A.NE.B) THEN 
                  PRINT*,'10m PHS2 frame counts do not match'
                  PRINT*,I,N,J,A,B
                  NERR = NERR + 1
                ENDIF 
              ENDDO                                      ! next ADC 

              IF (GDF_VERSION.LT.80) THEN 

                DO J=1,GDF_FR10%NADC                          ! all ADC channels
                  A = FRAME10(N)%PED_ADC1(J)
                  B =   GDF_FR10%PED_ADC1(J)
                  IF (A.NE.B) THEN 
                    PRINT*,'10m ADC1 frame counts do not match'
                    PRINT*,I,N,J,A,B
                    NERR = NERR + 1
                  ENDIF 
                  A = FRAME10(N)%PED_ADC2(J)
                  B =   GDF_FR10%PED_ADC2(J)
                  IF (A.NE.B) THEN 
                    PRINT*,'10m ADC2 frame counts do not match'
                    PRINT*,I,N,J,A,B
                    NERR = NERR + 1
                  ENDIF 
                ENDDO                                      ! next ADC 

                DO J=1,GDF_FR10%NSCA                       ! all PHS channels
                  A = FRAME10(N)%SCALC(J)
                  B =   GDF_FR10%SCALC(J)
                  IF (A.NE.B) THEN 
                    PRINT*,'10m current frame values do not match'
                    PRINT*,I,J,A,B
                    NERR = NERR + 1
                  ENDIF 
                  A = FRAME10(N)%SCALS(J)
                  B =   GDF_FR10%SCALS(J)
                  IF (A.NE.B) THEN 
                    PRINT*,'10m scaler frame counts do not match'
                    PRINT*,I,J,A,B
                    NERR = NERR + 1
                  ENDIF 
                ENDDO                                      ! next ADC 

              ENDIF 

            ELSE
              PRINT*,'No 10 m frame record at event ',I
            ENDIF 
          ENDIF 
C-------------------------------------------------------------------------------
C         check 11m event 
C-------------------------------------------------------------------------------
          IF (EV11) THEN 
            IF (.NOT.GDF_EV11%NEW) THEN                 ! new event? pedestals?
              PRINT*,'No 11m event.'
              NERR = NERR + 1
            ELSE 
              U = GDF_EV11%UTC 
              V = UTC_START + I * UTC_STEP 
              IF (ABS(U-V).GT.1D-10) THEN 
                PRINT*, ' Wrong UTC time ',U,V
                NERR = NERR + 1
              ENDIF 
              IF (GDF_EV11%NADC.NE.NADC_11) THEN 
                PRINT*,'Wrong 11m ADC # ',GDF_EV11%NADC
                NERR = NERR + 1
              ENDIF 
              IF (GDF_EV11%NTDC.NE.NTDC_11) THEN 
                PRINT*,'Wrong 11m TDC # ',GDF_EV11%NTDC
                NERR = NERR + 1
              ENDIF 

              DO J=1,GDF_EV11%NADC                       ! all ADC channels
                A = MOD(I+J+11,MODADC)
                B = GDF_EV11%ADC(J)
                IF (A.NE.B) THEN 
                  PRINT*,'11m ADC counts do not match'
                  PRINT*,I,J,A,B
                  NERR = NERR + 1
                ENDIF 
              ENDDO                                      ! next ADC 

              DO J=1,GDF_EV11%NTDC 
                A = MOD(I+J+11,MODTDC)
                B = GDF_EV11%TDC(J)
                IF (A.NE.B) THEN 
                  PRINT*,'11m TDC counts do not match'
                  PRINT*,I,J,A,B
                  NERR = NERR + 1
                ENDIF 
              ENDDO                                     

              DO J=1,GDF_EV11_NPHS
                A = MOD(I+J+11,MODPHS)
                B = GDF_EV11%PHS(J)
                IF (A.NE.B) THEN 
                  PRINT*,'11m Phase scaler counts do not match'
                  PRINT*,I,J,A,B
                  NERR = NERR + 1
                ENDIF 
              ENDDO                                  

              DO J=1,GDF_EV11_NBRST
                A = MOD(I+J+11,MODBRS)
                B = GDF_EV11%BURST(J)
                IF (A.NE.B) THEN 
                  PRINT*,'11m Burst scaler counts do not match'
                  PRINT*,I,J,A,B
                  NERR = NERR + 1
                ENDIF 
              ENDDO                                     

            ENDIF                                            
          ENDIF                                            
        ENDDO                                          ! next event 
C-------------------------------------------------------------------------------
C       close file, print error statistics
C-------------------------------------------------------------------------------
        CALL GDF$CLOSE(UNIT,IERR)                        ! close file 

        IF (GDF_OPTION%TIME) THEN 
          CALL SYSTEM_CLOCK(N,M) 
          TIME = (REAL(N)/REAL(M)) - TIME  
          PRINT*
          PRINT*,'Read data: '
          PRINT*,'  Avg time per event  [ms]',INT(1E3*TIME/NEV) 
          PRINT*,'  Avg Event rate      [Hz]',INT(NEV/TIME) 
          PRINT*
          PRINT*,  SRN
          PRINT*
          PRINT*,' Events read    : ',NEV
          PRINT*,' Detected errors: ',NERR 
          PRINT*
        ENDIF 

      ENDIF 

      RETURN 

 998  CONTINUE 
      IERR = GDF$ERROR(SRN,'Unexpected end of file.',I)
      RETURN 

 999  CONTINUE 
      IERR = GDF$ERROR(SRN,'Error during read/write.',I)
      RETURN 

      END SUBROUTINE GDF$TEST 
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

      SUBROUTINE GDF$TRACK(IREC,FLAG,STORE,NW,IERR)

C-------------------------------------------------------------------------------
C     
C-------------------------------------------------------------------------------
      IMPLICIT NONE 
 
C---- 
      INTEGER IREC 
      INTEGER FLAG       
      INTEGER STORE(*)
      INTEGER NW 
      INTEGER IERR 
C----
      INTEGER N_ADC,N_TDC,N_SCA,N_PHS,N_B
      INTEGER B16,B32,INT,CHA, DBL 
      INTEGER IT 

      PARAMETER 
     . (B16 = GDF_TYPE_BINARY16,
     .  B32 = GDF_TYPE_BINARY32,
     .  INT = GDF_TYPE_INTEGER,   
     .  CHA = GDF_TYPE_CHARACTER,   
     .  DBL = GDF_TYPE_DOUBLE)  

      CHARACTER(*),PARAMETER  :: SRN='GDF$TRACK' 

C---- how many header words?
      IF (STORE(1).GE.27) THEN 
        NW = 7
      ELSE 
        NW = 6
      ENDIF 

C---- which telescope?
      IF (IREC.EQ.GDF_XREF_TRACK_10) THEN 
        IT = GDF_TELE_10 
      ELSE IF (IREC.EQ.GDF_XREF_TRACK_11) THEN 
        IT = GDF_TELE_11 
      ELSE 
        MSG  = 'Unknown tracking data structure.'   ! error message  
        IERR = GDF$ERROR(SRN,MSG,1)                 ! complain 
        RETURN                                      ! refuse to continue 
      ENDIF                                         

      CALL GDF$MOVE(FLAG, 3,GDF_TRACK(IT)%TELESCOPE,NW,STORE)
      CALL GDF$MOVE(FLAG, 1,GDF_TRACK(IT)%STATUS   ,NW,STORE)
      CALL GDF$MOVE(FLAG,15,GDF_TRACK(IT)%RASC_2000,NW,STORE)
      CALL GDF$MOVE(FLAG,80,GDF_TRACK(IT)%SOURCE    ,NW,STORE)

      RETURN 
      END SUBROUTINE GDF$TRACK
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

      SUBROUTINE GDF$XREF(IERR)

C-------------------------------------------------------------------------------
C     set up cross reference table between records and banks 
C-------------------------------------------------------------------------------
      IMPLICIT NONE 

      CHARACTER(*),PARAMETER  :: SRN='GDF$XREF' 
  
      INTEGER   OFFSET                                 ! memory address

      INTEGER   NW                                     ! number of 32-bit words
      INTEGER   IBANK 
      INTEGER   I 
      INTEGER   IERR 
      INTEGER   LOCB     ! Cernlib function 

C---- position of first data word 
      OFFSET = locb(GDF_DATA_ALIGN)

C---- index pointing to NEW flag of each record 
      GDF_XREF(GDF_XREF_RUN)%NEW = (locb(GDF_RUN%NEW)-OFFSET)/4

      GDF_XREF(GDF_XREF_HV   _10)%NEW=(locb(GDF_HV   (1)%NEW)-OFFSET)/4
      GDF_XREF(GDF_XREF_HV   _11)%NEW=(locb(GDF_HV   (2)%NEW)-OFFSET)/4
      GDF_XREF(GDF_XREF_TRACK_10)%NEW=(locb(GDF_TRACK(1)%NEW)-OFFSET)/4
      GDF_XREF(GDF_XREF_TRACK_11)%NEW=(locb(GDF_TRACK(2)%NEW)-OFFSET)/4
      GDF_XREF(GDF_XREF_CCD  _10)%NEW=(locb(GDF_CCD  (1)%NEW)-OFFSET)/4
      GDF_XREF(GDF_XREF_CCD  _11)%NEW=(locb(GDF_CCD  (2)%NEW)-OFFSET)/4

      GDF_XREF(GDF_XREF_FR10)%NEW = (locb(GDF_FR10%NEW)-OFFSET)/4
      GDF_XREF(GDF_XREF_EV10)%NEW = (locb(GDF_EV10%NEW)-OFFSET)/4
      GDF_XREF(GDF_XREF_FR11)%NEW = (locb(GDF_FR11%NEW)-OFFSET)/4
      GDF_XREF(GDF_XREF_EV11)%NEW = (locb(GDF_EV11%NEW)-OFFSET)/4

      GDF_XREF(12)%NEW   = (locb(GDF_MCE     %NEW)-OFFSET)/4
      GDF_XREF(13)%NEW   = (locb(GDF_MCP     %NEW)-OFFSET)/4
      GDF_XREF(14)%NEW   = (locb(GDF_CAL(1)  %NEW)-OFFSET)/4
      GDF_XREF(15)%NEW   = (locb(GDF_CAL(2)  %NEW)-OFFSET)/4

C---- index pointing to first data word in each record 
      GDF_XREF(GDF_XREF_RUN)%FIRST = (locb(GDF_RUN%VERSION)-OFFSET)/4

      GDF_XREF(GDF_XREF_HV   _10)%FIRST=(locb(GDF_HV   (1))-OFFSET)/4
      GDF_XREF(GDF_XREF_HV   _11)%FIRST=(locb(GDF_HV   (2))-OFFSET)/4
      GDF_XREF(GDF_XREF_TRACK_10)%FIRST=(locb(GDF_TRACK(1))-OFFSET)/4
      GDF_XREF(GDF_XREF_TRACK_11)%FIRST=(locb(GDF_TRACK(2))-OFFSET)/4
      GDF_XREF(GDF_XREF_CCD  _10)%FIRST=(locb(GDF_CCD  (1))-OFFSET)/4
      GDF_XREF(GDF_XREF_CCD  _11)%FIRST=(locb(GDF_CCD  (2))-OFFSET)/4

      GDF_XREF(GDF_XREF_FR10)%FIRST = (locb(GDF_FR10%VERSION)-OFFSET)/4
      GDF_XREF(GDF_XREF_EV10)%FIRST = (locb(GDF_EV10%VERSION)-OFFSET)/4
      GDF_XREF(GDF_XREF_FR11)%FIRST = (locb(GDF_FR11%VERSION)-OFFSET)/4
      GDF_XREF(GDF_XREF_EV11)%FIRST = (locb(GDF_EV11%VERSION)-OFFSET)/4

      GDF_XREF(12)%FIRST = (locb(GDF_MCE     %VERSION)-OFFSET)/4
      GDF_XREF(13)%FIRST = (locb(GDF_MCP     %VERSION)-OFFSET)/4
      GDF_XREF(14)%FIRST = (locb(GDF_CAL(1)  %VERSION)-OFFSET)/4
      GDF_XREF(15)%FIRST = (locb(GDF_CAL(2)  %VERSION)-OFFSET)/4

C---- bank format to be used for each record 
      GDF_XREF(GDF_XREF_RUN)%BANK  = GDF_BANK_RUN 

      GDF_XREF(GDF_XREF_HV   _10)%BANK = GDF_BANK_HV 
      GDF_XREF(GDF_XREF_HV   _11)%BANK = GDF_BANK_HV  
      GDF_XREF(GDF_XREF_TRACK_10)%BANK = GDF_BANK_TRACK 
      GDF_XREF(GDF_XREF_TRACk_11)%BANK = GDF_BANK_TRACK 
      GDF_XREF(GDF_XREF_CCD  _10)%BANK = GDF_BANK_CCD  
      GDF_XREF(GDF_XREF_CCD  _11)%BANK = GDF_BANK_CCD 

      GDF_XREF(GDF_XREF_FR10)%BANK  = GDF_BANK_FRAME10
      GDF_XREF(GDF_XREF_EV10)%BANK  = GDF_BANK_EVENT10 
      GDF_XREF(GDF_XREF_FR11)%BANK  = GDF_BANK_FRAME11
      GDF_XREF(GDF_XREF_EV11)%BANK  = GDF_BANK_EVENT11 

      GDF_XREF(12)%BANK  = GDF_BANK_MCE 
      GDF_XREF(13)%BANK  = GDF_BANK_MCP 
      GDF_XREF(14)%BANK  = GDF_BANK_CAL 
      GDF_XREF(15)%BANK  = GDF_BANK_CAL 

C---- equivalenced common block large enough?
      IF (GDF_XREF(GDF_XREF_MAX)%NEW.GT.GDF_DATA_MAX) THEN 
        IERR = GDF$ERROR(SRN,'Not enough memory',1)
      END IF 

C---- total number of data words 
      DO I=1,GDF_XREF_MAX                             ! all records 
        IBANK = GDF_XREF(I)%BANK                      ! which bank?
        NW    = GDF_XREF(I)%NEW-GDF_XREF(I)%FIRST     ! bank length [32-bit] 
        IF (MOD(NW,2).NE.0) THEN                      ! mis-aligned?
          IF (.NOT.GDF_OPTION%SOLARIS) THEN           ! on SOLARIS give up 
            MSG = 'Bank '//GDF_BANK_NAME(IBANK)//' not 64-bit aligned'
            IERR = GDF$ERROR(SRN,MSG,2) 
            PRINT*,'Bank length in 32-bit words',NW 
          ENDIF 
        ENDIF 
***     GDF_BANK_ND(IBANK) = NW + GDF_BANK_NS(IBANK)   ! actual ZEBRA length 
        GDF_BANK_MD(IBANK) = NW + GDF_BANK_NS(IBANK)   ! max Zebra length 
      END DO 
       
      RETURN 
      END SUBROUTINE GDF$XREF
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
CDOC gdf$write.tex 
C  
C    \subsection*{Write data into file}
C    \begin{description}
C    \item{\sf Fortran binding:}
CVER-7 
      SUBROUTINE GDF$WRITE(UNIT,CHOPT,IERR)
C-------------------------------------------------------------------------------
C-    convert records into Zebra banks and output them into FZ file      
C-------------------------------------------------------------------------------
      IMPLICIT NONE 

      INTEGER       UNIT                     ! unit of FZ file 
      CHARACTER*(*) CHOPT                    ! selected option 
      INTEGER       IERR                     ! return code 
CTEX 
C      \item{\sf Action:}\\
C       Write all data records which are flagged by {\tt NEW=.TRUE.} 
C      into the output ZEBRA FZ file.   
C      \item{\sf Options:}\\
C      {\tt CHOPT='C'} requests a checksum calculation.
C     \end{description}
CEND 

      CHARACTER(*),PARAMETER  :: SRN='GDF$WRITE' 
      INTEGER       IRECORD, IBANK, ITELE 

C---- Zebra stuff 
      INTEGER DIV_NOW
      INTEGER DIV_INDEX_NOW
      INTEGER LBANK
      INTEGER LAST 
      INTEGER NEXt 

C---- misc 
      INTEGER NC, NC1,NC2                  ! system clock counts 
      INTEGER I,J 
      LOGICAL CHECKSUM                     ! do or do not calculate it 

      INTEGER    FLAG 
      PARAMETER (FLAG=GDF_MOVE_ZEBRA)
C-------------------------------------------------------------------------------
C     rection to IERR.NE.0 condition 
C-------------------------------------------------------------------------------
      IF (IERR.NE.0) THEN 
        IF (GDF_OPTION%RESET) IERR = 0 
        IF (GDF_OPTION%ABORT) RETURN 
      ENDIF  

      IF (INDEX(CHOPT,'C').GT.0) THEN
        CHECKSUM =  .TRUE.
      ELSE
        CHECKSUM = .FALSE.
      ENDIF 

C-------------------------------------------------------------------------------
C     wipe all Zebra divisons 
C-------------------------------------------------------------------------------
      DO I=1,GDF_DIV_MAX                           ! all division 
        IF (GDF_AREA(I).NE.0) THEN                 ! division not empty?
          CALL MZWIPE(GDF_DIV_INDEX(I))
        END IF          
      END DO 
C-------------------------------------------------------------------------------
C     loop over all possible records, copy them into Zebra banks  
C-------------------------------------------------------------------------------
      DO IRECORD=1,GDF_XREF_MAX                     ! all records
        IF (GDF_DATA_L(GDF_XREF(IRECORD)%NEW)) THEN ! any data to write?

C----     which bank format to be used?
          IBANK = GDF_XREF(IRECORD)%BANK            ! interrogate x-ref table 
          IF (IBANK.LT.1) THEN                      ! bank id illegal? 
            MSG = 'Illegal bank identifier.'        ! complain 
            IERR = GDF$ERROR(SRN,MSG,2)
            RETURN                                  ! give up 
          ELSE IF (IBANK.GT.GDF_BANK_MAX) THEN 
            MSG = 'Unknown bank identifier.'        ! complain 
            IERR = GDF$ERROR(SRN,MSG,3)
            RETURN                                  ! give up 
          END IF 

C----     which Zebra division index? 
          DIV_NOW  = GDF_DIV_CHOICE(IBANK)        ! divison number 
          IF (DIV_NOW.LT.1) THEN                  ! division ok?
            MSG = 'Illegal division index.'       ! complain 
            IERR = GDF$ERROR(SRN,MSG,10)
            RETURN                                ! give up 
          ELSE IF (DIV_NOW.GT.GDF_DIV_MAX) THEN   ! division ok?
            MSG = 'Can not assign division.'      ! complain 
            IERR = GDF$ERROR(SRN,MSG,11)
            RETURN                                ! give up 
          ELSE 
            DIV_INDEX_NOW=GDF_DIV_INDEX(DIV_NOW)  ! ZEBRA divison index
          END IF 


C----     create a top level or a dependant bank
          IF (GDF_AREA(DIV_NOW).EQ.0) THEN        ! any top level bank?
            CALL MZBOOK(                          ! create new top level bank  
     +        DIV_INDEX_NOW,                      ! division index 
     +        LBANK,                              ! pointer new bank 
     +        GDF_AREA(DIV_NOW),                  ! supporting link
     +        1,                                  ! 1=top level bank
     +        GDF_BANK_NAME(IBANK),               ! becomes Hollerith ID 
     +        0,                                  ! no extra links 
     +        0,                                  ! no structural links 
     +        GDF_BANK_MD(IBANK),                 ! max # of data words
     +        9,                                  ! retrieve MZFORM info   
     +        -1)                                 ! do not preset 
          ELSE                                    ! create next bank 
            NEXT = GDF_AREA(DIV_NOW)              ! pointer to 1st bank 
            LAST = GDF_AREA(DIV_NOW)              ! remember                
            DO WHILE(NEXT.NE.0)                   ! any next bank?
              LAST = NEXT                         ! remember last bank  
              NEXT = GDF_STORE_LQ(NEXT)           ! pointer to next bank 
            END DO                                ! if last bank found, ... 
            CALL MZBOOK(                          ! create new next bank 
     +        DIV_INDEX_NOW,                      ! divison index 
     +        LBANK,                              ! pointer new bank 
     +        LAST,                               ! pointer previous bank 
     +        0,                                  ! 0=dependant bank 
     +        GDF_BANK_NAME(IBANK),               ! becomes Hollerith ID 
     +        0,                                  ! # of links 
     +        0,                                  ! # structural links 
     +        GDF_BANK_MD(IBANK),                 ! # of data words 
     +        9,                                  ! retrieve MZFORM info 
     +        -1)                                 ! no preseting  
          END IF 

C----     now copy structure into bank
*         CALL UCOPY(                             ! copy data block 
*    +     GDF_DATA_I(GDF_XREF(IRECORD)%FIRST),  ! Fortran structure
*    +     GDF_STORE_IQ(LBANK+1),                ! first target word in store
*    +     GDF_BANK_ND(IBANK))                   ! number of words to copy 

          CALL UCOPY(                             ! copy header block 
     +      GDF_DATA_I(GDF_XREF(IRECORD)%FIRST),  ! into Fortran structure
     +      GDF_STORE_IQ(LBANK+1),                ! 
     +      6)                                    ! only header words
          GDF_STORE_IQ(LBANK+1) = GDF_VERSION     ! set version number 
          GDF_STORE_IQ(LBANK+2) = IRECORD         ! set record id 


C----     copy contents of Fortran record into bank  
          IF (IBANK.EQ.GDF_BANK_EVENT10) THEN 
            CALL GDF$EVENT10(FLAG,
     .        GDF_STORE_IQ(LBANK+1),
     .        GDF_BANK_ND(IBANK))
          ELSE IF (IBANK.EQ.GDF_BANK_EVENT11) THEN 
            CALL GDF$EVENT11(FLAG,
     .        GDF_STORE_IQ(LBANK+1),
     .        GDF_BANK_ND(IBANK))
          ELSE IF (IBANK.EQ.GDF_BANK_FRAME10) THEN 
            CALL GDF$FRAME10(FLAG,
     .        GDF_STORE_IQ(LBANK+1),
     .        GDF_BANK_ND(IBANK))
          ELSE IF (IBANK.EQ.GDF_BANK_FRAME11) THEN 
            CALL GDF$FRAME11(FLAG,
     .        GDF_STORE_IQ(LBANK+1),
     .        GDF_BANK_ND(IBANK))
          ELSE IF (IBANK.EQ.GDF_BANK_TRACK) THEN 
            CALL GDF$TRACK(IRECORD,FLAG,
     .        GDF_STORE_IQ(LBANK+1),
     .        GDF_BANK_ND(IBANK),
     .        IERR)
          ELSE IF (IBANK.EQ.GDF_BANK_HV) THEN 
            CALL GDF$HV(IRECORD,FLAG,
     .        GDF_STORE_IQ(LBANK+1),
     .        GDF_BANK_ND(IBANK),
     .        IERR)
          ELSE IF (IBANK.EQ.GDF_BANK_CCD) THEN 
            CALL GDF$CCD(IRECORD,FLAG,
     .        GDF_STORE_IQ(LBANK+1),
     .        GDF_BANK_ND(IBANK),
     .        IERR)
          ELSE IF (IBANK.EQ.GDF_BANK_RUN) THEN 
            CALL GDF$RUN(FLAG,
     .        GDF_STORE_IQ(LBANK+1),
     .        GDF_BANK_ND(IBANK))
          ELSE
            MSG  = 'No routine to pack data'
            IERR = GDF$ERROR(SRN,MSG,99)
            RETURN 
          ENDIF  


C----     shorten bank length to actual length required
          GDF_BANK_ND(IBANK)=GDF_BANK_ND(IBANK)-1 ! 
          CALL MZPUSH(
     .      DIV_INDEX_NOW,                        ! divison index 
     .      LBANK,                                ! bank address
     .      0,                                    ! no change in # of links 
     .      GDF_BANK_ND(IBANK)-GDF_BANK_MD(IBANK),! decrease in data words 
     .      'I')                                  ! only structural links

C----     calculate checksum 
          IF (CHECKSUM) THEN                      ! calculate checksum?
            CALL DZCHVC(' ',                      ! no printout 
     +        GDF_STORE_INDEX,                    ! store index 
     +        LBANK+5,                            ! index first word to use
     +        LBANK+GDF_BANK_ND(IBANK),           !       last word 
     +        ' ',                                ! only do checksum 
     +        GDF_STORE_IQ(LBANK+3))              ! store checksum in bank 
          ELSE                                    ! do not calculate checksum  
            GDF_STORE_IQ(LBANK+3) = 0             ! reset  
            GDF_STORE_IQ(LBANK+4) = 0             ! reset 
          END IF
        END IF                                             
      END DO                                      ! next record 

      DO IRECORD=1,GDF_XREF_MAX                   ! for all records
        GDF_DATA_L(GDF_XREF(IRECORD)%NEW)=.FALSE. ! reset .NEW. flag
      END DO                                      ! next record 

C-------------------------------------------------------------------------------
C     verfify structure is correct  
C-------------------------------------------------------------------------------
*      DO I=1,GDF_DIV_MAX
*        CALL DZVERI(
*     +   'Check of division structure',
*     +    GDF_DIV_INDEX(I),
*     +   'CLSU')
*        IF (IQUEST(1).NE.0) THEN 
*          IERR = GDF$ERROR(SRN,'Faulty ZEBRA division.',8)
*          RETURN  
*        END IF
*      END DO 
C-------------------------------------------------------------------------------
C     output all divisions 
C-------------------------------------------------------------------------------
      DO I=1,GDF_DIV_MAX                           ! all division 
        IF (GDF_AREA(I).NE.0) THEN                 ! division not empty?

          GDF_HEADER(GDF_HEADER_RUN ) = GDF_RUN%RUN    !  
          GDF_HEADER(GDF_HEADER_TYPE) = I              !  

          CALL SYSTEM_CLOCK(NC1)                    ! get system time 

          CALL FZOUT(UNIT, 
     +      GDF_DIV_INDEX(I),      ! divison 
     +      GDF_AREA(I),           ! entry to structure 
     +      1,                     ! new event
     +      'DP',                  ! D=complete division, P=permit error returns
     +      2,                     ! header contains integer only  
     +      GDF_HEADER_MAX,        ! # of header words 
     +      GDF_HEADER)            ! the event header 

          CALL SYSTEM_CLOCK(NC2)                     ! get system time 
          NC = NC2 - NC1                             ! take difference 
          GDF_TIME%FZOUT = GDF_TIME%FZOUT + NC       ! sum(difference) 
    
          IF (IQUEST(1).LT.0) THEN 
            IERR = GDF$ERROR(SRN,'FZOUT failed.',1)
            PRINT'(1X,I4,I12)',(J,IQUEST(J),J=1,20)
            RETURN 
          END IF
        END IF 
      END DO 

      RETURN 
      END SUBROUTINE GDF$WRITE 
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

      SUBROUTINE ZTELL(ID,IFLAG)
C-------------------------------------------------------------------------------
C      
C-------------------------------------------------------------------------------
      IMPLICIT NONE
 

      INTEGER ID
      INTEGER IFLAG 
      INTEGER IERR 

      IERR = GDF$ERROR('ZTELL','Fatal ZEBRA error.',1)     
      PRINT*,'ZTELL: ID,IFLAG',ID,IFLAG 
 
      RETURN 
      END SUBROUTINE ZTELL 
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      END MODULE GDF 
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
CDOC gdf_title.tex
C
C   \makeindex
C
C   \newcommand{\Structure} [2]{\subsection{#1}}
C   \newcommand{\Hertz}     [1]{#1\:\mathrm{Hz}}
C   \newcommand{\Collaboration}{Whipple Collaboration}
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C   \begin{document}
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C   \bibliographystyle{tex_inputs:unsrt}
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C  \begin{titlepage}
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C  \begin{center}
C  \vfill 
C
C  \Ptitle{GDF} \vspace*{2.0cm} \\
C  \Large\rm  GRANITE Data Format, {\em Reference Manual} \vspace*{2.0cm}\\
C  Version 0.76, August 1997 
C
C  \vfill
C  \end{center} 
C  \end{titlepage}
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%    Copyright  page                                               
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C  \thispagestyle{empty}
C  \framebox[\textwidth][t]{\hfill\begin{minipage}{0.96\textwidth}%
C  \vspace*{3mm}\begin{center}Copyright Notice\end{center}
C  \parskip\baselineskip
C  {\bf GDF -- Granite Data Format}
C
C   Copyright \Collaboration, 1993 - 1997  
C
C   Copyright and any other appropriate legal protection of these
C   computer programs and associated documentation reserved in all
C   countries of the world.
C
C \begin{flushleft}
C
C  Requests for information should be send to: \\
C % \vspace*{-.5\baselineskip}
C \begin{tabular}{@{\hspace{36pt}}l} 
C {\tt Joachim Rose}\\
C {\tt Department of Physics and Astronomy}\\
C {\tt Leeds University}\\
C {\tt Leeds, LS2 9JT}\\ 
C {\tt United Kingdom}\\
C {\tt e-mail: h.j.rose@leeds.ac.uk}\\
C \end{tabular}
C \end{flushleft}
C
C \vspace*{2mm}
C \end{minipage}\hfill}%end of minipage in framebox
C \vspace{6mm}
C
C {\bf All trademarks appearing in this manual are acknowledged as such.}
C \vfill 
C
C \newpage
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C \section*{Acknowledgements}
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C \pagenumbering{roman}
C \setcounter{page}{1}
C  
C %%% Michael 
C The data format used by the Granite Collaboration has evolved 
C over many years. 
C Naturaly quite a number of features of the present version are 
C derived from the previous versions.
C The Fortran90 derived types are similar to the VMS Fortran structures  
C in the Granite data management software by Michael Schubnell 
C \cite{ms:structure,ms:management}.
C
C %%% 
C Most of the data to be stored is produced by the CAMAC data acquisition 
C software written by Glenn Sembroski.
C Other sources of data are for example the high voltage systems,  
C the tracking computers or the CCD cameras.
C Many collaborators were involved in defining approiate derived data types 
C for each of these data sources.
C
C %%% Monte Carlo 
C The structures for Monte Carlo data follow the conventions inside the 
C MOCCA program written by Michael Hillas. 
C
C %%% ZEBRA 
C Although not visible from to the calling program GDF 
C relies on the package ZEBRA \cite{cern:zebra}.
C Memory allocation is handled by Zebra-MZ \cite{cern:zebra:mz} 
C while data is stored in tape or disk files by calling Zebra-FZ 
C \cite{cern:zebra:fz} routines.
C
C \section*{How to get GDF}
C
C  The GDF source code, the object code libraries for different 
C  operating systems, this manual, and more information on GDF 
C  are available on 
C  \begin{center}
C    {\tt http://phyvs1.leeds.ac.uk/whipple/gdf} 
C   \end{center}
C  \label{webpage}
C  Please report any errors and problems to Joachim Rose, 
C  {\tt h.j.rose@leeds.ac.uk}.   
C  Comments and suggestions on how to improve the software
C  or this manual are also very welcome. 
C
C  \section*{About this manual}
C  This document has been produced using \LaTeX\ \cite{latex} together 
C  with the {\tt CERNMAN} style option, developed at CERN by Michell Goossens.
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%    table of contents, list of figures, list of tables 
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C \cleardoublepage 
C \tableofcontents
C \newpage
C \listoffigures
C %\listoftables
C \clearpage
C \setcounter{page}{1}
C \pagenumbering{arabic}
C
CEND
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
CDOC gdf_intro.tex 
C
C
C  \chapter{A tutorial introduction} 
C
C  This chapter serves as a quick introduction.
C  The aim is to show the essential elements of the package in real programs,
C  but without getting lost in details, rules and exceptions.
C  The example programs in this chapter are not trying to 
C  show all the features and possibilities.  
C  All examples do work and can be used as a starting point to 
C  build useful programs as quickly as possible. 
C  Therefore this section concentrates on the basics: 
C  initialisation, opening of files for reading, and access to the data 
C  via derived type data structures in memory. 
C
C
C  %%% overview the main program 
C  The main program below calls different subroutines depending 
C  on how many parameters were given on the command line. 
C  \index{example!program}
C  \begin{itemize}
C    \item 
C    If no parameter is given the routine \Rind{GDF\$MANAUL} 
C    produces the GDF manual as a \LaTeX\ file. 
C    \index{manual!generation}
C    The master file is \verb!GDF.TEX! \index{gdf.tex} which contains 
C    references to other files that are also generated.
C  \item 
C    If a single file name is given when invoking the program 
C    the several examples subroutine are called which each read 
C    the input file. 
C    \index{example!subroutines}
C  \item 
C    Two files names on the input lines will cause two  
C    calls to \Rind{GDF\$TEST}.
C    The first call to write a new test data file, 
C    the second call one to read a test data file 
C    and check that its contents are read correctly. 
C    The second file can be different from the first and may originate 
C    from a different type of computer. 
C    This makes it possible to verify that the data is read correctly 
C    after transfer from one operating system to another.
C  \end{itemize}
C  In any case then the main program initializes {\tt GDF} first  
C  by calling \Rind{GDF\$INIT}.
C  \index{initialize}
C  The value of first argument passed to the routine is set to {\tt 'Z'} 
C  instructing the routine to initialize  \Rind{ZEBRA}.
C  Like all subroutines \Rind{GDF\$INIT} returns a status code 
C  of {\tt IERR=0} if sucessful. 
C  Run time options are set by calling \Rind{GDF\$OPTION}, see 
C  \pageref{gdf$option} for details.  
C  Before the program terminates \Rind{GDF\$EXIT} is called to 
C  release all resources held by {\tt GDF}.
C  \index{terminate}
C \begin{XMPt}{Example main program}
C \small\tt
C \input{gdf_example.tex}
C \end{XMPt}
C
C  \section{A very simple example}
C
C  %%% intro to first example 
C  The example shown below is a minimalist program.
C  It opens a filem reads first few events and prints out the run number, 
C  any event numbers and frame numbers if the information is available.
C  The call to \Rind{GDF\$OPEN} opens an input file for later reading. 
C  Inside the loop each call to the routine \Rind{GDF\$READ} reads 
C  all records from the file up to and including the next event record. 
C  The call to \Rind{GDF\$CLOSE} closes the file again.
C  \index{open}
C  \index{close}
C  \index{read}
C
C  %%% data structures 
C  After reading an event the data is available to the caller inside  
C  as a Fortran90 derived data type.
C  \index{FORTRAN90}
C  The data types are defined in the Fortran90 module \Rind{GDF}.
C  One way to find out precisely which information is stored 
C  one can take a look at the source for the module in \verb!GDF.FOR! 
C  \index{file!gdf.for}.  
C  Chapter \ref{ch:structures} of this manual presents and explains 
C  the contents of the dereived data types in detail.
C  For the moment is is enough to note that for example to 
C  hold the run information there is a derived data type \verb!GDF_RUN!
C  which include a field \verb!RUN! such that 
C  \verb!GDF_RUN%RUN! is the run number.  
C  The 10~m event number is for example \verb!GDF_EV10%EVENT! and 
C  the ADC values for a given event are stored in \verb!GDF_EV10%ADC!. 
C  \index{FORTRAN90!derived data type}
C  \index{FORTRAN90!module}
C  To get access to the \verb!GDF! module data a 
C  a \verb!USE GDF! statement must be present. 
C
C
C  %%% use of NEW 
C  Whenever the contents of a derived data type object are 
C  updated during a \Rind{GDF\$READ} a value of \verb!NEW=.TRUE.!
C  is set. 
C  For all other objects the filed is set to a value of \verb!NEW=.FALSE.! 
C  before the routine returns control to the caller.  
C  In this example \verb!NEW! is used to only print out new information.
C  In an analysis program a value of \verb!GDF_EV10%NEW=.TRUE.!, 
C  which indicates a newly read event with 10 meter data, 
C  would for example trigger the 10 meter event reconstruction. 
C
C
C  \begin{XMPt}{Example: read input file, print one line per record}
C  \small\tt
C  \input{gdf$example_1.tex}
C  \end{XMPt}
C
C  \section{How to link a complete program}
C
C  %%% CERNLIBS 
C  To link the example program and any program that uses GDF the Cernlib 
C  \cite{cern:cernlib} object libraries are needed. 
C  They are available for different computer hardware 
C  and operating systems including DEC Alphas (both VMS and Unix), 
C  Sun Solaris, Intel PCs running Windows NT, and Intel PCs under Linux.    
C  The GDF web page, see page \pageref{webpage}, has links pointing 
C  to information on the CERN software. 
C  \index{CERNLIBs}
C
C  %%% GDF files 
C  The complete\Rind{GDF} source code resides in a single Fortran90 
C  file \verb!GDF.FOR! \index{file!gdf.for}.
C  Compilation of this file will create a module file, 
C  for example \verb!gdf.mod!, and an object code file, 
C  for example \verb!gdf.obj!. 
C  \index{file!gdf.mod}
C  \index{file!gdf.obj}
C  At a larger site it makes sense to put the GDF module file 
C  into the default directory for Fortran90 module files, 
C  and to put the object code file, 
C  converted into a (shared) library file, 
C  into the default directory for object libraries. 
C
C  The details on how to compile and link depend on the operating
C  system. \index{compile}
C  A Unix script would look like this \index{UNIX!installation}
C  \index{link}
C  \begin{XMPt}{Unix example}
C    set LIB = /usr/local/cernlib/cern/97a/lib
C    set Cernlib=(-L$LIB -lpacklib -lmathlib -lkernlib)
C    f90 -o gdf_example gdf_example.for gdf.for $Cernlib 
C  \end{XMPt}
C  The equivalent sequence for VMS would be:
C  \index{VMS!installation}
C  \begin{XMPt}{VMS: DCL}
C    $ F90 GDF,GDF_EXAMPLE
C    $ LINK GDF_EXAMPLE, GDF, -  
C        CERN:[pro.lib]MATHLIB/LIB,PHTOOLS/LIB,PACKLIB/LIB,KERNLIB/LIB
C  \end{XMPt}
C  The GDF web site, see page \pageref{webpage},  
C  has the GDF source code and the example source code on it. 
C  There may also be already compiled modules and the binary code files.  
C   
C  \section{More examples}
C
C  %%% the GDF$PRINT demostartion routine 
C  \index{print}
C   The next example is am extend version of the previous example. 
C   After reading from the input file the content of the 
C   derived data type objects are printed out by calling the 
C   routine \Rind{GDF\$PRINT}. 
C  \begin{XMPt}{Read input file, print contents}
C  \small\tt
C  \input{gdf$example_2.tex}
C  \end{XMPt}
C
C  To illustrate how to link GDF with the data analysis code, 
C  the next example calculates the sum of ADC signals for 
C  a number of events and then displays the distribution as a 
C  histogram.
C  The \verb!GDF_EV10.NEW! flag indicates a new set of ADC values
C  while the flag \verb!GDF_FR10.VALID! indicates that the frame 
C  information is present which then is used to estimate the 
C   pedestals values.  
C  \index{flag!new}
C  \index{flag!valid}
C  \begin{XMPt}{example: create a histogram of total ADC signal}
C    \small\tt
C    \input{gdf$example_3.tex}
C  \end{XMPt}
C 
C  %%% what does it do?
C  The next example shows how to read 10 meter and 11 meter data 
C  from the same file. 
C  Selected stereo events are then written to a new output file.  
C  An event is considered a stereo event if both the 10 meter and 
C  the 11 meter records were updated during the last read operation. 
C  In addition the satellite clock times are compared.
C  In case the times do not match an error message is printed.
C  If the satellite times match the routine to write data is called.
C  \index{data!stereo}
C  \index{stereo}
C
C  %%% elaborate on NEW
C  The variable {\tt NEW} in each record serves two purposes.
C  Like in the previous example the value indicates whether the record 
C  was updated during the last read. 
C  Before writting any new data into the output file \Rind{GDF\$WRITE}
C  evaluates all {\tt NEW} variables and then only writes the records 
C  which have this variable set to true into the output file. 
C  
C  \begin{XMPt}{example: select stereo events and store them into a new file}
C  \small\tt
C      PROGRAM GDF_SELECT
C-------------------------------------------------------------------------------
C-    select stereo events and store them in a new file  
C-------------------------------------------------------------------------------
C
C     USE GDF 
C
C     REAL*8 DELTA                                               ! in days  
C     REAL*8 OFFSET                                              ! difference 
C     DATA   OFFSET / 2.2D0 /                                    !  GEOS-GPS 
C
C     CALL GDF$INIT('Z',IERR)
C     CALL GDF$OPEN(10, 'mixed.fz','R',IERR)                     ! input file
C     CALL GDF$OPEN(20,'stereo.fz','W',IERR)                     ! output file
C
C     DO WHILE (IERR.EQ.0)                                       ! any error? 
C       CALL GDF$READ(10,' ',IERR)                               ! read event
C       IF (GDF_EV10%NEW.AND.GDF_EV11%NEW) THEN                  ! stereo?
C           DELTA = GDF_EV10%UTC-GDF_EV11%UTC
C           IF (ABS(DELTA*86.4D9-OFFSET).LT.1D0) THEN            ! dT < 1 msec?
C             CALL GDF$WRITE(20,'C',IERR)                        ! output event
C           ELSE
C             PRINT*,'No GPS/GEOS match: ',MSEC1,MSEC2 
C           END IF 
C         END IF 
C       END IF 
C     END DO  
C
C     CALL GDF$CLOSE(10,IERR)
C     CALL GDF$CLOSE(20,IERR)
C     CALL GDF$EXIT(IERR)
C
C     END 
C  \end{XMPt}
C
CEND
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
CDOC gdf_calls.tex 
C
C  \chapter{Subroutine calls}
C
C
C  \index{FORTRAN!subroutines}
C  \index{FORTRAN!arguments}
C  \index{FORTRAN!conventions}
C  The names of arguments in the specification of subroutine calls 
C  follow a simple convention. 
C  Any arguments starting with {\tt I-N} are of type integer, 
C  while arguments starting with {\tt CH} are character variables. 
C  Variable names staring with the letter {\tt L} indicate a 
C  Fortran logical. 
C  All other arguments are of type real, unless the detailed 
C  description of the the subroutine arguments says something else.   
C 
C  As usual in Fortran all variables are passed by reference.
C  Their value remains unchanged unless an {\tt *} following the 
C  name indicates that a variable is used to return a value to 
C  the calling routine.
C 
C  All routines return a status code of \verb!IERR=0! if they complete 
C  successfully. Unless stated otherwise any other value indicates 
C  an error.  
C  \index{IERR}
C  \index{return code}
C 
C  \section{Control}
C    \input{gdf$init.tex}
C    \input{gdf$option.tex}
C    \input{gdf$exit.tex}
C  \section{File operations}
C    \input{gdf$open.tex}
C    \input{gdf$read.tex}
C    \input{gdf$write.tex}
C    \input{gdf$close.tex}
C  \section{Miscellaneous}
C    \input{gdf$manual.tex}
C    \input{gdf$print.tex}
C    \input{gdf$test.tex}
C
CEND
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
CDOC gdf_types.tex 
C
C  \chapter{Derived data types and structures}
C  \label{ch:structures}
C
C  \section{General considerations}
C
C  Each design of a data format has to address a few standard questions.
C  How can the data format software change and stay backwards compatible?  
C  Should the software try to detect errors and problems in the data? 
C  Which information should be recorded? Only the events (images) themsleves?
C  Should other information such as high voltage values or tracking system 
C  be included?
C
C  \begin{itemize}
C
C  \item % time information 
C  Almost all data structures must have some {\em time information} in them,
C  in order to correlate them to other data structures and or external 
C  information. 
C  At event rates of up to 200~Hz the time information does not need 
C  to be much more accurate than a millisecond.
C  The modified Julian day including the fraction of day 
C  \index{time!modified Julian days} is used as unit of time  
C  and stored as a 64-bit real variable. 
C  For a number representation with 50 bits to store the mantissa 
C  of the real number  
C  \footnote{as for example on Sun SparcStations, 
C  the Zebra data exchange format itself uses two more bits} 
C  the least significant bit is equivalent to $3.8\:\mu\mathrm{s}$
C  for modified Julian days up to $50000$.
C
C  In the event data structures higher accuracy time is available.
C  The prcise time is stored in three 32-bit integer variables which 
C  contain the number of modified Julian days, 
C  the number of seconds since midnight and 
C  the number of nanoseconds since the last full second.  
C  \index{time!GPS}
C  \index{time!UTC}
C
C  \item % use of enumerators 
C  Integer values are defined for {\em status information} such as 
C  the sky quality or the position of trigger bits in the 
C  trigger status word.
C  Clearly defined values should allow the analysis software 
C  to use this information more reliably.  
C  For a list of possible values see appendix \ref{parameter}. 
C  \index{parameter!definitions}
C  \index{sky quality}
C  \index{status information}
C  \index{trigger setup}
C
C  \item %%%
C  Each Fortran structure contains a {\em checksum} over the contents 
C  of the record. This allows to detect errors corrupting the data. 
C  \index{checksum}
C  \index{error detection}
C
C  \item %%% future changes 
C  In the later data analysis it would be very inconvenient if  
C  the inevitable {\em changes to the data format} 
C  were not backwards compatible. 
C  Each Fortran structure starts with the  
C  version number of the format used when writing the data.
C  To allow the later insertion of more data words the Fortran 
C  structures are organized into sectors of similar type data words.
C  When stored in a ZEBRA bank each sector is preceeded by a header word 
C  which indicates the type and the length of the sector. 
C  The version number and the sector header words together 
C  allow backwards compatible changes in the data format. 
C  \index{changes!future}
C  All this is normally of no concern to the calling program, 
C  which can assume that a structure has been updated correctly.  
C
C  \end{itemize}  
C  
C  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C  \section{Global data}
C  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C  
C  \Structure{Run status}{RUN}
C  
C  \index{record!run status}
C  This data structure (figure \ref{fig:run}) contains global information
C  on the status of the whole detector and observer comments. 
C  The information is entered into the data stream before the first event
C  is recorded and whenever there is a change during the run.  
C  For instance in case of additional observer comments 
C  during the run this record appears several times.
C  The run status record is inserted into the data sequential file 
C  in between the event records recorded at the time the observer 
C  enters the comment.  
C  
C  The character strings containing information on sky quality and trigger 
C  conditions are only there for historical reasons. 
C  For new data integer numbers describe sky quality and trigger  
C  condition (see appendix \ref{parameter}). 
C  
C   
C  \begin{figure}[tbph]
C  \begin{XMPt}{run status information}
C  \input{gdf_run.tex}
C  \end{XMPt}
C  \caption[Run status record]
C        {Data structure for run status information.
C         The array index 1 refers to the 10 meter telescope while 
C         2 refers to the 11 meter telescope.}
C     \label{fig:run}
C  \end{figure}
C
C  \Structure{CCD camera}{CCD}
C  \index{record!CCD}
C  The CCD cameras on both telescopes are controlled using the same software.
C  For each image taken by the CCD camera the position of the brightest 
C  stars is located.   
C  After the image analysis the data records for CCD results
C  from both telescopes are in the same format (figure \ref{fig:ccd}). 
C  \begin{figure}[tbph]
C  \begin{XMPt}{CCD camera}
C  \input{gdf_ccd.tex}
C  \end{XMPt}
C  \caption[CCD camera records]
C          {Data structure for CCD camera.
C           The same structure is used to define two Fortran records.
C           The first one for the 10 meter telescope the second for the 
C           11 meter telescope.}
C   \label{fig:ccd}
C   \end{figure}
C
C
C  \Structure{Tracking system} 
C  \index{record!tracking}
C
C  During a run both telecope tracking systems pass information on 
C  the source coordinates, the current mode of operation and the 
C  actual elevation and azimuth of the telescope to the data acquisition 
C  computer.  
C
C  Since the tracking control systems are similar the same data format 
C  (figure \ref{fig:track}) is used for both telescopes. 
C  The observed source is identified by its right ascension and 
C  declination in the FK5 J2000 reference frame.
C  A source name is recorded as well, although in general it should 
C  not be used to determine which source the telescope pointed at. 
C  For off-source runs the source coordinates remain the same as 
C  for the on source run. 
C  However the variables describing the telescope pointing direction 
C  show the actual orientation.
C  An additional variable records the angle between the actual 
C  and the nominal orientation. 
C  The differences in the coordinates beween on-source and off-source 
C  run are stored in a separate set of variables. 
C  If the telescopes are inclined towards an assumed common interaction 
C  point the height of this point above ground and the computed changes 
C  in elevation and azimuth are available.   
C 
C  \begin{figure}[tbph]
C  \begin{XMPt}{tracking system information}
C  \input{gdf_track.tex}
C  \end{XMPt}
C  \caption[Tracking system records]
C          {Data structure for both tracking systems.}
C  \label{fig:track}
C  \end{figure}
C
C  \Structure{High voltage}{HV}
C  \index{record!high voltage}
C
C  The status of the high voltage is continually monitored during a run. 
C  Each time the currents and voltages are measured their values 
C  are included in the data stream. 
C  Before a run starts the HV software  
C  will normally make sure that the high voltage is on. 
C  Therefore the first high voltage information record 
C  (figure \ref{fig:hv}) will preceed the first event record.  
C  To tell whether the measured voltages and currents are acceptable 
C  the nominal values, the current requested values and the allowed 
C  tolerances are included as well. 
C  In addition to the measured values a status value for each channel 
C  and a global status flag for the whole high voltage system is recorded 
C  as well (appendix \ref{parameter}).
C
C  \begin{figure}[tbph]
C  \begin{XMPt}{High voltage information}
C  \input{gdf_hv.tex}
C  \end{XMPt}
C  \caption[High voltage records]
C           {Data structure high voltage information}
C  \label{fig:hv}
C  \end{figure}
C
C  % \Structure{Calibration}{CAL}
C  % \index{record!calibration}
C  % This structure (see figure \ref{fig:cal}) contains the result of 
C  % a pedestal and gain calculation.
C  % This is intended for a reconstruction program which calculates the 
C  % calibration constants to then insert the calibration record 
C  % into the data file.  
C  % The next time the reconstruction program can then read the calibration 
C  % for the file, which should safe time because in many case there 
C  % is not need to repeat the calibration.  
C
C  % An integer value inside the structure is used to indicate the method 
C  % used to calculate pedestal and gains (see appendix \ref{parameter}). 
C  % The variable {\tt status} is zero for all tubes with a valid 
C  % pedestal and gain calculation. 
C  % Any non zero value indicates a problem during the calculation.  
C   
C  % Up to five values $p_i,(i=1,...,5)$ may describe the result of a pedestal 
C  % calculation which parametrizes the observed ADC distribution 
C  % near its maximum as $f(x,p_i)$.
C  % \begin{eqnarray}
C  %  f(x,p_i) &=& p_1\cdot 
C  %             \mathrm{e}^{-\frac{1}{2}
C  %             \left(\frac{\Delta(x)}{\sigma(x)}\right)^2} \\
C  %  \Delta(x)&=& (x-p_2) \\
C  %  \sigma(x)&=& p_3 \left[ 1 + p_4\:\Delta + p_5\:\Delta^2 \right]
C  % \end{eqnarray}
C  % The parameters $p_1$ and $p_2$ describe the height and position of the peak. 
C  % $p_2$ is normaly refered to as the pedestal value.
C  % The third parameter $p_3$ called {\tt width} is a measure of the noise 
C  % (electronic noise, sky background) at ADC values near the pedestal value.
C  % The remaining two parameters (inside the Fortran structure $p_4$ is 
C  % called {\tt asycor}, $p_5$ is named {\tt symcor}) modify the gaussian
C  % distribution into an asymmetric function.  
C  % This takes care of the fact that the observed distribution is rather 
C  % a convolution of power law and random fluctuations
C  % than a gaussian. 
C  % After a fit the error on the parameters and a probability which describes 
C  % the likelihood that the observed distribution matches the parametrization 
C  % can be entered.
C  %
C  % A calculation which does not parametrize the ADC distribution will only 
C  % enter values for the height, the pedestal value itself and the width.
C  % The other values should be zero with the exception of the probability 
C  % which should be set to one. 
C  %
C  % If the gain calculation parametrizes the ADC distribution a gain 
C  % correction factor $a$ and an exponent $b$ to renormalize the ADC value 
C  % $x$, forcing all distributions to obey the same power law, are available. 
C  % \begin{eqnarray}
C  %    x' &=& a \cdot (x-p_2)^b  
C  % \end{eqnarray}
C  % In addition parameter errors and a probability that the distribution matches
C  % the parametrization is stored.
C  %
C  % For other methods a linear gain correction is available and the 
C  % probalility is set to one.
C  %
C  % \begin{figure}[tbph]
C  % \begin{XMPt}{calibration results}
C  % \input{gdf_cal.tex}
C  % \end{XMPt}
C  % \caption[Calibration results]
C  %    {Data structure for calibration results.
C  %     The array index 1 refers to the 10 meter telescope while 
C  %     2 refers to the 11 meter telescope.}
C  % \label{fig:cal}
C  % \end{figure}
C  %
C  % \Structure{Monte Carlo (very preliminary)}{MCR}
C  % \index{record!Monte Carlo}
C  %
C  % Two cartesian coordinate systems are used to store Monte Carlo information.
C  % The orgin of both systems is the intersection between the shower axis 
C  % and a plane parallel to the sea level. 
C  % The {\em geographic coordinate system} ($x,y,z$) is aligned 
C  % to this horizontal plane.
C  % \begin{DL}{x-axis}
C  % \item[x-axis] points east within horizontal plane. 
C  % \item[y-axis] north north within horizontal plane  
C  % \item[z-axis] is upwards perpendicular to horizontal plane  
C  % \end{DL}
C  % The data structure figure \ref{fig:mce} includes the height 
C  % of the horizontal plane above the sea level. 
C  % The variable {\tt position(3,n)} referes to the $(x=1,y=2,z=3)$
C  % geographic position of the {\tt n} telescopes. 
C  %
C  %
C  % The second {\em shower coordinate system} is rotated 
C  % with respect to the  last coordinate system. 
C  % \begin{DL}{x'-axis}
C  % \item[x'-axis] points along the shower axis towards the incoming shower.
C  % \item[y'-axis] is choosen to be horizontal, which means the $x'-axis$ 
C  %               lies within the horizontal plane of the geographic coordinate
C  %               system. 
C  %               The aproaching shower will see this axis pointing to the 
C  %               right.
C  % \item[z'-axis] is the third axis of a right handed coordinate system.
C  %               The axis points upwards, away from the ground.    
C  % \end{DL}
C  % The variable {\tt location(3,n)} refer to the $(x'=1,y'=2,z'=3)$
C  % position of the {\tt n} telescopes in the shower coordinate frame.  
C  %
C  % \begin{figure}[tbph]
C  % \begin{XMPt}{Monte Carlo event data}
C  % \input{gdf_mc.cde}
C  % \end{XMPt}
C  % \caption[Monte Carlo event data]
C  %    {Part of the data structure Monte Carlo data.}
C  % \label{fig:mce}
C  % \end{figure}
C
C  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C  \section{Event records and frames}
C  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C  Event records and frame records together account for almost all of 
C  the recorded data in a file. 
C  There is little difference between the 10m and the 11m records,
C  simply because the data acquisition systems are very similiar.  
C  The details of the 10m and 11m data records are described further 
C  below.
C  The next few paragraphs first discuss some general points which apply
C  to both the 10m and the 11m records.
C
C  %%% storage of 32-bit words 
C  The {\em unsigned 16-bit or 32-bit numbers} read from CAMAC system 
C  and stored as integers are interpreted as {\em signed} numbers by 
C  Fortran compilers.  
C  If there is no better alternative then such numbers are stored as ZEBRA
C  32-bit patterns and the program calling a GDF routine is expected 
C  to correctly decode the information stored as a 32-bit Fortran LOGICAL.     
C  However, to avoid problems like integer overflows or 
C  wrong signs numbers are mostly reformatted.  
C  For example the total livetime is determined by two 32-bit scaler values. 
C  After reformatting the livetime is stored as the integer number of seconds 
C  since the start of the run and the integer number of nanoseconds. 
C
C  ADC values and TDC values are stored in 16-bit Fortran {\tt INTEGER} arrays.
C  If the calling program is coded in Fortran77 then care must be taken not 
C  to pass these arrays to a subroutine expecting a {\tt REAL} or 32 bit 
C  {\tt INTEGER} array.     
C  Only the lower 11 bits of ADC value can be non-zero. 
C  The other 5 bits are always zero. 
C  The highest ADC bit signals an {\em ADC overflow}.
C  It is left to the calling program to detect\footnotemark this condition 
C  and then deal with this situation, for instance by rejecting the whole event.
C  \footnotetext{
C    A convenient way to test the overflow bit is the
C    FORTRAN intrinsic logical function {\tt BTEST(ADC,10)}. 
C    Note that the least significant bit is at position 0.
C    Position 10 in {\tt BTEST} refers to the 11th bit.}
C
C  %%% efficiency
C  The event records take up most of the space when the data is written 
C  onto a storage media. 
C  The physical length of this structure effectivly determines the 
C  total space needed. 
C  Storing for example 541 ADCs values in 16-bit words 
C  requires 1082 bytes, plus one extra 4 byte header word
C  to indicate the data type and the number of words to follow.  
C  An event can include up to several hundred more bytes,
C  for example TDC and counter values, or time information. 
C  The ZEBRA overhead is typically 48 bytes per event. 
C  Data compression utilities such as {\tt compress} or {\tt gzip} 
C  reduce the file size by about one half to a third. 
C
C  %%% total amount of data per hour 
C  At a trigger rate of $\Hertz{200}$ a 541 pixel camera produces 
C  around $1.5\mathrm{kb}\cdot\Hertz{200} = 350 \mathrm{kb/s}$ 
C  of data.
C  The average data rates can reach the order of $1\mathrm{Gb}$ per hour
C  or $10\mathrm{Gb}$ per night, or $1\mathrm{Tb}$ per year.
C   
C
C  %%% array length
C  The frame and the event records contain variable length arrays.
C  The maximum length is identical to the declared size of an array 
C  defined via parameter statements. 
C  The number of valid entries depends on the actual number of 
C  ADCs and TDC channels which is included in the data structure.
C  Any program using the GDF data structure should use these
C  actual numbers, not the maximum length defined in the parameter 
C  statements. 
C  Future version of GDF are likely to allocate the memory for 
C  variable length arrays dynamicaly. 
C
C
C\Structure{10 meter frame and 11m frame}{FR10}
C\index{record!frame}
C
C  %%% explain what is a frame 
C  The purpose of the 10m and 11m frame records is identical.
C  While the readout system takes events extra data is recorded  
C  for calibration and monitoring purposes. 
C  An example are the single channel discriminator rates. 
C  Before summer 1997 the ADC values taken at a random time 
C  which can be used to calculate 'night sky' pedestal values
C  where part of the frame data structure.  
C  For new data the number of ADC values in a frame is always zero. 
C  To simplify the readout the ADC values for pedestals events 
C  are included in normal event records. 
C  A trigger bit set in the event record indentifies these events as 
C  pedestals events. 
C
C  \begin{figure}[tbph]
C   \begin{XMPt}{10 meter frame}
C   \input{gdf_fr10.tex}
C   \end{XMPt}
C   \caption[10 meter frame record]
C         {Data structure for 10 meter frame.}
C   \label{fig:fr10}
C   \end{figure}
C
C   \begin{figure}[tbph]
C   \begin{XMPt}{11 meter frame}
C   \input{gdf_fr11.tex}
C   \end{XMPt}
C   \caption[11 meter frame record]
C        {Data structure for 11 meter frame.}
C   \label{fig:fr11}
C   \end{figure}
C
C   \Structure{10m and 11m event}{EV10}
C   \index{record!event}
C    Figure \ref{fig:ev10} shows the data structure for the 10 meter event 
C   information.
C
C 
C   \begin{figure}[tbph]
C   \begin{XMPt}{10 meter event}
C   \input{gdf_ev10.tex}
C   \end{XMPt}
C   \caption[10 meter event record]
C           {Data structure for 10 meter events.}
C   \label{fig:ev10}
C   \end{figure}
C
C   %%% contents of this structure 
C   For each event this structure (figure \ref{fig:ev11})
C   contains ADC values, TDC values, scaler contents and 
C   memory contents read out from the CAMAC system.  
C   The number of ADCs and TDCs is part of the record itself 
C   since the camera and its geometry might change from year to year. 
C   It should still be possible to analyse data from several years together.  
C
C   %%% trigger bit pattern 
C   Previously the type of event was recorded as an integer number.
C   This scheme does not allow an event to pass several trigger criteria 
C   simultaneously.  
C   Although not used at present a trigger bit pattern is included inside
C   which any of 32-bits can be set. 
C   This allows to record the status of several trigger signals in parallel. 
C
C   %%% satellite time 
C   The UTC time information from the GPS or GEOS module is decoded and then 
C   stored as {\tt REAL*8} variable containing the modified Julian day, 
C   and the fraction of day.
C   The status of the satellite clock module is recorded in the status word. 
C
C   %%% storage of integer*2 words.
C   Pairs of two ADC or TDC values are stored within a 32-bit word. 
C   Zebra treats each such word as a bit pattern, which makes sure each bit 
C   appears in the same position regardless of the computer used. 
C   However, when referring to the 16-bit half word using an 
C   {\tt integer*2} array the result can be different depending on 
C   the computer used. 
C   The array ordering is correct on DecStations and Alphas, 
C   while on SUN and other systems with a IEEE number 
C   representation the ordering will be wrong.
C   However normaly this is of no concern to the caller. 
C   If necessary a internal GDF routine will swap the half words.
C   Still it is a good idea to compare a ADC values against 
C   the original values on the data acqusition computer 
C   after installing a new version of GDF. 
C
C   \begin{figure}[tbph]
C   \begin{XMPt}{11 meter event}
C   \input{gdf_ev11.tex}
C   \end{XMPt}
C   \caption[11 meter event record]
C          {Data structure for 11 meter events.}
C   \label{fig:ev11}
C   \end{figure}
C
CEND 
CDOC gdf_inter.tex 
C
C  \chapter{Implementation}
C 
C  From the viewpoint of the calling program the subroutines provided 
C  by GDF simply copy the contents of Fortran structures into files 
C  and vice versa. 
C  As long as reading and writing is reasonably fast and disk space 
C  overheads are small it is of not much interest to know  
C  how this actually happens.
C  This is only true as long as there is no need to modify the software
C  and adapt it to changed requirements.
C  This chapter describes some of the basic ideas and techniques which 
C  should be kept in mind when planning future changes. 
C
C  \section{Fortran records} %%% advantages of Fortran structures.
C  Data is exchanged with the calling program via data structures 
C  specifiea a Fortran90 dreived data types.
C  The reason to choose this method are:
C  \begin{itemize}
C  \item The previous formats \cite{ms:structure} and older Fortran77 
C        GDF versions used structures as well. 
C        Therefore the it should be easy to adapt the existing analysis 
C        software, 
C        which represents a large investment and should not become obsolete.
C        Ideally all that is required should be to rename a few variables 
C        and subroutines. 
C  \item The caller is shielded from the details like number representation,
C        or operating system dependent I/O~operations.   
C  \item Reference by name is more user friendly 
C        (leads to rather more readable, structured and shorter code) 
C        and thereby safer than reference by an integer pointer to the 
C        location of a data word with an large array. 
C  \item To group variables in a structure allows to have multiple 
C        instances of the same structure. 
C        For example the two seperate 
C        records describing the CCD results for both telescopes 
C        are identical.
C  \end{itemize}
C
C
C  \section{Choice of data representation software package} 
C  Almost every high energy experiment needs to store and then exchange 
C  data between different computers. 
C  Most experiments choose to adopt an existing software package to solve 
C  the problem for them. 
C  Instead of trying to come up with a completly new solution the 
C  GDF software acts as an interface been the caller who only deals 
C  with Fortran records and the Zebra package \cite{cern:zebra}.
C  The advantages of using Zebra are:
C
C  \begin{itemize}
C  \item Zebra represents many years of experience in how to store and handle 
C        data from high energy physics experiments. 
C  \item It has a large worldwide user community. Many high level application 
C        programms (PAW) and packages (HBOOK,HIGZ,KUIP) use Zebra to  
C        store and exchange data. 
C  \item It is supported by the CERN Application Software group, 
C        which includes porting it to new computer models (DEC Alpha)
C        and operating systems (Solaris, NT). The use of the software is free
C        and it is available both as an object library or as source code. 
C  \item The Zebra-FZ record header allows to quickly search through a large 
C        data set to select events because it eliminates the need to read
C        the whole event. 
C        Direct access to events records could be impletemented 
C        if for some resason seqquential reading of event is not 
C        longer sufficient.   
C  \item The Zebra-FZ exchange file mechanism provides machine independent
C        reading and writing of data readable on different computers. 
C  \item The data can be exchanged over computer networks either in binary 
C        format (using \Cind{FTP} or \Cind{ZFTP}) or if the network is not 
C        truely transparent in ASCII format. 
C  \item Since Zebra banks are self describing future format changes present 
C        no problem. 
C  \item Zebra includes routines for error detection by checksum and 
C        is able to recover the remaining data from files if a 
C        physical record block is no longer readable.
C  \end{itemize} 
C
C
C  \section{Relation between Zebra banks and Fortran structures}
C  For each Fortran structure a corresponding Zebra bank is defined. 
C  The contents of a record are copied into a Zebra bank without 
C  any changes. 
C  This is stricly true only for new data.
C  To be able to read old data after a changes in the Fortran record format
C  each structure is devided into a header (version number, checksum, 
C  UTC time) and sectors.
C  Sectors are blocks of identical type variables (32-bit patterns, integer,
C  real, double precision real, character).
C  The sectors are preceeded by a word which describes the length 
C  and the type of sector. 
C  In future a sector within a Fortran structure may become larger.
C  It is then still possible to copy banks written in a previous format 
C  into the new 
C  format sectors because the software is able to tell the difference 
C  between bank and structure by looking at the sector header words.
C  If there is any difference 
C  it can then introduce empty data words as needed.
C
C  %%% adding banks 
C  Adding completly new Fortran structure does not require much effort.
C  Apart from defining the structure itself only a few arrays  
C  have to be increased in length.
C
C
C  \section{Relation between Zebra records and Zebra store divisions}
C
C  Zebra organizes the memory space into Zebra divisions.
C  The file space is devided into Zebra event records.
C  GDF uses the convention that the contents of a division end up 
C  in the same record inside a file. 
C  For instance all event data is collected as a Zebra bank structure inside 
C  one division and the written into the same data file record.
C  This record may contain both the 10 meter and the 11 meter information.
C
C  Information of validity time goes into different divisions.
C  Whenever data is read from a record/divison with a longer validity span
C  all Fortran records with shorter validity a flaged as invalid. 
C  For example after reading a new run status record all other records 
C  become invalid. 
C
C
CEND 
CDOC gdf_appen.tex 
C
C  \begin{appendix}
C
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C  \chapter{Parameter values}
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C  \label{parameter}
C  \begin{XMPt}{Predefined parameter values}
C  \input{gdf_para.tex}
C  \end{XMPt}
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C  \chapter{Converting old data}
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C  A dataset may only be available in a pre GDF data format.
C  The Fortran77 version of GDF included subroutines to convert 
C  events from the previous format into the GDF format.
C  If this conversion is still needed then the source code 
C  can be taken from earlier version of GDF.      
C
C  GDF itself tries to be backward compatible and will read files
C  written by earlier version of GDF. 
C
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C  \chapter{Source code and documentation}
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C  \label{install}
C
C  The complete GDF source code and the documentation are maintained 
C  in a single file named {\tt gdf.for}.
C  The file is available from the web server described on 
C  page \pageref{webpage}.
C
C  Any Fortran90 compiler should accept the source code as it is. 
C  The code contains no external references to other Fortran90 modules. 
C
C  The routine \Rind{GDF\$MANUAL} read the {\tt gdf.for} file and the
C  creates a set of \LaTeX\ files. 
C  The main file is {\tt gdf.tex} which references all the other \LaTeX\ files. 
C  After compilation with \LaTeX\ and then {\tt dvips} the file {\tt gdf.ps}
C  contains the complete manual.   
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C% \chapter{C language interface}
C%
C% The C language interface is provided courtesy of Rod Lessard Dublin.
C% It is included in the {\tt gdf.car} file. 
C% As customary in the C and Unix world it is {\em provided as is} 
C% with no warranty.   
C% 
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C\chapter{Version history and future changes}
C
C \section*{ }
C \begin{XMPt}{Version history}
C \input{gdf_vers.tex}
C \end{XMPt}
C
C
C \end{appendix}
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%  bibilography and index
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C \bibliography{gdf}
C
C  \input{\jobname.ind}
C
CEND 
CDOC gdf.bib
C
C @string{apj     = "Ap.J."}
C @string{leeds   = "The University of Leeds"}
C @string{internal="Granite collaboration, internal note"}
C
C @book{astalm,
C  title    ="Astronomical Almanac",
C  publisher="United States Naval Observatory, Royal Greenwhich Observatory",
C  year     ="1981-1993"}
C
C @manual{slalib,
C  title  ="{SLALIB} - a Library of Subprograms",
C  author ="P.~T.~Wallace",
C  organization="Starlink Project",
C  note   ="Starlink User Note 67.14"}
C
C @unpublished{jodrell,
C   author="A.~G.~Lyne, R.~S.~Prichard",
C   title ="{Jodrell Bank} {Crab} Pulsar Timing Results Monthly Ephemeris",
C   month ="February",
C   year  ="1993",
C   note  ="University of Manchester, Nuffield Astronomy Laboratories,
C           Jodrell Bank, Maccelsfield, Chesire, SK11 9DL, U.K."} 
C
C @manual{vms:syslib,
C  title  ="{VMS} system services reference manual",
C  organization="Digital Equipment Corporation",
C  note   ="{VMS} version 5.0",
C  year   ="1984",
C  month  ="April"}
C
C @manual{vms:fortran,
C  title  ="{VAX FORTRAN} user manual, 
C           {VAX FORTRAN} language reference manual",
C  organization="Digital Equipment Corporation",
C  note   ="{VAX FORTRAN} version 5.0",
C  year   ="1988",
C  month  ="June"}
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C @manual{cern:zebra,
C  title  ="{ZEBRA} users guide",
C  author ="R.~Brun, J.~Zoll",
C  organization="{CERN} program library",
C  note   ="long write-up {Q100}, version 3.53",
C  year   ="1987",
C  month  ="January"}
C
C @manual{cern:zebra:mz,
C  title  ="{ZEBRA} reference manual, {MZ} memory management",
C  author ="J.~Zoll",
C  organization="{CERN} program library",
C  note   ="{ZEBRA} version 3.55",
C  year   ="1987",
C  month  ="November"}
C
C @manual{cern:zebra:fz,
C  title  ="{ZEBRA} reference manual, {FZ} sequential {I/0}",
C  author ="J.~Zoll",
C  organization="{CERN} program library",
C  note   ="{ZEBRA} version 3.54",
C  year   ="1987",
C  month  ="July"}
C
C @manual{cern:zebra:dia,
C  title  ="{ZEBRA} reference manual, {DIA} error diagnostics",
C  author ="J.~Zoll",
C  organization="{CERN} program library",
C  note   ="version 3.61",
C  year   ="1989",
C  month  ="July"}
C
C @manual{cern:patchy,
C  title  ="{PATCHY} reference manual",
C  author ="H.~J.~Klein, J.~Zoll",
C  organization="{CERN} program library",
C  note   ="version 4.13",
C  year   ="1988",
C  month  ="March"}
C
C @manual{cern:cernlib,
C  title  ="{CERNLIB} Short Writeups",
C  author ="Application Software Group",
C  organization="{CERN} Computing and Networks Division",
C  year   ="1993",
C  month  ="May",
C  note   ="PostScript version avaliable on asis01.cern.ch",}
C
C @manual{cern:hbook,
C  title  ="{HBOOK} reference manual",
C  author ="Application Software Group",
C  organization="{CERN} Computing and Networks Division",
C  note   ="version 4.15",
C  year   ="1992",
C  month  ="September"}
C
C @manual{cern:higz:hplot,
C  title  ="{HIGZ} user's guide, {HPLOT} user's guide",
C  author ="Application Software Group",
C  organization="{CERN} Computing and Networks Division",
C  year   ="1993",
C  month  ="May"}
C
C @manual{cern:paw,
C  title  ="{PAW} the complete reference",
C  author ="Application Software Group",
C  organization="{CERN} Computing and Networks Division",
C  note   ="version 1.14",
C  year   ="1992",
C  month  ="July"}
C
C @manual{cern:kuip,
C  title  ="{KUIP}, Kit for a user interface package",
C  author ="Application Software Group",
C  organization="{CERN} Computing and Networks Division",
C  note   ="version 2.0",
C  year   ="1991",
C  month  ="December"}
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C @manual{gsi:guide,
C  title  ="{Unix} primer, {GSI} local guide",
C  editor ="M.~Dahlinger",
C  organization="{GSI} Darmstadt, Germany ",
C  note   ="version 1.01",
C  year   ="1992",
C  month  ="December"}
C
C
C @book{latex,
C  title  ="\LaTeX\ user's guide \& reference manual",
C  author ="L.~Lamport",
C  publisher="Addison-Wesely",
C  year   ="1986"}
C
C @book{tanenbaum,
C  title  ="Operating systems",
C  author ="A.~S.~Tanenbaum",
C  publisher="Prentice-Hall",
C  year   ="1987"}
C
C @book{gnu:emacs,
C  title  ="Learning {GNU Emacs}",
C  author ="D.~Cameron and B.~Rosenblatt"}
C
C @book{emacs,
C  title  ="{GNU Emacs}",
C  author ="M.~A.~Schonover, J.~S.~Bowie, W.~R.~Arnold",
C  publisher="Addison-Wesley",
C  year   ="1992"}
C
C @book{fortran90,
C  title  ="Fortran90 explained",
C  author ="M.~Metcalf, J.~Reid",
C  publisher="Oxford University Press",
C  year   ="1990"}
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%C
C
C @unpublished{ms:coincidence,
C   author="M.Schubnell",
C   title ="Status of coincident event analysis",
C   day   ="21",
C   month ="September",
C   year  ="1992",
C   note  = internal}
C
C @unpublished{ms:structure,
C   author="M.Schubnell",
C   title ="The {GRANITE} data and file organization",
C   day   ="6",
C   month ="October",
C   year  ="1992",
C   note  = internal}
C
C @unpublished{ms:management,
C   author="M.Schubnell",
C   title ="Data and file management for {GRANITE}",
C   day   ="6",
C   month ="October",
C   year  ="1992",
C   note  = internal}
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C @manual{leeds:granite,
C   title  ="{GRANITE} reference manual, version 0.34",
C   author ="H.~J.~Rose",
C   organization="University of {Leeds}"}
C
C
C @manual{leeds:gralib,
C   title  ="{GRALIB} reference manual, version 0.03",
C   organisation=leeds}
C
C @manual{leeds:gbm,
C  title  ="{Granite Buffer Manager}, reference manual",
C  author ="H.~J.~Rose",
C  organization="University of {Leeds}",
C  year   ="1993",
C  month  ="August"}
C
C @manual{leeds:cpc,
C  title  ="Cluster process communication package, 
C           reference manual for use at the {Whipple Observatory}",
C  author ="H.~J.~Rose",
C  organization="University of {Leeds}",
C  year   ="1993",
C  month  ="August"}
C
C @manual{leeds:gdf,
C  title  ="{Granite Data Format}, reference manual",
C  author ="H.~J.~Rose",
C  organization="University of {Leeds}",
C  year   ="1993",
C  month  ="August"}
C
C @manual{leeds:local,
C  title  ="{Local Guide}, {An} introduction to data analysis at {Leeds}",
C  organization="University of {Leeds}, Department of Physics",
C  year   ="1993",
C  month  ="August"}
C
C
CEND 
CDOC gdf.html 
C
C<HTML>
C<HEAD>
C   <TITLE> Granite Data Format </TITLE>
C   <META CONTENT="GDF version 82">
C</HEAD>
C<body bgcolor="white">
C
C<P>&nbsp;</P>
C
C<BLOCKQUOTE><P><IMG SRC="/gif/SB01-1.GIF" WIDTH=640 HEIGHT=5
CX-SAS-UseImageWidth X-SAS-UseImageHeight ALIGN=bottom></P>
C
C<H1>GDF</H1>
C
C<P><IMG SRC="/gif/SB01-1.GIF" WIDTH=640 HEIGHT=5
CX-SAS-UseImageWidth X-SAS-UseImageHeight ALIGN=bottom></P>
C
C<H4><FONT SIZE="+1">Changes</FONT></H4>
C
C<P><FONT SIZE="+1">Version 82 has been modification to make it 
Cwork on Sun Sparc/Solaris.  
C</FONT></P>
C
C<P><FONT SIZE="+1">The Fortran90 source code is in 
C   <A HREF="gdf.for">GDF.FOR</A>. 
C   and a the example program is 
C   <A HREF="gdf_example.for">GDF_EXAMPLE.FOR</A>. 
C   The main LaTeX file is 
C   <A HREF="gdf.tex">GDF.TEX</A>m which references many other 
C   files in this  
C   <A HREF="/gdf/v082/">directory.</A>. 
C   </FONT>
C
C
C</P>
C
C<H4><FONT SIZE="+1">Installation</FONT></H4>
C
C<P><FONT SIZE="+1">The program </FONT><FONT
CSIZE="+1"><A HREF="/gdf/v076/gdf_example.for"> </FONT><TT><FONT
CSIZE="+1">GDF_EXAMPLE.F90</A></FONT></TT><FONT SIZE="+1"> has several
Cpurposes. If this program is run without any command line arguments
Cit will produce the GDF documentation files in Latex format.
CCompiling the file GDF.TEX with Latex and the converting GDF.DVI
Cproduces the postscript file GDF.PS. This file will normally be
Cavailable in the same directory as the GDF source code.  </FONT></P>
C
C<P><FONT SIZE="+1">If </FONT><TT><FONT
CSIZE="+1">GDF_EXAMPLE</FONT></TT><FONT SIZE="+1"> is with a file name
Cas a single command line argument it will print out the contents of
Can existing data file. This can be used to check that the files
Ccontent is read correctly. For example a command such as
C</FONT><CODE><FONT SIZE="+1"> gdf_example gt008742.fz
C</FONT></CODE><FONT SIZE="+1"> will display the contents of the first
Cfew events in the data file.</FONT></P>
C
C<P><FONT SIZE="+1">The example program can also perform a GDF
Cself-test, which writes a new data file, and then reads it to checks
Cthat the values read are as expected. The command is
C</FONT><CODE><FONT SIZE="+1"> </FONT></CODE><TT><CODE><FONT
CSIZE="+1">gdf_example test.fz test.fz</FONT></CODE><FONT
CSIZE="+1">.</FONT></TT><FONT SIZE="+1"> The first argument is the
Ctest file to be created, the second argument the name of the file to
Cbe read and checked.</FONT></P>
C
CEND 

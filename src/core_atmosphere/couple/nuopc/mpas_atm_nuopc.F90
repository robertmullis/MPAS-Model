module mpas_atm_nuopc

  !-----------------------------------------------------------------------------
  ! This is the NUOPC cap for MPAS-Atmosphere 
  !-----------------------------------------------------------------------------

  use ESMF, only: operator(+)
  use ESMF, only: ESMF_GridComp, ESMF_GridCompSetEntryPoint, ESMF_GridCompGet
  use ESMF, only: ESMF_VM, ESMF_VMGet
  use ESMF, only: ESMF_State
  use ESMF, only: ESMF_Clock, ESMF_ClockGet, ESMF_ClockPrint
  use ESMF, only: ESMF_Time, ESMF_TimePrint
  use ESMF, only: ESMF_TimeInterval, ESMF_TimeIntervalGet
  use ESMF, only: ESMF_LogWrite
  use ESMF, only: ESMF_SUCCESS, ESMF_FAILURE
  use ESMF, only: ESMF_LOGMSG_INFO, ESMF_LOGMSG_ERROR
  use ESMF, only: ESMF_METHOD_INITIALIZE
  use ESMF, only: ESMF_KIND_R8

  use NUOPC, only: NUOPC_CompDerive, NUOPC_CompFilterPhaseMap
  use NUOPC, only: NUOPC_CompSetEntryPoint, NUOPC_CompSpecialize

  use NUOPC_Model, only: SetVM
  use NUOPC_Model, only: NUOPC_ModelGet
  use NUOPC_Model, only: model_routine_SS => SetServices
  use NUOPC_Model, only: model_label_Advance => label_Advance

  use mpas_nuopc_shr, only: ChkErr

  use mpas_kind_types, only: rkind, r8kind, strkind
  use mpas_derived_types, only: core_type, domain_type
  use mpas_derived_types, only: block_type, mpas_pool_type, mpas_time_type
  use atm_core, only: atm_core_run_start, atm_core_run_advance
  use mpas_subdriver, only: mpas_init, mpas_finalize

  implicit none
  private

  !-----------------------------------------------------------------------------
  ! Public module routines
  !-----------------------------------------------------------------------------

  public SetVM
  public SetServices

  !-----------------------------------------------------------------------------
  ! Module private routines
  !-----------------------------------------------------------------------------

  private :: InitializeP0        ! Phase zero of initialization
  private :: InitializeAdvertise ! Advertise the fields that can be passed

  !-----------------------------------------------------------------------------
  ! Private module data
  !-----------------------------------------------------------------------------

  type mpas_cpl_type
     type(core_type), pointer :: corelist => null()
     type(domain_type), pointer :: domain => null()
     type(block_type), pointer :: block_ptr => null()
     real(kind=rkind) :: dt
     logical, pointer :: config_do_restart => null()
     character(len=strkind), pointer :: config_restart_timestamp_name => null()
     real(kind=r8kind) :: diag_start_time
     real(kind=r8kind) :: diag_stop_time
     type(mpas_pool_type), pointer :: state => null()
     type(mpas_pool_type), pointer :: diag => null()
     type(mpas_pool_type), pointer :: diag_physics => null()
     type(mpas_pool_type), pointer :: mesh => null()
     real(kind=r8kind) :: input_start_time
     real(kind=r8kind) :: input_stop_time
     real(kind=r8kind) :: output_start_time
     real(kind=r8kind) :: output_stop_time
     real(kind=r8kind) :: integ_start_time
     real(kind=r8kind) :: integ_stop_time
     logical, pointer :: config_apply_lbcs => null()
     type(mpas_time_type), pointer :: currTime => null()
     type (mpas_pool_type), pointer :: tend => null()
     type (mpas_pool_type), pointer :: tend_physics => null()
     character(len=strkind) :: timestamp
     integer :: itimestep
     character(len=strkind) :: input_stream
     character(len=strkind) :: read_time
     integer :: stream_dir
  end type mpas_cpl_type

  type(mpas_cpl_type), target :: mpas_cpl

  character(len=*), parameter :: modName = "(mpas_atm_nuopc)"

  character(len=*), parameter :: u_FILE_u = &
     __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine SetServices(gcomp, rc)

    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(len=*), parameter :: subname = trim(modName)//':(SetServices) '
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    !------------------
    ! register the generic methods
    !------------------

    call NUOPC_CompDerive(gcomp, model_routine_SS, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !------------------
    ! switching to IPD versions
    !------------------
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
         userRoutine=InitializeP0, phase=0, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !------------------
    ! set entry point for methods that require specific implementation
    !------------------
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
         phaseLabelList=(/"IPDv01p1"/), userRoutine=InitializeAdvertise, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
         phaseLabelList=(/"IPDv01p3"/), userRoutine=InitializeRealize, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_Advance, &
         specRoutine=ModelAdvance, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine SetServices

  !===============================================================================

  subroutine InitializeP0(gcomp, importState, exportState, clock, rc)

    ! input/output variables
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! Switch to IPDv01 by filtering all other phaseMap entries
    call NUOPC_CompFilterPhaseMap(gcomp, ESMF_METHOD_INITIALIZE, acceptStringList=(/"IPDv01p"/), rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

  end subroutine InitializeP0

  !===============================================================================

  subroutine InitializeAdvertise(gcomp, importState, exportState, clock, rc)

    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    character(len=*), parameter :: subname=trim(modName)//':(InitializeAdvertise) '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    ! ---------------------
    ! Advertise coupling fields
    ! ---------------------


    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine InitializeAdvertise

  !===============================================================================

  subroutine InitializeRealize(gcomp, importState, exportState, clock, rc)

    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_VM) :: vm
    integer :: petCount, localPet, comm
    integer :: ierr
    character(len=*), parameter :: subname=trim(modName)//':(InitializeRealize) '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    ! ---------------------
    ! Query VM
    ! ---------------------

    call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, mpiCommunicator=comm, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! ---------------------
    ! Initialize MPAS
    ! ---------------------

    call mpas_init(mpas_cpl%corelist, mpas_cpl%domain, external_comm=comm)

    ! ---------------------
    ! Prepare MPAS to run
    ! ---------------------

    ierr = atm_core_run_start( &
       mpas_cpl%domain, &
       mpas_cpl%block_ptr, & 
       mpas_cpl%dt, &
       mpas_cpl%config_do_restart, &
       mpas_cpl%config_restart_timestamp_name, &
       mpas_cpl%diag_start_time, &
       mpas_cpl%diag_stop_time, &
       mpas_cpl%state, &
       mpas_cpl%diag, &
       mpas_cpl%diag_physics, &
       mpas_cpl%mesh, &
       mpas_cpl%input_start_time, &
       mpas_cpl%input_stop_time, &
       mpas_cpl%output_start_time, &
       mpas_cpl%output_stop_time, &
       mpas_cpl%config_apply_lbcs, &
       mpas_cpl%currTime, &
       mpas_cpl%timestamp, &
       mpas_cpl%itimestep)
    if (ierr /= 0) then
       call ESMF_LogWrite(trim(subname)//": "//' MPAS atm_core_run_start() is failed!. Exiting ...', ESMF_LOGMSG_ERROR)
       rc = ESMF_FAILURE
       return
    end if

    ! ---------------------
    ! Get coupling specific options
    ! ---------------------

    ! ---------------------
    ! Create MPAS mesh
    ! ---------------------

    ! ---------------------
    ! Realize coupling fields
    ! ---------------------

    ! ---------------------
    ! Create export state
    ! ---------------------

    ! ---------------------
    ! Diagnostics
    ! ---------------------

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine InitializeRealize

  !===============================================================================

  subroutine DataInitialize(gcomp, rc)

    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
  
    ! local variables
    character(len=*), parameter :: subname=trim(modName)//':(DataInitialize) '
    !-------------------------------------------------------------------------------
  
    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)


    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine DataInitialize

  !===============================================================================

  subroutine ModelAdvance(gcomp, rc)

    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    integer :: n, ierr
    integer, save :: nSteps = 0
    logical, save :: first_time = .true.
    real(ESMF_KIND_R8) :: dt_cpl
    type(ESMF_Clock) :: clock
    type(ESMF_Time) :: startTime, currTime
    type(ESMF_TimeInterval) :: timestep
    type(ESMF_State) :: importState, exportState
    character(len=255) :: msgString 
    character(len=*), parameter :: subname=trim(modName)//':(ModelAdvance) '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    !-----------------------
    ! Query the Component for its clock, importState and exportState
    !-----------------------

    call NUOPC_ModelGet(gcomp, modelClock=clock, importState=importState, exportState=exportState, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! ---------------------
    ! Calculate number of MPAS advance steps
    ! ---------------------

    if (first_time) then
       ! Query model clock
       call ESMF_ClockGet(clock, timestep=timestep, rc=rc) 
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       call ESMF_TimeIntervalGet(timestep, s_r8=dt_cpl, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       ! Check if coupling time step is evenly divisible by MPAS time step or not 
       if (mpas_cpl%dt /= 0.0d0 .and. abs(dt_cpl/mpas_cpl%dt - nint(dt_cpl/mpas_cpl%dt)) < 1.0d-12) then
          nSteps = nint(dt_cpl/mpas_cpl%dt)
       else
          call ESMF_LogWrite(trim(subname)//": "//&
             "Coupling time step must be evenly divisible by MPAS time step!", ESMF_LOGMSG_ERROR)
          write(msgString, fmt="(A,F8.1,A,F8.1,A)") &
             "Coupling time step is ", dt_cpl, "s but MPAS time step is ", mpas_cpl%dt, "s" 
          call ESMF_LogWrite(trim(subname)//": "//trim(msgString), ESMF_LOGMSG_ERROR)
          rc = ESMF_FAILURE
          return
       end if
       write(msgString, fmt="(A,F8.1,A)") "Coupling time step =", dt_cpl, "s"
       call ESMF_LogWrite(trim(subname)//": "//trim(msgString), ESMF_LOGMSG_INFO)
       write(msgString, fmt="(A,F8.1,A)") "MPAS time step = ", mpas_cpl%dt, "s"
       call ESMF_LogWrite(trim(subname)//": "//trim(msgString), ESMF_LOGMSG_INFO)
       write(msgString, fmt="(A,I5)") "MPAS will advance #step = ", nSteps
       call ESMF_LogWrite(trim(subname)//": "//trim(msgString), ESMF_LOGMSG_INFO)

       first_time = .false.
    end if

    ! ---------------------
    ! Run MPAS
    ! ---------------------

    ! HERE THE MODEL ADVANCES: currTime -> currTime + timeStep

    call ESMF_ClockPrint(clock, options="currTime", &
       preString="------>Advancing OCN from: ", unit=msgString, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_LogWrite(trim(subname)//": "//trim(msgString), ESMF_LOGMSG_INFO)

    call ESMF_ClockGet(clock, startTime=startTime, currTime=currTime, &
       timeStep=timeStep, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_TimePrint(currTime + timeStep, &
       preString="--------------------------------> to: ", unit=msgString, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_LogWrite(trim(subname)//": "//trim(msgString), ESMF_LOGMSG_INFO)

    do n = 1, nSteps
       write(msgString, fmt="(A,I8)") "Advancing MPAS, itimestep = ", mpas_cpl%itimestep
       call ESMF_LogWrite(trim(subname)//": "//trim(msgString), ESMF_LOGMSG_INFO) 
       ierr = atm_core_run_advance( &
          mpas_cpl%domain, &
          mpas_cpl%timestamp, &
          mpas_cpl%block_ptr, &
          mpas_cpl%config_apply_lbcs, &
          mpas_cpl%input_start_time, &
          mpas_cpl%input_stop_time, &
          mpas_cpl%output_start_time, &
          mpas_cpl%output_stop_time, &
          mpas_cpl%input_stream, &
          mpas_cpl%read_time, &
          mpas_cpl%stream_dir, &
          mpas_cpl%integ_start_time, &
          mpas_cpl%integ_stop_time, &
          mpas_cpl%diag_start_time, &
          mpas_cpl%diag_stop_time, &
          mpas_cpl%dt, &
          mpas_cpl%itimestep, &
          mpas_cpl%state, &
          mpas_cpl%mesh, &
          mpas_cpl%diag, &
          mpas_cpl%diag_physics, &
          mpas_cpl%tend, &
          mpas_cpl%tend_physics, &
          mpas_cpl%config_restart_timestamp_name)
       if (ierr /= 0) then
          call ESMF_LogWrite(trim(subname)//": "//' MPAS atm_core_run_advance() is failed!. Exiting ...', ESMF_LOGMSG_ERROR)
          rc = ESMF_FAILURE
          return
       end if
    end do
    
    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine ModelAdvance

  !===============================================================================

  subroutine ModelSetRunClock(gcomp, rc)

    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(len=*), parameter :: subname=trim(modName)//':(ModelSetRunClock) '
    !-------------------------------------------------------------------------------
  
    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)


    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine ModelSetRunClock

  !===============================================================================

  subroutine ModelCheckImport(gcomp, rc)

    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
  
    ! local variables
    character(len=*), parameter :: subname=trim(modName)//':(ModelCheckImport) '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

  
    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine ModelCheckImport

  subroutine ModelFinalize(gcomp, rc)

    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
           
    ! local variables
    character(len=*), parameter :: subname=trim(modName)//':(ModelFinalize) '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)


    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine ModelFinalize

end module mpas_atm_nuopc

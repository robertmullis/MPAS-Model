module mpas_atm_nuopc

  !-----------------------------------------------------------------------------
  ! This is the NUOPC cap for MPAS-Atmosphere 
  !-----------------------------------------------------------------------------

  use ESMF, only: ESMF_GridComp, ESMF_GridCompSetEntryPoint, ESMF_GridCompGet
  use ESMF, only: ESMF_VM, ESMF_VMGet
  use ESMF, only: ESMF_State
  use ESMF, only: ESMF_Clock
  use ESMF, only: ESMF_LogWrite
  use ESMF, only: ESMF_SUCCESS, ESMF_LOGMSG_INFO
  use ESMF, only: ESMF_METHOD_INITIALIZE

  use NUOPC, only: NUOPC_CompDerive, NUOPC_CompFilterPhaseMap
  use NUOPC, only: NUOPC_CompSetEntryPoint, NUOPC_CompSpecialize

  use NUOPC_Model, only: SetVM
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
     real(kind=rkind), pointer :: dt => null()
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
     logical, pointer :: config_apply_lbcs => null()
     type(mpas_time_type) :: currTime
     character(len=strkind) :: timestamp
     integer :: itimestep
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
    character(len=*), parameter :: subname=trim(modName)//':(ModelAdvance) '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    !config_dt = 720.0
    !coupling_dt = 3600
    !5 step

    
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

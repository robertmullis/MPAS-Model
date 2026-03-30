module mpas_atm_nuopc_flds

  !-----------------------------------------------------------------------------
  ! Import and export fields related routines 
  !-----------------------------------------------------------------------------

  use ESMF, only: operator(==)
  use ESMF, only: ESMF_GridComp, ESMF_LOGMSG_ERROR, ESMF_FAILURE
  use ESMF, only: ESMF_LogWrite, ESMF_LOGMSG_INFO, ESMF_SUCCESS
  use ESMF, only: ESMF_State, ESMF_StateGet, ESMF_STATEITEM_FIELD
  use ESMF, only: ESMF_Finalize, ESMF_END_ABORT, ESMF_StateItem_Flag
  use ESMF, only: ESMF_MeshLoc_Element, ESMF_FieldCreate, ESMF_FieldWriteVTK
  use ESMF, only: ESMF_TYPEKIND_R8, ESMF_KIND_R8, ESMF_MAXSTR
  use ESMF, only: ESMF_Field, ESMF_FieldGet, ESMF_Mesh, ESMF_StateRemove
  use ESMF, only: ESMF_LogFoundError, ESMF_LOGERR_PASSTHRU

  use NUOPC, only: NUOPC_Advertise, NUOPC_Realize, NUOPC_IsConnected
  use NUOPC_Model, only: NUOPC_ModelGet

  use mpas_atm_nuopc_shr, only: ChkErr
  use mpas_atm_nuopc_types, only: mpas_cpl_type, mpas_cpl

  use mpas_kind_types, only: rkind
  use mpas_derived_types, only: block_type, mpas_pool_type
  use mpas_derived_types, only: domain_type
  use mpas_pool_routines, only: mpas_pool_get_array
  use mpas_pool_routines, only: mpas_pool_get_subpool
  use mpas_pool_routines, only: mpas_pool_get_dimension

  implicit none
  private

  !-----------------------------------------------------------------------------
  ! Public module routines
  !-----------------------------------------------------------------------------

  public :: ChkErr
  public :: advertise_fields
  public :: realize_fields
  public :: state_diagnose
  public :: export_fields

  !-----------------------------------------------------------------------------
  ! Public module data 
  !-----------------------------------------------------------------------------

  type fldListType
     character(len=128) :: stdname
     character(len=128) :: internalgroup
     character(len=128) :: internalname
     integer :: ungridded_lbound = 0
     integer :: ungridded_ubound = 0
     logical :: connected = .false.
  end type fldListType

  integer, parameter :: fldsMax = 10
  integer :: fldsToMPAS_num = 0
  integer :: fldsFrMPAS_num = 0
  type(fldListType) :: fldsToMPAS(fldsMax)
  type(fldListType) :: fldsFrMPAS(fldsMax)

  !-----------------------------------------------------------------------------
  ! Private module data
  !-----------------------------------------------------------------------------

  character(*), parameter :: modName = "(mpas_atm_fields)"

  character(len=*), parameter :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine advertise_fields(gcomp, rc)

    ! input/output variables
    type(ESMF_GridComp), intent(in)  :: gcomp
    integer,             intent(out) :: rc

    ! local variables
    type(ESMF_State)  :: importState
    type(ESMF_State)  :: exportState
    integer           :: n
    character(len=*), parameter :: subname=trim(modName)//':(advertise_fields)'
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    call NUOPC_ModelGet(gcomp, importState=importState, exportState=exportState, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !--------------------------------
    ! Advertise export fields
    !--------------------------------

    ! export to ocn 
    call fldlist_add(fldsFrMPAS_num, fldsFrMPAS, 'Sa_u10m', 'diag', 'u10', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call fldlist_add(fldsFrMPAS_num, fldsFrMPAS, 'Sa_v10m', 'diag', 'v10', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Now advertise above export fields
    do n = 1, fldsFrMPAS_num
       call NUOPC_Advertise(exportState, standardName=fldsFrMPAS(n)%stdname, &
            TransferOfferGeomObject='will provide', rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    enddo

    !--------------------------------
    ! Advertise import fields
    !--------------------------------

    ! import from ocn 
    !call fldlist_add(fldsToMPAS_num, fldsToMPAS, 'So_t', 'sfc', 'sst', rc=rc)
    !if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Now advertise import fields
    !do n = 1, fldsToMPAS_num
    !   call NUOPC_Advertise(importState, standardName=fldsToMPAS(n)%stdname, &
    !        TransferOfferGeomObject='will provide', rc=rc)
    !   if (ChkErr(rc,__LINE__,u_FILE_u)) return
    !enddo

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine advertise_fields

  !=============================================================================

  subroutine realize_fields(importState, exportState, mesh, rc)

    ! input/output variables
    type(ESMF_State), intent(inout) :: importState
    type(ESMF_State), intent(inout) :: exportState
    type(ESMF_mesh) , intent(in)    :: mesh 
    integer         , intent(out)   :: rc

    ! local variables
    character(len=*), parameter :: subname=trim(modName)//':(realize_fields)'
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    call fldlist_realize( &
         state=exportState, &
         fldList=fldsFrMPAS, &
         numflds=fldsFrMPAS_num, &
         tag=subname//':MPAS_Export',&
         mesh=mesh, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call fldlist_realize( &
         state=importState, &
         fldList=fldsToMPAS, &
         numflds=fldsToMPAS_num, &
         tag=subname//':MPAS_Import',&
         mesh=mesh, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine realize_fields

  !=============================================================================

  subroutine fldlist_add(num, fldlist, stdname, intgrp, intname, ungridded_lbound, ungridded_ubound, rc)

    ! input/output variables
    integer,           intent(inout) :: num
    type(fldListType), intent(inout) :: fldlist(:)
    character(len=*),  intent(in)    :: stdname
    character(len=*),  intent(in)    :: intgrp
    character(len=*),  intent(in)    :: intname
    integer, optional, intent(in)    :: ungridded_lbound
    integer, optional, intent(in)    :: ungridded_ubound
    integer, optional, intent(out)   :: rc

    ! local variables
    character(len=*), parameter :: subname=trim(modName)//':(fldlist_add)'
    !---------------------------------------------------------------------------

    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    ! Set up a list of field information
    num = num + 1
    if (num > fldsMax) then
       call ESMF_LogWrite(trim(subname)//": ERROR num > fldsMax "//trim(stdname), &
         ESMF_LOGMSG_ERROR)
       rc = ESMF_FAILURE
       return
    endif

    fldlist(num)%stdname = trim(stdname)
    fldlist(num)%internalgroup = trim(intgrp)
    fldlist(num)%internalname = trim(intname)

    if (present(ungridded_lbound) .and. present(ungridded_ubound)) then
       fldlist(num)%ungridded_lbound = ungridded_lbound
       fldlist(num)%ungridded_ubound = ungridded_ubound
    end if

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine fldlist_add

  !=============================================================================

  subroutine fldlist_realize(state, fldList, numflds, mesh, tag, rc)

    ! input/output variables
    type(ESMF_State) , intent(inout) :: state
    type(fldListType), intent(inout) :: fldList(:)
    integer          , intent(in)    :: numflds
    character(len=*) , intent(in)    :: tag
    type(ESMF_Mesh)  , intent(in)    :: mesh
    integer          , intent(inout) :: rc

    ! local variables
    integer :: n
    type(ESMF_Field) :: field
    character(len=80) :: stdname
    character(len=*), parameter :: subname=trim(modName)//':fldlist_realize)'
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    do n = 1, numflds
       stdname = trim(fldList(n)%stdname)
       !if (NUOPC_IsConnected(state, fieldName=stdname)) then
       if (.true.) then
          ! Create the field
          if (fldlist(n)%ungridded_lbound > 0 .and. fldlist(n)%ungridded_ubound > 0) then
             field = ESMF_FieldCreate(mesh, ESMF_TYPEKIND_R8, name=stdname, meshloc=ESMF_MESHLOC_ELEMENT, &
                  ungriddedLbound=(/fldlist(n)%ungridded_lbound/), &
                  ungriddedUbound=(/fldlist(n)%ungridded_ubound/), &
                  gridToFieldMap=(/2/), rc=rc)
             if (ChkErr(rc,__LINE__,u_FILE_u)) return
          else
             field = ESMF_FieldCreate(mesh, ESMF_TYPEKIND_R8, name=stdname, meshloc=ESMF_MESHLOC_ELEMENT, rc=rc)
             if (ChkErr(rc,__LINE__,u_FILE_u)) return
          end if
          call ESMF_LogWrite(trim(subname)//trim(tag)//" Field = "//trim(stdname)//" is connected using mesh", &
               ESMF_LOGMSG_INFO)

          ! NOW call NUOPC_Realize
          call NUOPC_Realize(state, field=field, rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return

          ! Set flag for connected fields
          fldList(n)%connected = .true.
       else
          call ESMF_LogWrite(subname // trim(tag) // " Field = "// trim(stdname) // " is not connected.", &
               ESMF_LOGMSG_INFO)
          call ESMF_StateRemove(state, (/stdname/), rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
       end if
    end do

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine fldlist_realize

  !===============================================================================

  subroutine export_fields(exportState, domain, rc)

    ! input/output variables
    type(ESMF_State), intent(inout) :: exportState
    type(domain_type), intent(in), pointer :: domain
    integer, intent(out) :: rc

    ! local variables
    integer :: n, iCell, gCell, nCells, tCells, cell_offset
    type(ESMF_Field) :: lfield
    type(ESMF_StateItem_Flag) :: itemType
    type(block_type), pointer :: block => null()
    type(mpas_pool_type), pointer :: meshPool
    type(mpas_pool_type), pointer :: diagnosticsPool
    type(mpas_pool_type), pointer :: sfcInputPool
    real(kind=rkind), dimension(:), pointer :: fldPtrInt
    real(ESMF_KIND_R8), dimension(:), pointer :: fldPtrExt
    integer, dimension(:), pointer :: nCellsArray
    character(len=*), parameter :: subname=trim(modName)//':(export_fields)'
    ! ----------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    ! -----------------------
    ! Loop over export fields and update them 
    ! -----------------------

    do n = 1, fldsFrMPAS_num
       ! Check field
       call ESMF_StateGet(exportState, itemName=trim(fldsFrMPAS(n)%stdname), itemType=itemType, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       if (itemType == ESMF_STATEITEM_FIELD) then
          ! Get field
          call ESMF_StateGet(exportState, itemName=trim(fldsFrMPAS(n)%stdname), field=lfield, rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return

          ! Query field pointer and initialize
          call ESMF_FieldGet(lfield, farrayPtr=fldPtrExt, rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
          fldPtrExt(:) =1.0d20

          ! Query internal pointer and fill export field 
          cell_offset = 0
          block => domain % blocklist
          do while (associated(block))
             ! Access internal pointer
             if (trim(fldsFrMPAS(n)%internalgroup) == 'diag') then
                call mpas_pool_get_subpool(block % structs, 'diag_physics', diagnosticsPool)
                call mpas_pool_get_array(diagnosticsPool, trim(fldsFrMPAS(n)%internalname), fldptrInt)
             else
                call mpas_pool_get_subpool(block % structs, 'sfc_input', sfcInputPool)
                call mpas_pool_get_array(sfcInputPool, trim(fldsFrMPAS(n)%internalname), fldptrInt)
             end if
             
             ! Get number of cells in decomposition block
             call mpas_pool_get_subpool(block % structs, 'mesh', meshPool)
             call mpas_pool_get_dimension(meshPool, 'nCellsArray', nCellsArray)
             nCells = nCellsArray(1)

             ! Loop over cells and fill pointer of export field
             do iCell = 1, nCells
                gCell = iCell + cell_offset
                fldPtrExt(gCell) = dble(fldptrInt(iCell))
             end do

             ! Increment cell offset
             cell_offset = cell_offset + nCells

             ! Go to next block
             block => block % next
          end do

          ! Init pointers
          nullify(fldPtrExt)
       else
          call ESMF_LogWrite(subname//' '//trim(fldsFrMPAS(n)%stdname)//' is not in the state!', ESMF_LOGMSG_INFO)
       end if
    end do

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine export_fields

  !===============================================================================

  subroutine state_diagnose(state, string, rc)

    type(ESMF_State), intent(in)  :: state
    character(len=*), intent(in)  :: string
    integer         , intent(out) :: rc

    ! local variables
    integer                         :: n
    type(ESMF_Field)                :: lfield
    integer                         :: fieldCount, lrank
    character(ESMF_MAXSTR), pointer :: lfieldnamelist(:)
    real(ESMF_KIND_R8), pointer     :: dataPtr1d(:)
    character(len=1024)             :: msgString
    character(len=*), parameter     :: subname='(state_diagnose)'
    ! ----------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    call ESMF_StateGet(state, itemCount=fieldCount, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    allocate(lfieldnamelist(fieldCount))

    call ESMF_StateGet(state, itemNameList=lfieldnamelist, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    do n = 1, fieldCount
       call ESMF_StateGet(state, itemName=lfieldnamelist(n), field=lfield, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return

       call ESMF_FieldGet(lfield, farrayPtr=dataPtr1d, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return

       if (size(dataPtr1d) > 0) then
          write(msgString,'(A,3g14.7,i8)') trim(string)//': '//trim(lfieldnamelist(n)), &
             minval(dataPtr1d), maxval(dataPtr1d), sum(dataPtr1d), size(dataPtr1d)
       else
          write(msgString,'(A,a)') trim(string)//': '//trim(lfieldnamelist(n))," no data"
       endif
       call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)

       call ESMF_FieldWriteVTK(lfield, 'export_'//trim(lfieldnamelist(n)), rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    enddo

    deallocate(lfieldnamelist)

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine state_diagnose

end module mpas_atm_nuopc_flds

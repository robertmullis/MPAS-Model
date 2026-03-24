module mpas_nuopc_shr

  !-----------------------------------------------------------------------------
  ! Coupling related shared routines
  !-----------------------------------------------------------------------------

  use ESMF, only: ESMF_LogFoundError, ESMF_LOGERR_PASSTHRU

  implicit none
  private

  !-----------------------------------------------------------------------------
  ! Public module routines
  !-----------------------------------------------------------------------------

  public :: ChkErr

  !-----------------------------------------------------------------------------
  ! Private module data
  !-----------------------------------------------------------------------------

  character(*), parameter :: modName = "(mpas_nuopc_shr)"

  character(len=*), parameter :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

  logical function ChkErr(rc, line, file)

    integer, intent(in) :: rc
    integer, intent(in) :: line
    character(len=*), intent(in) :: file

    integer :: lrc

    ChkErr = .false.
    lrc = rc
    if (ESMF_LogFoundError(rcToCheck=lrc, msg=ESMF_LOGERR_PASSTHRU, line=line, file=file)) then
       ChkErr = .true.
    endif
  end function ChkErr

end module mpas_nuopc_shr

module txt_interface
  implicit none

  public :: txt_file

  private
  !=============================================================================
  type :: txt_file
    integer :: unit
    character(:), allocatable :: file
  contains
    procedure :: is_exist => file_is_exist
    procedure :: is_open => file_is_open
    procedure :: open  => file_open
    procedure :: close => file_close
    procedure :: rewind => file_rewind
    procedure :: delete => file_delete
  end type txt_file


contains
  !=============================================================================
  subroutine file_rewind(this)
    class(txt_file), intent(inout) :: this
    rewind(this%unit)
  end subroutine file_rewind
  !=============================================================================
  function file_is_exist(this) result(is_exist)
    !< confirm if the file is exist
    class(txt_file),intent(in) :: this
    logical :: is_exist
    inquire(file=this%file, exist=is_exist)
  end function file_is_exist
  !=============================================================================
  function file_is_open(this) result(is_open)
    !< confirm if the file is opened
    class(txt_file),intent(in) :: this
    logical :: is_open
    inquire(file=this%file, opened=is_open)
  end function file_is_open
  !=============================================================================
  subroutine file_close(this, status)
    class(txt_file), intent(in) :: this
    character(*), intent(in), optional :: status

    integer :: ios
    character(:), allocatable :: lstatus

    lstatus = 'keep'

    if(present(status)) lstatus = status
    close(unit=this%unit, status=lstatus, iostat = ios)
    if(ios /=0 ) error stop "unable to close file :"//this%file//new_line('a')//&
    &'status = '//lstatus

  end subroutine file_close
  !=============================================================================
  subroutine file_open(this, file, status, action)
    class(txt_file), intent(inout) :: this
    character(*), intent(in)   :: file
    character(*), intent(in), optional :: status
    character(*), intent(in), optional :: action

    integer :: ios
    character(:), allocatable :: lstatus
    character(:), allocatable :: laction

    this%file = file
    lstatus = 'unknown'
    laction = 'readwrite'
    if(present(status)) lstatus = status
    if(present(action)) laction = action

    open(newunit=this%unit, file = this%file, iostat = ios, status = lstatus, action= laction)
    if(ios /= 0) error stop "unable to open file :"//this%file//new_line('a')//&
    &'status = '//lstatus//';'//'action = '//laction

  end subroutine file_open
  !=============================================================================
  subroutine file_delete(this)
    class(txt_file), intent(in) :: this

    call delete_file(this%file)

  end subroutine file_delete
  !=============================================================================
  subroutine delete_file(file)
    character(*), intent(in) :: file
    integer :: unit
    logical :: is_exist, is_open

    inquire(file=file, exist=is_exist)
    if (is_exist) then
      inquire(file=file, opened = is_open)
      if (is_open) then
        inquire(file=file, number=unit)
      else
        open(newunit=unit, file=file)
      end if
      close(unit=unit, status='delete')
    end if

  end subroutine delete_file

end module txt_interface

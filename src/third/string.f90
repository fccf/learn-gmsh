!> author: FANGCHAO
!  data: 2017/7/12
!
!## description
! This module provide some string operation
module string
  implicit none

  public :: to_str


  private

  interface to_str
    module procedure :: scalar_to_string, vector_to_string, matrix_to_string
  end interface to_str

contains
  !=============================================================================
  pure function scalar_to_string(value) result(s)
    !< convert any scalar type (integer, real, logical, character(*)) to string
    class(*), intent(in)      :: value
    character(:), allocatable :: s
    integer, parameter        :: max_num_len_ = 32
    character(max_num_len_)   :: ls

    select type(v_p => value)
    type is(integer)
      write(ls,'(i0)') v_p
      s = trim(adjustl(ls))
    type is(real)
      write(ls,fmt=*) v_p
      s = trim(adjustl(ls))
    type is(logical)
      if (v_p) then
        write(ls,'(a)') 'true'
      else
        write(ls,'(a)') 'false'
      end if
      s = trim(adjustl(ls))
    type is(character(*))
      s = trim(adjustl(v_p))
    class default
      write(ls,'(a)') '***'
      s = trim(adjustl(ls))
    end select
  end function scalar_to_string
  !=============================================================================
  pure function vector_to_string(value, vsep, shell) result(s)
    !< convert any vector type (integer, real, logical, character(*)) to string
    class(*), intent(in) :: value(:)
    character(*), intent(in), optional :: vsep   !< vector separator ',', ' '
    logical,intent(in), optional :: shell        !< if have the shell []
    character(:), allocatable :: s

    character(:),allocatable :: lsep
    logical :: lshell
    integer :: n

    lsep = ','      !< default vector separator
    lshell = .TRUE. !< default shell = true
    if(present(vsep)) lsep = vsep !< local optional argument
    if(present(shell)) lshell = shell !< another local argument

    s = ''
    if(lshell) s = '['
    do n = 1, size(value)
      if (n > 1) s = s//lsep
      s = s // to_str(value(n))    !< str => scalar_to_string
    end do
    if(lshell) s = s// ']'

  end function vector_to_string
  !=============================================================================
  pure function matrix_to_string(value, vsep, msep, shell) result(s)
    class(*), intent(in) :: value(:,:)
    character(*), intent(in), optional :: vsep  !< vector separator ',', ' '
    character(*), intent(in), optional :: msep  !< matrix separator ';', new_line('a')
    logical,intent(in), optional :: shell       !< if have the shell []

    character(:), allocatable :: s

    character(:),allocatable :: lvsep, lmsep
    logical :: lshell
    integer   :: n, m
    lvsep = ','
    lmsep = ';'
    lshell = .TRUE.
    if(present(vsep)) lvsep = vsep
    if(present(msep)) lmsep = msep
    if(present(shell)) lshell = shell

    s = ''
    if(lshell) s = '['
    do n = 1, size(value,2)
      if (n > 1) s = s//lmsep
      if(lshell) s = s//'['
      do m = 1, size(value, 1)
        if (m > 1) s = s//lvsep
        s = s // to_str(value(m,n))    !< str => scalar_to_string
      enddo
      if(lshell) s = s//']'
    enddo
    if(lshell) s = s//']'

  end function matrix_to_string

end module string

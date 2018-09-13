!> data of gmsh msh file
module gmsh_interface
  use txt_interface
  implicit none

  public :: msh_file

  private
  !=============================================================================
  character(3),parameter :: gmsh_version_  = '2.2'
  integer,parameter      :: gmsh_nod_ = 15
  integer,parameter      :: gmsh_lin_ = 1
  integer,parameter      :: gmsh_tri_ = 2
  integer,parameter      :: gmsh_tet_ = 4

  integer,parameter      :: region_tag_  = 1
  !=============================================================================
  type node
    integer :: id
    real    :: xyz(3)
  end type node

  !=============================================================================
  type elem
    integer :: id
    integer :: type
    integer :: ntags
    integer :: enn       !< element node number
    integer,allocatable :: tags(:)  !< (ntags)
    integer,allocatable :: eni(:)   !< element node index (enn)
  contains
    procedure :: fill    => ele_fill
    procedure :: destory => ele_destory
    final     :: ele_final
  end type elem
  !=============================================================================
  type, extends(txt_file) :: msh_file
    logical   :: is_init = .FALSE.
    integer                :: nd
    type(node),allocatable :: nlist(:) !< node list
    type(elem),allocatable :: elist(:) !< elements list
    type(elem),allocatable :: flist(:) !< faces element list
  contains
    procedure :: init      => msh_init
    procedure :: destory   => msh_destory
    procedure :: get_nd    => msh_get_nd
    procedure :: get_nn    => msh_get_nn
    procedure :: get_ne    => msh_get_ne
    procedure :: get_nf    => msh_get_nf
    ! procedure :: get_nr    => msh_get_nr
    ! procedure :: get_ns    => msh_get_ns
    procedure :: get_er    => msh_get_er
    procedure :: get_fr    => msh_get_fr
    procedure :: get_enn   => msh_get_enn
    procedure :: get_fnn   => msh_get_fnn
    procedure :: get_eni   => msh_get_eni
    procedure :: get_fni   => msh_get_fni
    procedure :: get_eid   => msh_get_eid
    procedure :: get_fid   => msh_get_fid
    procedure :: get_nid   => msh_get_nid
    procedure :: get_coord => msh_get_coord

    procedure,private :: read_header
    procedure,private :: read_nlist
    procedure,private :: read_elist
    final     :: msh_final
  end type msh_file

contains
  !=============================================================================
  subroutine msh_init(this, file)
    class(msh_file), intent(inout) :: this
    character(*), intent(in) :: file

    call this%open(file,status='old',action='read')

    call this%read_header()
    call this%read_nlist()
    call this%read_elist()

    call this%close()

    this%is_init = .TRUE.

  end subroutine msh_init
  !=============================================================================
  subroutine msh_destory(this)
    class(msh_file), intent(inout) :: this

    if(allocated(this%nlist)) deallocate(this%nlist)
    if(allocated(this%elist)) deallocate(this%elist)
    if(allocated(this%flist)) deallocate(this%flist)

    this%is_init = .FALSE.
  end subroutine msh_destory
  !=============================================================================
  subroutine msh_final(this)
    type(msh_file), intent(inout) :: this
    call this%destory()
  end subroutine msh_final
  !=============================================================================
  pure function msh_get_nd(this) result(nd)
    class(msh_file), intent(in) :: this
    integer :: nd
    nd = this%nd
  end function msh_get_nd
  !=============================================================================
  pure function msh_get_nn(this) result(nn)
    class(msh_file), intent(in) :: this
    integer :: nn
    nn = size(this%nlist)
  end function msh_get_nn
  !=============================================================================
  pure function msh_get_nf(this) result(nf)
    class(msh_file), intent(in) :: this
    integer :: nf
    nf = size(this%flist)
  end function msh_get_nf
  !=============================================================================
  pure function msh_get_ne(this) result(ne)
    class(msh_file), intent(in) :: this
    integer :: ne
    ne = size(this%elist)
  end function msh_get_ne
  !=============================================================================
  ! pure function msh_get_nr(this) result(nr)
  !   class(msh_file), intent(in) :: this
  !   integer :: nr
  !   nr = maxval(this%get_er())
  ! end function msh_get_nr
  ! !=============================================================================
  ! pure function msh_get_ns(this) result(ns)
  !   class(msh_file), intent(in) :: this
  !   integer :: ns
  !   ns = ne = maxval(this%get_fr())
  ! end function msh_get_ns
  !=============================================================================
  pure function msh_get_enn(this) result(enn)
    class(msh_file), intent(in) :: this
    integer :: enn
    enn = this%elist(1)%enn
  end function msh_get_enn
  !=============================================================================
  pure function msh_get_fnn(this) result(fnn)
    class(msh_file), intent(in) :: this
    integer :: fnn
    fnn = this%flist(1)%enn
  end function msh_get_fnn
  !=============================================================================
  pure function msh_get_er(this) result(er)
    class(msh_file), intent(in) :: this
    integer, allocatable :: er(:)
    integer :: ne, i

    ne = this%get_ne()
    allocate(er(ne))
    do i = 1,ne
      er(i) = this%elist(i)%tags(region_tag_)
    enddo

  end function msh_get_er
  !=============================================================================
  pure function msh_get_fr(this) result(fr)
    class(msh_file), intent(in) :: this
    integer, allocatable :: fr(:)
    integer :: nf, i

    nf = this%get_nf()
    allocate(fr(nf))
    do i = 1,nf
      fr(i) = this%flist(i)%tags(region_tag_)
    enddo
  end function msh_get_fr
  !=============================================================================
  pure function msh_get_eni(this) result(eni)
    class(msh_file), intent(in) :: this
    integer, allocatable :: eni(:,:)
    integer :: ne, enn, i

    ne = this%get_ne()
    enn = this%get_enn()

    allocate(eni(enn,ne))
    do i = 1,ne
      eni(1:enn, i) = this%elist(i)%eni(1:enn)
    enddo

  end function msh_get_eni
  !=============================================================================
  pure function msh_get_fni(this) result(fni)
    class(msh_file), intent(in) :: this
    integer, allocatable :: fni(:,:)
    integer :: nf, fnn, i

    nf = this%get_nf()
    fnn = this%get_fnn()

    allocate(fni(fnn,nf))
    do i = 1,nf
      fni(1:fnn, i) = this%flist(i)%eni(1:fnn)
    enddo

  end function msh_get_fni
  !=============================================================================
  pure function msh_get_eid(this) result(eid)
    class(msh_file), intent(in) :: this
    integer, allocatable :: eid(:)
    integer :: ne, i

    ne = this%get_ne()
    allocate(eid(ne))
    do i = 1,ne
      eid(i) = this%elist(i)%id
    enddo

  end function msh_get_eid
  !=============================================================================
  pure function msh_get_fid(this) result(fid)
    class(msh_file), intent(in) :: this
    integer, allocatable :: fid(:)
    integer :: nf, i

    nf = this%get_nf()
    allocate(fid(nf))
    do i = 1,nf
      fid(i) = this%flist(i)%id
    enddo
  end function msh_get_fid
  !=============================================================================
  pure function msh_get_nid(this) result(nid)
    class(msh_file), intent(in) :: this
    integer, allocatable :: nid(:)
    integer :: nn, i

    nn = this%get_nn()
    allocate(nid(nn))
    do i = 1,nn
      nid(i) = this%nlist(i)%id
    enddo
  end function msh_get_nid
  !=============================================================================
  pure function msh_get_coord(this) result(coord)
    class(msh_file), intent(in) :: this
    real, allocatable :: coord(:,:) !< (nd, nn)
    integer :: nn, nd, i

    nd = this%get_nd()
    nn = this%get_nn()

    allocate(coord(nd,nn))
    do i = 1,nn
      coord(1:nd,i) = this%nlist(i)%xyz(1:nd)
    enddo

  end function msh_get_coord
  !=============================================================================
  subroutine read_header(this)
    class(msh_file), intent(inout) :: this
    character(256) :: char
    integer        :: filetype,datasize
    real           :: version

    read(this%unit, *) char
    if( trim(char) .ne. "$MeshFormat" ) then
       error stop "Error: can't find '$MeshFormat' (GMSH mesh file?)"
    end if

    read(this%unit, *) char, filetype, datasize
    read(char,*) version
    if( version .lt. 2.0 .or. version .ge. 3.0 ) then
       error stop "Error: GMSH mesh version must be 2.x"
    end if

    if( datasize .ne. 8 ) then
       error stop "Error: GMSH data size does not equal 8"
    end if

    read(this%unit, *) char
    if( trim(char) .ne. "$EndMeshFormat" ) then
       error stop "Error: can't find '$EndMeshFormat' (is this a GMSH mesh file?)"
    end if
  end subroutine read_header
  !=============================================================================
  subroutine read_nlist(this)
    class(msh_file), intent(inout) :: this
    character(256) :: char
    integer :: nn
    integer :: i

    read(this%unit, *) char
    if( trim(char) .ne. "$Nodes" ) then
       error stop "Error: can't find '$Nodes' in GMSH mesh file"
    end if

    read(this%unit,*) nn
    allocate(this%nlist(nn))

    do i = 1,nn
       read(this%unit,*) this%nlist(i)%id,this%nlist(i)%xyz
    end do

    read(this%unit, *) char
    if( trim(char) .ne. "$EndNodes" ) then
       error stop "Error: can't find '$EndNodes' in GMSH mesh file"
    end if
  end subroutine read_nlist
  !=============================================================================
  subroutine read_elist(this)
    class(msh_file), intent(inout) :: this
    type(elem), allocatable:: ele_p(:)

    character(256) :: char
    integer :: ne, nd
    integer :: id,type,ntags
    integer :: num_nod, num_lin, num_tri, num_tet
    integer :: num_fac,fac_type,num_ele,ele_type
    integer :: tmp1,tmp2,tmp3
    integer :: e,e_i,b_i

    read(this%unit, *) char
    if( trim(char) .ne. "$Elements" ) then
       error stop "Error: can't find '$Elements' in GMSH mesh file"
    end if

    read(this%unit,*) ne

    allocate(ele_p(ne))

    do e = 1,ne
       read(this%unit,"(a)",end=888) char
888    read(char,*) id,type,ntags

       call ele_p(e)%fill(id,type,ntags)

       read(char,*) tmp1,tmp2,tmp3,ele_p(e)%tags,ele_p(e)%eni
    end do

    read(this%unit, *) char
    if( trim(char) .ne. "$EndElements" ) then
       error stop "Error: can't find '$EndElements' in GMSH mesh file"
    end if

    !! find num_ele and num_fac
    num_lin = 0
    num_tri = 0
    num_tet = 0
    num_nod = 0

    do e=1, ne

       select case ( ele_p(e)%type )
       case (gmsh_lin_)
          num_lin = num_lin+1
       case (gmsh_tri_)
          num_tri = num_tri+1
       case (gmsh_tet_)
          num_tet = num_tet+1
       case (gmsh_nod_)
          num_nod = num_nod+1
       case default
          write(*,*) "element id,type: ", ele_p(e)%id, ele_p(e)%type
          error stop "Unsupported element type"
       end select

    end do

    if (num_tet>0) then
       num_ele  = num_tet
       ele_type = gmsh_tet_
       num_fac  = num_tri
       fac_type = gmsh_tri_
       nd = 3

    elseif (num_tri>0) then
       num_ele  = num_tri
       ele_type = gmsh_tri_
       num_fac  = num_lin
       fac_type = gmsh_lin_
       nd = 2

    elseif (num_lin > 0) then
       num_ele  = num_lin
       ele_type = gmsh_lin_
       num_fac  = num_nod
       fac_type = gmsh_nod_
       nd = 1

    else
       error stop "Error: Unsupported mixture of face/element types"
    end if

    !! copy elem to elist and flist
    allocate(this%elist(num_ele))
    allocate(this%flist(num_fac))

    e_i = 1
    b_i = 1
    do e = 1,ne
       if(ele_p(e)%type == fac_type) then

          call this%flist(b_i)%fill(ele_p(e)%id,ele_p(e)%type,ele_p(e)%ntags)
          this%flist(b_i)%tags = ele_p(e)%tags
          this%flist(b_i)%eni  = ele_p(e)%eni

          b_i = b_i + 1
       elseif(ele_p(e)%type == ele_type) then
          call this%elist(e_i)%fill(ele_p(e)%id,ele_p(e)%type,ele_p(e)%nTags)
          this%elist(e_i)%tags = ele_p(e)%tags
          this%elist(e_i)%eni  = ele_p(e)%eni

          e_i = e_i + 1
       end if
    end do
    !! destory ele_p
    do e = 1,ne
       call ele_p(e)%destory()
    end do
    deallocate(ele_p)
    this%nd = nd

  end subroutine read_elist
  !=============================================================================
  subroutine ele_fill(this,id,type,ntags)
    class(elem),intent(inout) :: this
    integer,intent(in) :: id,type,ntags

    ! call this%destory()

    this%id    = id
    this%type  = type
    this%ntags = ntags

    if(this%type == gmsh_nod_) then
       this%enn = 1
    elseif(this%type == gmsh_lin_) then
       this%enn = 2
    elseif(this%type == gmsh_tri_) then
       this%enn = 3
    elseif(this%type == gmsh_tet_) then
       this%enn = 4
    else
       error stop "Error: Unsupported element type"
    end if

    allocate(this%eni(this%enn))
    allocate(this%tags(this%ntags))

  end subroutine ele_fill
  !=============================================================================
  elemental subroutine ele_destory(this)
    class(elem),intent(inout) :: this

    this%id    = 0
    this%type  = 0
    this%ntags = 0
    this%enn   = 0
    if(allocated(this%eni))  deallocate(this%eni)
    if(allocated(this%tags)) deallocate(this%tags)
  end subroutine ele_destory
  !=============================================================================
  elemental subroutine ele_final(this)
    type(elem),intent(inout) :: this
    call this%destory()
  end subroutine ele_final

end module gmsh_interface

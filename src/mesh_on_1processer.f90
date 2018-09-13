module mesh_on_1processer
  use gmsh_interface, only: msh_file
  use string, only: to_str
  implicit none

  public :: msh_on_1proc

  private

contains


  subroutine msh_on_1proc(file, ip)

    character(*), intent(in) :: file
    integer, intent(in), optional :: ip

    type(msh_file) :: gmshf
    real, allocatable :: coord(:,:)
    integer, allocatable :: eni(:,:), fni(:,:), er(:), fr(:), nid(:), eid(:)
    integer :: nd, nn, ne, nf, enn, fnn
    character(:), allocatable :: file_
    character(6) :: pad_

    if( present(ip)) then
      write(pad_,'(i6.6)') ip+1
      file_ = file//'_'//pad_
    else
      file_ = file
    endif

    call gmshf%init(file_)

    nd = gmshf%get_nd()
    nn = gmshf%get_nn()
    ne = gmshf%get_ne()
    nf = gmshf%get_nf()
    enn = gmshf%get_enn()
    fnn = gmshf%get_fnn()

    coord = gmshf%get_coord()
    eni = gmshf%get_eni()
    fni = gmshf%get_fni()
    er = gmshf%get_er()
    fr = gmshf%get_fr()
    eid = gmshf%get_eid()
    nid = gmshf%get_nid()

    if(present(ip)) print*, '# processer ',to_str(ip)
    print*, 'nd = '//to_str(nd)
    print*, 'nn = '//to_str(nn)
    print*, 'ne = '//to_str(ne)
    print*, 'nf = '//to_str(nf)
    print*, 'enn = '//to_str(enn)
    print*, 'fnn = '//to_str(fnn)
    ! print*, 'nid = '//to_str(nid)
    ! print*, 'eid = '//to_str(eid)
    ! print*, 'er = '//to_str(er)
    ! print*, 'fr= '//to_str(fr)
    ! print*, 'eni = '//to_str(eni)
    ! print*, 'fni = '//to_str(fni)
    ! print*, 'coord = '//to_str(coord)

    call gmshf%destory()

  end subroutine msh_on_1proc

end module mesh_on_1processer

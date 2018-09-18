program vg_psb_cdall
  use gmsh_interface
  use metis_interface
  use string
  use iso_c_binding
  use psb_base_mod
  use psb_prec_mod
  use psb_krylov_mod
  use psb_util_mod
  implicit none

  character(:), allocatable :: file
  integer(c_int) :: options(0:39)
  type(msh_file) :: gmshf
  integer, allocatable :: eni(:,:)
  integer :: nd, nn, ne, enn
  integer :: ie

  integer :: info

  integer, allocatable :: eptr(:), eind(:), epart(:), npart(:)
  integer :: ist,ied, objval

  integer :: ip, np, ic
  ! communications data structure
  type(psb_desc_type):: desc_a
  integer :: nr_lc, nc_lc, nr_gl, nc_gl

  file = '../data/gmsh.msh'


  call gmshf%init(file)
  nn = gmshf%get_nn()
  ne = gmshf%get_ne()
  enn = gmshf%get_enn()
  eni = gmshf%get_eni()

  allocate(eptr(ne+1))
  allocate(eind(ne*enn))
  allocate(epart(ne), npart(nn))

  eptr(ne+1) = enn*ne
  do ie = 1, ne
    ied = enn*ie
    ist = ied - enn + 1
    eptr(ie) = ist
    eind(ist:ied) = eni(:,ie)
  enddo

  info = METIS_SetDefaultOptions(options)
  options(17) = 0

  info = METIS_PartMeshNodal(ne,nn,eptr,eind,nparts=4,options=options,&
  objval=objval,epart=epart,npart=npart)

  call psb_init(ic)
  call psb_info(ic, ip, np)
  call psb_set_errverbosity(2)
  call psb_cdall(ic,desc_a,info,vg = npart)

  nr_gl = desc_a%get_global_rows()
  nc_gl = desc_a%get_global_cols()

  if(ip == 0) then
    print*, 'nn = '//to_str(nn)
    print*, 'global_rows = '//to_str(nr_gl)
    print*, 'global_cols = '//to_str(nc_gl)
  endif

  nr_lc = desc_a%get_local_rows()
  nc_lc = desc_a%get_local_cols()

  print*, 'local_rows_'//to_str(ip)//' = '//to_str(nr_lc)
  print*, 'local_cols_'//to_str(ip)//' = '//to_str(nc_lc)

  call psb_exit(ic)

end program vg_psb_cdall

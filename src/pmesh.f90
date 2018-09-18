program pmesh
  use gmsh_interface
  use metis_interface
  use string
  use iso_c_binding
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


  print*, 'nn = '//to_str(nn)
  print*, 'ne = '//to_str(ne)
  print*, 'epart = '//to_str(epart)
  print*, 'npart = '//to_str(npart)


end program pmesh

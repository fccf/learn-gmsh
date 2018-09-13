program gmsh_reader_mult_proc
  use mesh_on_1processer
  use psb_base_mod
  implicit none
  character(:), allocatable :: file
  integer :: cm, ip, np

  file = '../data/gmsh.msh'

  !> test multi process
  call psb_init(cm)
  call psb_info(cm,ip,np)

  call msh_on_1proc(file, ip)

  call psb_exit(cm)

end program gmsh_reader_mult_proc

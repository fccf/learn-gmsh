program gmsh_reader_1proc
  use mesh_on_1processer
  implicit none
  character(:), allocatable :: file

  file = '../data/gmsh.msh'

  !> test serial
  call msh_on_1proc(file)

end program gmsh_reader_1proc

!| 境界の種類を表す定数を提供する．
!
!定数は，第1種（Dirichlet）境界条件および
!第2種（Neumann）境界条件を表す定数が定義される．
!
module grid_uniform_staggered_op_custom_bc_type
    use, intrinsic :: iso_c_binding
    implicit none
    private
    public :: boundary_Dirichlet, &
              boundary_Neumann

    enum, bind(c)
        enumerator :: boundary_Dirichlet = 1
            !! 第1種（Dirichlet）境界条件を表す定数
        enumerator :: boundary_Neumann
            !! 第2種（Neumann）境界条件を表す定数
    end enum

end module grid_uniform_staggered_op_custom_bc_type

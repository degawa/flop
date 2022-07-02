!>境界の種類を表す定数を提供する．
!>
!>定数は，第1種（Dirichlet）境界条件および
!>第2種（Neumann）境界条件を表す定数が定義される．
!>
module grid_uniform_stg_op_custom_bc_vars_type
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: iso_c_binding
    implicit none
    private
    public :: boundary_Dirichlet, &
              boundary_Neumann
    public :: is_Dirichlet_boundary, &
              is_Neumann_boundary

    enum, bind(c)
        enumerator :: boundary_Dirichlet = 1
            !! 第1種（Dirichlet）境界条件を表す定数
        enumerator :: boundary_Neumann
            !! 第2種（Neumann）境界条件を表す定数
    end enum

contains
    !>境界の種類がDirichlet境界条件の場合に`.true.`を，
    !>そうでない場合に`.false.`を返す．
    logical function is_Dirichlet_boundary(boundary_type)
        implicit none

        integer(int32), intent(in) :: boundary_type
            !! 境界の種類

        is_Dirichlet_boundary = (boundary_type == boundary_Dirichlet)
    end function is_Dirichlet_boundary

    !>境界の種類がNeumann境界条件の場合に`.true.`を，
    !>そうでない場合に`.false.`を返す．
    logical function is_Neumann_boundary(boundary_type)
        implicit none

        integer(int32), intent(in) :: boundary_type
            !! 境界の種類

        is_Neumann_boundary = (boundary_type == boundary_Neumann)
    end function is_Neumann_boundary
end module grid_uniform_stg_op_custom_bc_vars_type

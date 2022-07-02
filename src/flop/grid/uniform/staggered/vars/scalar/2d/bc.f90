!>スカラ量に対する境界条件を取り扱う派生型を提供する．
!>
!>派生型には，スカラ量に対する境界条件（境界の位置，種別および値）をまとめて
!>取り扱うための型が含まれる．
!>
module grid_uniform_stg_vars_scalar_2d_bc
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_stg_op_cust_bc_vars_type
    use :: grid_uniform_stg_op_cust_bc_vars_position
    use :: grid_uniform_stg_op_cust_bc_vars_scalar_grad_on_bnd
    use :: grid_uniform_stg_op_cust_bc_vars_scalar_value_on_bnd
    implicit none
    private

    !>スカラ量に対する境界条件（境界の位置，種別および値）
    !>をまとめて取り扱うための派生型．
    type, public :: scalar_boundary_condition_type
        type(scalar_value_on_boundary_type) :: &
            boundary_value(number_of_boundaries)
            !! 境界の位置（を示すインデックス）およびスカラ量の値
        type(scalar_gradient_on_boundary_type) :: &
            boundary_gradient(number_of_boundaries)
            !! 境界の位置（を示すインデックス）および
            !! スカラ量の外向き法線方向勾配
        integer(int32) :: boundary_type(number_of_boundaries)
            !! 境界の種別（DirichletもしくはNeumann）
    contains
        procedure, public, pass :: get_boundary_value
            !! 指定された境界におけるスカラ量を返却
        procedure, public, pass :: get_boundary_gradient
            !! 指定された境界におけるスカラ量の外向き法線方向勾配を返却
    end type scalar_boundary_condition_type

contains
    !>指定された境界におけるスカラ量を返す．
    !>
    !>指定された境界がDirichlet境界でない場合，値は不定．
    !>
    !>呼出し元で，境界の判別をしてから呼び出すことが推奨される．
    !>例えば，
    !>
    !>```Fortran
    !>if (scr_bc%boundary_type(bnd_idx) == boundary_Dirichlet) then
    !>    scr_val = scr_bc%get_boundary_value(bnd_idx)
    !>end if
    !>```
    !>
    function get_boundary_value(this, boundary_index) result(boundary_value)
        implicit none
        !&<
        class(scalar_boundary_condition_type)   , intent(in) :: this
            !! 当該実体仮引数
        integer(int32)                          , intent(in) :: boundary_index
            !! 境界の位置を示すインデックス
        !&>
        real(real64) :: boundary_value
            !! 境界におけるスカラ量

        ! 境界がDirichletの場合だけ値を取得．
        if (this%boundary_type(boundary_index) == boundary_Dirichlet) then
            boundary_value &
                = this%boundary_value(boundary_index)%scalar_value%value
        end if
    end function get_boundary_value

    !>指定された境界におけるスカラ量の外向き法線方向勾配を返す．
    !>
    !>指定された境界がNeumann境界でない場合，値は不定．
    !>
    !>呼出し元で，境界の判別をしてから呼び出すことが推奨される．
    !>例えば，
    !>
    !>```Fortran
    !>if (scr_bc%boundary_type(bnd_idx) == boundary_Neumann) then
    !>    scr_grad = scr_bc%get_boundary_gradient(bnd_idx)
    !>end if
    !>```
    !>
    function get_boundary_gradient(this, boundary_index) result(boundary_grad)
        implicit none
        !&<
        class(scalar_boundary_condition_type)   , intent(in) :: this
            !! 当該実体仮引数
        integer(int32)                          , intent(in) :: boundary_index
            !! 境界の位置を示すインデックス
        !&>
        real(real64) :: boundary_grad
            !! 境界におけるスカラ量の外向き法線方向勾配

        ! 境界がNeumannの場合だけ値を取得．
        if (this%boundary_type(boundary_index) == boundary_Neumann) then
            boundary_grad &
                = this%boundary_gradient(boundary_index)%scalar_gradient%gradient
        end if
    end function get_boundary_gradient
end module grid_uniform_stg_vars_scalar_2d_bc

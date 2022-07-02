!>ある境界における境界条件を作成する手続を提供する．
!>
!>手続には，境界の位置と，境界における値や勾配を組み合わせて，
!>ある境界における境界条件を作成する手続が含まれる．
!>
!>- ベクトル量に対するDirichlet境界条件
!>- スカラ量に対するDirichlet境界条件
!>- スカラ量に対するNeumann境界条件
!>
!>を作成する．
!>
!>また，`Dirichlet([0d0, 0d0]) .on. B1`などと表現することを実現するための
!>ユーザ定義演算子`.on.`を定義するインタフェースも含まれる．
!>
module grid_uniform_stg_op_custom_bc_on
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_stg_op_custom_bc_vars_type
    use :: grid_uniform_stg_op_custom_bc_vars_position
    use :: grid_uniform_stg_op_custom_bc_vars_vector_value_Dirichlet
    use :: grid_uniform_stg_op_custom_bc_vars_vector_value_on_bnd
    use :: grid_uniform_stg_op_custom_bc_vars_scalar_value_Dirichlet
    use :: grid_uniform_stg_op_custom_bc_vars_scalar_value_on_bnd
    use :: grid_uniform_stg_op_custom_bc_vars_scalar_grad_Neumann
    use :: grid_uniform_stg_op_custom_bc_vars_scalar_grad_on_bnd
    implicit none
    private
    public :: operator(.on.)

    !>ユーザ定義演算子`.on.`を定義するインタフェース
    interface operator(.on.)
        procedure :: on_vecval_pos
        procedure :: on_scrval_pos
        procedure :: on_scrgrad_pos
    end interface

contains

    !>ベクトル量のDirichlet境界条件と境界の位置を組み合わせ，
    !>ある境界におけるベクトル量の境界条件を返す．
    function on_vecval_pos(Dirichlet_vector_value, position) result(new_vecval_bc)
        implicit none
        !&<
        class(Dirichlet_vector_value_type)  , intent(in) :: Dirichlet_vector_value
            !! 境界用に設定されたベクトル量の値
        class(boundary_position_type)       , intent(in) :: position
            !! 境界の位置
        !&>
        type(vector_value_on_boundary_type) :: new_vecval_bc
            !! ある境界におけるベクトル量のDirichlet境界条件

        new_vecval_bc%vector_value = Dirichlet_vector_value
        new_vecval_bc%boundary_index = position%position
    end function on_vecval_pos

    !>スカラ量のDirichlet境界条件と境界の位置を組み合わせ，
    !>ある境界におけるスカラ量の境界条件を返す．
    function on_scrval_pos(Dirichlet_scalar_value, position) result(new_scrval_bc)
        implicit none
        !&<
        class(Dirichlet_scalar_value_type)  , intent(in) :: Dirichlet_scalar_value
            !! 境界用に設定されたスカラ量の値
        class(boundary_position_type)       , intent(in) :: position
            !! 境界の位置
        !&>
        type(scalar_value_on_boundary_type) :: new_scrval_bc
            !! ある境界におけるスカラ量のDirichlet境界条件

        new_scrval_bc%scalar_value = Dirichlet_scalar_value
        new_scrval_bc%boundary_index = position%position
    end function on_scrval_pos

    !>スカラ量のDirichlet境界条件と境界の位置を組み合わせ，
    !>ある境界におけるスカラ量の境界条件を返す．
    function on_scrgrad_pos(Neumann_scalar_gradient, position) result(new_scrgrad_bc)
        implicit none
        !&<
        class(Neumann_scalar_gradient_type) , intent(in) :: Neumann_scalar_gradient
           !! 境界用に設定されたスカラ量の値
        class(boundary_position_type)       , intent(in) :: position
            !! 境界の位置
        !&>
        type(scalar_gradient_on_boundary_type) :: new_scrgrad_bc
            !! ある境界におけるスカラ量のNeumann境界条件

        new_scrgrad_bc%scalar_gradient = Neumann_scalar_gradient
        new_scrgrad_bc%boundary_index = position%position
    end function on_scrgrad_pos
end module grid_uniform_stg_op_custom_bc_on

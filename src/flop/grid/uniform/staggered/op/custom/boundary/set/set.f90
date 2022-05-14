!>ある境界における境界条件から，全体の境界条件を組み立てる手続を提供する．
!>
!>手続には，スカラ量もしくはベクトル量の境界条件を取り扱う型に，
!>ある境界における境界条件の情報を追加する手続が含まれる．
!>
!>また，`BC_u .set. (Dirichlet([0d0, 0d0]) .on. B1)`などと
!>表現することを実現するためのユーザ定義演算子`.set.`を
!>定義するインタフェースも含まれる．
!>
module grid_uniform_staggered_op_custom_bc_set
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_staggered_op_custom_bc_vars_type
    use :: grid_uniform_staggered_op_custom_bc_vars_position
    use :: grid_uniform_staggered_op_custom_bc_vars_vector_value_on_bnd
    use :: grid_uniform_staggered_op_custom_bc_vars_scalar_grad_on_bnd
    use :: grid_uniform_staggered_op_custom_bc_vars_scalar_value_on_bnd
    use :: grid_uniform_staggered_vars_vector_2d_bc
    use :: grid_uniform_staggered_vars_scalar_2d_bc
    implicit none
    private
    public :: operator(.set.)

    !>ユーザ定義演算子`.set.`を定義するインタフェース
    interface operator(.set.)
        procedure :: set_vecval_bc
        procedure :: set_scrgrad_bc
        procedure :: set_scrval_bc
    end interface

contains

    !>ベクトル量に対する領域全体の境界条件に対して，
    !>ある境界におけるベクトル量のDirichlet境界条件を追加し，
    !>全体の境界条件を返す．
    function set_vecval_bc(vec_bc, vec_bnd_val) result(new_vec_bc)
        implicit none
        !&<
        class(vector_boundary_condition_type)   , intent(in) :: vec_bc
            !! ベクトル量に対する領域全体の境界条件
        class(vector_value_on_boundary_type)    , intent(in) :: vec_bnd_val
            !! ベクトル量に対する，あるDirichlet境界の境界条件
        !&>
        type(vector_boundary_condition_type) :: new_vec_bc
            !! 境界の情報が追加された，ベクトル量に対する領域全体の境界条件

        ! 一度全成分をコピーしたのち，
        ! 追加される境界の情報で上書きする
        new_vec_bc = vec_bc

        ! vec_bnd_valが設定されている境界の位置を確認し，
        ! 領域全体の境界条件に追加
        block
            integer(int32) :: bnd_idx
            bnd_idx = vec_bnd_val%boundary_index
            new_vec_bc%boundary_value(bnd_idx) = vec_bnd_val
            new_vec_bc%boundary_type(bnd_idx) = boundary_Dirichlet
        end block
    end function set_vecval_bc

    !>スカラ量に対する領域全体の境界条件に対して，
    !>ある境界におけるスカラ量のNeumann境界条件を追加し，
    !>全体の境界条件を返す．
    function set_scrgrad_bc(scr_bc, scr_bnd_grad) result(new_scr_bc)
        implicit none
        !&<
        class(scalar_boundary_condition_type)   , intent(in) :: scr_bc
           !! スカラ量に対する領域全体の境界条件
        class(scalar_gradient_on_boundary_type) , intent(in) :: scr_bnd_grad
           !! スカラ量に対する，あるNeumann境界の境界条件
        !&>
        type(scalar_boundary_condition_type) :: new_scr_bc
           !! 境界の情報が追加された，スカラ量に対する領域全体の境界条件

        ! 一度全成分をコピーしたのち，
        ! 追加される境界の情報で上書きする
        new_scr_bc = scr_bc

        ! scr_bnd_gradが設定されている境界の位置を確認し，
        ! 領域全体の境界条件に追加
        block
            integer(int32) :: bnd_idx
            bnd_idx = scr_bnd_grad%boundary_index
            new_scr_bc%boundary_gradient(bnd_idx) = scr_bnd_grad
            new_scr_bc%boundary_type(bnd_idx) = boundary_Neumann
        end block
    end function set_scrgrad_bc

    !>スカラ量に対する領域全体の境界条件に対して，
    !>ある境界におけるスカラ量のNeumann境界条件を追加し，
    !>全体の境界条件を返す．
    function set_scrval_bc(scr_bc, scr_bnd_val) result(new_scr_bc)
        implicit none
        !&<
        class(scalar_boundary_condition_type), intent(in) :: scr_bc
            !! スカラ量に対する領域全体の境界条件
        class(scalar_value_on_boundary_type), intent(in) :: scr_bnd_val
           !! スカラ量に対する，あるNeumann境界の境界条件
        !&>
        type(scalar_boundary_condition_type) :: new_scr_bc
            !! スカラ量に対する領域全体の境界条件

        ! 一度全成分をコピーしたのち，
        ! 追加される境界の情報で上書きする
        new_scr_bc = scr_bc

        ! scr_bnd_valが設定されている境界の位置を確認し，
        ! 領域全体の境界条件に追加
        block
            integer(int32) :: bnd_idx
            bnd_idx = scr_bnd_val%boundary_index
            new_scr_bc%boundary_value(bnd_idx) = scr_bnd_val
            new_scr_bc%boundary_type(bnd_idx) = boundary_Dirichlet
        end block
    end function set_scrval_bc
end module grid_uniform_staggered_op_custom_bc_set

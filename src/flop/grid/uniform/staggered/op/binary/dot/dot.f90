!>ベクトルの内積\(\cdot\)に関する手続を提供する．
!>
!>手続には，スカラ量に対する内積
!>\[
!>  f \cdot g = \sum_{i}{f_ig_i}
!>\]
!>が含まれる．
!>
!>また，ユーザ定義演算子`.dot.`として公開するための
!>インタフェースも含まれる．
!>
module grid_uniform_stg_op_binary_dot
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_stg_2d
    use :: grid_uniform_stg_vars_scalar_2d
    implicit none
    private
    public :: operator(.dot.)

    !>ユーザ定義演算子`.dot.`を定義するインタフェース
    interface operator(.dot.)
        procedure :: dot_scr_scr
    end interface
contains
    !>二つのスカラ量の内積を返す．
    function dot_scr_scr(scr_l, scr_r) result(dot)
        use :: space_vars_Cartesian, &
            x_min => x_min_index, x_max => x_max_index, &
            y_min => y_min_index, y_max => y_max_index
        implicit none

        class(scalar_2d_type), intent(in) :: scr_l
            !! 内積の演算子の左側のスカラ量
        class(scalar_2d_type), intent(in) :: scr_r
            !! 内積の演算子の右側のスカラ量

        real(real64) :: dot
            !! 内積の計算結果

        type(staggered_uniform_grid_2d_type), pointer :: grid
        integer(int32) :: Ncx, Ncy

        grid => scr_l%get_base_grid()
        call grid%get_number_of_grid_center_to(Ncx, Ncy)

        dot = sum(scr_l%val(1:Ncx, 1:Ncy)*scr_r%val(1:Ncx, 1:Ncy))
    end function dot_scr_scr
end module grid_uniform_stg_op_binary_dot

!>単項演算子gradient（∇）に関する手続を提供する．
!>
!>手続には，スカラ量に対するgradient
!>\[
!>\nabla p = \frac{\partial p}{\partial x_j}
!>\]
!>が含まれる．
!>
!>また，ユーザ定義演算子`.grad.`として公開するための
!>インタフェースも含まれる．
!>
!>ベクトル量に対するgradient
!>\[
!>\nabla^2 \boldsymbol{u} = \frac{\partial u_i}{\partial x_j}
!>\]
!>は，laplacianをdiv･gradとして表現することを目的に，
!>今後実装する予定である．
!>
module grid_uniform_staggered_op_unary_grad_acc2
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_staggered_2d
    use :: grid_uniform_staggered_vars_scalar_2d
    use :: grid_uniform_staggered_vars_vector_2d
    implicit none
    private
    public :: operator(.grad.)

    !>ユーザ定義演算子`.grad.`を定義するインタフェース
    interface operator(.grad.)
        procedure :: grad_scr
    end interface

contains
    !>引数のスカラ量にgradient
    !>\[
    !>\nabla^2 p = \frac{\partial p}{\partial x_j}
    !>\]
    !>を適用し，得られた結果をベクトル量で返す．
    function grad_scr(scr) result(new_vec)
        use :: space_Cartesian, &
            x_dir => x_dir_index, y_dir => y_dir_index, &
            x_min => x_min_index, x_max => x_max_index, &
            y_min => y_min_index, y_max => y_max_index
        implicit none

        type(scalar_2d_type), intent(in) :: scr
            !! gradientの被演算子

        type(vector_2d_type) :: new_vec
            !! gradientが適用された結果のベクトル量

        type(staggered_uniform_grid_2d_type), pointer :: grid
        integer(int32) :: vr(2, 4)
        real(real64) :: dx, dy

        grid => scr%get_base_grid()

        call new_vec%construct(grid) ! 返値を構築

        call grid%get_interval_to(dx, dy) ! 格子幅を取得
        vr = grid%get_vector_range() ! ベクトル量の定義範囲

        block
            integer(int32) :: i, jc, ic
            !&<
            do jc = vr(x_dir, y_min)  , vr(x_dir, y_max)
            do i  = vr(x_dir, x_min)+1, vr(x_dir, x_max)-1
                ic = i
                new_vec%x(i, jc) = (scr%val(ic,jc)-scr%val(ic-1,jc))/dx
            end do
            end do
            !&>
        end block

        block
            integer(int32) :: ic, j, jc
            !&<
            do j  = vr(y_dir, y_min)+1, vr(y_dir, y_max)-1
            do ic = vr(y_dir, x_min)  , vr(y_dir, x_max)
                jc = j
                new_vec%y(ic, j) = (scr%val(ic, jc)-scr%val(ic,jc-1))/dy
            end do
            end do
            !&>
        end block
    end function grad_scr
end module grid_uniform_staggered_op_unary_grad_acc2

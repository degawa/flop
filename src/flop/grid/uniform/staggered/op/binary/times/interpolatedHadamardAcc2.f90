!> 変数の積に関する手続を提供する．
!>
!>手続には，スカラ量とベクトル量に対するHadamard積が含まれる．
!>
!>また，ユーザ定義演算子`.times.`として公開するための
!>インタフェースも含まれる．
!>
!>@note
!>乗算演算子`*`をオーバーロードすることで簡潔に記述できるが，
!>FORDでドキュメントを生成する際にエラーが生じるため，
!>現状ではオーバーロードはコメントアウトしている．
!>@endnote
!>
module grid_uniform_stg_op_binary_times_interpolatedHadamard
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_stg_2d
    use :: grid_uniform_stg_vars_vector_2d
    use :: grid_uniform_stg_vars_scalar_2d
    implicit none
    private
    public :: operator(.times.)
    ! public :: operator(*)

    !>ユーザ定義演算子`.times.`を定義するインタフェース
    interface operator(.times.)
        procedure :: interpolated_Hadamard_product
    end interface

    !>乗算演算子`*`をオーバーロードするためのインタフェース
    ! interface operator(*)
    !     procedure :: interpolated_Hadamard_product
    ! end interface
contains
    !>スカラ量とベクトル量のHadamard積を計算し，
    !>得られた結果をテンソル量で返す．
    !>
    !>計算には，スカラ量の補間が必要となるため，
    !>2次精度の中心補間を用いる．
    !>\[
    !>f\boldsymbol{u} = \overline{f}^x u_x, \overline{f}^y u_y
    !>\]
    function interpolated_Hadamard_product(scr, vec) result(new_vec)
        use :: space_vars_Cartesian, &
            x_dir => x_dir_index, y_dir => y_dir_index, &
            x_min => x_min_index, x_max => x_max_index, &
            y_min => y_min_index, y_max => y_max_index
        implicit none

        class(scalar_2d_type), intent(in) :: scr
            !! 演算子左側のスカラ量
            !! （補間される）
        class(vector_2d_type), intent(in) :: vec
            !! 演算子右側側のベクトル量

        type(vector_2d_type) :: new_vec
            !! Hadamard積によって得られたベクトル量

        type(staggered_uniform_grid_2d_type), pointer :: grid
        integer(int32) :: vr(2, 4)

        grid => scr%get_base_grid()
        call new_vec%construct(grid)
        vr = grid%get_vector_range()

        x: block
            integer(int32) :: i, jc, ic
            real(real64) :: scr_intp
            !&<
            do jc = vr(x_dir, y_min)  , vr(x_dir, y_max)
            do i  = vr(x_dir, x_min)+1, vr(x_dir, x_max)-1
                ic = i
                scr_intp = (scr%val(ic, jc) + scr%val(ic-1, jc))/2d0
                new_vec%x(i, jc) = scr_intp*vec%x(i, jc)
            end do
            end do
            !&>
        end block x

        y: block
            integer(int32) :: ic, j, jc
            real(real64) :: scr_intp
            !&<
            do j  = vr(y_dir, y_min)+1, vr(y_dir, y_max)-1
            do ic = vr(y_dir, x_min)  , vr(y_dir, x_max)
                jc = j
                scr_intp = (scr%val(ic, jc) + scr%val(ic, jc-1))/2d0
                new_vec%y(ic, j) = scr_intp*vec%y(ic, j)
            end do
            end do
            !&>
        end block y
    end function interpolated_Hadamard_product
end module grid_uniform_stg_op_binary_times_interpolatedHadamard

!| 2項演算子のテンソル積\(\otimes\)に関する手続を提供する．
!
!手続には，ベクトル量に対するテンソル積
!\[
!\boldsymbol{u}\otimes\boldsymbol{v} = u_i v_j
!\]
!が含まれる．
!
!また，ユーザ定義演算子`.times.`として公開するための
!インタフェースも含まれる．
!
!WENOやMUSCL等を用いた上流化は，必要に応じて実装する．
!
module grid_uniform_staggered_op_binary_times_central
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_staggered_2d
    use :: grid_uniform_staggered_vars_vector_2d
    use :: grid_uniform_staggered_vars_tensor_2d
    implicit none
    private
    public :: operator(.times.)

    !| ユーザ定義演算子`.times.`を定義するインタフェース
    interface operator(.times.)
        procedure :: tensor_product
    end interface

contains
    !| 引数の二つのベクトル量を用いてテンソル積
    !\[
    !\boldsymbol{u}\otimes\boldsymbol{v} = u_i v_j
    !\]
    !を計算し，得られた結果をテンソル量で返す．
    !
    !上流化は行わず，中心補間を用いる．
    function tensor_product(vec_l, vec_r) result(new_tsr)
        use :: space_Cartesian, &
            xx => xx_index, xy => xy_index, yx => yx_index, yy => yy_index, &
            x_min => x_min_index, x_max => x_max_index, &
            y_min => y_min_index, y_max => y_max_index
        implicit none

        class(vector_2d_type), intent(in) :: vec_l
            !! テンソル積の演算子の左側のベクトル量
        class(vector_2d_type), intent(in) :: vec_r
            !! テンソル積の演算子の右側のベクトル量

        type(tensor_2d_type) :: new_tsr
            !! テンソル積の結果のテンソル量

        type(staggered_uniform_grid_2d_type), pointer :: grid
        integer(int32) :: tr(4, 4)

        grid => vec_l%get_base_grid()

        call new_tsr%construct(grid) ! 返値を構築

        tr = grid%get_tensor_range() ! テンソル量の定義範囲

        xx_: block
            integer(int32) :: ic, jc, i
            !&<
            do jc = tr(xx,y_min), tr(xx,y_max)
            do ic = tr(xx,x_min), tr(xx,x_max)
                i = ic
                new_tsr%xx(ic,jc) = ( vec_l%x(i+1,jc) + vec_l%x(i ,jc))/2d0 &
                                   *( vec_r%x(i+1,jc) + vec_r%x(i ,jc))/2d0
            end do
            end do
            !&>
        end block xx_

        xy_: block
            integer(int32) :: i, j, ic, jc
            !&<
            do j = tr(xy,y_min)+1, tr(xy,y_max)-1
            do i = tr(xy,x_min)+1, tr(xy,x_max)-1
                ic = i
                jc = j
                new_tsr%xy(i,j) = (vec_l%y(ic,j ) + vec_l%y(ic-1,j   ))/2d0 &
                                 *(vec_r%x(i ,jc) + vec_r%x(i   ,jc-1))/2d0
            end do
            end do
            !&>
        end block xy_

        yx_: block
            integer(int32) :: i, j, ic, jc
            !&<
            do j = tr(yx,y_min)+1, tr(yx,y_max)-1
            do i = tr(yx,x_min)+1, tr(yx,x_max)-1
                ic = i
                jc = j
                new_tsr%yx(i,j) = (vec_l%x(i ,jc) + vec_l%x(i   ,jc-1))/2d0 &
                                 *(vec_r%y(ic,j ) + vec_r%y(ic-1,j   ))/2d0
            end do
            end do
            !&>
        end block yx_

        yy_: block
            integer(int32) :: ic, jc, j
            !&<
            do jc = tr(yy,y_min), tr(yy,y_max)
            do ic = tr(yy,x_min), tr(yy,x_max)
                j = jc
                new_tsr%yy(ic,jc) = (vec_l%y(ic,j+1) + vec_l%y(ic,j))/2d0 &
                                   *(vec_r%y(ic,j+1) + vec_r%y(ic,j))/2d0
            end do
            end do
            !&>
        end block yy_
    end function tensor_product
end module grid_uniform_staggered_op_binary_times_central

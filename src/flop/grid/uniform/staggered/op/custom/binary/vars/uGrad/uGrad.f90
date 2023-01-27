!>Navier-Stokes方程式の移流項\((\boldsymbol{u}\cdot\nabla)\boldsymbol{u}\)
!>に関する派生型を提供する．
!>
!>派生型には，Navier-Stokes方程式の移流項\((\boldsymbol{u}\cdot\nabla)\boldsymbol{u}\)のうち，
!>\(\boldsymbol{u}\cdot\nabla\)を扱う派生型が含まれる．
!>
!>@note
!>メインルーチンの中では明確に型宣言されない．
!>演算に使われる他の型の成分として宣言，演算子に渡す値として生成，
!>演算の結果として中間的に生成される．
!>@endnote
!>
!>@note
!>
!>\(\boldsymbol{u}\cdot\nabla\)は非常に特殊であり，
!>単純に単項演算子や2項演算子としては記述できない．
!>また，Fortranではユーザ定義演算子を返す関数を作れない．
!>
!>そこで，単純な演算子として記述することを諦め，`(u_dot_nabla).op.u`と記述する．
!>`u_dot_nabla`は，移流速度\(\boldsymbol{u}\)を成分にもち，
!>実際に\((\boldsymbol{u}\cdot\nabla)\boldsymbol{u}\)を離散的に計算する
!>手続を含んでいればよい．
!>
!>`.op.`を介して実行される手続内から`u_dot_nabla`の手続を呼び出せば，
!>移流項の表現が実現できる．
!>
!>`.op.`には`*`が利用できる．
!>
!>@endnote
!>
module grid_uniform_stg_op_cust_binary_vars_uGrad
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_stg_2d
    use :: grid_uniform_stg_vars_vector_2d
    implicit none
    private

    !>Navier-Stokes方程式の移流項のうち，
    !>\(\boldsymbol{u}\cdot\nabla\)を扱う派生型．
    type, public :: u_grad_type
        type(vector_2d_type), public :: u
            !! 移流速度
    contains
        procedure, public, pass :: construct
        !* 移流速度に基づいて実体を構築
        procedure, public, pass :: compute
        !* 移流項の計算結果を返却
        generic :: operator(*) => compute
    end type u_grad_type

contains
    !>移流速度に基づいて，`u_grad_type`を構築する．
    subroutine construct(this, u)
        implicit none
        !&<
        class(u_grad_type)      , intent(inout) :: this
            !! 当該実体仮引数
        class(vector_2d_type)   , intent(in)    :: u
            !! 移流速度
        !&>

        this%u = u
    end subroutine construct

    !>`u_grad_type`に格納された移流速度と，
    !>引数で渡された速度（保存量）を用いて，Navier-Stokes方程式の移流項
    !>\[
    !>(\boldsymbol{u}\cdot\nabla)\boldsymbol{u} = u_j\frac{\partial u_i}{\partial x_j}
    !>\]
    !>を計算し，得られた結果をベクトル量で返す．
    !>
    !>上流化は行わず，中心補間を用いる．
    !>これは，梶島(1994) (https://doi.org/10.1299/kikaib.60.2058)によって
    !>示された，移流項に対する適切な差分形式のうち，勾配型
    !>\[
    !> \overline{\overline{u_j}^{x_i}\delta_{x_j}u_i}^{x_j}
    !>\]
    !>に相当する．
    function compute(u_grad, u) result(new_vec)
        use :: space_vars_Cartesian, &
            x_dir => x_dir_index, y_dir => y_dir_index, &
            x_min => x_min_index, x_max => x_max_index, &
            y_min => y_min_index, y_max => y_max_index
        implicit none
        !&<
        class(u_grad_type)      , intent(in) :: u_grad
            !! 移流項の\((\boldsymbol{u}\cdot\nabla)\)
        class(vector_2d_type)   , intent(in) :: u
            !! 保存量の速度
        !&>
        type(vector_2d_type) :: new_vec
            !! 移流項の計算結果

        type(staggered_uniform_grid_2d_type), pointer :: grid
        real(real64) :: dx, dy
        integer(int32) :: vr(2, 4)

        grid => u%get_base_grid()
        call new_vec%construct(grid)

        call grid%get_interval_to(dx, dy)
        vr = grid%get_vector_range()

        block
            integer(int32) :: i, jc, ic, j
            !&<
            do jc = vr(x_dir,y_min)+1,vr(x_dir,y_max)-1
            do i  = vr(x_dir,x_min)+1,vr(x_dir,x_max)-1
                ic = i
                j  = jc
                new_vec%x(i,jc) = ( ( (u_grad%u%x(i-1,jc)+u_grad%u%x(i  ,jc))/2d0 &
                                     *(      -u%x(i-1,jc)+       u%x(i  ,jc))/dx ) &
                                   +( (u_grad%u%x(i  ,jc)+u_grad%u%x(i+1,jc))/2d0 &
                                     *(      -u%x(i  ,jc)+       u%x(i+1,jc))/dx ) &
                                  )/2d0 &
                                 +( ( (u_grad%u%y(ic-1,j   )+u_grad%u%y(ic,j   ))/2d0 &
                                     *(      -u%x(i   ,jc-1)+       u%x(i ,jc  ))/dy ) &
                                   +( (u_grad%u%y(ic-1,j +1)+u_grad%u%y(ic,j +1))/2d0 &
                                     *(      -u%x(i   ,jc  )+       u%x(i ,jc+1))/dy ) &
                                  )/2d0
            end do
            end do
            !&>
        end block

        block
            integer(int32) :: ic, j, i, jc
            !&<
            do j  = vr(y_dir,y_min)+1,vr(y_dir,y_max)-1
            do ic = vr(y_dir,x_min)+1,vr(y_dir,x_max)-1
                i  = ic
                jc = j
                new_vec%y(ic,j) = ( ( (u_grad%u%x(i   ,jc-1)+u_grad%u%x(i   ,jc))/2d0 &
                                     *(      -u%y(ic-1,j   )+       u%y(ic  ,j ))/dx ) &
                                   +( (u_grad%u%x(i +1,jc-1)+u_grad%u%x(i +1,jc))/2d0 &
                                     *(      -u%y(ic  ,j   )+       u%y(ic+1,j ))/dx ) &
                                  )/2d0 &
                                 +( ( (u_grad%u%y(ic,j-1)+u_grad%u%y(ic,j  ))/2d0 &
                                     *(      -u%y(ic,j-1)+       u%y(ic,j  ))/dy ) &
                                   +( (u_grad%u%y(ic,j  )+u_grad%u%y(ic,j+1))/2d0 &
                                     *(      -u%y(ic,j  )+       u%y(ic,j+1))/dy ) &
                                  )/2d0
            end do
            end do
            !&>
        end block
    end function compute
end module grid_uniform_stg_op_cust_binary_vars_uGrad

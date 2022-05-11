!| スカラ量に関する型を提供する．
!
!定義される型には，Staggered格子[[staggered_uniform_grid_2d_type]]上で
!定義されるスカラ量を取り扱う型が含まれる．
!
module grid_uniform_staggered_vars_scalar_2d
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_staggered_2d
    implicit none
    private
    public :: scalar_2d_type

    !| 2次元Staggered格子上で定義されるスカラ量型．
    type :: scalar_2d_type
        type(staggered_uniform_grid_2d_type), private, pointer :: grid
            !! スカラ量が定義されている格子の情報<br>
            !! `base_grid`として参照
        real(real64), allocatable, public :: val(:, :)
            !! スカラ量を格納する配列
    contains
        !&<
        procedure, public, pass :: construct_by_base_grid
            !! 成分`grid`に基づいて配列を割り付け
        procedure, public, pass :: construct_by_grid
            !! 格子の情報に基づいて配列を割り付け
        procedure, public, pass :: construct_by_mold
            !! `scalar_2d_type`の情報に基づいて配列を割り付け
        procedure, public, pass :: construct_by_grid_pointer
            !! 格子（ポインタ）の情報に基づいて配列を割り付け
        generic :: construct => construct_by_base_grid, &
                                construct_by_grid_pointer, &
                                construct_by_mold
        generic :: init_on   => construct_by_grid
            !! `call u%init_on(grid)`と呼ぶためのインタフェース
        procedure, public, pass :: destruct
            !! 割り付けた配列を解放
        final :: finalize
            !! 後始末手続
        !&>
        procedure, public, pass :: associate_grid
            !! 格子を関連付けた新しいスカラ量を返却．
        procedure, public, pass :: initialize
            !! 関連付けられた格子に基づいて配列が割り付けられた，
            !! 新しいスカラ量を返却．
        generic :: operator(.on.) => associate_grid
            !! `p .on. grid`で格子を関連付けるためのインタフェース
        generic :: operator(.init.) => initialize
            !! `.init. p`で配列を割り付けるためのインタフェース

        procedure, public, pass :: get_base_grid
            !! `scalar_2d_type`に関連付けられている
            !! 格子へのポインタを返却
        !&<
        procedure, public, pass :: assign
            !! `scalar_2d_type`を代入
        procedure, public, pass :: multiply_r8
            !! 倍精度実数を乗算し，結果を返却．<br>
            !! `scalar_2d_type*real64`の形
        procedure, public, pass(this) :: r_multiply_r8
            !! 倍精度実数を乗算し，結果を返却．<br>
            !! `real64*scalar_2d_type`の形
        generic :: assignment(=) => assign
        generic :: operator(*)   => multiply_r8, r_multiply_r8
        !&>
    end type scalar_2d_type
contains
    !| 成分の`grid`の情報に基づいて，`scalar_2d_type`を構築する．
    !
    !格子を`scalar_2d_type`のbase_gridとして関連付け，
    !配列を動的に割り付ける．
    subroutine construct_by_base_grid(this)
        use :: space_Cartesian, &
            x_min => x_min_index, x_max => x_max_index, &
            y_min => y_min_index, y_max => y_max_index
        implicit none
        !&<
        class(scalar_2d_type), intent(inout)         :: this
            !! 構築される`scalar_2d_type`<br>
            !! `this%grid`が関連付けられている必要がある<br>
            !! 当該実体仮引数
        !&>

        integer(int32) :: is, ie, js, je
        integer(int32) :: scalar_range(4)

        scalar_range = this%grid%get_scalar_range()

        is = scalar_range(x_min)
        ie = scalar_range(x_max)
        js = scalar_range(y_min)
        je = scalar_range(y_max)
        allocate (this%val(is:ie, js:je), source=0d0)
    end subroutine construct_by_base_grid

    !| 渡された格子（ポインタ）の情報に基づいて，`scalar_2d_type`を構築する．
    !
    !格子を`scalar_2d_type`のbase_gridとして関連付け，
    !成分の配列を動的に割り付ける．
    !
    !導入の理由は[[grid_uniform_staggered_vars_vector_2d(module):construct_by_grid_pointer(subroutine)]]
    !を参照のこと．
    subroutine construct_by_grid_pointer(this, grid)
        implicit none
        !&<
        class(scalar_2d_type)               , intent(inout)             :: this
            !! 構築される`scalar_2d_type`<br>
            !! 当該実体仮引数
        type(staggered_uniform_grid_2d_type), intent(in)    , pointer   :: grid
            !! `scalar_2d_type`が定義される格子への**ポインタ**
        !&>

        ! bese_gridと格子を関連付け
        this%grid => grid ! `=`ではない
        ! 実際の割付は委譲
        call this%construct()
    end subroutine construct_by_grid_pointer

    !| 渡された`scalar_2d_type`の情報に基づいて，
    !`scalar_2d_type`を構築する．
    !
    !格子を`scalar_2d_type`のbase_gridとして関連付け，
    !成分の配列を動的に割り付ける．
    subroutine construct_by_mold(this, mold)
        implicit none
        !&<
        class(scalar_2d_type), intent(inout)    :: this
            !! 構築される`scalar_2d_type`<br>
            !! 当該実体仮引数
        class(scalar_2d_type), intent(in)       :: mold
            !! 割付の参考となる型見本
        !&>

        !実際の割付は委譲
        call this%construct(mold%grid)
    end subroutine construct_by_mold

    !| 渡された格子の情報に基づいて，`scalar_2d_type`を構築する．
    !
    !格子を`scalar_2d_type`のbase_gridとして関連付け，
    !配列を動的に割り付ける．
    subroutine construct_by_grid(this, grid)
        implicit none
        !&<
        class(scalar_2d_type)               , intent(inout)         :: this
            !! 構築される`scalar_2d_type`<br>
            !! 当該実体仮引数
        type(staggered_uniform_grid_2d_type), intent(in),   target  :: grid
            !! `scalar_2d_type`が定義される格子<br>
            !! **`target`属性は必須**
        !&>

        ! bese_gridと格子を関連付け
        this%grid => grid ! `=`ではない
        ! 実際の割付は`construct_by_base_grid`に委譲
        call this%construct()
    end subroutine construct_by_grid

    !| 構築した`scalar_2d_type`を破棄する．
    !
    !成分の配列を解放し，base_gridとの関連を絶つ．
    subroutine destruct(this)
        implicit none
        class(scalar_2d_type), intent(inout) :: this
            !! 破棄される`scalar_2d_type`<br>
            !! 当該実体仮引数

        if (allocated(this%val)) deallocate (this%val)
        this%grid => null()
    end subroutine destruct

    !| 動的に割り付けられた`scalar_2d_type`を始末する．
    !
    !成分の配列を解放し，base_gridとの関連を絶つ．
    subroutine finalize(this)
        implicit none
        type(scalar_2d_type), intent(inout) :: this
            !! 破棄される`scalar_2d_type`<br>
            !! 当該実体仮引数

        call this%destruct()
    end subroutine finalize

    !| 渡された格子を関連付けた，新しい`scalar_2d_type`を返す．
    !
    !戻り値となる`scalar_2d_type`に格子をbase_gridとして関連付けるが，
    !配列は割り付けない．
    !
    !割付は，[[grid_uniform_staggered_vars_scalar_2d(module):initialize(function)]]
    !が行う．
    !
    !演算子指向的に`p = p .on. grid`と呼ぶことを目的に，
    !`.on.`2項演算子に関連付けられる．
    function associate_grid(this, grid) result(new_scr)
        implicit none
        !&<
        class(scalar_2d_type)               , intent(in)            :: this
            !! 当該実体仮引数<br>
            !! 2項演算の都合上必要なだけで，特に何も使わない
        type(staggered_uniform_grid_2d_type), intent(in), target    :: grid
            !! `scalar_2d_type`が定義される格子<br>
            !! **`target`属性は必須**
        !&>

        type(scalar_2d_type) :: new_scr
            !! 作成される新しい`scalar_2d_type`<br>
            !! base_gridは関連付けられているが
            !! 成分の配列は割り付けられていない．

        ! 未使用警告を抑制
        if (same_type_as(this, new_scr)) continue

        new_scr%grid => grid ! `=`だと以降で実行時エラー
    end function associate_grid

    !| base_gridに基づいて配列を割り付けた新しい`scalar_2d_type`を返す．
    !
    !演算子指向的に`p = .init. (p)`と呼ぶことを目的に，
    !`.init.`単項演算子に関連付けられる．
    function initialize(this) result(new_scr)
        implicit none

        class(scalar_2d_type), intent(in) :: this
           !! 当該実体仮引数

        type(scalar_2d_type) :: new_scr
            !! 作成される新しい`scalar_2d_type`

        ! 構築をconstructに委譲
        call new_scr%construct(this%grid)
    end function initialize

    !| スカラ量が定義される格子へのポインタを返す．
    function get_base_grid(this) result(grid)
        implicit none
        class(scalar_2d_type), intent(in) :: this
            !! 格子を取得するスカラ量<br>
            !! 当該実体仮引数

        type(staggered_uniform_grid_2d_type), pointer :: grid
           !! スカラ量が定義されている格子へのポインタ

        grid => this%grid
    end function get_base_grid

    !------------------------------------------------------------------!
    !|`scalar_2d_type`を代入する．
    !
    !単体で呼び出すことはなく，代入演算子をオーバーロードして利用する．
    subroutine assign(lhs, rhs)
        implicit none
        !&<
        class(scalar_2d_type), intent(out)  :: lhs
            !! 代入される`scalar_2d_type`<br>
            !! `=`演算子の左側に置かれる量<br>
            !! 当該実体仮引数
        class(scalar_2d_type), intent(in)   :: rhs
            !! 代入する`scalar_2d_type`<br>
            !! `=`演算子の右側に置かれる量
        !&>

        lhs%grid => rhs%grid
        lhs%val = rhs%val ! 自動割り付け
    end subroutine assign

    !| `scalar_2d_type`と倍精度実数の乗算を計算し，
    !結果を`scalar_2d_type`で返す．
    !
    !`scalar_2d_type*real64`の形で計算する．
    !
    !単体で呼び出すことはなく，乗算演算子をオーバーロードして利用する．
    function multiply_r8(this, factor) result(new_scr)
        implicit none
        !&<
        class(scalar_2d_type), intent(in) :: this
            !! `*`演算子の左側に置かれる量<br>
            !! 当該実体仮引数
        real(real64), intent(in) :: factor
            !! `*`演算子の右側に置かれる係数
        !&>
        type(scalar_2d_type) :: new_scr
            !! 乗算結果を格納する`scalar_2d_type`

        call new_scr%construct(this%grid)
        new_scr%val = this%val*factor
    end function multiply_r8

    !| `scalar_2d_type`と倍精度実数の乗算を計算し，
    !結果を`scalar_2d_type`で返す．
    !
    !`real64*scalar_2d_type`の形で計算する．
    !
    !単体で呼び出すことはなく，乗算演算子をオーバーロードして利用する．
    function r_multiply_r8(factor, this) result(new_scr)
        implicit none
        !&<
        real(real64), intent(in) :: factor
           !! `*`演算子の左側に置かれる係数
        class(scalar_2d_type), intent(in) :: this
            !! `*`演算子の右側に置かれる量<br>
            !! 当該実体仮引数
        !&>
        type(scalar_2d_type) :: new_scr
            !! 乗算結果を格納する`scalar_2d_type`

        ! 乗算は左右の入れ替えが可能なので，
        ! 計算は[[grid_uniform_staggered_vars_scalar_2d(module):multiply_r8(function)]]に委譲
        new_scr = this*factor
    end function r_multiply_r8

end module grid_uniform_staggered_vars_scalar_2d

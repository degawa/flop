!| ベクトル量に関する型を提供する．
!
!定義される型には，Staggered格子[[staggered_uniform_grid_2d_type]]上で
!定義されるベクトル量を取り扱う型が含まれる．
!
module grid_uniform_staggered_vars_vector_2d
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_staggered_2d
    implicit none
    private
    public :: vector_2d_type

    !| 2次元Staggered格子上で定義されるベクトル量型．
    type :: vector_2d_type
        type(staggered_uniform_grid_2d_type), private, pointer :: grid
            !! ベクトル量が定義されている格子の情報<br>
            !! `base_grid`として参照
        real(real64), allocatable, public :: x(:, :)
            !! ベクトル量の\(x\)方向成分を格納する配列
        real(real64), allocatable, public :: y(:, :)
            !! ベクトル量の\(y\)方向成分を格納する配列
    contains
        !&<
        procedure, public, pass :: construct_by_base_grid
            !! 成分`grid`に基づいて配列を割り付け
        procedure, public, pass :: construct_by_grid
            !! 格子の情報に基づいて配列を割り付け
        procedure, public, pass :: construct_by_mold
            !! `vector_2d_type`の情報に基づいて配列を割り付け
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
            !! 格子を関連付けた新しいベクトル量を返却．
        procedure, public, pass :: initialize
            !! 関連付けられた格子に基づいて配列が割り付けられた，
            !! 新しいベクトル量を返却．
        generic :: operator(.on.) => associate_grid
            !! `u .on. grid`で格子を関連付けるためのインタフェース
        generic :: operator(.init.) => initialize
            !! `.init. u`で配列を割り付けるためのインタフェース

        procedure, public, pass :: get_base_grid
            !! `vector_2d_type`に関連付けられている
            !! 格子へのポインタを返却

        !&<
        procedure, public, pass :: assign
            !! `vector_2d_type`を代入
        procedure, public, pass :: add_vec
            !! `vector_2d_type`を加算し，
            !! 結果を返却．
        procedure, public, pass :: minus_vec
            !! `vector_2d_type`を減算し，
            !! 結果を返却．
        procedure, public, pass :: negate
            !! 各成分に`-`を適用し，結果を返却．
        procedure, public, pass :: multiply_r8
            !! 倍精度実数を乗算し，結果を返却．<br>
            !! `vector_2d_type*real64`の形
        procedure, public, pass(this) :: r_multiply_r8
            !! 倍精度実数を乗算し，結果を返却．<br>
            !! `real64*vector_2d_type`の形
        generic :: assignment(=) => assign
        generic :: operator(+)   => add_vec
        generic :: operator(-)   => minus_vec, negate
        generic :: operator(*)   => multiply_r8, r_multiply_r8
        !&>
    end type vector_2d_type

contains
    !| 成分の`grid`の情報に基づいて，`vector_2d_type`を構築する．
    !
    !成分の配列を動的に割り付ける．
    subroutine construct_by_base_grid(this)
        use :: space_Cartesian, &
            x_dir => x_dir_index, y_dir => y_dir_index, &
            x_min => x_min_index, x_max => x_max_index, &
            y_min => y_min_index, y_max => y_max_index
        implicit none
        !&<
        class(vector_2d_type), intent(inout) :: this
            !! 構築される`vector_2d_type`<br>
            !! `this%grid`が関連付けられている必要がある<br>
            !! 当該実体仮引数
        !&>

        integer(int32) :: is, ie, js, je
        integer(int32) :: vector_range(2, 4)

        vector_range = this%grid%get_vector_range()

        is = vector_range(x_dir, x_min)
        ie = vector_range(x_dir, x_max)
        js = vector_range(x_dir, y_min)
        je = vector_range(x_dir, y_max)
        allocate (this%x(is:ie, js:je), source=0d0)

        is = vector_range(y_dir, x_min)
        ie = vector_range(y_dir, x_max)
        js = vector_range(y_dir, y_min)
        je = vector_range(y_dir, y_max)
        allocate (this%y(is:ie, js:je), source=0d0)
    end subroutine construct_by_base_grid

    !| 渡された格子（ポインタ）の情報に基づいて，`vector_2d_type`を構築する．
    !
    !格子を`vector_2d_type`のbase_gridとして関連付け，
    !成分の配列を動的に割り付ける．
    !
    !@note
    !
    !gnu, nag fortranでは不要だが，intel fortranは
    !`target`属性を持つ実体と，`pointer`で動作が異なる場合がある．
    !そのため，intel fortran用に`pointer`を引数に取るコンストラクタを作成．
    !
    !`construct_by_grid`と引数が同じと判断されるので，
    !`construct_by_grid`は`init_on`という別名を与える．
    !gridの実体を与えるのはmainルーチンのみであることが想定されるので，
    !ユーザの利便性を考え，ライブラリ外部からは`call u%init_on(grid)`
    !として設定し，ポインタしか参照しない内部では`u%construct`で設定する．
    !
    !@endnote
    !
    subroutine construct_by_grid_pointer(this, grid)
        implicit none
        !&<
        class(vector_2d_type)               , intent(inout)             :: this
            !! 構築される`vector_2d_type`<br>
            !! 当該実体仮引数
        type(staggered_uniform_grid_2d_type), intent(in)    , pointer   :: grid
            !! `vector_2d_type`が定義される格子への**ポインタ**
        !&>

        ! bese_gridと格子を関連付け
        this%grid => grid
        ! 実際の割付は委譲
        call this%construct()
    end subroutine construct_by_grid_pointer

    !| 渡された`vector_2d_type`の情報に基づいて，
    !`vector_2d_type`を構築する．
    !
    !格子を`vector_2d_type`のbase_gridとして関連付け，
    !成分の配列を動的に割り付ける．
    subroutine construct_by_mold(this, mold)
        implicit none
        !&<
        class(vector_2d_type), intent(inout)    :: this
            !! 構築される`vector_2d_type`<br>
            !! 当該実体仮引数
        class(vector_2d_type), intent(in)       :: mold
            !! 割付の参考となる型見本
        !&>

        !実際の割付は委譲
        call this%construct(mold%grid)
    end subroutine construct_by_mold

    !| 渡された格子（実体）の情報に基づいて，`vector_2d_type`を構築する．
    !
    !格子を`vector_2d_type`のbase_gridとして関連付け，
    !成分の配列を動的に割り付ける．
    subroutine construct_by_grid(this, grid)
        implicit none
        !&<
        class(vector_2d_type)               , intent(inout)             :: this
            !! 構築される`vector_2d_type`<br>
            !! 当該実体仮引数
        type(staggered_uniform_grid_2d_type), intent(in)    , target    :: grid
            !! `vector_2d_type`が定義される格子<br>
            !! **ポインタではなく実体であり，`target`属性は必須**
        !&>

        ! bese_gridと格子を関連付け
        this%grid => grid ! `=`ではない
        ! 実際の割付は`construct_by_base_grid`に委譲
        call this%construct()
    end subroutine construct_by_grid

    !| 構築した`vector_2d_type`を破棄する．
    !
    !成分の配列を解放し，base_gridとの関連を絶つ．
    subroutine destruct(this)
        implicit none
        class(vector_2d_type), intent(inout) :: this
            !! 破棄される`vector_2d_type`<br>
            !! 当該実体仮引数

        if (allocated(this%x)) deallocate (this%x)
        if (allocated(this%y)) deallocate (this%y)
        this%grid => null()
    end subroutine destruct

    !| 動的に割り付けられた`vector_2d_type`を始末する．
    !
    !成分の配列を解放し，base_gridとの関連を絶つ．
    subroutine finalize(this)
        implicit none
        type(vector_2d_type), intent(inout) :: this
            !! 破棄される`vector_2d_type`<br>
            !! 当該実体仮引数

        call this%destruct()
    end subroutine finalize

    !| 渡された格子を関連付けた，新しい`vector_2d_type`を返す．
    !
    !戻り値となる`vector_2d_type`に格子をbase_gridとして関連付けるが，
    !配列は割り付けない．
    !
    !割付は，[[grid_uniform_staggered_vars_vector_2d(module):initialize(function)]]
    !が行う．
    !
    !演算子指向的に`u = u .on. grid`と呼ぶことを目的に，
    !`.on.`2項演算子に関連付けられる．
    function associate_grid(this, grid) result(new_vec)
        implicit none
        !&<
        class(vector_2d_type)               , intent(in)            :: this
            !! 当該実体仮引数<br>
            !! 2項演算の都合上必要なだけで，特に何も使わない
        type(staggered_uniform_grid_2d_type), intent(in), target    :: grid
            !! `vector_2d_type`が定義される格子<br>
            !! **実体かつ`target`属性は必須**
        !&>
        type(vector_2d_type) :: new_vec
            !! 作成される新しい`vector_2d_type`<br>
            !! base_gridは関連付けられているが
            !! 成分の配列は割り付けられていない．

        ! 未使用警告を抑制
        if (same_type_as(this, new_vec)) continue

        new_vec%grid => grid ! `=`だと以降で実行時エラー
    end function associate_grid

    !| base_gridに基づいて配列を割り付けた新しい`vector_2d_type`を返す．
    !
    !演算子指向的に`u = .init. (u)`と呼ぶことを目的に，
    !`.init.`単項演算子に関連付けられる．
    function initialize(this) result(new_vec)
        implicit none

        class(vector_2d_type), intent(in) :: this
            !! 当該実体仮引数

        type(vector_2d_type) :: new_vec
            !! 作成される新しい`vector_2d_type`

        ! 構築を委譲
        call new_vec%construct_by_grid_pointer(this%grid)
    end function initialize

    !| ベクトル量が定義される格子へのポインタを返す．
    function get_base_grid(this) result(grid)
        implicit none

        class(vector_2d_type), intent(in) :: this
            !! 格子を取得するベクトル量<br>
            !! 当該実体仮引数

        type(staggered_uniform_grid_2d_type), pointer :: grid
            !! ベクトル量が定義されている格子へのポインタ

        grid => this%grid
    end function get_base_grid

    !------------------------------------------------------------------!
    !|`vector_2d_type`を代入する．
    !
    !単体で呼び出すことはなく，代入演算子をオーバーロードして利用する．
    subroutine assign(lhs, rhs)
        implicit none
        !&<
        class(vector_2d_type), intent(out) :: lhs
            !! 代入される`vector_2d_type`<br>
            !! `=`演算子の左側に置かれる量<br>
            !! 当該実体仮引数
        class(vector_2d_type), intent(in) :: rhs
            !! 代入する`vector_2d_type`<br>
            !! `=`演算子の右側に置かれる量
        !&>

        lhs%grid => rhs%grid
        lhs%x = rhs%x ! 自動割り付け
        lhs%y = rhs%y !
    end subroutine assign

    !| `vector_2d_type`同士の加算を計算し，
    !結果を`vector_2d_type`で返す．
    !
    !単体で呼び出すことはなく，加算演算子をオーバーロードして利用する．
    function add_vec(this, vec_r) result(new_vec)
        implicit none
        !&<
        class(vector_2d_type), intent(in) :: this
            !! `+`演算子の左側に置かれる量<br>
            !! 当該実体仮引数
        class(vector_2d_type), intent(in) :: vec_r
            !! `+`演算子の右側に置かれる量
        !&>
        class(vector_2d_type), allocatable :: new_vec
            !! 加算結果を格納する`vetor_2d_type`

        allocate (new_vec, source=this)
        new_vec%x(:, :) = this%x(:, :) + vec_r%x(:, :)
        new_vec%y(:, :) = this%y(:, :) + vec_r%y(:, :)
    end function add_vec

    !| `vector_2d_type`同士の減算を計算し，
    !結果を`vector_2d_type`で返す．
    !
    !単体で呼び出すことはなく，減算演算子をオーバーロードして利用する．
    function minus_vec(this, vec_r) result(new_vec)
        implicit none
        !&<
        class(vector_2d_type), intent(in) :: this
            !! `-`演算子の左側に置かれる量<br>
            !! 当該実体仮引数
        class(vector_2d_type), intent(in) :: vec_r
            !! `-`演算子の右側に置かれる量
        !&>
        class(vector_2d_type), allocatable :: new_vec
            !! 減算結果を格納する`vetor_2d_type`

        allocate (new_vec, source=this)
        new_vec%x(:, :) = this%x(:, :) - vec_r%x(:, :)
        new_vec%y(:, :) = this%y(:, :) - vec_r%y(:, :)
    end function minus_vec

    !| `vector_2d_type`の各成分に-1をかけた結果を
    !`vector_2d_type`で返す．
    !
    !単体で呼び出すことはなく，単項演算子をオーバーロードして利用する．
    function negate(this) result(new_vec)
        implicit none
        !&<
        class(vector_2d_type), intent(in) :: this
            !! -1がかけられる量<br>
            !! 当該実体仮引数
        !&>
        class(vector_2d_type), allocatable :: new_vec
            !! 単項演算を格納する`vetor_2d_type`

        allocate (new_vec, source=this)
        new_vec%x(:, :) = -1d0*new_vec%x(:, :)
        new_vec%y(:, :) = -1d0*new_vec%y(:, :)
    end function negate

    !| `vector_2d_type`と倍精度実数の乗算を計算し，
    !結果を`vector_2d_type`で返す．
    !
    !`vector_2d_type*real64`の形で計算する．
    !
    !単体で呼び出すことはなく，乗算演算子をオーバーロードして利用する．
    function multiply_r8(this, factor) result(new_vec)
        implicit none
        !&<
        class(vector_2d_type)   , intent(in) :: this
            !! `*`演算子の左側に置かれる量<br>
            !! 当該実体仮引数
        real(real64)            , intent(in) :: factor
            !! `*`演算子の右側に置かれる係数
        !&>
        type(vector_2d_type) :: new_vec
            !! 乗算結果を格納する`vetor_2d_type`

        call new_vec%construct(this%grid)
        new_vec%x = this%x*factor
        new_vec%y = this%y*factor
    end function multiply_r8

    !| `vector_2d_type`と倍精度実数の乗算を計算し，
    !結果を`vector_2d_type`で返す．
    !
    !`real64*vector_2d_type`の形で計算する．
    !
    !単体で呼び出すことはなく，乗算演算子をオーバーロードして利用する．
    function r_multiply_r8(factor, this) result(new_vec)
        implicit none
        !&<
        real(real64)            , intent(in) :: factor
           !! `*`演算子の左側に置かれる係数
        class(vector_2d_type)   , intent(in) :: this
            !! `*`演算子の右側に置かれる量<br>
            !! 当該実体仮引数
        !&>
        type(vector_2d_type) :: new_vec
            !! 乗算結果を格納する`vetor_2d_type`

        ! 乗算は左右の入れ替えが可能なので，
        ! 計算は[[grid_uniform_staggered_vars_vector_2d(module):multiply_r8(function)]]に委譲
        new_vec = this*factor
    end function r_multiply_r8

end module grid_uniform_staggered_vars_vector_2d

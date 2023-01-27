# FLOP - Operator-Oriented Fortran Library for Two-Dimensional Incompressible Fluid Flow Simulation

```Fortran
! solving top lid driven cavity flow containing objects represented by the volume penalization method

u = .init.(u .on. grid)
p = .init.(p .on. grid)

BC_u = BC_u .set. (Dirichlet([   0d0, 0d0]) .on. B1) &
            .set. (Dirichlet([   0d0, 0d0]) .on. B2) &
            .set. (Dirichlet([   0d0, 0d0]) .on. B3) &
            .set. (Dirichlet([U_wall, 0d0]) .on. B4)
BC_p = BC_p .set. (Neumann(0d0) .on. B1) &
            .set. (Neumann(0d0) .on. B2) &
            .set. (Neumann(0d0) .on. B3) &
            .set. (Neumann(0d0) .on. B4)

u_aux = (u + dt*(-((u.dot.nabla)*u) &        ! advection
                 + kvisc*.laplacian.u &      ! diffusion
                 + relct*(m.times.(u_s-u)) & ! penalization
                ) &
        ) .impose. BC_u
! ((u.dot.nabla)*u) can be rewritten in (.div. (u .times. u))

p = .inverse.((laplacian(p).with.BC_p) == (dens/dt*.div.u_aux))
! p = .inverse.(( &
!               (laplacian(p).with.BC_p) == (dens/dt*.div.u_aux)) &
!               .using. CG() &            ! selecting a matrix solver. SOR and Red-Black SOR are also available
!               .until. below_criterion & ! error tolerance configuration for iterative methods
!     )

u = (u_aux - dt/dens*.grad.p) .impose. BC_u
```

```Fortran
characteristics = characteristics .set. Reynolds_number(1000d0) &
                                  .set. kinetic_viscosity(Water%kinetic_viscosity) &
                                  .set. characteristic_velocity(U_wall)
l = characteristics.value.of_length

! discretize space and time
x = x .set. [0d0, l]
y = y .set. [0d0, l]
space = space .set. Cartesian([x, y])
grid = .divide.(space .into. cells([40, 40]))

t = t .set. [0d0, 50d0*l/U_wall]
stability_conditions = stability_conditions .set. Courant(grid.value.of_minimum_interval, U_wall, 0.1d0) &
                                            .set. Diffusion(grid.value.of_minimum_interval, kvisc, 0.5d0)
dt = .stabilize.(dt .by. stability_conditions)
delta_t = .divide.(t .into. intervals(dt))
relct = .stabilize. Reluctivity(delta_t.value.of_time_interval)
```

```Fortran
! input from npy file
m = input(m .in. npy .from. "mask")

! output to vtr file
call output(p .as. vtr .to. "p")
call output(u .as. vtr .to. "u")
```

**English documentations are in preparation.**

## 概要
FLOPは，非圧縮性流れの数値計算を実行する際に必要な手続を，演算子の形態で提供します．

Fortranはマルチパラダイム言語であり，手続型，オブジェクト指向型でのプログラミングが可能です．また，言語規格として並列計算の機能が定義されており，Fortranのみで共有メモリ，分散メモリ並列化が可能です．

ところで，あまり注目されていませんが，Fortranには他の言語ではあまり見られない独特の機能が備えられています．それが，ユーザ定義演算子です．Fortranでは，単項演算子や2項演算子を独自に定義できます．

例えば，Arjen Markusは，FortranCon 2020で[Experimental Programming in Fortran](https://tcevents.chem.uzh.ch/event/12/contributions/54/)と題した講演を行い，その中で移流拡散反応方程式の計算を，演算子を用いて下記のように記述しました．

```Fortran
decay = merge( degay0, 0.0, conc > 0.0 )
deriv = .div. (-flow * conc + disp * .grad. conc) - decay
conc = conc + deriv * deltt
```

非圧縮性流れの数値計算では，Navier-Stokes方程式だけでなく，速度の発散を0にするために圧力（スカラポテンシャル）のPoisson方程式の求解が必要になる場合があります．しかし，演算子を用いて連立方程式の求解を表現する方法は，これまで知られていませんでした．

FLOPでは，非圧縮性流れの数値計算における一連の手続を分解し，演算子として提供します．また，それだけではなく，スカラ量やベクトル量を表す型，境界条件を表現する型や条件を定めるための演算子も提供します．

## Usage
### 必須ソフトウェア
- Fortranコンパイラ
    - このライブラリは，gfortran 10.3.0, intel fortran 2021.1, nag fortran 7.1でビルド・実行確認されています．
- fpm
    - このライブラリは，fpm(fortran-lang/fpm)を用いてビルドされます．
- FORD (optional)
    - FORDを利用するとAPI辞彙が生成できます．
- VTKFortran
    - ファイルをVTR形式で出力するために使用されます．
    - 2次元配列の出力の対応，エラー処理のために，独自に拡張されたバージョンを用いています．
- Fortran-stdlib
    - npy形式での入出力のために使用されます．
    - `stdlib_io_npy_load.f90`内の`parse_header`の`intent(out)`属性を持つ引数に値を設定するようにしました．

### ソースの入手
ソースコードを入手するには，下記のコマンドをターミナルで実行します．

```console
git clone https://github.com/degawa/flop.git
cd flop
```

### fpmを用いたビルド
FLOPをビルドするには，flopディレクトリ内で下記のコマンドを実行します．

#### gfortran
```console
fpm build --profile debug --flag "-std=f2018 -cpp -D_R16P"
```
#### intel fortran
```console
fpm build --compiler ifort --profile debug --flag "/stand=f18 /fpp /D_R16P"
```
#### nag fortran
```console
fpm build --compiler nagfor --profile debug --flag "-f2018 -fpp"
```
NAG Fortranは4倍精度実数をサポートしていないので，プリプロセッサ識別子`_R16P`は指定しません．

### サンプルの実行
FLOPには，サンプルとしてキャビティ流れを計算するプログラムが添付されています．下記のコマンドを利用して，プログラムを実行します．

#### gfortran
```console
fpm run --profile debug --flag "-std=f2018 -cpp -D_R16P" --example cavity
```
#### intel fortran
```console
fpm run --compiler ifort --profile debug --flag "/stand=f18 /fpp /D_R16P"  --example cavity
```
#### nag fortran
```console
fpm run --compiler nagfor --profile debug --flag "-f2018 -fpp"  --example cavity
```

### API辞彙の生成
FORDを用いると，辞彙を生成できます.

```console
ford api-doc-ford-settings.md
```

### 他のfpmプロジェクトからの参照
他のfpmプロジェクトからFLOPを利用するには，参照したいプロジェクトの`fpm.toml`に下記の設定を追記します．

```toml
[dependencies]
flop = {git = "https://github.com/degawa/flop"}
```

また，FLOPを利用するソースに，次の一文を追加します．
```Fortran
use :: flop
```

### 物体表現
物体表現にはVolume Penalization法を用いており，マスク関数と名付けられたスカラ量の値に基づいて，物体と流体を識別します．マスク関数の値が1の箇所を物体，0の箇所を流体と見なします．0より大きく1未満の場合は，物体と流体が混在していると見なします．

example/cavity_vpでは，npy形式のファイルから値を読み込みます．実行ディレクトリに応じて，適切な場所にコピーしてください．
実行結果は下記のようになります．

![](example/cavity_vp/cavity_vp.png)

## Issue
- [ ] 境界の値が一定値ではない（例えば流入速度がPoieuille分布になっている）場合への対応
- [ ] 境界の値が式（例えば対流流出条件）で記述される場合の対応
- [ ] 圧力のDirichlet境界条件の対応
    - 格子点や格子辺中点に補間値として与えられる場合
    - 境界となる格子や仮想格子の中点に値が与えられる場合
- [ ] 実行速度の改善
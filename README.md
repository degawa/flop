# FLOP - Operator-Oriented Fortran Library for Two-Dimensional Incompressible Fluid Flow Simulation

```Fortran
! solving top lid driven cavity flow

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

u_aux = (u + dt*(-(.l.(u.dot.nabla).r.u) + kvisc*.laplacian.u)) &
        .impose. BC_u

! (.l.(u.dot.nabla).r.u) can be rewritten in (.div. (u .times. u))

p = .inverse.(( &
              (laplacian(p).with.BC_p) .results. (dens/dt*.div.u_aux)) &
            .until. below_criterion &
    )

u = (u_aux - dt/dens*.grad.p) .impose. BC_u
```

```Fortran
! discretize space and time
x = x .set. [0d0, l]
y = y .set. [0d0, l]
space = space .set. Cartesian([x, y])
grid = .divide.space .into. cells([40, 40])

t = t .set. [0d0, 50d0*l/U_wall]
delta_t = .divide.t .into. intervals(dt)
```

```Fortran
! output to csv file
call output((p.as.csv) .to. unit("p.txt"))
call output((u.as.csv) .to. unit("u.txt"))

close (unit("p.txt"))
close (unit("u.txt"))
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
fpm build --profile debug --flag "-std=f2018"
```
#### intel fortran
```console
fpm build --compiler ifort --profile debug --flag "/stand=f18"
```
#### nag fortran
```console
fpm build --compiler nagfor --profile debug --flag "-f2018"
```

### サンプルの実行
FLOPには，サンプルとしてキャビティ流れを計算するプログラムが添付されています．下記のコマンドを利用して，プログラムを実行します．

#### gfortran
```console
fpm run --profile debug --flag "-std=f2018" --example cavity
```
#### intel fortran
```console
fpm run --compiler ifort --profile debug --flag "/stand=f18"  --example cavity
```
#### nag fortran
```console
fpm run --compiler nagfor --profile debug --flag "-f2018"  --example cavity
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

## 提供される機能
上記の例を用いて，FLOPが提供する機能を説明します．

### ユーザ定義型
いくつかのユーザ定義型が提供されています．

- ベクトル量（`u, u_aux`）
- スカラ量（`p`）
- 格子（`grid`）
- ベクトル量に対する全体境界条件 (`BC_u`)
- スカラ量に対する全体境界条件 (`BC_p`)
- 境界場所型（`B1, B2, B3, B4`）
- ナブラ演算子型（`nabla`）
    - 何もしない擬似的な型

明示的に実体として用いられているのは上記のユーザ定義型ですが，演算の戻り値として利用されるユーザ定義型が定義されています．

- テンソル量（`u .times. u`の返値）
- 非線形演算子型（`u.dot.nabla`の返値）
- 連立方程式の左辺型（`laplacian(p)`の返値）
- 連立方程式型（`(laplacian(p).with.BC_p) .results. (dens/dt*.div.u_aux)`の返値）
- 境界値型（`Dirichlet()`, `Neumann()`の返値）
- 境界条件型（`境界値型 .on. 境界場所型`の返値）
- 出力型（`物理量 .as. 書式`の返値）

### ユーザ定義演算子
#### 単項演算子
- `.grad.`: スカラ，ベクトル量の勾配を計算する．
    - `.grad. scalar -> vector`
    - `.grad. vector -> tensor`
- `.div.`: スカラ，ベクトル量の発散を計算する．
    - `.div. vector -> scalar`
    - `.div. tensor -> vector`
- `.laplacian.`: : スカラ，ベクトル量のラプラシアンを計算する．
    - `.laplacian. scalar -> scalar`
    - `.laplacian. vector -> vector`

#### 2項演算子
- `.times.`: ベクトル量同士のテンソル積を計算する．
    - `vector .times. vector -> tensor`

#### 独自の演算子
##### 型束縛された演算子
- `.on.`: スカラ量，ベクトル量に格子を関連付ける
- `.init.`: 関連付けられた格子に基づいて，スカラ量，ベクトル量の成分の配列を割り付ける．

##### 境界条件に関係する演算子
- `.on.`: 境界条件の値（`Dirichlet()`, `Neumann()`）と組み合わせて，境界条件型を生成する．
- `.set.`: 境界条件型の情報を，全体境界条件型に代入する．
- `.impose.`: スカラ量，ベクトル量に全体境界条件型のDirichlet境界条件を適用する．
- `.with.`: 連立方程式の左辺型に，全体境界条件型の情報を代入する．

##### 連立方程式に関係する演算子
- `.results.`: 連立方程式の左辺型とスカラ量を組み合わせて，連立方程式型を作成する．
- `.until.`: 連立方程式型に，連立方程式を反復法で解く場合の収束判定条件の値を代入する．
- `.inverse.`: 連立方程式型の型束縛手続きを呼び出して連立方程式を解く．

##### 移流項の計算に関係する演算子
- `.dot.`: 演算子左側のベクトル量（移流速度）を取り込んだ非線形演算子型を作成する．
- `.l.`: 左括弧演算子．非線形演算子型を受け取り，そのまま返す単項演算子．
- `.r.`: 右括弧演算子．非線形演算子型とベクトル量（保存量としての速度）を引数として，非線形演算子型の型束縛手続きを呼び出して移流項を計算する2項演算子．

##### ファイル出力に関係する演算子
- `.as.`: 演算子右側に指定した書式で物理量を出力する出力型を作成する．
- `.to.`: 出力型に出力装置番号を渡す．

## Issue
- [ ] 時空間情報の設定を行う演算子の定義
- [ ] 境界の値が一定値ではない（例えば流入速度がPoieuille分布になっている）場合への対応
- [ ] 境界の値が式（例えば対流流出条件）で記述される場合の対応
- [ ] 圧力のDirichlet境界条件の対応
    - 格子点や格子辺中点に補間値として与えられる場合
    - 境界となる格子や仮想格子の中点に値が与えられる場合
- [ ] 実行速度の改善
# Common Lisp の package について

## 編集履歴

- 2020-10-09: いくつかの概念について誤解があったので, 修正
- 2020-09-27

## はじめに

この記事では, プログラミング言語Common Lispのpackageと呼ばれる概念について書きます.

人に向けて書いている風の備忘録なので悪しからず.


- Common Lispでのプログラミングを始めたがpackageの使い方が分かっていない
- Common Lispのpackageという概念について知りたい

というような人向けになっています. 処理系や仕様としての実装の詳細には立ち入らない予定です.

また, asdfを用いた依存関係の解決などについてもここでは触れません.
(別のページに書く可能性はあります.

また, この記事を書くに当たり用いている処理系は, SBCL 2.0.6です.

## 名前空間

プログラミング言語一般の話として, 名前空間と呼ばれる概念があります.

これはコード中で用いられるある名前(コンパイラやインタプリタが読み取る文字列)に対して,
その名前が何を指しているのかという対応付けのことです.

複数の名前空間を扱えるプログラミング言語では, この対応付け(名前空間)を分けることで,
不必要に冗長な命名規則(いつでもどこでも接頭辞が付くような書き方など)を除きつつ,
名前の衝突を避けることが出来ます.

ここで名前の衝突というのは, プログラムが扱う別々の対象について, ある同じ名前がつけられてしまうことを言います.

この時, 名前から一意にプログラムが扱う対象を引くことができなくなるため, 困ります.
例えば, 意図せず関数が再定義されたり, プログラムがクラッシュしてしまうことがあるかもしれません.

多くのプログラミング言語ではこれを回避するために, 再定義に関するエラーを出したり,
複数の名前空間を持つことで一見同じ名前であっても, 名前空間Aのfooはbarを指して名前空間Bのfooはbazを指すというようなことが出来るようになっています.

## symbolとpackage

Common Lispで, 読み取り機がコードを読み取って, コードの表現を値にするまで, 例えば,

```lisp
CL-USER> pi
3.141592653589793d0
```

これは, piという名前から値`3.141592653589793d0`を引いています. これを理解するためには,

- piという文字列を読み取ってsymbol `PI`を引く
- symbol `PI`から値`3.141592653589793d0`を引く

という2つの対応付けのことを考えないといけません.

この文章のテーマであるpackageというのは, 1つ目の対応付け(コード上の文字列からsymbolを引くということ)に関連します.

(2つ目の対応付けについては, ざっくりとした言い方をすると, Common Lispでは関数名や変数名がsymbolで,
symbolというオブジェクト(データ)が関数や変数を指し示すためのものとなります.)

symbolとpackageの関係からsymbolに次の3つの状態があると考えると分かりやすいです.

- symbolが参照出来る. (A symbol can be referred to.)
- あるpackage内でsymbolがアクセス出来る. (A symbol is accessible in a package.)
- あるpackageにsymbolが存在している. (A symbol is present in a package.)

参照できるというのは, その名の通りあるsymbolを参照する方法がある状態であり,
メモリ上のどこかにそのsymbolがあるという程度のことで, これだけではあまりpackageとは関係がありません.

どのようにしてsymbolを参照できるのかということに, packageが関わってきます.

## current package

packageとsymbolの関係の前に, current packageについて説明しておきます.

ある時点で, 一つのpackageがcurrentです. このpackageをcurrent packageと呼びます.
これは, `*package*`変数を評価した時の値です.

あるpackage内でsymbolがアクセス出来る.
と上で書きましたが, これは, そのあるpackageがcurrent packageである時にpackage prefixなしにsymbolを参照できることを指します.
(package prefixについては後述します.)

symbolをpackageに存在させることを, internすると言います.

symbolが最初にinternされるpackageを, symbolのhome packageと呼びます.
symbolはこのhome packageの情報を持ちます.

home packageは`symbol-package`で確認できます.

```lisp
CL-USER> (symbol-package 'pi)
#<PACKAGE "COMMON-LISP">
```

実は, home packageつまり`symbol-package`で確認できるpackageを変更することが出来ますが,
ここではそのことは考えません.

symbolは複数のpackageにinternされますが, symbolのhome packageは高々一つです.

また,　symbolが最初にinternされると, packageにsymbolが存在する(is present)状態になります.

これが上で挙げた3つ目の状態(あるpackageにsymbolが存在している)です.

また, 言い換えになりますが, symbolがアクセス可能なpackageでは, symbolは名前(これはコード上の文字列です)で一意に識別出来ます.

## package prefix

変数`PI`の例を見てみます.

```lisp
CL-USER> pi
3.141592653589793d0
CL-USER> cl:pi
3.141592653589793d0
CL-USER> cl::pi
3.141592653589793d0
CL-USER> cl-user::pi
3.141592653589793d0
```

これら, `pi`, `cl:pi`, `cl::pi`, `cl-user::pi`は, 全て同じsymbolを指しています.

`pi`の前についている, `cl:`や`cl-user::`等がpackage prefixの例になります.

packageを表す文字列の後に, コロンが1つあるいは2つが続いて, その後にsymbolの名前を表す文字列が続きます.

コロンの数による違いは, 指定するpackageにおいて,
symbolがinternalなのかexternalなのかによって動作が変わります.

なお, このコロンのことをpackage markerと呼びます.

## internal と external

symbolがあるpackageでアクセス出来るということを話しました.

アクセスできるsymbolは2つの観点からそれぞれ, 2種類の状態があります.

1つ目の2種類は, 次の２つです.

- internal
- external

これは, symbolがexternalかどうかという観点です.

あるpackageにおいてsymbolがexternalであるというのは, そのsymbolが, そのpackageが外部に提供するインターフェースの一つであることを指します.

上で書いた`PI`の例で, `cl:pi`とclというパッケージ名の後にはコロンが一つでしたが, これはCOMMON-LISP packageにおいてsymbol `PI`がexternalであるからです.

externalなsymbolのみが, パッケージ名を表す文字列のあとにコロン1つで参照できます.

internalなsymbolでも, パッケージ名を表す文字列のあとにコロン2つで参照できますが, コロン1つでは参照できません.
externalなsymbolはコロン2つでも参照できます.

current packageであるsymbolがinternalなのかexternalなのかということは, `intern`の第一引数にsymbolの名前, 第二引数を指定しないとすると確認することが出来ます.

```lisp
CL-USER> (in-package :cl)
#<PACKAGE "COMMON-LISP">
CL> (intern "PI")
PI
:EXTERNAL
```

`in-package`はcurrent packageを切り替えます. また先程から登場しているcl(CL)というのはCOMMON-LISP packageのニックネームです.
(ニックネーム機能については後で紹介します.)

既にinternされているsymbolの名前を`intern`に渡してやると, そのsymbolとsymbolがinternalかexternalかということを返します.

`:INTERNAL`あるいは`:INHERITED`が返ってくる場合は, current packageにおいて, そのsymbolはcurrent packageでinternalです.
`:EXTERNAL`が返ってくる場合は, current packageにおいて, そのsymbolはcurrent packageでexternalです.

inheritedについては, 後で説明する意味がありますが,  このやり方でinheritedと返ってきたときには, そのsymbolはcurrent packageでinternalであることも意味します.

```lisp
CL-USER> (INTERN "PI")
PI
:INHERITED
CL-USER> (INTERN "PI" :cl)
PI
:EXTERNAL
```

このように第2引数を指定すると, current package以外のpackageでの扱いについて確認することも出来ます.

## present と inherited

2つ目の観点はpackageに存在するsymbolかどうかです.

あるpackageでアクセス可能なsymbolは,

- present
- inherited

のどちらかです. 

あるpackageでアクセス出来るけれども, inheritedというのは別のpackageにsymbolが存在するという状態です.

これの例は, COMMON-LISP-USER packageにおける`PI`です.

```lisp
CL-USER> *package*
#<PACKAGE "COMMON-LISP-USER">
CL-USER> (intern "PI")
PI
:INHERITED
```

current packageがCOMMON-LISP-USERであって, package prefixなしに参照は出来ますが,
`intern`を用いると2つ目の返り値が`:INHERITED`となっています.
package prefixなしに参照できる, つまりaccessibleではありますが,
このsymbolはCOMMON-LISP-USERというpackageには存在していません.

あとで紹介する`use-package`等によって, package(の持つ名前とsymbolの対応)間に継承関係を設定できます.

`PI`の例場合, package CL-USERがpackage CLを使っているという状態にあって,
CLがCL-USERのuse listというものに含まれています.

symbol `PI`はuse listを通してpackage CLから探されます.

## defpackage

symbolとpackageの関係性をざっと説明しました.
これらをどのように制御するのかということが, Common Lispにおけるpackage管理になります.

とはいえ, packageを作らないことには始まりません.
`defpackage`を使ってpackageを作ることが出来ます.

以下のようにすると名前が`FOO`であるpackageが作成されます.

```lisp
CL-USER> (defpackage :foo)
#<PACKAGE "FOO">
```

`defpackage`の第一引数はpackageの名前となるもので,
文字列, 文字, シンボルを使うことが出来ます. これらはstring designator(文字列指定子)と呼ばれます.

また, ここでは使えませんが, これらにpackage自身を合わせたpackageを指定するもののことを, package designator(パッケージ指定子)と呼びます.

`defpackage`においてpackageやsymbolの名前を表すために, 
`#`付きのkeyword symbolを用いることがありますが,
ここではその流儀を採用しません.
(もし気になる方は, 何故そのような流儀があるかを調べてみてください.)

packageの名前は以下のように`package-name`で調べることが出来ます.
packageはそれ自体objectですが, これを取得するためには`find-package`を用います.

```lisp
CL-USER> (defpackage :foo)
#<PACKAGE "FOO">
CL-USER> (find-package :foo)
#<PACKAGE "FOO">
CL-USER> (package-name (find-package :foo))
"FOO"
```

## in-package

current packageを変更するには, `in-package`を使います.

```lisp
CL-USER> (defpackage :foo)
#<PACKAGE "FOO">
CL-USER> (in-package :foo)
#<COMMON-LISP:PACKAGE "FOO">
FOO> cl:*package*
#<COMMON-LISP:PACKAGE "FOO">
```

SLIMEなどのREPLでは, これまで示してきたようにプロンプトにcurrent packageの名が表示されます.

COMMON-LISP-USERではなくCL-USERと表示されていますが, これはnicknameです.

nicknameを確認するには, `package-nicknames`を使います.

```lisp
CL-USER> (package-nicknames *package*)
("CL-USER")
```

ここでcurrent packageがpackage FOOの時に, `cl:*package*`とpackage prefixをつけたのは,
このsymbolがpackage FOOでアクセス可能ではないからです.

`defpackage`でpackageを作成する時に, どんなsymbolがaccessibleになるかということは実装依存ですが,
ここで用いている処理系(SBCL)ではどのsymbolもaccessibleになりません.

## intern

symbolをpackageにinternしてみます. `cl:intern`を使ってみましょう.

```lisp
CL-USER> (defpackage :foo)
#<PACKAGE "FOO">
CL-USER> (in-package :foo)
#<COMMON-LISP:PACKAGE "FOO">
FOO> (cl:intern "HOGE")
HOGE
COMMON-LISP:NIL
FOO> (cl:intern "HOGE")
HOGE
:INTERNAL
```

このように, その名前を持つsymbolがinternされていない時に, `cl:intern`を(第2引数を省略して)使うと,
symbolが作成され, current packageにその名前を持つsymbolがinternされます.

この場合は, symbol `foo::hoge`がpackage FOOにinternされます.

しかし, もっと実用的に出てくるものとして, 関数定義でもsymbolはinternされます.

`cl:defun`を使って関数を定義してみます.

```lisp
FOO> (cl:defun piyo () 42)
PIYO
FOO> (cl:intern "PIYO")
PIYO
:INTERNAL
```

この例ですと, 関数名になるsymbol `foo::piyo`がcurrent package (package FOO)にinternされます. 

その実, symbolは読み取られるとcurrent packageにinternされます.

```lisp
FOO> 'fuga
FUGA
FOO> (cl:intern "FUGA")
FUGA
:INTERNAL
```

## export

symbolはあるpackageに対してinternalとexternalという２つの種類の状態があると先に紹介しましたが,
直前のinternの説明でsymbolがpackageにinternされてinternal symbolになるところは確認できました.

これらをexternalにするには, `cl:export`を使います.

```lisp
FOO> (cl:export 'hoge)
COMMON-LISP:T
FOO> (cl:intern "HOGE")
HOGE
:EXTERNAL
```

exportの第一引数にはsymbolのlistを渡すことも出来ます.

```lisp
FOO> (cl:export '(piyo fuga))
COMMON-LISP:T
FOO> (cl:intern "PIYO")
PIYO
:EXTERNAL
FOO> (cl:intern "FUGA")
FUGA
:EXTERNAL
```

先に書いたとおり, external symbolはコロンが一つのpackage prefixで参照出来ます.

```lisp
FOO> (cl:in-package :cl-user)
#<PACKAGE "COMMON-LISP-USER">
CL-USER> (foo:piyo)
42
CL-USER> 'foo:hoge
FOO:HOGE
```

## use-package

先にも`PI`の例について書きましたが,
package CL-USERでは`in-package`や`defun`をpackage prefixなしに使っています.
つまり, CL-USERで`cl:defun`や`cl:in-package`がアクセス可能ということです.

`cl:use-package`を使うと, あるpackageのexternal symbolを全てアクセス可能にすることが出来ます.

```lisp
CL-USER> (in-package :foo)
#<COMMON-LISP:PACKAGE "FOO">
FOO> (cl:use-package :cl)
T
FOO> (in-package :cl-user)
#<PACKAGE "COMMON-LISP-USER">
CL-USER> 
```

上の例では, package FOOに, package CL(package COMMON-LISP)のexternal symbol全てがinternされます.
そのため`in-package`がpackage prefixなしで参照出来るようになりました.

先にも書きましたが, このようにして,アクセス可能になったsymbolはinheritedであるといいます.

これをpackage FOOがpackage CLを使うと言います.

使っているpackageはpackage-use-listで確認できます.

```lisp
FOO> (package-use-list :foo)
(#<PACKAGE "COMMON-LISP">)
```

Common Lisp処理系を立ち上げると, current packageはpackage COMMON-LISP-USERになっています.
package COMMON-LISP-USERは, package COMMON-LISPを使っています.

## shadow

あるpackageのexternal symbolを別のpackageでアクセス可能にする方法について, `use-package`を紹介しました.

この方法では, external symbolを全てアクセス可能にします.

これだけでは, あるpackageを使いたいけれども, そのpackageのexternal symbol全てが必要ではないという場合.
特に, あるsymbolについて同じ名前のsymbolを使いたい時に困ってしまいます.
具体的には, 同じ名前の別の関数を定義したい場合などです.

これまでの例を一度忘れて, package FOO内で関数barを定義したとします.

```lisp
CL-USER> (defpackage :foo)
#<PACKAGE "FOO">
CL-USER> (in-package :foo)
#<COMMON-LISP:PACKAGE "FOO">
FOO> (cl:use-package :cl)
T
FOO> (defun bar () 1)
BAR
FOO> (export 'bar)
T
```

この状態で, current packageをpackage CL-USERに戻して, package FOOを使うことにします.

```lisp
FOO> (in-package :cl-user)
#<PACKAGE "COMMON-LISP-USER">
CL-USER> (use-package :foo)
T
CL-USER> (bar)
1
```

この時, 関数`bar`を再定義すると, それはsymbol `foo:bar`に結び付けられてしまします.

```lisp
CL-USER> (defun bar () 2)
WARNING: redefining FOO:BAR in DEFUN
BAR
CL-USER> (bar)
2
CL-USER> (in-package :foo)
#<PACKAGE "FOO">
FOO> (bar)
2
```

わざとこのようにしたいのならともかくとしても,
これではpackage FOOに依存している別のpackageにまで影響を及ぼしてしまいます.

逆に, 何らかのpackageを使う際に,
既にinternされているsymbolと同じ名前のものを取り込もうとするとエラーとなります.

```lisp
CL-USER> (defpackage :hoge)
#<PACKAGE "HOGE">
CL-USER> (in-package :hoge)
#<COMMON-LISP:PACKAGE "HOGE">
HOGE> (cl:intern "BAR")
BAR
COMMON-LISP:NIL
HOGE> (cl:export 'bar)
COMMON-LISP:T
HOGE> (cl:in-package :cl-user)
#<PACKAGE "COMMON-LISP-USER">
CL-USER> (use-package :hoge)
; ここで以下のエラーが出ます.
; USE-PACKAGE #<PACKAGE "HOGE"> causes name-conflicts in
; #<PACKAGE "COMMON-LISP-USER"> between the following symbols:
;   HOGE:BAR, FOO:BAR
;    [Condition of type NAME-CONFLICT]
```

前者の場合,
package CL-USER内でbarがpackage FOOに存在する`foo:bar`を指すということが問題なので,
代わりに, package CL-USERに存在するsymbol `bar`を指すように出来ればうまく行きます.

このような時には, `shadow`を使います.

```lisp
CL-USER> (defpackage :foo)
#<PACKAGE "FOO">
CL-USER> (in-package :foo)
#<COMMON-LISP:PACKAGE "FOO">
FOO> (cl:use-package :cl)
T
FOO> (defun bar () 1)
BAR
FOO> (export 'bar)
T
FOO> (in-package :cl-user)
#<PACKAGE "COMMON-LISP-USER">
CL-USER> (use-package :foo)
T
CL-USER> (shadow 'bar)
T
CL-USER> (defun bar () 2)
BAR
CL-USER> (bar)
2
CL-USER> (symbol-package 'bar)
#<PACKAGE "COMMON-LISP-USER">
CL-USER> (in-package :foo)
#<PACKAGE "FOO">
FOO> (bar)
1
FOO> (symbol-package 'bar)
#<PACKAGE "FOO">
```

`shadow`は,
第1引数に与えられた名前を持つsymbolが,
第2引数で指定するpackage内に存在することを保証する状態にするものです.
第2引数を省略した場合にはcurrent packageになります.

第1引数には名前を指定しますが, 文字列以外にも文字列指定子を用いることが出来ます.
(この場合は先に書いたpackageの名前ではなくsymbolの名前として働く.)
文字列で指定すると大文字小文字のことを考えないといけませんが,
例のように, symbolを使うとそのsymbolの普段のコード中の表現を使えるので便利です.

上の場合, symbol `bar`がpackage CL-USERに存在することになるので, `(defun bar () ...)`で`bar`が,
package FOOに存在する`bar`ではなくて, `cl-user::bar`を指すことになります.

これは, package CL-USERにおいて`cl-user::bar`が, 他のbarという名前を持つsymbolをまるで隠してしまうように振る舞います.
(メモ: 実際の専門用語としてshadowingなのかmaskingなのか分かってないので, あとで調べる.)

## import

あるpackageに別のpackageに存在するsymbolをアクセス可能にする方法として, `use-package`を紹介しましたが,
これでは, external symbolが全てアクセス可能にされてしまいます.

特定のsymbolのみ, あるいはinternal symbolをアクセス可能にしたい場合には, `import`を使います.

例を考えます.

```lisp
CL-USER> (defpackage :foo)
#<PACKAGE "FOO">
CL-USER> (in-package :foo)
#<COMMON-LISP:PACKAGE "FOO">
FOO> (cl:use-package :cl)
T
FOO> (defun hoge () 0)
HOGE
FOO> (defun piyo () 1)
PIYO
FOO> (defun fuga () 2)
FUGA
FOO> (export '(hoge piyo))
T
FOO> (in-package :cl-user)
#<PACKAGE "COMMON-LISP-USER">
CL-USER> 
```

package FOOを作って, そこに`foo:hoge`, `foo:piyo`, `foo::fuga`が存在しています.
`foo:hoge`, `foo:piyo`はexternalです.

`use-package`を使うと, externalである`foo:hoge`も`foo:piyo`もpackage CL-USERでアクセス可能になります.

`foo:piyo`だけアクセス可能にしたい場合は, `import`を使います.

```lisp
CL-USER> (import 'foo:piyo)
T
CL-USER> (piyo)
1
```

current packageがCL-USERである時に, `'piyo`だと`CL-USER::PIYO`を指すので注意してください.

externalではないsymbolも同様にアクセス可能にすることが出来ます.

```lisp
CL-USER> (import 'foo::fuga) ; package FOOでfugaはinternalなのでコロンは2つ
T
CL-USER> (fuga)
2
```

また, `import`ではアクセス可能になるsymbolはinheritedではなくinternされます.

## shadowing-import

`shadow`はある名前のsymbolが指定するpackage(あるいはcurrent package)に存在することを保証しました.

あるpackageで別のpackageのsymbolがアクセス可能であることを保証したい場合もあります.

例えば, ２つのpackageのexternal symbolを全てアクセス可能にする(2つのpackageをuseする)ことを考えて,
そのうちいくつかの名前が重複しているような場合にどちらか一方のsymbolのみを選択して,
アクセス可能にしたい時等です.

どちらのpackageのexternal symbolも数個程度なら`import`を使うことも出来ますが,
重複しているsymbolは数個なのに, external symbolは100個以上となると少々つらいです.

そのような時には, `shadowing-import`を用います.

先程の`import`の例の続きを考えます.

```lisp
CL-USER> (shadowing-import 'foo:hoge)
T
CL-USER> (hoge)
0
CL-USER> (defpackage :bar)
#<PACKAGE "BAR">
CL-USER> (in-package :bar)
#<COMMON-LISP:PACKAGE "BAR">
BAR> (cl:use-package :cl)
T
BAR> (defun hoge () 3)
HOGE
BAR> (export 'hoge)
T
BAR> (in-package :cl-user)
#<PACKAGE "COMMON-LISP-USER">
CL-USER> (use-package :bar)
T
CL-USER> (hoge)
0
```

`foo:hoge`を`shadowing-import`します.
この状態で, 別のpackage(ここではBAR)で同名のsymbolをexternalにして, そのpackageを使います.

HOGEという名前が重複するのですが,
この状態ではpackage CL-USERでHOGEという名前は`foo:hoge`を示すことが決まった状態になっているので,
衝突のエラーは出ずに, HOGEという名前で`bar:hoge`は参照できず, `foo:hoge`にアクセスできます.

`shadowing-import`でも`import`と同様にsymbolはinternされます.

## defpackage

そのうち書く.

## 謝辞

- [hyotang666](https://github.com/hyotang666)さん `use-package`に関する処理系依存の挙動について指摘をくださり, ありがとうございます.
- [t-sin](https://github.com/t-sin)さん internとinheritedについての私の誤解について指摘くださり, ありがとうございます.

## 参考文献

- [CLHS 11.1.1 Introduction to Packages](http://www.lispworks.com/documentation/HyperSpec/Body/11_aa.htm)
- [CLHS System Class SYMBOL](http://www.lispworks.com/documentation/HyperSpec/Body/t_symbol.htm#symbol)
- <http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_a.htm#accessible>
- <http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_i.htm#interned>
- <http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_p.htm#present>
- <http://www.lispworks.com/documentation/HyperSpec/Body/11_aab.htm>
- <http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_c.htm#current_package>
- <http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_p.htm#package_prefix>
- <http://www.lispworks.com/documentation/HyperSpec/Body/f_shadow.htm>
- <http://www.lispworks.com/documentation/HyperSpec/Body/m_defpkg.htm>
- <http://www.lispworks.com/documentation/HyperSpec/Body/11_abb.htm>
- [実践Common Lisp](https://ci.nii.ac.jp/ncid/BA86732786)

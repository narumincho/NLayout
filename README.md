# Definyのための新しいレイアウト記述ルール

Definyは理想の言語。今作っている。言語仕様のほんの一部は https://weblike-narumincho.ssl-lolipop.jp/Definy/spec.html で。

Definyのエディタを作るときに、CSSの仕様はやけに複雑で統一感がなくていまいちだということを悟り、CSSに代わる新しいレイアウト記述のルールを作ることにした。
レンダリングはいまのところ「SVG」でやっている。

理由は
- Canvasと違って、ベクタ形式できれいに表示される
- HTMLのdiv要素とCSSのコラボと違って、たいていどのブラウザでも同じように表示される
- テキストを選択でき、アクセシビリティに優れている (もしかしたら読み上げたりしてくれるかもしれない)

でも欠点があって、自前で座標を計算しているだけあってウィンドウサイズを変えたりすると重くなる。そこはなんとかしたい。
簡単な構造だったらHTMLのdiv&CSSで自前で座標を計算しないようにするか。でもゆーて、そんなに表示サイズ変えないだろうという気もする。

ここにきて文字列処理がうまくいけるかどうか不安になった。div&CSSでやるのが今のところ正攻法か……
一旦保留、また帰ってくる。

追記
やはり縦横伸びるか伸びないかで型を分けるのは不便だと感じた。Elmの型システムがまだ弱いからかも知れないが、縦横のリストの表現が面倒。どう強くするのかもいまいち分からない。DefinyのElmのUiモジュールでDefinyのUIを作りながらDefiny標準UIプロジェクトを作っていく。

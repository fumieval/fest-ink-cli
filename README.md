[イカフェスレート](https://fest.ink)のAPIを利用し、コマンドラインでSplatoonの[フェス](http://www.nintendo.co.jp/wiiu/agmj/festival/)に関する情報を取得する。

インストール
----

* Haskellのビルドツール[stack](https://github.com/commercialhaskell/stack/wiki/Downloads)をダウンロードし、インストールする。
* 以下のコマンドを実行する。

```
$ git clone https://github.com/fumieval/fest-ink-cli.git
$ stack setup
$ stack install
```

使い方
-----

`fest-ink index`でフェスの一覧を取得する。<>で囲まれているのは未来の、[]は進行中のフェスを表す。

```
$ fest-ink index
<5>     ボケ vs ツッコミ
 4      キリギリス vs アリ
 3      レモンティー vs ミルクティー
 2      赤いきつね vs 緑のたぬき
 1      ごはん vs パン
```

`fest-ink (序数)`で特定のフェスににおける勝率を表示する。

```
$ fest-ink.exe 4
キリギリス      0.52
アリ            0.48
```

リンク
----

* [イカフェスレート | API](https://fest.ink/api)

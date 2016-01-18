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

`fest-ink index`でフェスの一覧を取得する。

```
$ fest-ink
10  (Upcoming)                      カンペキなカラダ vs カンペキな頭脳
9   (Winner: 緑のたぬき)            第2回 赤いきつね vs 緑のたぬき
8   (Winner: 海の幸)                山の幸 vs 海の幸
7   (Winner: 愛)                    愛 vs おカネ
6   (Winner: イカ)                  イカ vs タコ
5   (Winner: ツッコミ)              ボケ vs ツッコミ
4   (Winner: キリギリス)            キリギリス vs アリ
3   (Winner: ミルクティー)          レモンティー vs ミルクティー
2   (Winner: 赤いきつね)            赤いきつね vs 緑のたぬき
1   (Winner: ごはん)                ごはん vs パン
```

`fest-ink (序数)`で特定のフェスににおける勝率を表示する。

```
$ fest-ink.exe 9
第2回 赤いきつね vs 緑のたぬき (Concluded)
From: 2015-12-26T09:00:00+09:00
To:   2015-12-27T09:00:00+09:00
赤いきつね 7364 (44%)
緑のたぬき 9463 (56%)
```

リンク
----

* [イカフェスレート | API](https://fest.ink/api)

# 概要
これは、細川敦司の卒業研究で用いたオンライン実験システムです。このWebシステムでは、他の参加者またはACT-Rモデルを相手にカードゲームをプレイできます。  
知り合いの研究者の方と研究室の先輩の実験システムを参考に作成しました。

## ACT-Rとは
ACT-R (Adaptive Control of Thought-Rational) とは、カーネギーメロン大学の John R. Anderson が中心となって開発した認知アーキテクチャです。これは、認知モデルを構築するためのフレームワークです。詳しくは次のサイトをご覧ください。  
http://act-r.psy.cmu.edu  
認知モデルとは、特定のタスクを遂行する人間の認知プロセスを再現するプログラムであり、このシステムにはカードゲームをプレイする認知モデルがいくつか組み込まれています。これらの認知モデルはACT-Rに基づいて構築されており、Lisp処理系で実行されます。  
## ACT-RのHTMLインタフェース
ACT-Rには、モデルが遂行しているタスクの環境をWebブラウザに表示するHTMLインタフェースが含まれています。これを用いてゲームのユーザインタフェースを作成しました。

# 設計
* サーバ
  * Node.js
    * Socket.io
  * ACT-R
* クライアント
  * ACT-RのHTMLインタフェース
* データベース
  * MySQL
  * SQLite

# 導入

## Node.js
次のページからダウンロード・インストールし、PATHを通す。  
https://nodejs.org

## Steel Bank Common Lisp (SBCL) & QuickLisp

次のページからバージョン1.4.14をダウンロードし、インストールする。  
https://sourceforge.net/projects/sbcl/files/sbcl/1.4.14/  
コマンド名を `sbcl` から `sbcl1.4` に変更し、PATHを通す。

次のページから `quicklisp.lisp` をダウンロードする。  
https://www.quicklisp.org  
コマンド `sbcl1.4` でSBCLを起動し、次のLispコードを実行する。
```lisp
* (load "(ファイルの場所)/quicklisp.lisp")
* (quicklisp-quickstart:install)
* (ql:add-to-init-file)
* (exit)
```
これで、QuickLispがインストールされる。

文字コードをUTF-8とするために、SBCLの初期設定ファイル `~/.sbclrc` に次のコードを記入する。
```lisp:~/.sbclrc
(setf sb-impl::*default-external-format* :utf-8)
(setf sb-alien::*default-c-string-external-format* :utf-8)
```

## MySQL
次のページからダウンロード・インストールし、PATHを通す。  
https://dev.mysql.com/downloads/mysql/

## SQLite
次のページからダウンロードし、PATHを通す。  
https://www.sqlite.org/download.html

## 本システムの導入

```bash
$ git clone https://github.com/Hosokawa-Atsushi/the-game-online-experiment.git
$ cd the-game-online-experiment
$ npm install
```

`setup_database.sql` と `config/server.json` の `"mysql_password"` をthe_game_userのパスワードに置き換える。  
次のコマンドを実行し、MySQLインストール時に決めたrootユーザのパスワードを入力する。
```bash
$ mysql -u root -p < setup_database.sql
```
これで、MySQLデータベースが設定される。

コマンド `sbcl1.4` でSBCLを起動し、次のLispコードを実行する。
```lisp
* (load "db/schema.lisp")
* (in-package :the-game.db)
* (initialize-database)
* (in-package :cl-user)
* (exit)
```
これで、SQLiteデータベースが設定される。

次のコマンドでWebサーバを起動する。
```bash
$ node app.js
```

# 使い方
PC上で、Internet Explorer 以外のWebブラウザから次のURLにアクセスする。  
http://localhost:3000/node2/the-game/  
以降はWebページの説明に従って進める。次のような流れになっている。
1. 実験説明
2. ゲームルール確認クイズ
3. カードゲームのプレイ (30分間)
4. アンケート

ゲームの結果は `db/result.db` (SQLiteデータベース) に記録される。

# ファイル構成

```
the-game-online-experiment
├── README.md
├── package.json
├── package-lock.json
├── setup_database.sql          // MySQL環境構築用のSQL文
├── app.js                      // サーバサイドの処理を行っている
├── config
│   └── server.json                 // **サーバサイドの設定ファイル**
├── signup.html                 // サインアップページ
├── cardgame_rule.pdf
├── views
│   ├── explanation_check.ejs
│   └── waiting.ejs
├── admin.html
├── ActrAdmin.js                // ACT-Rプロセスを管理している
├── InterfaceAdmin.js           // HTMLインタフェースを管理している
├── AssociationClient.js
├── util
│   ├── delay.js
│   ├── promisify.js
│   ├── retry.js
│   ├── TaskQueue.js
│   └── unusedPort.js
├── actr7.x                     // ACT-Rのソースコード
│   ├── load-the-game-system.lisp   // 必要なファイルをロードする
│   ├── load-act-r.lisp             // ACT-R本体をロードする
│   ├── user-made-codes
│   │   └── the-game-human-vs-another.lisp // カードゲームのプログラム
│   ├── the-game-models             // ゲームをプレイするACT-Rモデル
│   │   ├── random.lisp
│   │   ├── AS-IS+COMPLETE+CASE-BASE.lisp
│   ︙ (後略)
│
├── db
│   └── schema.lisp                 // SQLite用の関数
└── connection                  // ACT-RのHTMLインタフェース
     ├── environment.js              // インタフェースの処理を行っている
     ├── expwindow.html
     └── environment.html
```

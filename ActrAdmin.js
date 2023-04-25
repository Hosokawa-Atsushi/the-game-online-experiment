/**
 * @file ActrAdmin.js
 *
 */
"use strict";

const child_process = require("child_process");
const path = require("path");
const debug = require("debug")("Admin");
const treeKill = require("tree-kill");
const delay = require("./util/delay");
const promisify = require("./util/promisify");
const retry = require("./util/retry");
const TaskQueue = require("./util/TaskQueue");
const unusedPort = require("./util/unusedPort");


/**
 * @class ActrAdmin
 *
 */
class ActrAdmin {
  /**
   * @constructor
   * @param {string} command - 実行可能ファイル
   * @param {string[]} args - コマンドライン引数の配列
   * @param {number|false} idleTimeout - アイドル時にサーバーを終了するまでの待ち時間、ミリ秒。0またはfalseのとき無効
   */
  constructor(command, args, idleTimeout) {
    /** 実行可能ファイル */
    this.command = command;
    /** コマンドライン引数の配列 */
    this.args = args;
    /** アクセスのないサーバーを終了するまでの時間 */
    this.idleTimeout = idleTimeout;

    /** ユーザーごとのサーバーの接続およびサーバープロセス */
    this.servers = Object.create(null);
    /** プロセス起動/終了の同期用キュー */
    this.queue = new TaskQueue();
  }

  /**
   * @member updateIdleTimer
   * @private
   * サーバーのアイドルタイマーを更新する
   * @param {string} user - 対象となるユーザー名
   */
  updateIdleTimer(user) {
    this.clearIdleTimer(user);

    // idleTimeout値が0でない数値のとき
    if (this.idleTimeout) {
      // アイドルタイマーを設定
      this.servers[user].timer = setTimeout(() => {
        // サーバーを停止する
        debug(`idle timeout expired for ${user}, shutting down server process`);
        this.shutdownServer(user);
      }, this.idleTimeout);
    }
  }

  /**
   * @member clearIdleTimer
   * @private
   * サーバーのアイドルタイマーを停止する
   * @param {string} user - 対象となるユーザー名
   */
  clearIdleTimer(user) {
    clearTimeout(this.servers[user].timer);
  }

  /**
   * @member setupServer
   * @private
   * 指定したユーザーのACT-RプロセスとHTMLインタフェースを開始する
   * @param {string} user - 対象となるユーザー名
   * @param {InterfaceAdmin} ifad
   * @returns {Promise}
   */
  setupServer(user, ifad) {
    let server;

    // 二重起動およびポートの競合を避けるため同期化する
    return this.queue.enqueue(() => {
      // すでにサーバー起動済みの場合は開始しない
      // (並行して起動中だった可能性があるため同期化してからもう一度調べる)
      if (this.servers[user]) {
        return Promise.resolve();
      }

      console.log(`setting up ACT-R server process for ${user}`);

      return unusedPort().then((port) => {
        // 空きポートを調べ、サーバープロセスを起動する

        // 取得したポート番号をコマンドラインに入れる
        const portSetting = `(defparameter *given-port* ${port})`;
        this.args[this.args.indexOf('portSetting')] = portSetting;

        // debug(`spawn "${path.resolve(this.command)} ${user} ${port}"`);
        console.log(`spawn "${this.command} ${this.args}"`);

        // server = child_process.spawn(path.resolve(this.command), [user, port]);
        server = child_process.spawn(this.command, this.args);
        let actrPortnum;
        server.stdout.on('data', (data) => {
          console.log(`----- ACT-R -----\n${data}`);
          // 'Server started on port XXXXX'という出力におけるXXXXXはACT-Rサーバーが起動したポート番号
          if (data.toString().match('Server started on port')) {
            actrPortnum = parseInt(data.toString().substr('Server started on port '.length));
          }
          // ACT-Rサーバーが使用可能になったらHTMLインタフェースを起動する
          if (data.toString().match('ACT-RserverAvailable')) {
            ifad.setupServer(user, actrPortnum);
          }
        });
        server.stderr.on('data', (data) => {
          console.log(`----- ACT-R -----\n${data}`);
        });

        return Promise.resolve();
      }).then(() => {
        this.servers[user] = {server};
        this.updateIdleTimer(user);
        return Promise.resolve();
      });
    });
  }

  /**
   * @member killServer
   * @private
   * 対象のサーバーを終了する。
   * プロセスを強制終了する。
   * @param {string} user - 対象となるユーザー名
   * @returns {Promise}
   */
  killServer(user) {
    return this.queue.enqueue(() => {
      // すでにkillServerされている場合は処理しない
      if (!this.servers[user]) {
        return Promise.resolve();
      }

      debug(`killing ACT-R server process for ${user}`);
      this.clearIdleTimer(user);

      treeKill(this.servers[user].server.pid);
      delete this.servers[user];

      return Promise.resolve();
    });
  }

  /**
   * @member close
   * すべてのサーバーを終了する
   * @returns {Promise}
   */
  close() {
    return Promise.all(Object.keys(this.servers).map((user) => {
      return this.killServer(user);
    }));
  }
}

module.exports = ActrAdmin;

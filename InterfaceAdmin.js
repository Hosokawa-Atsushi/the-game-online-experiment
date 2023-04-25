/**
 * @file lib/InterfaceAdmin.js
 *
 */
"use strict";

const child_process = require("child_process");
const { resolve } = require("path");
const path = require("path");
const debug = require("debug")("Admin");
const treeKill = require("tree-kill");
const AssociationClient = require("./AssociationClient");
const delay = require("./util/delay");
const promisify = require("./util/promisify");
const retry = require("./util/retry");
const TaskQueue = require("./util/TaskQueue");
const unusedPort = require("./util/unusedPort");


/**
 * @class InterfaceAdmin
 *
 */
class InterfaceAdmin {
  /**
   * @constructor
   * @param {string} command - 実行可能ファイル
   * @param {string} serverModule - 起動するサーバープログラム
   * @param {number|false} idleTimeout - アイドル時にサーバーを終了するまでの待ち時間、ミリ秒。0またはfalseのとき無効
   */
  constructor(command, serverModule, idleTimeout) {
    /** 実行可能ファイル */
    this.command = command;
    /** 起動するサーバープログラム */
    this.serverModule = serverModule;
    /** アクセスのないサーバーを終了するまでの時間 */
    this.idleTimeout = idleTimeout;

    /** ユーザーごとのサーバーの接続およびサーバープロセス */
    this.servers = Object.create(null);
    /** プロセス起動/終了の同期用キュー */
    this.queue = new TaskQueue();
  }

  /**
   * @member getClient
   * @private
   * 指定したユーザーに対応するサーバーへの接続を取得する
   * @param {string} user - 対象となるユーザー名
   * @returns {Promise.<AssociationClient>}
   */
  getClient(user) {
    if (this.servers[user]) {
      // idle timerをリセットする
      this.updateIdleTimer(user);

      // すでに作成されているクライアントを返す
      return Promise.resolve(this.servers[user].client);
    } else {
      return Promise.resolve();
    }
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
   * 指定したユーザーのサーバーを開始し、接続を行う
   * @param {string} user - 対象となるユーザー名
   * @param {number} actrPortnum - ACT-Rサーバーのポート番号
   * @returns {Promise.<AssociationClient>}
   */
  setupServer(user, actrPortnum) {
    let server, client;

    // 二重起動およびポートの競合を避けるため同期化する
    return this.queue.enqueue(() => {
      // すでにサーバー起動済みの場合はそのインスタンスを返す
      // (並行して起動中だった可能性があるため同期化してからもう一度調べる)
      if (this.servers[user]) {
        return Promise.resolve(this.servers[user].client);
      }

      console.log(`setting up interface server process for ${user}`);

      return unusedPort().then((port) => {
        // 空きポートを調べ、パラメータに指定してサーバープロセスを起動する
        // debug(`spawn "${path.resolve(this.command)} ${user} ${port}"`);
        console.log(`fork "${this.serverModule} ${user}, for ${actrPortnum} on ${port}"`);

        // server = child_process.spawn(path.resolve(this.command), [user, port]);
        server = child_process.fork(this.serverModule, [user, actrPortnum, port]);
        // server.stdout.on('data', (data) => {
        //   console.log(`----- interface -----\n${data}`);
        // });
        // server.stderr.on('data', (data) => {
        //   console.log(`----- interface -----\n${data}`);
        // });
        client = new AssociationClient(port);

        return Promise.resolve();
      }).then(() => {
        this.servers[user] = {server, client};
        this.updateIdleTimer(user);
        return Promise.resolve(client);
      });
    });
  }

  matchAgainstEachOther(user1, user2) {
    let server1, server2;
    return new Promise(resolve => {
      const intervalId = setInterval(() => {
        server1 = this.servers[user1];
        server2 = this.servers[user2];
        if (server1 && server2) {
          clearInterval(intervalId);
          server1 = server1.server;
          server2 = server2.server;
          console.log('server1: ' + JSON.stringify(server1));
          console.log('server2: ' + JSON.stringify(server2));
          server1.send({ isModel: false, id: user2 });
          server2.send({ isModel: false, id: user1 });
          resolve();
        }
      }, 100);
    });
  }

  matchAgainstTheModel(user, modelId) {
    let server;
    return new Promise(resolve => {
      const intervalId = setInterval(() => {
        server = this.servers[user];
        if (server) {
          clearInterval(intervalId);
          server = server.server;
          console.log('server: ' + JSON.stringify(server));
          server.send({ isModel: true, id: modelId });
          resolve();
        }
      }, 100);
    });
  }

  /**
   * @member shutdownServer
   * @private
   * 指定したユーザーのサーバーを終了する
   * @param {string} user - 対象となるユーザー名
   * @returns {Promise}
   */
  shutdownServer(user) {
    return this.queue.enqueue(() => {
      // すでにkillServerされている場合は処理しない
      if (!this.servers[user]) {
        return Promise.resolve();
      }

      debug(`shutting down server process for ${user}`);
      this.clearIdleTimer(user);

      // `stop-server` メッセージを送信しプロセス終了を待つ
      return this.servers[user].client.disconnect().then(() => {
        return Promise.race([
          promisify.event(this.servers[user].server, "exit").then((code) => {
            if (code !== 0) {
              // exit code !== 0のとき異常終了
              return Promise.reject(new Error(`association server exited with non-zero code: ${code}`));
            }
          }),
          // 1000ms経過でタイムアウト
          delay(1000).then(() => Promise.reject(new Error("association server failed to exit: timeout")))
        ]);
      }).then(() => {
        debug(`stopped server for ${user}`);

        delete this.servers[user];
      }, (err) => {
        debug(`failed to stop server for ${user}: `, err);

        return Promise.reject(err);
      });
    });
  }

  /**
   * @member killServer
   * @private
   * 対象のサーバーを終了する。
   * サーバーから接続を切断された際にプロセスを強制終了する。
   * @param {string} user - 対象となるユーザー名
   * @returns {Promise}
   */
  killServer(user) {
    return this.queue.enqueue(() => {
      // すでにshutdownServerされている場合は処理しない
      if (!this.servers[user]) {
        return Promise.resolve();
      }

      debug(`killing interface server process for ${user}`);
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
      // return this.shutdownServer(user);
      return this.killServer(user);
    }));
  }
}

module.exports = InterfaceAdmin;

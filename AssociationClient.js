/**
 * @file lib/AssociationClient.js
 * 画像検索サーバーとの通信を実装する
 */
"use strict";

const events = require("events");
const net = require("net");
const readline = require("readline");
const debug = require("debug")("app:AssociationClient");
const promisify = require("./util/promisify");
const TaskQueue = require("./util/TaskQueue");


/**
 * @class AssociationClient
 * 画像検索サーバーとの通信を実装する
 */
class AssociationClient extends events.EventEmitter {
  /**
   * @constructor
   * @param {number} port - 接続先サーバーのTCPポート番号
   */
  constructor(port) {
    super();

    /** 接続先サーバーのTCPポート番号 */
    this.port = port;
    /** 複数のgetNextImageId要求を直列化するためのキュー */
    this.queue = new TaskQueue();
    /** サーバーへのTCP接続を行うnet.Socket */
    this.conn = null;
    /** サーバー応答を行ごとに分割するreadline.Interface */
    this.rl = null;

    this.on("error", (err) => {});
  }

  /**
   * @method connect
   * サーバーとの接続を確立する
   * @returns {Promise}
   */
  connect() {
    debug(`connecting to localhost:${this.port}`);

    // TCP接続を開始
    const conn = net.connect(this.port, "localhost");

    // connectイベントかerrorイベントの早い方を採用
    return promisify.anyEvent([[conn, {
      "connect": () => null,
      "error": (err) => Promise.reject(err)
    }]]).then(() => {
      debug(`connected to localhost:${this.port}`);

      this.conn = conn;

      // 接続切断イベントをAssociationServiceに報告
      this.conn.on("close", () => {
        this.emit("close");
      });

      // 通信エラー時にプロセスを終了しない
      // 他でエラーが処理されない場合デバッグ出力する
      this.conn.on("error", (err) => {
        if (this.conn.listenerCount("error") === 1) {
          debug(`localhost:${this.port}: `, err);
        }
      });

      // 受信データを行ごとに分割する
      this.rl = readline.createInterface({
        input: conn
      });
    }, (err) => {
      debug(`failed to connect to localhost:${this.port}: `, err);
      return Promise.reject(err);
    });
  }

  /**
   * @method getNextImageId
   * サーバーに次画像の検索を要求する
   * @returns {Promise.<string>} - 次画像のID
   */
  getNextImageId() {
    // 並行したrun-nextの実行時に結果が混線するのを避けるため同期化
    return this.queue.enqueue(() => {
      debug(`localhost:${this.port}> run-next`);

      // コマンドの送信
      return promisify(this.conn.write.bind(this.conn))("run-next\r\n").then(() => {
        // 応答行の受信、切断、通信エラーのいずれかで完了
        return promisify.anyEvent([[this.rl, {
          "line": (line) => line
        }], [this.conn, {
          "close": () => Promise.reject(new Error("connection closed")),
          "error": (err) => Promise.reject(err)
        }]]);
      }).then((line) => {
        debug(`localhost:${this.port}< ${line}`);
        return line;
      }, (err) => {
        debug(`localhost:${this.port}: `, err);
        return Promise.reject(err);
      });
    });
  }

  /**
   * @method disconnect
   * サーバーとの接続を切断する
   * @returns {Promise}
   */
  disconnect() {
    return this.queue.enqueue(() => {
      debug(`localhost:${this.port}> stop-server`);

      // 終了コマンドを送信
      return promisify(this.conn.write.bind(this.conn))("stop-server\r\n").then(() => {
        return promisify.anyEvent([[this.conn, {
          // 接続切断
          "close": () => Promise.resolve(),
          // 接続エラー (RST等)
          "error": (err) => Promise.reject(err),
        }]]);
      }).then(() => {
        debug(`localhost:${this.port}: closed`);
      }, (err) => {
        debug(`localhost:${this.port}: `, err);
        return Promise.reject(err);
      });
    });
  }
}


module.exports = AssociationClient;

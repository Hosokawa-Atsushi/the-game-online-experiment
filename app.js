const app = require('express')();
const mainServer = require('http').createServer(app);
const io = require('socket.io')(mainServer, {
  path: '/node2/the-game/socket.io',
  transports: ['websocket']
});

const util = require('util');
const conf = require('./config/server.json'); // サーバーサイド設定ファイル

mainServer.listen(conf.LISTEN.PORT);

const ActrAdmin = require('./ActrAdmin');
const InterfaceAdmin = require('./InterfaceAdmin');
const actrCommand = 'sbcl1.4'; // SBCL実行可能ファイル
const actrArgs = ['--eval', 'portSetting', '--load', 'actr7.x/load-the-game-system.lisp']; // SBCLのコマンドラインオプション、'portSetting'はポート番号の設定式に置き換えられる
const actrIdleTimeout = false;
const interfaceCommand = 'node'; // Node.js実行可能ファイル
const interfaceServerModule = 'connection/environment.js'; // ACT-RのHTMLインタフェースのパス
const interfaceIdleTimeout = false;
const actrad = new ActrAdmin(actrCommand, actrArgs, actrIdleTimeout);
const ifad = new InterfaceAdmin(interfaceCommand, interfaceServerModule, interfaceIdleTimeout);

// テンプレートエンジンの指定
app.set("view engine", "ejs");

/* mysql初期設定 */
const mysql = require('mysql');
const pool = mysql.createPool(conf.MYSQL);

/**
 * MySQLにSQL文を投げる関数 sqlインジェクション対策版
 * @param {String} sqlStatement SQL文
 * @param {Array} placeholder SQL文へ代入する変数リスト
 */
async function sql(sqlStatement, placeholder, isLog = true) {
  if (isLog) console.log('[mysql.query] ' + sqlStatement + ',' + placeholder); // 投げられた文をトレース
  pool.query = util.promisify(pool.query);
  try {
    const result = await pool.query(sqlStatement, placeholder);
    if (isLog) console.log('[mysql.result] ' + JSON.stringify(result)); // 実行結果を返す。そのまま連結すると中身が見れなくなるのでJSON.stringify()を使用
    return result;
  } catch (err) {
    throw err;
  }
}

/**
 * 既存のIDかどうかを返す関数
 */
async function isExistingId(kameiId) {
  const result = (await sql('SELECT COUNT(*) FROM players WHERE kamei_id = ?', [kameiId]));
  return result[0]['COUNT(*)'];
}

/**
 * 番号付けされたIDを返す関数
 */
async function getNumberedId(kameiId) {
  let i = 1;
  let uid = kameiId;
  const splited = kameiId.split('-');
  while (await isExistingId(uid)) {
    if (i > 1) {
      splited.pop();
    }
    splited.push(i);
    uid = splited.join('-');
    i++;
  }
  return uid;
}

/**
 * pairsテーブルのstatusカラムを引数に応じて更新する関数
 * @param {String} pairId
 * @param {String} status game status
 */
async function setStatus(pairId, status) {
  await sql('UPDATE pairs SET status = ? WHERE id = ?', [status, pairId]);
}

/**
* pairsテーブルのstatusカラムを取得
* @param {String} pairId
* @return {String} status
*/
async function getStatus(pairId) {
  const res = await sql('SELECT status FROM pairs WHERE id = ?', [pairId]);
  return res[0]['status'];
}

/**
 * kameiIdの通信状態を更新する関数
 * @param {String} kameiId
 * @param {Boolean} isConnect
 */
async function setIsConnect(kameiId, isConnect) {
  await sql('UPDATE players SET is_connect = ? WHERE kamei_id = ?', [isConnect, kameiId]);
}

/**
 * socket.idを入力すると仮名IDを返す関数
 * @param {String} socket.id ソケットID
 * @return {String} kameiId 仮名ID
 */
async function getKameiId(socketId) {
  const result = await sql('SELECT kamei_id FROM players WHERE socket_id = ?', [socketId]);
  if (result[0]) return result[0]['kamei_id'];
  else return null;
}

/**
 * 仮名idが入室しているroom name(pairId)を返す関数
 * @param {String} kameiId 自分の仮名id
 * @return {String} room name(pairId)
 */
async function getPairId(kameiId) {
  const result = await sql('SELECT id FROM pairs WHERE host_id = ? OR guest_id = ?', [kameiId, kameiId]);
  return result[0]['id'];
}

async function assignTheModel(kameiId, modelId, socket) {
  await ifad.matchAgainstTheModel(kameiId, modelId);
  await sql('UPDATE players SET socket_id = ?, is_host = ?, is_connect = ?, partner_id = ? WHERE kamei_id = ?', [socket.id, true, true, modelId, kameiId]);
  await sql('INSERT INTO pairs SET host_id = ?, host_entered_at = CURRENT_TIMESTAMP, guest_id = ?, guest_entered_at = CURRENT_TIMESTAMP, status = ?', [kameiId, modelId, conf.STATUS.MATCHMAKING_GOTE]);
  // 対戦処理を呼び出し
  socket.emit('match');
}

app.get('/node2/the-game/', (req, res) => {
  res.redirect('/node2/the-game/signup');
});

app.get('/node2/the-game/signup', (req, res) => {
  res.sendFile(__dirname + '/signup.html');
});

app.get('/node2/the-game/rule', (req, res) => {
  res.sendFile(__dirname + '/cardgame_rule.pdf');
});

app.get('/node2/the-game/explanation_check', (req, res) => {
  const user = req.query.user;
  res.render('./explanation_check.ejs', { user: user });
});

app.get('/node2/the-game/game', async (req, res) => {
  const user = req.query.user;
  // 指定したユーザーに対応するACT-RのHTMLインタフェースを取得する
  const client = await ifad.getClient(user);
  if (client) {
    // 取得できたら、HTMLインタフェースにリダイレクトする
    res.redirect('http://' + req.hostname + ':' + client.port + '/expwindow');
  } else {
    // 取得できなかったら、
    if (await isExistingId(user)) {
      const nid = await getNumberedId(user);
      await sql('INSERT INTO players SET kamei_id = ?', [nid]);
      // ユーザーを指定して /setup にリダイレクトする
      res.redirect('/node2/the-game/setup?user=' + nid);
    } else {
      await sql('INSERT INTO players SET kamei_id = ?', [user]);
      // ユーザーを指定して /setup にリダイレクトする
      res.redirect('/node2/the-game/setup?user=' + user);
    }
  }
});

app.get('/node2/the-game/setup', (req, res) => {
  const user = req.query.user;
  // 指定したユーザーに対応するACT-RプロセスとHTMLインタフェースを起動する
  actrad.setupServer(user, ifad);
  res.redirect('/node2/the-game/waiting?user=' + user);
});

app.get('/node2/the-game/waiting', (req, res) => {
  const user = req.query.user;
  res.render('./waiting.ejs', { user: user });
});

// 接続確立時の処理
io.of('/waiting').on('connection', async (socket) => {
  console.log('[express] 待機室接続確立: ' + socket.id);
  /* マッチメイキング */
  await socket.on('entry', async (encodedKameiId) => {
    const kameiId = decodeURI(encodedKameiId); // ユーザー名をURIデコード
    const mostWaitingPairId = (await sql('SELECT MIN(id) FROM pairs WHERE guest_id IS NULL'))[0]['MIN(id)'];
    if (mostWaitingPairId) {
      // Guest
      await sql('UPDATE pairs SET guest_id = ?, guest_entered_at = CURRENT_TIMESTAMP, status = ? WHERE id = ?', [kameiId, conf.STATUS.MATCHMAKING_GOTE, mostWaitingPairId]);
      const hostId = (await sql('SELECT host_id FROM pairs WHERE id = ?', [mostWaitingPairId]))[0]['host_id'];
      await sql('UPDATE players SET socket_id = ?, is_host = ?, is_connect = ?, partner_id = ? WHERE kamei_id = ?', [socket.id, false, true, hostId, kameiId]);
      await sql('UPDATE players SET partner_id = ? WHERE kamei_id = ?', [kameiId, hostId]); // hostのパートナーは自分
      console.log('[waiting] ' + kameiId + ' is guest of room' + mostWaitingPairId);
      console.log('[waiting] room' + mostWaitingPairId + ' matchmaking complete (host:' + hostId + ', guest:' + kameiId + ')');
      await socket.join(mostWaitingPairId);
      await ifad.matchAgainstEachOther(hostId, kameiId);
      await socket.to(mostWaitingPairId).emit('match'); // ホスト(部屋全体)にもマッチング完了を伝える
      await socket.emit('match'); // ゲストが入ったらマッチング完了なのでゲスト側のクライアントにそう伝える
    } else {
      const pairCount = (await sql('SELECT COUNT(*) FROM pairs'))[0]['COUNT(*)']; // ペア数
      switch (pairCount % 15) {
        case 0:
          // Host
          await sql('UPDATE players SET socket_id = ?, is_host = ?, is_connect = ? WHERE kamei_id = ?', [socket.id, true, true, kameiId]);
          const pairId = (await sql('INSERT INTO pairs SET host_id = ?, host_entered_at = CURRENT_TIMESTAMP, status = ?', [kameiId, conf.STATUS.MATCHMAKING_SENTE]))['insertId'];
          console.log('[waiting] ' + kameiId + '(' + socket.id + ') is host of room' + pairId);
          await socket.join(pairId);
          break;

        case 1:
          assignTheModel(kameiId, 'model1', socket);
          break;

        case 2:
          assignTheModel(kameiId, 'model2', socket);
          break;

        case 3:
          assignTheModel(kameiId, 'model3', socket);
          break;

        case 4:
          assignTheModel(kameiId, 'model4', socket);
          break;

        case 5:
          assignTheModel(kameiId, 'model5', socket);
          break;

        case 6:
          assignTheModel(kameiId, 'model6', socket);
          break;

        case 7:
          assignTheModel(kameiId, 'model7', socket);
          break;

        case 8:
          assignTheModel(kameiId, 'model8', socket);
          break;

        case 9:
          assignTheModel(kameiId, 'model9', socket);
          break;

        case 10:
          assignTheModel(kameiId, 'model10', socket);
          break;

        case 11:
          assignTheModel(kameiId, 'model11', socket);
          break;

        case 12:
          assignTheModel(kameiId, 'model12', socket);
          break;

        case 13:
        case 14:
          assignTheModel(kameiId, 'random', socket);
          break;
      }
    }
  });
  /* ソケット切断時の処理 */
  await socket.on('disconnect', async () => {
    const kameiId = await getKameiId(socket.id);
    if (kameiId) { // 仮名IDと連結されているプレイヤーがソケット通信を切断　ここでバグ発生検知できるかも
      const pairId = await getPairId(kameiId);
      const status = await getStatus(pairId);
      switch (status) {
        case conf.STATUS.MATCHMAKING_SENTE:
          await sql('DELETE FROM pairs WHERE id = ?', pairId); // 幽霊とマッチングしないようにするため
          break;
        default:
          break;
      }
      // if (status != conf.STATUS.QUESTIONNAIRE) socket.to(pairId).emit('partner disconnected'); // パートナーにお知らせ
      await io.of('/admin').emit('user disconnect', kameiId, socket.id, status, pairId);
      console.log('[express] 待機室接続切断: ' + socket.id + '(' + kameiId + ')');
      await setIsConnect(kameiId, false);
    }
    else {
      await io.of('/admin').emit('user disconnect', '--', socket.id, '--', '--');
      console.log('[express] 待機室接続切断: ' + socket.id + '(仮名IDなし)');
    }
  });
});

app.get('/node2/the-game/admin', (req, res) => {
  res.sendFile(__dirname + '/admin.html');
});

app.get('/node2/the-game/admin/kill', (req, res) => {
  const user = req.query.user;
  actrad.killServer(user);
  ifad.killServer(user);
  res.send(user + 'のサーバーを強制終了します\n');
});

app.get('/node2/the-game/admin/close', (req, res) => {
  actrad.close();
  ifad.close();
  res.send('すべてのサーバーを強制終了します\n');
});

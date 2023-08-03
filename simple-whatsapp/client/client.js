const net = require('net');

const HOST = '127.0.0.1';
const PORT = 8000;
const userId = generateRandomId(8);
const client = new net.Socket();

client.connect(PORT, HOST, () => {
  sendOnlineMessage();
  console.log(`UserId:- ${userId}`);
  console.log(`Connected to ${HOST}:${PORT}`);
  console.log('write messages in the format recipiendId:message below:-');
});

client.on('data', data => {
  const senderId = data.toString('utf8', 0, 8);
  const message = data.toString('utf8', 8, data.byteLength);
  console.log(`you have 1 new message from ${senderId} : ${message}`);
});

client.on('close', () => {
  console.log('Connection closed');
});

process.stdin.on('data', data => {
  const line = data.toString().trim();
  const recipiendId = line.split(':')[0];
  const message = line.split(':')[1];
  sendNewMessage(recipiendId, message);
});

function generateRandomId(length) {
  const characters = 'abcdefghijklmnopqrstuvwxyz0123456789';
  let randomString = '';
  for (let i = 0; i < length; i++) {
    const randomIndex = Math.floor(Math.random() * characters.length);
    randomString += characters[randomIndex];
  }
  return randomString;
}

function sendOnlineMessage() {
  const messageType = 0;
  const data = Buffer.concat([Buffer.from([messageType]), Buffer.from(userId)]);
  client.write(data);
}

function sendNewMessage(recipiendId, Message) {
  const messageType = 1;
  const data = Buffer.concat([
    Buffer.from([messageType]),
    Buffer.from(recipiendId),
    Buffer.from(Message),
  ]);
  client.write(data);
}

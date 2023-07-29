const dgram = require('dgram');
const socket = dgram.createSocket('udp4');

socket.bind(8081);

socket.send('hi from nodejs', 8789, '127.0.0.1', () => {
  console.log('send message to 8789');
});

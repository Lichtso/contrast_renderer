const fs = require('fs'),
      path = require('path'),
      http = require('http'),
      contentTypeByFileExtension = {
    '.html': 'text/html',
    '.css': 'text/css',
    '.js': 'text/javascript',
    '.mjs': 'text/javascript',
    '.json': 'application/json',
    '.wasm': 'application/wasm',
    '.png': 'image/png',
    '.svg': 'image/svg+xml'
};

const PORT = 8080,
      server = http.createServer((request, response) => {
    const filePath = '../../'+request.url,
          fileExtension = path.extname(filePath),
          contentType = (contentTypeByFileExtension[fileExtension]) ? contentTypeByFileExtension[fileExtension] : 'text/plain';
    fs.readFile(filePath, (error, content) => {
        if(error) {
            if(error.code == 'ENOENT')
                response.writeHead(404);
            else
                response.writeHead(500);
        } else {
            response.writeHead(200, {
                'Content-Type': contentType
            });
            response.write(content);
        }
        response.end();
    });
}).on('clientError', (err, socket) => {
    socket.end('HTTP/1.1 400 Bad Request\r\n\r\n');
}).listen(PORT, () => {
    console.log(`http://localhost:${PORT}/examples/showcase/index.html`);
});

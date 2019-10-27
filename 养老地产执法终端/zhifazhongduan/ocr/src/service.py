"""
http service接口，需要python 3.7
"""

from http.server import ThreadingHTTPServer, BaseHTTPRequestHandler
from urllib.parse import urlparse
import json
import datetime
import time
import os
import sys

sys.path.append(os.path.abspath(os.path.join(os.getcwd(), '../../../common')))
import algorithm


# 响应请求
def response(handler, response_data):
    handler.protocol_version = 'HTTP/1.1'
    handler.send_response(200)
    handler.end_headers()
    handler.wfile.write(bytes(json.dumps(response_data, ensure_ascii=False), encoding='utf-8'))


# POST请求地址：http://{IP:Port}/{urlPath}
# 例：http://127.0.0.1:8000/ai/ocr/foodbusinesslicense/v1.0

class ServiceHTTPRequestHandler(BaseHTTPRequestHandler):
    # POST
    def do_POST(self):
        start_time = time.time()

        url = urlparse(self.path)

        log = dict()
        log['request_datetime'] = datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')

        length = int(self.headers['Content-Length'])
        in_str = self.rfile.read(length).decode('utf-8')
        request_data = json.loads(in_str)
        log['data_in'] = request_data

        try:
            if url.path == '/ai/ocr/foodbusinesslicense/v1.0':
                print('食品经营许可证')
                data = food_business_license(request_data['filePath'], request_data['type'],
                                             request_data['version'])
                response_data = {'requestID': request_data['requestID'], 'data': data}
                print(request_data)
                response(self, response_data)

                logging(log, url, response_data)
            elif url.path == '/ai/ocr/identitycard/v1.0':
                print('身份证')
            else:
                response_data = {'result': 'url错误'}
                response(self, response_data)
                logging(log, url, response_data)
        except BaseException as exception:
            print('exception', exception)
            response_data = {'result': str(exception)}
            response(self, response_data)
            logging(log, url, response_data)

        print(round(time.time() - start_time, 3), 'seconds')

    # GET
    def do_GET(self):
        url = urlparse(self.path)

        try:
            response_data = {'result': '不支持get请求'}
            response(self, response_data)

            log = dict()
            log['request_datetime'] = datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')
            log['data_in'] = url.query

            logging(log, url, response_data)
        except BaseException as exception:
            print('exception', exception)


def logging(log, url, response_data):
    log['request_datetime'] = datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')
    log['url'] = url.path
    log['data_out'] = response_data
    log['response_datetime'] = datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')
    print(log)


def run(server_class=ThreadingHTTPServer, handler_class=ServiceHTTPRequestHandler):
    ip = algorithm.get_config('ip')
    port = algorithm.get_config('port')
    server_address = (ip, int(port))
    httpd = server_class(server_address, handler_class)
    print('Server startup on IP:' + ip + ' port:' + str(port) + ' ...')
    httpd.serve_forever()


if __name__ == '__main__':
    run()

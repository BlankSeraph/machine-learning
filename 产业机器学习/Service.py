from http.server import BaseHTTPRequestHandler, HTTPServer
from urllib.parse import urlparse
import json
import pymysql
import datetime
import jiwang
import guomin
import chati
import getnote
import similar







# 响应请求
def response(service, response_data):
    service.protocol_version = 'HTTP/1.1'
    service.send_response(200)
    service.end_headers()
    service.wfile.write(bytes(json.dumps(response_data, ensure_ascii=False), encoding='utf-8'))
# POST请求地址：http://{IP:Port}/{urlPath}
# 例：http://127.0.0.1:8000/check/api/v1.0/

class ServiceHTTPRequestHandler(BaseHTTPRequestHandler):
    # POST
    def do_POST(self):
        url = urlparse(self.path)

        log = dict()
        log['request_datetime'] = datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')
        log['url'] = url.path

        cursor = None

        try:
            if url.path == '/check/api/v1.0/':
                length = int(self.headers['Content-Length'])
                in_str = self.rfile.read(length).decode('utf-8')
                log['data_in'] = in_str
                print('data_in', in_str)
                request_data = json.loads(in_str)

                alg_id = None
                if 'entityID' in request_data:
                    alg_id = request_data['entityID']
                if alg_id is None:
                    response_data = {'result': 'entityID不能为空'}
                    response(self, response_data)
                    log['data_out'] = str(response_data)
                elif alg_id == '002':
                    print('既往史')

                    print(request_data)
                    content=request_data['content']
                    entityList= jiwang.GetEntity(alg_id, content)
                    print('response_data', entityList)
                    log['data_save'] = str(entityList)
                    response(self, entityList)

                elif alg_id == '001':
                    print('过敏史')
                    print(request_data)
                    content = request_data['content']
                    entityList = guomin.GetEntity(alg_id, content)
                    print('response_data', entityList)
                    log['data_save'] = str(entityList)
                    response(self, entityList)

                elif alg_id == '003':
                    print('生命体征')
                    print(request_data)
                    dbRecords = request_data['dbRecords']
                    content = request_data['content']
                    datas = getnote.GetProgressNote(dbRecords, content)
                    if datas != 1:
                        for date in datas:
                            print(111)
                        entityList = chati.GetEntity(alg_id, datas)

                        print('response_data', entityList)
                        log['data_save'] = str(entityList)
                    else:
                        entityList=0
                    response(self, entityList)
                elif alg_id == '004':
                    print('病程记录重复性检查')
                    print(request_data)
                    dbRecords = request_data['dbRecords']
                    content = request_data['content']

                    entityList = similar.IsCommon(dbRecords, content)

                    print('response_data', entityList)
                    log['data_save'] = str(entityList)
                    response(self, entityList)

            elif url.path == '/maintenance/api/v1.0/':
                print('既往史维护')
                length = int(self.headers['Content-Length'])
                request_data = json.loads(self.rfile.read(length).decode('utf-8'))
                log['data_in'] = str(request_data)
                print('data_in', request_data)

                # 先删除标准表历史记录
                args = {'medRecordID': request_data['medRecordID'], 'ChartNo': request_data['chartNo'],
                        'VisitNo': request_data['visitNo']}
            else:
                response_data = {'result': 'url错误'}
                response(self, response_data)
                log['data_out'] = str(response_data)
        except BaseException as exception:
            print('exception', exception)
            response_data = {'result': str(exception)}
            response(self, response_data)
            log['data_out'] = str(response_data)

        try:
            if cursor:
                log['response_datetime'] = datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')
        except BaseException as exception:
            print('exception', exception)


    # GET
    def do_GET(self):
        url = urlparse(self.path)
        connection = None

        try:
            response_data = {'result': '不支持get请求'}
            response(self, response_data)


            log = dict()
            log['request_datetime'] = datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')
            log['url'] = url.path
            log['data_in'] = url.query
            log['data_out'] = str(response_data)
            log['response_datetime'] = datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')

        except BaseException as exception:
            print('exception', exception)
def run(server_class=HTTPServer, handler_class=ServiceHTTPRequestHandler):
    ip = 'localhost'
    port = 8000
    server_address = (ip, port)
    httpd = server_class(server_address, handler_class)
    print('Serving HTTP on ' + ip + ' port ' + str(port) + ' ...')
    httpd.serve_forever()


if __name__ == '__main__':
    run()

from http.server import BaseHTTPRequestHandler, HTTPServer
from urllib.parse import urlparse
import json
import pymysql
import datetime

# 姿态检测项目相关包和代码块，实际部署时需要修改各个代码块中的文件读取目录
import os
import time
import angle_detection
import standing_time
import frequency_detection
import json

# Service框架，此部分代码应在补充完整后迁移至run()函数下
# 另外后续的各个函数也需要进行修改
while True:
    # 调用response响应用户上传的视频
    flag = response(service,response_data)
    time.sleep(10) # 休眠10秒，可以修改
    # flag初始化为零，当接收到上传数据时修改为1
    if flag == 1:
        # 执行CMD命令行，调用AlphaPose进行视频处理，产出后续步骤的csv文件
        os.system('CMD 代码填入')  # 填入在平台上运行AlphaPose时用的CMD命令
        # 执行顺序固定如下：
        angle_detection.run()  # 执行角度计算，没有返回值，但生成过程文件
        time_list = standing_time.run()  # 执行站立时间计算，得到时间列表
        swing_count = frequency_detection.run()  # 执行摆动次数计算，得到摆动次数列表
        # 这两个列表的每个元素代表着每个csv文件对应的结果
        frequency_list = [swing_count[i] / time_list[i] for i in range(len(time_list))]  # 得到频率列表
        # 得到的频率列表的顺序对应读入csv文件的顺序，可以加上csv文件的名称之类的
        #上述步骤都会产生/修改过程文件
        # 利用上述time_list和frequency，构造JSON格式，进行结果返回，注意还要加上初始视频名，方便用户知悉
        print('此处需要键入构造JSON文件的代码')
        #
        #
        #

        # 检测服务器相关过程文件的数量，定量进行清理历史文件
        # 建议成功返回结果就进行过程文件清理
        for a, b, c in os.walk('Files Path'):
            print('此处需要键入遍历过程文件的代码，计数后决定进行文件清理')
            #
            #
            #

        files = os.listdir('Files Path')  # 列出目录下的文件
        for file in files:
            os.remove(file)  # 删除文件

        # 执行完一次识别，重置flag，系统休眠
        flag = 0




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

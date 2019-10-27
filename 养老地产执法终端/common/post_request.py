from urllib import request
import json
import algorithm


def get_url(path):
    return 'http://' + algorithm.get_config('ip') + ':' + algorithm.get_config('port') + path


def test(args):
    response = request.urlopen(args[0], bytes(json.dumps(args[1]), 'utf-8'))
    print(response.read().decode("utf-8"))


def alpha_pose():
    url = get_url('/ai/alphapose/skeletonpose/v1.0')
    data = {'requestID': 'b8efacbb-7d43-4da5-9f48-9a75d8c68d30',
            'filePath': 'E:/zhengmianqiege/测试1-1.mp4',
            'subjectNumber': 'single',
            'inputType': 'video',
            'framePerSecond': '12'}
    return url, data


def ocr():
    url = get_url('/ai/ocr/foodbusinesslicense/v1.0')
    data = {'requestID': 'b8efacbb-7d43-4da5-9f48-9a75d8c68d30',
            'filePath': 'E:/zhengmianqiege/测试1-1.mp4',
            'type': 1,
            'version': '1903'}
    return url, data


if __name__ == '__main__':
    # test(alpha_pose())

    test(ocr())

"""
使用AlphaPose通过视频或图片获取人体姿态数据
需要python 3.6、cuda 9、pytorch 0.4
类脑计算中心镜像：xqk-py37-pytorch1.0.1-cu90或cu90-py36-tf19-t2t110-torch041
"""

import torch
from torch.autograd import Variable
import torch.nn.functional as F
import torchvision.transforms as transforms

import torch.nn as nn
import torch.utils.data
import numpy as np
from opt import opt

from dataloader import ImageLoader, DetectionLoader, DetectionProcessor, DataWriter, Mscoco
from yolo.util import write_results, dynamic_write_results
from SPPE.src.main_fast_inference import *

import os
import sys
from tqdm import tqdm
import time
from fn import getTime
import cv2
import argparse
from pPose_nms import pose_nms, write_json
import json
import time
import uuid
import shutil

args = opt
args.dataset = 'coco'
args.sp = 'sp'
if not args.sp:
    torch.multiprocessing.set_start_method('forkserver', force=True)
    torch.multiprocessing.set_sharing_strategy('file_system')


def video2frames(pathIn='',
                 pathOut='',
                 only_output_video_info=False,
                 extract_time_points=None,
                 initial_extract_time=0,
                 end_extract_time=None,
                 extract_time_interval=-1,
                 output_prefix='frame',
                 jpg_quality=100,
                 isColor=True,
                 subject=''):
    '''
    pathIn：视频的路径，比如：F:\python_tutorials\test.mp4
    pathOut：设定提取的图片保存在哪个文件夹下，比如：F:\python_tutorials\frames1\。如果该文件夹不存在，函数将自动创建它
    only_output_video_info：如果为True，只输出视频信息（长度、帧数和帧率），不提取图片
    extract_time_points：提取的时间点，单位为秒，为元组数据，比如，(2, 3, 5)表示只提取视频第2秒， 第3秒，第5秒图片
    initial_extract_time：提取的起始时刻，单位为秒，默认为0（即从视频最开始提取）
    end_extract_time：提取的终止时刻，单位为秒，默认为None（即视频终点）
    extract_time_interval：提取的时间间隔，单位为秒，默认为-1（即输出时间范围内的所有帧）
    output_prefix：图片的前缀名，默认为frame，图片的名称将为frame_000001.jpg、frame_000002.jpg、frame_000003.jpg......
    jpg_quality：设置图片质量，范围为0到100，默认为100（质量最佳）
    isColor：如果为False，输出的将是黑白图片
    '''

    cap = cv2.VideoCapture(pathIn)  ##打开视频文件
    n_frames = int(cap.get(cv2.CAP_PROP_FRAME_COUNT))  ##视频的帧数
    fps = cap.get(cv2.CAP_PROP_FPS)  ##视频的帧率
    dur = n_frames / fps  ##视频的时间

    ##如果only_output_video_info=True, 只输出视频信息，不提取图片
    if only_output_video_info:
        print('only output the video information (without extract frames)::::::')
        print("Duration of the video: {} seconds".format(dur))
        print("Number of frames: {}".format(n_frames))
        print("Frames per second (FPS): {}".format(fps))

        ##提取特定时间点图片
    elif extract_time_points is not None:
        if max(extract_time_points) > dur:  ##判断时间点是否符合要求
            raise NameError('the max time point is larger than the video duration....')
        try:
            os.mkdir(pathOut)
        except OSError:
            pass
        success = True
        count = 0
        while success and count < len(extract_time_points):
            cap.set(cv2.CAP_PROP_POS_MSEC, (1000 * extract_time_points[count]))
            success, image = cap.read()
            if success:
                if not isColor:
                    image = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)  ##转化为黑白图片
                print('Write a new frame: {}, {}th'.format(success, count + 1))
                if subject == 'single':
                    x, y = image.shape[0:2]
                    image = image[int(1 / 4 * x):int(3 / 4 * x), :]
                cv2.imwrite(os.path.join(pathOut, "{}_{:06d}.jpg".format(output_prefix, count + 1)), image,
                            [int(cv2.IMWRITE_JPEG_QUALITY), jpg_quality])  # save frame as JPEG file
                count = count + 1

    else:
        ##判断起始时间、终止时间参数是否符合要求
        if initial_extract_time > dur:
            raise NameError('initial extract time is larger than the video duration....')
        if end_extract_time is not None:
            if end_extract_time > dur:
                raise NameError('end extract time is larger than the video duration....')
            if initial_extract_time > end_extract_time:
                raise NameError('end extract time is less than the initial extract time....')

        ##时间范围内的每帧图片都输出
        if 1 / extract_time_interval == -1:
            if initial_extract_time > 0:
                cap.set(cv2.CAP_PROP_POS_MSEC, (1000 * initial_extract_time))
            try:
                os.mkdir(pathOut)
            except OSError:
                pass
            print('Converting a video into frames......')
            if end_extract_time is not None:
                N = (end_extract_time - initial_extract_time) * fps + 1
                success = True
                count = 0
                while success and count < N:
                    success, image = cap.read()
                    if success:
                        if not isColor:
                            image = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
                        print('Write a new frame: {}, {}/{}'.format(success, count + 1, n_frames))
                        if subject == 'single':
                            x, y = image.shape[0:2]
                            image = image[int(1 / 4 * x):int(3 / 4 * x), :]
                        cv2.imwrite(os.path.join(pathOut, "{}_{:06d}.jpg".format(output_prefix, count + 1)), image,
                                    [int(cv2.IMWRITE_JPEG_QUALITY), jpg_quality])  # save frame as JPEG file
                        count = count + 1
            else:
                success = True
                count = 0
                while success:
                    success, image = cap.read()
                    if success:
                        if not isColor:
                            image = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
                        print('Write a new frame: {}, {}/{}'.format(success, count + 1, n_frames))
                        if subject == 'single':
                            x, y = image.shape[0:2]
                            image = image[int(1 / 4 * x):int(3 / 4 * x), :]
                        cv2.imwrite(os.path.join(pathOut, "{}_{:06d}.jpg".format(output_prefix, count + 1)), image,
                                    [int(cv2.IMWRITE_JPEG_QUALITY), jpg_quality])  # save frame as JPEG file
                        count = count + 1

        ##判断提取时间间隔设置是否符合要求
        elif 1 / extract_time_interval > 0 and 1 / extract_time_interval < 1 / fps:
            raise NameError('extract_time_interval is less than the frame time interval....')
        elif 1 / extract_time_interval > (n_frames / fps):
            raise NameError('extract_time_interval is larger than the duration of the video....')

        ##时间范围内每隔一段时间输出一张图片
        else:
            try:
                os.mkdir(pathOut)
            except OSError:
                pass
            print('Converting a video into frames......')
            if end_extract_time is not None:
                N = (end_extract_time - initial_extract_time) / (1 / extract_time_interval) + 1
                success = True
                count = 0
                while success and count < N:
                    cap.set(cv2.CAP_PROP_POS_MSEC,
                            (1000 * initial_extract_time + count * 1000 * (1 / extract_time_interval)))
                    success, image = cap.read()
                    if success:
                        if not isColor:
                            image = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
                        print('Write a new frame: {}, {}th'.format(success, count + 1))
                        if subject == 'single':
                            x, y = image.shape[0:2]
                            image = image[int(1 / 4 * x):int(3 / 4 * x), :]
                        cv2.imwrite(os.path.join(pathOut, "{}_{:06d}.jpg".format(output_prefix, count + 1)), image,
                                    [int(cv2.IMWRITE_JPEG_QUALITY), jpg_quality])  # save frame as JPEG file
                        count = count + 1
            else:
                success = True
                count = 0
                while success:
                    cap.set(cv2.CAP_PROP_POS_MSEC,
                            (1000 * initial_extract_time + count * 1000 * (1 / extract_time_interval)))
                    success, image = cap.read()
                    if success:
                        if not isColor:
                            image = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
                        print('Write a new frame: {}, {}th'.format(success, count + 1))
                        if subject == 'single':
                            x, y = image.shape[0:2]
                            image = image[int(1 / 4 * x):int(3 / 4 * x), :]
                        cv2.imwrite(os.path.join(pathOut, "{}_{:06d}.jpg".format(output_prefix, count + 1)), image,
                                    [int(cv2.IMWRITE_JPEG_QUALITY), jpg_quality])  # save frame as JPEG file
                        count = count + 1


def alpha_pose(indir, subjectNumber, inputType, framePerSecond):
    """
    使用alpha_pose找到输入文件中的人体的17个骨骼点坐标
    :param indir: 视频或图片的本地路径
    :param subjectNumber: 识别对象的数量，str类型
        参数值：single、multiple；当参数为single时，仅识别画面中间区域，对应单人场景；当参数为multiple时，全屏识别多人的人体姿态
    :param inputType: 输入数据的类型，str类型
        参数值：video、image；输入为图片时，需按行为顺序命名，例如：frame-0001.jpg、frame-0002.jpg、……
        frame-0001.jpg图片的动作发生在frame-0002.jpg之前
    :param framePerSecond: 每秒抽取多少帧进行计算，int类型，-1为使用视频的原始帧率
    :return: 人体姿态数据列表
        列表中的元素是由本帧图片名和关键点坐标列表组成的字典
        17个关键点依次为：鼻子、左眼、右眼、左耳、右耳、左肩、右肩、左肘、右肘、左手、右手、左胯、右胯、左膝、右膝、左脚、右脚
        视频/图片左上角坐标为(0,0)，单位是像素
    """
    temp_folder = str(uuid.uuid1())
    if inputType == 'video':
        ##### 视频分帧
        pathIn = indir
        video2frames(pathIn, only_output_video_info=True)
        pathOut = 'examples/cut/' + temp_folder + '/'
        temp_path_gen = 'examples/cut/'
        if not os.path.exists(temp_path_gen):
            os.mkdir(temp_path_gen)
        if not os.path.exists(pathOut):
            os.mkdir(pathOut)
        else:
            shutil.rmtree(pathOut)
            os.mkdir(pathOut)

        video2frames(pathIn, pathOut, extract_time_interval=float(framePerSecond), subject=subjectNumber)  # 0.5s取一帧
        inputpath = pathOut

    elif args.form == 'image':
        inputpath = indir
    else:
        print('input error')
        exit()
    args.inputpath = inputpath

    temp_path_res = 'examples/res/'
    if not os.path.exists(temp_path_res):
        os.mkdir(temp_path_res)

    path2 = 'examples/res/' + temp_folder + '/'
    if os.path.exists(path2):
        shutil.rmtree(path2)
    args.outputpath = path2

    inputlist = args.inputlist
    mode = args.mode
    if not os.path.exists(args.outputpath):
        os.mkdir(args.outputpath)

    if len(inputlist):
        im_names = open(inputlist, 'r').readlines()
    elif len(inputpath) and inputpath != '/':
        for root, dirs, files in os.walk(inputpath):
            im_names = files
    else:
        raise IOError('Error: must contain either --indir/--list')
    # Load input images
    data_loader = ImageLoader(im_names, batchSize=args.detbatch, format='yolo').start()

    # Load detection loader
    print('Loading YOLO model..')
    sys.stdout.flush()
    det_loader = DetectionLoader(data_loader, batchSize=args.detbatch).start()
    det_processor = DetectionProcessor(det_loader).start()

    # Load pose model
    pose_dataset = Mscoco()
    if args.fast_inference:
        pose_model = InferenNet_fast(4 * 1 + 1, pose_dataset)
    else:
        pose_model = InferenNet(4 * 1 + 1, pose_dataset)
    pose_model.cuda()
    pose_model.eval()

    runtime_profile = {
        'dt': [],
        'pt': [],
        'pn': []
    }

    # Init data writer
    writer = DataWriter(args.save_video).start()

    data_len = data_loader.length()
    im_names_desc = tqdm(range(data_len))

    batchSize = args.posebatch
    for i in im_names_desc:
        start_time = getTime()
        with torch.no_grad():
            (inps, orig_img, im_name, boxes, scores, pt1, pt2) = det_processor.read()
            if boxes is None or boxes.nelement() == 0:
                writer.save(None, None, None, None, None, orig_img, im_name.split('/')[-1])
                continue

            ckpt_time, det_time = getTime(start_time)
            runtime_profile['dt'].append(det_time)
            # Pose Estimation

            datalen = inps.size(0)
            leftover = 0
            if (datalen) % batchSize:
                leftover = 1
            num_batches = datalen // batchSize + leftover
            hm = []
            for j in range(num_batches):
                inps_j = inps[j * batchSize:min((j + 1) * batchSize, datalen)].cuda()
                hm_j = pose_model(inps_j)
                hm.append(hm_j)
            hm = torch.cat(hm)
            ckpt_time, pose_time = getTime(ckpt_time)
            runtime_profile['pt'].append(pose_time)
            hm = hm.cpu()
            writer.save(boxes, scores, hm, pt1, pt2, orig_img, im_name.split('/')[-1])

            ckpt_time, post_time = getTime(ckpt_time)
            runtime_profile['pn'].append(post_time)

        if args.profile:
            # TQDM
            im_names_desc.set_description(
                'det time: {dt:.3f} | pose time: {pt:.2f} | post processing: {pn:.4f}'.format(
                    dt=np.mean(runtime_profile['dt']), pt=np.mean(runtime_profile['pt']),
                    pn=np.mean(runtime_profile['pn']))
            )

    print('===========================> Finish Model Running.')
    if (args.save_img or args.save_video) and not args.vis_fast:
        print('===========================> Rendering remaining images in the queue...')
        print(
            '===========================> If this step takes too long, you can enable the --vis_fast flag to use fast rendering (real-time).')
    while (writer.running()):
        pass
    writer.stop()
    final_result = writer.results()
    write_json(final_result, args.outputpath)
    with open(args.outputpath + 'alphapose-results.json') as f:
        res_back = json.load(f)

    #删除临时文件
    if os.path.exists(pathOut):
        shutil.rmtree(pathOut)
    if os.path.exists(path2):
        shutil.rmtree(path2)

    return res_back


def pose_data(indir, subjectNumber, inputType, framePerSecond):
    """
    :param indir: 视频或图片的本地路径
    :param subjectNumber: 识别对象的数量，str类型
        参数值：single、multiple；当参数为single时，仅识别画面中间区域，对应单人场景；当参数为multiple时，全屏识别多人的人体姿态
    :param inputType: 输入数据的类型，str类型
        参数值：video、image；输入为图片时，需按行为顺序命名，例如：frame-0001.jpg、frame-0002.jpg、……
        frame-0001.jpg图片的动作发生在frame-0002.jpg之前
    :param framePerSecond: 每秒抽取多少帧进行计算，int类型，-1为使用视频的原始帧率
    :return: 人体姿态数据列表
        列表中的元素是由本帧图片名和关键点坐标列表组成的字典
        17个关键点依次为：鼻子、左眼、右眼、左耳、右耳、左肩、右肩、左肘、右肘、左手、右手、左胯、右胯、左膝、右膝、左脚、右脚
        视频/图片左上角坐标为(0,0)，单位是像素
    """
    # 获取AlphaPose原始数据
    data = alpha_pose(indir, subjectNumber, inputType, framePerSecond)

    for item in data:
        # 移除category_id, 1 for person
        del item['category_id']
        # 移除score, confidence score
        del item['score']
        keypoints = item['keypoints']

        # 移除坐标点的confidence score
        for index in range(len(keypoints) - 1, 0, -1):
            if (index + 1) % 3 == 0:
                del keypoints[index]

    return data


if __name__ == '__main__':
    print('pose_data')

    start_time = time.time()
    result = pose_data('/userhome/video/9_1.mp4', 'single', 'video', 12)
    print(result)

    print(round(time.time() - start_time, 3), 'seconds')

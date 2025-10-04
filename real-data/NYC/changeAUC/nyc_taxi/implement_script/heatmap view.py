# import matplotlib.pyplot as plt
# import numpy as np
# import pickle as pkl
#
#
# import os
# import pickle as pkl
# import random
# import sys
# import numpy as np
# # sys.path.insert(0, "./code/py")
# # if os.getenv("SLURM_SUBMIT_HOST") == "pronto.las.iastate.edu":
# #     root_dir = "/work/LAS/zhanruic-lab/rohitk/git_repos_data/changeAUC/"
# #     sys.path.insert(0, "/work/LAS/zhanruic-lab/rohitk/git_repos_data/changeAUC/output")
# #     sys.path.insert(0, "/work/LAS/zhanruic-lab/rohitk/git_repos_data/changeAUC/data")
# # elif os.getenv("SLURM_SUBMIT_HOST") == "hpc2021":
# #     root_dir = "/lustre1/u/rohitisu/git_repos_data/changeAUC/"
# #     sys.path.insert(0, "/lustre1/u/rohitisu/git_repos_data/changeAUC/output")
# #     sys.path.insert(0, "/lustre1/u/rohitisu/git_repos_data/changeAUC/data")
# # else:
# #     root_dir = ""
# #     sys.path.insert(0, "./output")
# #     sys.path.insert(0, "./data")
# # from get_change_point.get_multiple_change_point_v1 import get_multiple_change_point
# # from get_change_point.get_change_point_v1 import get_change_point
#
# root_dir = ""
# sys.path.insert(0, "./output")
# sys.path.insert(0, "./data")
#
# # === Step 1: 加载数据 ===
# with open(root_dir + "data/fhv_nyc/heatmaps_color_numeric.pkl", "rb") as f:
#     heatmaps_array = pkl.load(f)
#
# # === Step 2: 查看数据维度和类型 ===
# print("数据类型:", type(heatmaps_array))
# print("数组维度:", np.shape(heatmaps_array))
#
# # === Step 3: 可视化前5张热力图 ===
# num_to_show = 10
# for i in range(min(num_to_show, len(heatmaps_array))):
#     plt.figure(figsize=(6, 5))
#     plt.imshow(heatmaps_array[i], cmap='viridis')  # 或试试 'hot', 'plasma', 'coolwarm' 等
#     plt.colorbar()
#     plt.title(f"Heatmap #{i}")
#     plt.tight_layout()
#     plt.savefig(f"heatmap_{i}.png")  # 保存为图片
#     # plt.show()  # 如果你想直接显示到屏幕上而不是保存
#     plt.close()
#

#
# import os
# import pickle as pkl
# import numpy as np
# from PIL import Image
#
# root_dir = ""
# output_dir = "grey_figure"
# os.makedirs(output_dir, exist_ok=True)
#
# # === Step 1: 加载数据 ===
# with open(root_dir + "data/fhv_nyc/heatmaps_color_numeric.pkl", "rb") as f:
#     heatmaps_array = pkl.load(f)
#
# def rgb2gray(rgb):
#     return np.dot(rgb[..., :3], [0.2989, 0.5870, 0.1140])
#
# num_to_process = 10
# for i in range(min(num_to_process, len(heatmaps_array))):
#     img = heatmaps_array[i]
#
#     if img.ndim == 3 and img.shape[-1] == 3:
#         gray_img = rgb2gray(img)
#     elif img.ndim == 2:
#         gray_img = img
#     else:
#         raise ValueError(f"第{i}张图片格式无法识别，shape={img.shape}")
#
#     gray_uint8 = (255 * (gray_img - gray_img.min()) / (gray_img.ptp() + 1e-8)).astype(np.uint8)
#
#     im = Image.fromarray(gray_uint8)
#     im.save(os.path.join(output_dir, f"heatmap_gray_{i}.png"))




import os
import pickle as pkl
import numpy as np
from PIL import Image

root_dir = ""
output_dir = "grey_figure"
os.makedirs(output_dir, exist_ok=True)

def rgb2gray(rgb):
    return np.dot(rgb[..., :3], [0.2989, 0.5870, 0.1140])

num_to_process = 10
target_size = (32, 32)  # 你想要的宽高（像素）

with open(root_dir + "data/fhv_nyc/heatmaps_color_numeric.pkl", "rb") as f:
    heatmaps_array = pkl.load(f)

for i in range(min(num_to_process, len(heatmaps_array))):
    img = heatmaps_array[i]

    if img.ndim == 3 and img.shape[-1] == 3:
        gray_img = rgb2gray(img)
    elif img.ndim == 2:
        gray_img = img
    else:
        raise ValueError(f"第{i}张图片格式无法识别，shape={img.shape}")

    gray_uint8 = (255 * (gray_img - gray_img.min()) / (gray_img.ptp() + 1e-8)).astype(np.uint8)

    im = Image.fromarray(gray_uint8)
    im_resized = im.resize(target_size, resample=Image.BILINEAR)  # 双线性插值缩放
    im_resized.save(os.path.join(output_dir, f"heatmap_gray_{i}.png"))

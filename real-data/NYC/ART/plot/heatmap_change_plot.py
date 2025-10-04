from PIL import Image
import os
import numpy as np
from datetime import datetime, timedelta



### 18-01-01 to 18-02-26
start_date = datetime.strptime("18-01-01", "%y-%m-%d")
end_date = datetime.strptime("18-02-26", "%y-%m-%d")

image_dir = "./daily_heatmaps"

output_dir = "./averaged_heatmaps"
os.makedirs(output_dir, exist_ok=True)

date_list = [(start_date + timedelta(days=i)).strftime("%y-%m-%d")
             for i in range((end_date - start_date).days + 1)]

images = []
for date in date_list:
    img_path = os.path.join(image_dir, f"{date}.jpg")
    if os.path.exists(img_path):
        img = Image.open(img_path).convert("RGB")
        images.append(np.array(img))
    else:
        print(f"missing: {img_path}")

if images:
    # 计算像素平均
    avg_array = np.mean(images, axis=0).astype(np.uint8)
    avg_img = Image.fromarray(avg_array)

    # 保存平均热图
    output_path = os.path.join(output_dir, "avg_18-01-01_to_18-02-26.png")
    avg_img.save(output_path)
    print(f"saved：{output_path}")
else:
    print("no fig")






### 18-02-27 to 18-06-06
start_date = datetime.strptime("18-02-27", "%y-%m-%d")
end_date = datetime.strptime("18-06-06", "%y-%m-%d")

image_dir = "./daily_heatmaps"

output_dir = "./averaged_heatmaps"
os.makedirs(output_dir, exist_ok=True)

date_list = [(start_date + timedelta(days=i)).strftime("%y-%m-%d")
             for i in range((end_date - start_date).days + 1)]

images = []
for date in date_list:
    img_path = os.path.join(image_dir, f"{date}.jpg")
    if os.path.exists(img_path):
        img = Image.open(img_path).convert("RGB")
        images.append(np.array(img))
    else:
        print(f"missing: {img_path}")

if images:
    avg_array = np.mean(images, axis=0).astype(np.uint8)
    avg_img = Image.fromarray(avg_array)

    output_path = os.path.join(output_dir, "avg_18-02-27_to_18-06-06.png")
    avg_img.save(output_path)
    print(f"saved：{output_path}")
else:
    print("no fig")




### 18-06-07 to 19-01-31
start_date = datetime.strptime("18-06-07", "%y-%m-%d")
end_date = datetime.strptime("19-01-31", "%y-%m-%d")

image_dir = "./daily_heatmaps"

output_dir = "./averaged_heatmaps"
os.makedirs(output_dir, exist_ok=True)

date_list = [(start_date + timedelta(days=i)).strftime("%y-%m-%d")
             for i in range((end_date - start_date).days + 1)]

images = []
for date in date_list:
    img_path = os.path.join(image_dir, f"{date}.jpg")
    if os.path.exists(img_path):
        img = Image.open(img_path).convert("RGB")
        images.append(np.array(img))
    else:
        print(f"missing: {img_path}")

if images:
    avg_array = np.mean(images, axis=0).astype(np.uint8)
    avg_img = Image.fromarray(avg_array)

    output_path = os.path.join(output_dir, "avg_18-06-07_to_19-01-31.png")
    avg_img.save(output_path)
    print(f"saved：{output_path}")
else:
    print("no fig")





### 19-02-01 to 19-03-31
start_date = datetime.strptime("19-02-01", "%y-%m-%d")
end_date = datetime.strptime("19-03-31", "%y-%m-%d")

image_dir = "./daily_heatmaps"

output_dir = "./averaged_heatmaps"
os.makedirs(output_dir, exist_ok=True)

date_list = [(start_date + timedelta(days=i)).strftime("%y-%m-%d")
             for i in range((end_date - start_date).days + 1)]

images = []
for date in date_list:
    img_path = os.path.join(image_dir, f"{date}.jpg")
    if os.path.exists(img_path):
        img = Image.open(img_path).convert("RGB")
        images.append(np.array(img))
    else:
        print(f"missing: {img_path}")

if images:
    avg_array = np.mean(images, axis=0).astype(np.uint8)
    avg_img = Image.fromarray(avg_array)

    # 保存平均热图
    output_path = os.path.join(output_dir, "avg_19-02-01_to_19-03-31.png")
    avg_img.save(output_path)
    print(f"saved：{output_path}")
else:
    print("no fig")





### 19-04-01 to 19-09-02
start_date = datetime.strptime("19-04-01", "%y-%m-%d")
end_date = datetime.strptime("19-09-02", "%y-%m-%d")

image_dir = "./daily_heatmaps"

output_dir = "./averaged_heatmaps"
os.makedirs(output_dir, exist_ok=True)

date_list = [(start_date + timedelta(days=i)).strftime("%y-%m-%d")
             for i in range((end_date - start_date).days + 1)]

images = []
for date in date_list:
    img_path = os.path.join(image_dir, f"{date}.jpg")
    if os.path.exists(img_path):
        img = Image.open(img_path).convert("RGB")
        images.append(np.array(img))
    else:
        print(f"missing: {img_path}")

if images:
    avg_array = np.mean(images, axis=0).astype(np.uint8)
    avg_img = Image.fromarray(avg_array)

    # 保存平均热图
    output_path = os.path.join(output_dir, "avg_19-04-01_to_19-09-02.png")
    avg_img.save(output_path)
    print(f"saved：{output_path}")
else:
    print("no fig")






### 19-09-03 to 20-03-14
start_date = datetime.strptime("19-09-03", "%y-%m-%d")
end_date = datetime.strptime("20-03-14", "%y-%m-%d")

image_dir = "./daily_heatmaps"

output_dir = "./averaged_heatmaps"
os.makedirs(output_dir, exist_ok=True)

date_list = [(start_date + timedelta(days=i)).strftime("%y-%m-%d")
             for i in range((end_date - start_date).days + 1)]

images = []
for date in date_list:
    img_path = os.path.join(image_dir, f"{date}.jpg")
    if os.path.exists(img_path):
        img = Image.open(img_path).convert("RGB")
        images.append(np.array(img))
    else:
        print(f"missing: {img_path}")

if images:
    avg_array = np.mean(images, axis=0).astype(np.uint8)
    avg_img = Image.fromarray(avg_array)

    output_path = os.path.join(output_dir, "avg_19-09-03_to_20-03-14.png")
    avg_img.save(output_path)
    print(f"saved：{output_path}")
else:
    print("no fig")





### 20-03-15 to 20-06-07
start_date = datetime.strptime("20-03-15", "%y-%m-%d")
end_date = datetime.strptime("20-06-07", "%y-%m-%d")

image_dir = "./daily_heatmaps"

output_dir = "./averaged_heatmaps"
os.makedirs(output_dir, exist_ok=True)

date_list = [(start_date + timedelta(days=i)).strftime("%y-%m-%d")
             for i in range((end_date - start_date).days + 1)]

images = []
for date in date_list:
    img_path = os.path.join(image_dir, f"{date}.jpg")
    if os.path.exists(img_path):
        img = Image.open(img_path).convert("RGB")
        images.append(np.array(img))
    else:
        print(f"missing: {img_path}")

if images:
    avg_array = np.mean(images, axis=0).astype(np.uint8)
    avg_img = Image.fromarray(avg_array)

    output_path = os.path.join(output_dir, "avg_20-03-15_to_20-06-07.png")
    avg_img.save(output_path)
    print(f"saved：{output_path}")
else:
    print("no fig")






### 20-06-08 to 21-05-05
start_date = datetime.strptime("20-06-08", "%y-%m-%d")
end_date = datetime.strptime("21-05-05", "%y-%m-%d")

image_dir = "./daily_heatmaps"

output_dir = "./averaged_heatmaps"
os.makedirs(output_dir, exist_ok=True)

date_list = [(start_date + timedelta(days=i)).strftime("%y-%m-%d")
             for i in range((end_date - start_date).days + 1)]

images = []
for date in date_list:
    img_path = os.path.join(image_dir, f"{date}.jpg")
    if os.path.exists(img_path):
        img = Image.open(img_path).convert("RGB")
        images.append(np.array(img))
    else:
        print(f"missing: {img_path}")

if images:
    avg_array = np.mean(images, axis=0).astype(np.uint8)
    avg_img = Image.fromarray(avg_array)

    output_path = os.path.join(output_dir, "avg_20-06-08_to_21-05-05.png")
    avg_img.save(output_path)
    print(f"saved：{output_path}")
else:
    print("no fig")






### 21-05-06 to 22-04-01
start_date = datetime.strptime("21-05-06", "%y-%m-%d")
end_date = datetime.strptime("22-04-01", "%y-%m-%d")

image_dir = "./daily_heatmaps"

output_dir = "./averaged_heatmaps"
os.makedirs(output_dir, exist_ok=True)

date_list = [(start_date + timedelta(days=i)).strftime("%y-%m-%d")
             for i in range((end_date - start_date).days + 1)]

images = []
for date in date_list:
    img_path = os.path.join(image_dir, f"{date}.jpg")
    if os.path.exists(img_path):
        img = Image.open(img_path).convert("RGB")
        images.append(np.array(img))
    else:
        print(f"missing: {img_path}")

if images:
    avg_array = np.mean(images, axis=0).astype(np.uint8)
    avg_img = Image.fromarray(avg_array)

    output_path = os.path.join(output_dir, "avg_21-05-06_to_22-04-01.png")
    avg_img.save(output_path)
    print(f"saved：{output_path}")
else:
    print("no fig")






### 22-04-02 to 22-12-31
start_date = datetime.strptime("22-04-02", "%y-%m-%d")
end_date = datetime.strptime("22-12-31", "%y-%m-%d")

image_dir = "./daily_heatmaps"

output_dir = "./averaged_heatmaps"
os.makedirs(output_dir, exist_ok=True)

date_list = [(start_date + timedelta(days=i)).strftime("%y-%m-%d")
             for i in range((end_date - start_date).days + 1)]

images = []
for date in date_list:
    img_path = os.path.join(image_dir, f"{date}.jpg")
    if os.path.exists(img_path):
        img = Image.open(img_path).convert("RGB")
        images.append(np.array(img))
    else:
        print(f"missing: {img_path}")

if images:
    avg_array = np.mean(images, axis=0).astype(np.uint8)
    avg_img = Image.fromarray(avg_array)

    output_path = os.path.join(output_dir, "avg_22-04-02_to_22-12-31.png")
    avg_img.save(output_path)
    print(f"saved：{output_path}")
else:
    print("no fig")






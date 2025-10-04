from PIL import Image
import os
import numpy as np
from datetime import datetime, timedelta



### 20-08-01 to 20-08-31
start_date = datetime.strptime("20-08-01", "%y-%m-%d")
end_date = datetime.strptime("20-08-31", "%y-%m-%d")

image_dir = "./daily_heatmaps"

output_dir = "./null_averaged_heatmaps"
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

    output_path = os.path.join(output_dir, "avg_20-08-01_to_20-08-31.png")
    avg_img.save(output_path)
    print(f"saved：{output_path}")
else:
    print("no fig")







### 20-09-01 to 20-09-30
start_date = datetime.strptime("20-09-01", "%y-%m-%d")
end_date = datetime.strptime("20-09-30", "%y-%m-%d")

image_dir = "./daily_heatmaps"

output_dir = "./null_averaged_heatmaps"
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

    output_path = os.path.join(output_dir, "avg_20-09-01_to_20-09-30.png")
    avg_img.save(output_path)
    print(f"saved：{output_path}")
else:
    print("no fig")






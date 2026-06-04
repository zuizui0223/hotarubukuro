import os
import cv2
import numpy as np
import pandas as pd
from sklearn.cluster import KMeans # Import KMeans
# import openpyxl # pandasでExcel保存する場合は不要

# 1. 必要なライブラリのインストール (通常、ノートブックの先頭で行いますが、スクリプトとしてまとめる場合は含めます)
# %pip install Pillow pandas opencv-python scikit-learn

# --- 初期設定 ---
loaded_images = []
all_petal_masks = [] # 抽出したマスクを格納するリスト
directory = '/content/'

# --- HSV 色域の定義 (調整が必要な場合があります) ---
# Evaluation results (df_evaluation) or visual inspection should guide these parameters.
# For this script, let's assume we are using parameters similar to those explored in the evaluation.
# Example parameters (adjust based on your evaluation findings):
best_lower_pink = np.array([130, 60, 60])
best_upper_pink = np.array([185, 255, 255])
best_lower_white = np.array([0, 0, 180])
best_upper_white = np.array([180, 60, 255])

# モルフォロジー変換用カーネルとイテレーション数 (調整が必要な場合があります)
# Example parameters (adjust based on your evaluation findings):
best_kernel_size = (7, 7)
best_iterations = 3

# K-means クラスタリングのパラメータ (各花弁領域内の主要色の数)
n_dominant_colors_per_petal = 1 # 各花弁領域から抽出する主要色の数


# 2. PNGファイルのリスト取得
all_items = os.listdir(directory)
png_files = [item for item in all_items if os.path.isfile(os.path.join(directory, item)) and item.endswith('.png')]

print(f"Found {len(png_files)} images. Starting processing with refined methods...")

# 3. 画像の読み込みと花弁の領域特定
for png_file in png_files:
    image_path = os.path.join(directory, png_file)
    try:
        img = cv2.imread(image_path)

        if img is not None and img.size > 0:
            loaded_images.append({'filename': png_file, 'image_data': img}) # Store original image data

            # BGR -> HSV 変換
            hsv_image = cv2.cvtColor(img, cv2.COLOR_BGR2HSV)

            # --- マスク生成と強化 ---
            mask_pink = cv2.inRange(hsv_image, best_lower_pink, best_upper_pink)
            mask_white = cv2.inRange(hsv_image, best_lower_white, best_upper_white)
            mask_petals = cv2.bitwise_or(mask_pink, mask_white)

            # ノイズ除去と穴埋め (Opening -> Closing)
            kernel = np.ones(best_kernel_size, np.uint8)
            mask_petals = cv2.morphologyEx(mask_petals, cv2.MORPH_OPEN, kernel, iterations=best_iterations)
            mask_petals = cv2.morphologyEx(mask_petals, cv2.MORPH_CLOSE, kernel, iterations=best_iterations)

            all_petal_masks.append({'filename': png_file, 'petal_mask': mask_petals})

    except Exception as e:
        print(f"Exception while processing image {png_file}: {e}")

print(f"Processed {len(all_petal_masks)} images for petal regions.")

# 4. 各花弁領域からの主要色抽出 (K-meansを使用)
petal_dominant_colors_list = []

for item in all_petal_masks:
    filename = item['filename']
    petal_mask = item['petal_mask']

    # Corresponding original image data is available in loaded_images
    original_image_data = next((img_data['image_data'] for img_data in loaded_images if img_data['filename'] == filename), None)

    if original_image_data is not None:
        # マスクされた領域のピクセルデータを抽出
        # BGR 形式でピクセルを取得
        masked_pixels_bgr = original_image_data[petal_mask > 0]

        if masked_pixels_bgr.shape[0] > n_dominant_colors_per_petal: # クラスタリングに必要なピクセル数があるか確認
            # ピクセルデータをfloat32に変換し、K-meansに渡せる形にリシェイプ
            pixels_for_clustering = np.float32(masked_pixels_bgr.reshape(-1, 3))

            # K-means クラスタリングの実行
            # criteria = (cv2.TERM_CRITERIA_EPS + cv2.TERM_CRITERIA_MAX_ITER, 10, 1.0) # OpenCV criteria
            # ret, label, center = cv2.kmeans(pixels_for_clustering, n_dominant_colors_per_petal, None, criteria, 10, cv2.KMEANS_RANDOM_CENTERS) # OpenCV kmeans

            # scikit-learn の KMeans を使用 (よりシンプル)
            kmeans = KMeans(n_clusters=n_dominant_colors_per_petal, random_state=42, n_init=10) # Added n_init
            kmeans.fit(pixels_for_clustering)
            dominant_colors_bgr = kmeans.cluster_centers_

            # 抽出された主要色（BGR）をリストに追加
            # 各主要色を個別のエントリとして追加することも、リストとして保持することも可能
            # ここではシンプルに、1つの主要色を抽出してRGBに変換しリストに追加
            if n_dominant_colors_per_petal == 1:
                 # BGR -> RGB 変換
                 dominant_color_rgb = [dominant_colors_bgr[0][2], dominant_colors_bgr[0][1], dominant_colors_bgr[0][0]]
                 petal_dominant_colors_list.append({'filename': filename, 'dominant_petal_rgb': dominant_color_rgb})
            else:
                 # 複数の主要色を保持する場合は、構造を調整
                 # 例: dominant_petal_rgbs: [[R1,G1,B1], [R2,G2,B2], ...]
                 dominant_colors_rgb_list = [[c[2], c[1], c[0]] for c in dominant_colors_bgr]
                 petal_dominant_colors_list.append({'filename': filename, 'dominant_petal_rgbs': dominant_colors_rgb_list})


        else:
            # ピクセル数が足りない場合やマスク領域がない場合
            if n_dominant_colors_per_petal == 1:
                 petal_dominant_colors_list.append({'filename': filename, 'dominant_petal_rgb': [np.nan, np.nan, np.nan]})
            else:
                 petal_dominant_colors_list.append({'filename': filename, 'dominant_petal_rgbs': []}) # 空のリスト


    else:
        print(f"Original image data not found for {filename} during dominant color extraction.")


print(f"Extracted dominant petal colors for {len(petal_dominant_colors_list)} images.")


# 5. 結果のDataFrame作成
# ここでは、各花弁から1つの主要色を抽出した場合のDataFrameを作成します。
# 複数の主要色を抽出した場合は、DataFrameの構造を調整する必要があります。
df_dominant_petal_rgb = pd.DataFrame(petal_dominant_colors_list)

# 'dominant_petal_rgb' (RGB 形式) を R, G, B の個別の列に分割
if not df_dominant_petal_rgb.empty and 'dominant_petal_rgb' in df_dominant_petal_rgb.columns:
    df_dominant_petal_rgb[['Dominant_R', 'Dominant_G', 'Dominant_B']] = pd.DataFrame(
        df_dominant_petal_rgb['dominant_petal_rgb'].tolist(),
        index=df_dominant_petal_rgb.index
    )

    # 元の 'dominant_petal_rgb' 列は削除
    df_dominant_petal_rgb = df_dominant_petal_rgb.drop(columns=['dominant_petal_rgb'])

    print("\nDominant Petal RGB DataFrame:")
    display(df_dominant_petal_rgb)
else:
    print("\nNo dominant petal RGB data to display.")


# 6. Excelファイルへの保存 (pandasを使用)
output_filename_dominant = 'dominant_petal_rgb_values.xlsx'
if not df_dominant_petal_rgb.empty:
    df_dominant_petal_rgb.to_excel(output_filename_dominant, index=False)
    print(f"\nDominant petal RGB data saved to '{output_filename_dominant}' using pandas.")
else:
     print(f"\nNo data to save to '{output_filename_dominant}'.")

# 7. タスク完了 (実行後、ユーザーにExcelファイルのダウンロードを促す)
print("\nタスク完了: 各花弁の主要なRGB値の抽出とExcelファイルへの保存が完了しました。")
print(f"Excelファイルは '{output_filename_dominant}' として /content/ ディレクトリに保存されています。")




    
    
    



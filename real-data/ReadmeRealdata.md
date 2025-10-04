# Well-log Dataset
- Load the data from the R package "changepoint.influence".
- File `localization/fun.R` includes all the functions required for the analysis of well-log dataset.
- Run `localization/main.R` to obtain the result of localization for well-log dataset.
- Run file `localization/plot.R` to generate visualization results in Fig.4.


# NYC TLC trip record dataset
- File `transformation/heatmaps_color_numeric.pkl` contains the dataset.
- Run `transformation/clustering.py` to obtain the clustering transformation result.
- Run `localization and post_dection/nyc_loc.R` to perform changepoint testing and obtain the **localization** results in Table 5. 
- Run `localization_post/post_changeAUC.R`, `localization_post/post_changeforest.R`, and  
  `localization_post/post_Inspect.R` to obtain the **post-detection** results in Table 6.
- Run `plot/heatmap_change_plot.py` and `plot/heatmap_nochange_plot.py` to generate visualization results in Fig.5.
- Required package version:</small> python=3.7, numpy==1.21, scikit-learn, matplotlib==3.2.2, scipy==1.7.3, tensorflow==2.8.2, geopandas==0.10.2, Pillow==9.5.0, pandas==1.3.5, pyarrow==10.0.1.



# MNIST dataset
- Data for four distinct scenarios are saved in the following files: 
  - scenario (i): `datasets/0_selected_x_150.npy", "datasets/0_selected_y_150.npy`
  - scenario (ii): `datasets/383_selected_x_60_and_30.npy", "datasets/383_selected_y_60_and_30.npy`
  - scenario (iii): `datasets/123_selected_x_60_and_30.npy", "datasets/123_selected_y_60_and_30.npy`
  - scenario (iv): `datasets/0to5_selected_x_100.npy", "datasets/0to5_selected_y_100.npy`
- Use the code from https://github.com/XifengGuo/DEC-keras to implement the Deep Embedding Clustering method.
- Required package version:  python==3.6, scikit-learn==0.21, keras==2.0.9, tensorflow==1.15 , pandas==1.1.5, PyCharm==2021.
- Run `labels/DEC code/run_exp.py` in each folder to obtain the clustering label. Clustering label results are saved in `label_0.csv`, `label_383.csv`, `label_123.csv`, `label_0to5.csv`.
- Run `test_0.R` to obtain the p-value for the test under scenario (i).
- Run `test_local_0to5.R`, `test_local_123`, `test_local_383.R` to obtain the p-value for the test and changepoint localization with inference under scenario (ii), (iii) and (iv) in   Fig.S.4, Table S.2, Table S.3.
- Files "test_fun.R",  "localization.R",  "post_fun.R" respectively contain all the functions required for testing, localization with inference, and post-detection inference.






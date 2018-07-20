# mri_qap_report
Scripts to generate mri quality report from QAP output for HBN.

QAP Measures:

Spatial Anatomical
Signal-to-Noise Ratio (SNR) [snr]: The mean intensity within gray matter divided by the standard deviation of the values outside the brain. Higher values are better 1.
Contrast to Noise Ratio (CNR) [cnr]: The mean of the gray matter intensity values minus the mean of the white matter intensity values divided by the standard deviation of the values outside the brain. Higher values are better 1.
Foreground to Background Energy Ratio [fber]: The variance of voxels inside the brain divided by the variance of voxels outside the brain. Higher values are better.
Percent Artifact Voxels (Qi1) [qi1]: The proportion of voxels outside the brain with artifacts to the total number of voxels outside the brain. Lower values are better 2.
Smoothness of Voxels (FWHM) [fwhm, fwhm_x, fwhm_y, fwhm_z]: The full-width half maximum (FWHM) of the spatial distribution of the image intensity values in voxel units. Lower values are better 3.
Entropy Focus Criterion (EFC) [efc]: The Shannon entropy of voxel intensities proportional to the maximum possibly entropy for a similarly sized image. Indicates ghosting and head motion-induced blurring. Lower values are better 4.
Spatial Functional
Ghost to Signal Ratio (GSR) [ghost_x, ghost_y or ghost_z]: A measure of the mean signal in the areas of the image that are prone to ghosting based off the phase encoding direction. Lower values are better. 5

Temporal Functional
Foreground to Background Energy Ratio [fber]: The variance of voxels inside the brain divided by the variance of voxels outside the brain. Higher values are better.
Smoothness of Voxels (FWHM)]: The full-width half maximum (FWHM) of the spatial distribution of the image intensity values in voxel units. Lower values are better 3.
Entropy Focus Criterion (EFC) [efc]: The Shannon entropy of voxel intensities proportional to the maximum possibly entropy for a similarly sized image. Indicates ghosting and head motion-induced blurring. Lower values are better 4.
Standardized DVARS [dvars]: The average change in mean intensity between each pair of fMRI volumes in a series scaled to make comparisons across scanning protocols possible. Lower values are better 6.
Outlier Detection [outlier]: The mean count of outliers found in each volume using the 3dToutcountcommand from AFNI. Lower values are better 7.
Global Correlation [gcorr]: The average correlation of all pairs of voxel time series inside of the brain. Illustrates differences between data due to motion/physiological noise/imaging artifacts (such as signal bleeding). Values closer to zero are better. 8
Median Distance Index [quality]: The mean distance (1 – spearman’s rho) between each time point’s volume and the median volume using AFNI’s 3dTqual command. Lower values are better 7.
Mean RMSD [mean_fd]: A measure of subject head motion, which compares the motion between the current and previous volumes. This is calculated by summing the absolute value of displacement changes in the x, y and z directions and rotational changes about those three axes. The rotational changes are given distance values based on the changes across the surface of a 80mm radius sphere. Lower values are better 910.

